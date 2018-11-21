;;
;; display.lisp
;;

(in-package :rl)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display-ish code

(defun debug-message (e fmt &rest args)
  "Show a little message for debugging."
  (declare (type line-editor e))
  (with-saved-cursor (*terminal*)
    (tt-cursor-off)
    (tt-move-to 5 5)		; The only place we should call this
    (tt-standout t)
    ;; (apply #'tt-format (cons (line-editor-terminal e) (cons fmt args)))
    (apply #'terminal-format (cons (line-editor-terminal e) (cons fmt args)))
    (tt-standout nil)
    (tt-cursor-on)))

(defun log-message (e fmt &rest args)
  (when (debugging e)
    (push (apply #'format nil fmt args) (line-editor-debug-log e))))

(defun show-message-log (e)
  "Show the debugging message log."
  (declare (type line-editor e))
  (tt-save-cursor)
  (tt-cursor-off)
  (tt-bold t)
  (loop :for i :from 0 :below 8
     :for dd :in (line-editor-debug-log e)
     :do
     (tt-move-to (+ 10 i) 40)		; The “only” place we should call this
     (tt-erase-to-eol)
     (tt-write-string dd))
  (tt-bold nil)
  (tt-cursor-on)
  (tt-restore-cursor))

(defun message-pause (e fmt &rest args)
  "Show a little message for debugging."
  (apply #'message e fmt args)
  (get-a-char e))

(defun beep (e fmt &rest args)
  "Beep or display an error message."
  (when (debugging e)
    (apply #'message e fmt args))
  (tt-beep))

(defun string-display-length (str)
  (typecase str
    (string (display-length str))
    (fat-string (display-length str))
    (fatchar-string (display-length (fatchar-string-to-string str)))))

(defmacro without-messing-up-cursor ((e) &body body)
  (let ((old-row (gensym "OLD-ROW"))
	(old-col (gensym "OLD-COL")))
  `(let ((,old-row (screen-relative-row ,e))
	 (,old-col (screen-col ,e)))
     (prog1 ,@body
       (if (< ,old-row (screen-relative-row ,e))
	   (tt-up (- (screen-relative-row ,e) ,old-row))
	   (tt-down (- ,old-row (screen-relative-row ,e))))
       (tt-beginning-of-line)
       (tt-forward ,old-col)
       (setf (screen-relative-row ,e) ,old-row
	     (screen-col ,e) ,old-col)))))

(defun buffer-length-to (buf to-length)
  (loop :with i = 0
     :for buf-i = 0 :then (1+ buf-i)
     :while (< i to-length)
     :do (incf i (display-length (aref buf buf-i)))
     :finally (return buf-i)))

(defun replace-buffer (e str)
  "Replace the buffer with the given string STR."
  ;;(declare (type string str))
  (with-slots (buf point) e
    (setf point (length str))
    (buffer-delete e 0 (length buf))
    (buffer-insert e 0 str)))

(defun use-hist (e)
  "Replace the current line with the current history line."
  ;; @@@ Problems:
  ;; - The current line is lost.
  ;; - The undo history (and all other buffer properties) are not
  ;;   retained.
  (without-undo (e)
    (if (history-current (context e))
	(replace-buffer e (history-current (context e)))
	(replace-buffer e ""))))

;; If performance was really a problem, we could carefully maintain some of
;; the line endings. But for a line editor, unless we're running on very slow
;; systems, this seems unlikely to be a problem. It's not really advisable to
;; edit megabytes of text in a line editor. If someone wants to do a really
;; big paste, using an unprocessed read is better. Also for big pastes,
;; there's timing tricks that one can do with the lower level read.

(defun calculate-line-endings (e &key (buffer (buf e)) (start (start-col e)))
  "Return a list of pairs of character positions and columns, in reverse order
of character position, which should be the end of the displayed lines in the
buffer."
  (let (endings
	(col start)			; Start after the prompt
	(cols (terminal-window-columns (line-editor-terminal e)))
	(char-width 0)
	(last-col start)
	(i 0)
	c cc)
    (dbugf :rl "endings start ~s~%" start)
    ;; (if-dbugf (:rl)
    ;; 	      (symbol-call :deblarg :debugger-wacktrace 20))
    (loop :while (< i (olength buffer)) :do
       (setf c (oelt buffer i)
	     cc (if (fatchar-p c) (fatchar-c c) c))
       (if (char= cc #\newline)
	   (progn
	     (push (cons (1- i) last-col) endings)
	     (setf last-col col)
	     (setf col 0))
	   (progn
	     (setf char-width (display-length (oelt buffer i)))
	     (if (> (+ col char-width) cols)
		 (progn
		   (push (cons (1- i) last-col) endings)
		   (setf last-col col)
		   (setf col char-width))
		 (progn
		   (setf last-col col)
		   (incf col char-width)))))
       (incf i))
    ;; Make sure we get the last one
    (when (> (+ col char-width) cols)
      (push (cons (1- i) last-col) endings))
    endings))

(defun line-ending (pos endings)
  "Return the line ending at character position POS, from the line ENDINGS,
or NIL is there is none."
  (cdr (assoc pos endings)))

;; Here's the problem:
;;
;; People can put any old stuff in the prompt that they want. This includes
;; things that move the cursor around, characters that might be of different
;; widths, escape sequences that may or may not move the cursor. So unless we
;; emulate the terminal exactly, just to figure out where the cursor is after
;; the prompt, things can get messed up.
;;
;; We could be like other shells and require that you delimit non-echoing
;; characters yourself and allow you to specifiy an output width for
;; characters. Not only is that annoying, but it won't always work.
;;
;; Since emulating the terminal seems infeasible, unless we wrapped ourselves
;; in an emulation layer like screen or tmux, if we want to be sure to get
;; things right, we are stuck with with asking the terminal where the cursor
;; might be.
;;
;; The problem with asking the terminal, is that we have to output something,
;; and then read the coordinates back in. But there might be a bunch of input,
;; like a giant paste or something, or typing ahead, already in the terminal's
;; input queue, in front of the response to our "where is the cursor" query,
;; which blocks us from getting an answer.
;;
;; So we have to read all input available, BEFORE asking where the cursor
;; is. This is the reason for all the otherwise useless 'eat-typeahead' and
;; 'tty-slurp'ing. Of course this whole thing is quite kludgey and I think we
;; should really be able ask the terminal where the cursor is with a nice
;; _function call_, not going through the I/O queue. Of course that would
;; require the terminal to be in our address space, or to have a separate
;; command channel if it's far, far away.
;;
;; There is still a small opportunity for a race condition, between outputing
;; the query, and getting an answer back, but it seems unlikely. I wonder if
;; there's some way to 'lock' the terminal input queue for that time.
;;

(defun finish-all-output ()
  "Makes all output be in Finnish."
  (when (not (environment-variable "EMACS")) ; XXX so wrong
    ;;#+ccl (ccl::auto-flush-interactive-streams) ;; Jiminy Crickets!
    (finish-output *standard-output*)
    (finish-output *terminal-io*)
    (finish-output t)
    (finish-output)
    ;(finish-output *standard-input*)
    )
  (tt-finish-output)
  )

;; @@@ As a hack, on terminal-ansi we could check the string and see if there's
;; anything weird in it, and if not, don't bother to do the slow asking the
;; terminal to measure it for us.

(defun probably-safe (string)
  "Return true if STRING probably doesn't have any weird control characters
in it."
  (not (oposition-if (_ (and (control-char-p _)
			     (ochar/= _ #\newline))) string)))

#|
(defun do-prefix (e prompt-str)
  "Output a prefix."
  (flet ((write-it ()
	   (write prompt-str :stream *terminal* :escape nil :pretty nil)))
    (if (probably-safe prompt-str)
	(progn
	  (write-it)
	  (finish-output *terminal*)
	  (setf (prompt-height e) (ocount #\newline prompt-str))
	  (let ((saved-col (screen-col e))
		(saved-row (screen-relative-row e)))
	    (move-over e (olength prompt-str) :start 0 :buffer prompt-str
		       :move nil)
	    ;; really update the coordinates
	    (setf (screen-col e) (+ saved-col (screen-col e))
		  (screen-relative-row e) (+ saved-row (screen-relative-row e))
		  (start-col e) (screen-col e)
		  (start-row e) (screen-relative-row e))))
	(let (row col start-row)
	  (finish-all-output)
	  (multiple-value-setq (row col)
	    (terminal-get-cursor-position (line-editor-terminal e)))
	  (setf start-row row)
	  ;;(tt-write-string prompt-str)
	  (write-it)
	  ;;(finish-all-output)
	  ;; (tt-finish-output)
	  ;; (eat-typeahead e)
	  (multiple-value-setq (row col)
	    (terminal-get-cursor-position (line-editor-terminal e)))
	  (setf (screen-relative-row e) row ; @@@@@@@@
		(screen-col e) col
		;; save end of the prefix as the starting column
		(start-col e) col
		(start-row e) row
		(prompt-height e) (1+ (- row start-row)))))
    (log-message e "prompt-height = ~s" (prompt-height e))))
|#

#|
(defun do-prompt (e prompt output-prompt-func &key only-last-line)
  "Output the prompt in a specified way."
;  (format t "e = ~w prompt = ~w output-prompt-func = ~w~%"
;	  e prompt output-prompt-func)
  (let* ((s (if (and output-prompt-func
		     (or (functionp output-prompt-func)
			 (fboundp output-prompt-func)))
		(with-output-to-string (*standard-output*)
		  (log-message e "do-prompt output-prompt-func -> ~s"
			       output-prompt-func)
		  (or (ignore-errors (funcall output-prompt-func e prompt))
		      "Your prompt Function failed> "))
		(progn
		  (log-message e "do-prompt default-output-prompt")
		  (default-output-prompt e prompt))))
	 last-newline)
    ;; (log-message e "do-prompt only-last-line = ~s" only-last-line)
    ;; (log-message e "do-prompt last-newline = ~s"
    ;; 		 (oposition #\newline s :from-end t))
    (when (and only-last-line
	       (setf last-newline (oposition #\newline s :from-end t)))
      (setf s (osubseq s (1+ last-newline)))
      (log-message e "partial prompt ~s" s))
    ;; (log-message e "do-prompt s = ~s ~s" (olength s) s)
    (do-prefix e s)))
|#

(defun make-prompt (e)
  "Return the prompt as a string."
  (let* ((prompt (prompt-string e))
	 (output-prompt-func (prompt-func e))
	 (s (if (and output-prompt-func
		     (or (functionp output-prompt-func)
			 (fboundp output-prompt-func)))
		(with-output-to-string (*standard-output*)
		  (log-message e "do-prompt output-prompt-func -> ~s"
			       output-prompt-func)
		  (or (ignore-errors (funcall output-prompt-func e prompt))
		      "Your prompt Function failed> "))
		(progn
		  (log-message e "do-prompt default-output-prompt")
		  (default-output-prompt e prompt))))
	 #| last-newline |#)
    ;; (log-message e "do-prompt only-last-line = ~s" only-last-line)
    ;; (log-message e "do-prompt last-newline = ~s"
    ;; 		 (oposition #\newline s :from-end t))
    ;; (when (and only-last-line
    ;; 	       (setf last-newline (oposition #\newline s :from-end t)))
    ;;   (setf s (osubseq s (1+ last-newline)))
    ;;   (log-message e "partial prompt ~s" s))
    ;; (log-message e "do-prompt s = ~s ~s" (olength s) s)
    s))

#|
(defun redraw-line (e)
  "Erase and redraw the whole line."
  (tt-move-to-col 0)
  (tt-erase-to-eol)
  (setf (screen-col e) 0)
  (do-prompt e (prompt-string e) (prompt-func e) :only-last-line t)
  (finish-output (terminal-output-stream (line-editor-terminal e)))
  (display-buf e)
  (with-slots (point buf) e
    (when (< point (length buf))
      (move-over e (- (- (length buf) point)) :start (length buf))))
  (setf (need-to-redraw e) nil))
|#

#|

(defun recolor-line (e)
  "Erase and redraw the whole line."
  (with-slots (buf old-line need-to-recolor (point inator::point)) e
    (dbugf :recolor "buf = ~s~%old = ~s~%" buf old-line)
    (dbugf :recolor "point start ~s~%" point)
    (dbugf :recolor "start screen-col = ~s~%" (screen-col e))
    (let ((pos point) (i 0) (fs (make-fat-string :string buf)))
      (flet ((fake-move (n &key (really t) (start pos))
	       (dbugf :recolor "move by ~s~%" n)
	       (if start
		   (move-over e n :start start :move really)
		   (move-over e n :move really))
	       (incf pos n)))
	(loop
	   :with start
	   :and max = (min (length buf) (length old-line))
	   :while (< i max) :do
	     (when (fatchar/= (aref (buf e) i) (aref (old-line e) i))
	       ;; Go to the start of the difference
	       (fake-move (- i pos))
	       ;; Look to the end of the difference
	       (setf start i)
	       (loop :while (and (< i max)
				 (fatchar/= (aref (buf e) i)
					    (aref (old-line e) i)))
		  :do (incf i))
	       ;; Rely on tt-write-string to switch colors and attrs
	       (tt-write-string fs :start start :end i)
	       (dbugf :recolor "write [~s - ~s]~%" start i)
	       ;; (fake-move (+ pos (- i start)) :really nil))
	       (editor-update-pos-for-string e (osubseq fs start i)))
	     (incf i))
	(when (< i (length buf))	; buf is bigger than old-line
	  (fake-move (- i pos))
	  (tt-write-string fs :start i)	; write the rest
	  (editor-update-pos-for-string e (osubseq fs i))
	  (dbugf :recolor "end write ~s~%" i))
	;; Move back to point
	(if (< point (length buf))
	    (move-over e (- point pos))
	    (progn
	      (move-over e (- (1- point) pos))
	      ;; (incf (screen-col e))
	      ))
	(dbugf :recolor "end screen-col = ~s~%" (screen-col e))
	(setf need-to-recolor nil)))))
|#

(defun relative-move-to-row (old new)
  (if (< new old)
      (tt-up (- old new))
      (tt-down (- new old))))

;; (with-dbugf-to :rl "/dev/pts/2" (repl :terminal-type :crunch))

;; An update that probably requires an optimizing terminal to be at all
;; efficient.
(defun BROKEN-redraw-display (e &key erase)
  (with-slots (screen-relative-row screen-col prompt-str buf-str
	       start-col start-row last-line prompt-height point) e
    (let* ((prompt (make-prompt e))
	   (prompt-lines (calculate-line-endings e :buffer prompt :start 0))
	   buf-lines row col)
      (relative-move-to-row screen-relative-row 0)
      (tt-move-to-col 0)
      (multiple-value-setq (row col)
	(terminal-get-cursor-position *terminal*))
      (dbugf :rl "current row = ~s col = ~s~%" row col)

      (tt-move-to-col 0)
      (tt-erase-to-eol)
      (when erase
	(tt-erase-below))
      (write prompt :stream *terminal* :escape nil :pretty nil)
      (multiple-value-setq (start-row start-col)
	(terminal-get-cursor-position *terminal*))
      (setf start-row (- start-row row)
	    prompt-height start-row)
      (dbugf :rl "start-row = ~s~%" start-row)

      (setf buf-lines (calculate-line-endings e :start start-col))
      (tt-write-string (buf-str e))

      ;; Erase any part of a previous longer line 
      (let ((new-last
	     (- (terminal-get-cursor-position *terminal*) row)
	     ;;(length buf-lines)
	    ))
	(when last-line
	  (loop :for l :from new-last :to last-line :do
               (tt-erase-to-eol)
	       (tt-down)
	       (tt-move-to-col 0)))
	(setf last-line new-last))
      (dbugf :rl "last-line = ~s~%" last-line)

      (relative-move-to-row (1+ last-line) start-row)
      ;; (relative-move-to-row last-line start-row)
      (tt-move-to-col start-col)
      (tt-write-string (buf-str e) :end point)
      (setf screen-relative-row
      	    (- (terminal-get-cursor-position *terminal*) row))
      ;; (setf screen-relative-row
      ;; 	    (+ (length prompt-lines) (length buf-lines)))
      (dbugf :rl "screen-relative-row = ~s~%" screen-relative-row)
      )))

;; @@@ Not *as* broken, but not right. fails clearing EOL
(defun redraw-display (e &key erase)
  (with-slots (buf-str prompt-height point start-row start-col
	       screen-relative-row) e
    (let* ((prompt (make-prompt e))
	   (prompt-lines (calculate-line-endings e :buffer prompt :start 0))
	   buf-lines)
      (relative-move-to-row screen-relative-row 0)
      (tt-move-to-col 0)
      (tt-erase-to-eol)

      (write prompt :stream *terminal* :escape nil :pretty nil)
      (tt-erase-to-eol)
      (multiple-value-setq (start-row start-col)
	(terminal-get-cursor-position *terminal*))
      (setf prompt-height (length prompt-lines))

      (setf buf-lines (calculate-line-endings e :start start-col))
      (tt-write-string (buf-str e))

      ;; (relative-move-to-row (1+ last-line) start-row)
      (when (not (zerop (length buf-lines)))
	(tt-up (length buf-lines)))
      (tt-move-to-col start-col)
      (tt-write-string (buf-str e) :end point)
      (setf screen-relative-row
      	    (+ (length prompt-lines) (length buf-lines))))))

(defmethod update-display ((e line-editor))
  (redraw-display e))

(defun tmp-prompt (e fmt &rest args)
  (declare (ignore e))
  (tt-move-to-col 0)
  (tt-erase-to-eol)
  ;; (setf (screen-col e) 0)
  ;; (do-prefix e (apply #'format `(nil ,fmt ,@args)))
  (tt-write-string (apply #'format `(nil ,fmt ,@args)))
  (tt-finish-output))

(defun tmp-message (e fmt &rest args)
  ;;(apply #'tmp-prompt e fmt args)
  (with-slots (screen-col screen-relative-row buf point temporary-message) e
    (let ((saved-col screen-col)
	  (saved-row screen-relative-row)
	  (offset 0)
	  output-string endings)
      ;; (move-over e (- (length buf) point))
      (tt-scroll-down 1)
      (when (= saved-row (1- (tt-height))) ; at the bottom row
	(incf offset))			   ; at least one line was scrolled
      (tt-move-to-col 0)
      (tt-erase-to-eol)
      (setf output-string (with-output-to-fat-string (fs)
			    (apply #'format fs fmt args))
	    endings (calculate-line-endings
		     e :buffer (fat-string-string output-string) :start 0))
      (tt-write-string output-string)
      ;; If there's not enough room for the message at the bottom:
      (when (> (+ saved-row (length endings)) (1- (tt-height)))
	(log-message e "dorked it")
	(incf offset
	      (- (1+ (length endings)) (- (tt-height) saved-row))))
      (log-message e "endings ~s offset ~s" endings offset)
      (decf saved-row offset)
      ;;(tt-move-to saved-row saved-col)
      ;; (relative-move-to-row (+ saved-row offset) saved-row)
      (tt-up (1+ (length endings)))
      (tt-move-to-col saved-col)
      (setf screen-col saved-col
	    screen-relative-row saved-row
	    temporary-message (1+ (length endings))))))

(defmethod message ((e line-editor) fmt &rest args)
  (apply #'tmp-message e fmt args))

;; @@@ should probably be called something else, like "clear-under-area"?
;; "flense-undercarriage" "scrub-nether-region", hmmm... or maybe this is fine.
(defun clear-completions (e)
  "Erase completions, if there are any."
  (when (did-under-complete e)
    (without-messing-up-cursor (e)
      (when (< (screen-relative-row e)
	       (1- (terminal-window-rows (line-editor-terminal e))))
	(tt-down 1)
	(incf (screen-relative-row e))
	(tt-beginning-of-line)
	(setf (screen-col e) 0)
	(tt-erase-below)))))

;; EOF
