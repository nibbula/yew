;;
;; display.lisp
;;

;; Copyright © 2007-2017 Nibby Nebbulous
;; Licensed under the GPL (See file LICENSE for details).

(in-package :rl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display-ish code

(defun message (e fmt &rest args)
  "Show a little message for debugging."
  (declare (type line-editor e))
  (tt-cursor-off)
  (tt-move-to 5 5)		; The only place we should call this
  (tt-standout t)
  ;; (apply #'tt-format (cons (line-editor-terminal e) (cons fmt args)))
  (apply #'terminal-format (cons (line-editor-terminal e) (cons fmt args)))
  (tt-standout nil)
  (tt-cursor-on))

(defun log-message (e fmt &rest args)
  (when (debugging e)
    (push (apply #'format nil fmt args) (line-editor-debug-log e))))

(defun show-message-log (e)
  "Show the debugging message log."
  (declare (type line-editor e))
  (tt-cursor-off)
  (tt-standout t)
  (loop :for i :from 0 :below 8
     :for dd :in (line-editor-debug-log e)
     :do
     (tt-move-to (+ 10 i) 40)		; The “only” place we should call this
     (tt-erase-to-eol)
     (tt-write-string dd))
  (tt-standout nil)
  (tt-cursor-on))

(defun message-pause (e fmt &rest args)
  "Show a little message for debugging."
  (apply #'message e fmt args)
  (get-a-char e))

(defun beep (e fmt &rest args)
  "Beep or display an error message."
  (when (debugging e)
    (apply #'message e fmt args))
  (tt-beep))

;; Note: no tab or newline
(defparameter *control-char-graphics-vec*
  `((#\Null . #\@) (,(ctrl #\A) . #\A) (,(ctrl #\B) . #\B) (,(ctrl #\C) . #\C)
    (,(ctrl #\D) . #\D) (,(ctrl #\E) . #\E) (,(ctrl #\F) . #\F)
    (,(ctrl #\G) . #\G) (,(ctrl #\H) . #\H) (,(ctrl #\J) . #\J)
    (,(ctrl #\K) . #\K) (,(ctrl #\L) . #\L) (,(ctrl #\M) . #\M)
    (,(ctrl #\N) . #\N) (,(ctrl #\O) . #\O) (,(ctrl #\P) . #\P)
    (,(ctrl #\Q) . #\Q) (,(ctrl #\R) . #\R) (,(ctrl #\S) . #\S)
    (,(ctrl #\T) . #\T) (,(ctrl #\U) . #\U) (,(ctrl #\V) . #\V)
    (,(ctrl #\W) . #\W) (,(ctrl #\X) . #\X) (,(ctrl #\Y) . #\Y)
    (,(ctrl #\Z) . #\Z) (#\Escape . #\[) (#\Fs . #\\) (#\Gs . #\])
    (#\Rs . #\^) (#\Us . #\_) (#\Rubout . #\?))
  "Vector of control characters and corresponding caret notation char.")

(defun pair-vector-to-hash-table (vec table)
  (loop :for (k . v) :across vec
     :do (setf (gethash k table) v))
  table)

(defparameter *control-char-graphics* nil)

;; @@@ Perhaps this should so be somewhere else.
(defun control-char-graphic (c)
  (when (not *control-char-graphics*)
    (setf *control-char-graphics* (make-hash-table :test #'eql))
    (alist-to-hash-table *control-char-graphics-vec*
			 *control-char-graphics*))
  (gethash c *control-char-graphics*))

(defgeneric display-length (obj)
  (:documentation "Return how long is the object should be when displayed."))

#+sbcl
;; Older versions of SBCL don't have this.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :sb-unicode)
    (d-add-feature :has-sb-unicode)))

;; @@@ Perhaps this should so be somewhere else.
(defun double-wide-p (c)
  #+(and sbcl has-sb-unicode) (eq (sb-unicode:east-asian-width c) :w)
  #-(and sbcl has-sb-unicode) (declare (ignore c))
  #-(and sbcl has-sb-unicode) nil	; @@@ too hard without tables
  )

;; XXX This is still wrong for unicode! @@@
(defmethod display-length ((c character))
  "Return the length of the character for display."
  (cond
    ((graphic-char-p c)
     (cond
       ((combining-character-p c) 0)
       ((double-wide-p c) 2)
       (t 1)))				;normal case
    ((eql c #\tab)
     8)					;XXX @@@ wrong!
    ((eql c #\newline)
     0)					;XXX @@@ wrong!
    (t
     (if (control-char-graphic c)
	 2   ; ^X
	 4)  ; \000
     )))

(defmethod display-length ((s string))
  "Return the length of the string for display."
  (let ((sum 0))
    (map nil #'(lambda (c) (incf sum (display-length c))) s)
    sum))

(defun moo (e s)
  (let* ((tt (line-editor-terminal e))
	 (width (terminal-window-columns tt))
	 ;;(height (terminal-window-rows tt))
	 (row (screen-row e))
	 (col (screen-col e))
	 (plain (remove-effects s))
	 (last-col col))
    (loop :for c :across plain :do
       (cond
	 ((graphic-char-p c)
	  (cond
	    ((combining-character-p c) #|nothing|#)
	    ((double-wide-p c) (incf col 2))
	    (t (incf col))))		; normal case
	 ((eql c #\tab)
	  (setf col (1+ (logior 7 col)))
	  (when (>= col width)
	    (setf col (1- width))))
	 ((eql c #\newline)
	  (setf col 0) (incf row))
	 ((control-char-graphic c)	; ^X
	  (incf col 2))
	 (t (incf col 4)))		; \000
       (when (>= col width)
	 (setf col (- col last-col))
	 (incf row))
       (setf last-col col))
    (values col row)))

;; @@@ XXX wrong, because it doesn't account for double-width last col issues
(defun editor-update-pos-for-string (e s)
  "Update the screen row and column for inserting the string S.
Assumes S is already converted to display characters."
  (let* ((width (terminal-window-columns (line-editor-terminal e)))
	 ;;(len (length s))
	 (len (display-length s))
	 (last-remain (+ (screen-col e) len)))
     (loop :with remain = (max 0 (- len (- width (screen-col e))))
	  :while (> remain 0)
	  :do
	  (incf (screen-row e))
	  (setf (screen-col e) 0)
	  (decf remain (setf last-remain (min width remain))))
     (setf (screen-col e) last-remain)
;    (message-pause (line-editor-terminal e) "fug ~a" (screen-col e))
    ))

; Test code for editor-update-pos-for-string
; (defun fuglor (s col row)
;   (let* ((width 80)
; 	 (len (length s))
; 	 (last-remain (+ col len)))
;      (loop with remain = (max 0 (- len (- width col)))
; 	  while (> remain 0)
; 	  do
; 	  (incf row)
; 	  (setf col 0)
; 	  (decf remain (setf last-remain (min width remain))))
;     (setf col last-remain)
;     (format t "[~d ~d]~%" col row)))

(defun editor-write-char (e c)
  "Write a display character to the screen. Update screen coordinates."
  (with-slots (screen-col screen-row) e
    (let ((len (display-length c))
	  (width (terminal-window-columns (line-editor-terminal e))))
      (cond
	((> (+ screen-col len) width)
	 (setf screen-col len)
	 (incf screen-row)
	 (tt-scroll-down 1)
	 (tt-beginning-of-line)
	 (tt-write-char c))
	((= (+ screen-col len) width)
	 (tt-write-char c)
	 (setf screen-col 0)
	 (incf screen-row)
	 (tt-scroll-down 1)
	 (tt-beginning-of-line))
	(t
	 (tt-write-char c)
	 (incf screen-col len))))))

(defun editor-write-string (e s)
  "Write a display string to the screen. Update screen coordinates."
  ;; (finish-all-output) ;; @@@@ XXX for testing
  ;;(tt-write-string (line-editor-terminal e) s)
  (tt-write-string s)
  ;; (finish-all-output) ;; @@@@ XXX for testing
  (editor-update-pos-for-string e s))

(defun display-char (e c)
  "Output a character with visible display of control characters."
  (cond
    ((graphic-char-p c)
     (editor-write-char e c))
    ((eql c #\tab)
     ;; (editor-write-string e (make-string (- 8 (mod (screen-col e) 8))
     ;; 					 :initial-element #\space)))
     (dotimes (_ (- (1+ (logior 7 (screen-col e))) (screen-col e)))
       (tt-write-char #\space)))
    ((eql c #\newline)
     (setf (screen-col e) 0)
     (incf (screen-row e))
     (tt-write-char c))
    ((setf c (control-char-graphic c))
     (editor-write-char e #\^)
     (editor-write-char e c))
    (t ;; output non-graphic chars as char code
     (editor-write-char e #\\)
     (editor-write-string e (format nil "\\~3,'0o" (char-code c))))))

(defun display-buf (e &optional (start 0) end)
  "Display the buffer."
  (with-slots (buf) e
    ;; XXX Wrong!
    ;; ;; Just in case write-char does system calls, we output to a string stream.
    ;; (tt-write-string
    ;;  (with-output-to-string (s)
    ;; (if end
    ;; 	(subseq buf start end)
    ;; 	(subseq buf start))
    (loop :with sub = (subseq buf start end)
       :for c :across sub :do
       (display-char e c))))

(defmacro without-messing-up-cursor ((e) &body body)
  (let ((old-row (gensym "OLD-ROW"))
	(old-col (gensym "OLD-COL")))
  `(let ((,old-row (screen-row ,e))
	 (,old-col (screen-col ,e)))
     (prog1 ,@body
       (if (< ,old-row (screen-row ,e))
	   (tt-up (- (screen-row ,e) ,old-row))
	   (tt-down (- ,old-row (screen-row ,e))))
       (tt-beginning-of-line)
       (tt-forward ,old-col)
       (setf (screen-row ,e) ,old-row
	     (screen-col ,e) ,old-col)))))

(defun buffer-length-to (buf to-length)
  (loop :with i = 0
     :for buf-i = 0 :then (1+ buf-i)
     :while (< i to-length)
     :do (incf i (display-length (aref buf buf-i)))
     :finally (return buf-i)))

(defun update-for-delete (e delete-length char-length)
  "Update the display, assuming DELETE-LENGTH characters were just deleted at 
the current cursor position."
  (declare (ignore char-length)) ;; @@@
  (with-slots (buf point terminal) e
    (let ((width (terminal-window-columns terminal))
	  (col (screen-col e))
	  (right-len (display-length (subseq buf point)))
	  to-delete)
      ;; If the rest of the buffer extends past the edge of the window.
      ;;(when (>= (+ col right-len) width)
      (when (>= (+ col (+ right-len delete-length)) width)
	;; Cheaty way out: redraw whole thing after point
	(without-messing-up-cursor (e)
	  (display-buf e point)
	  ;;(tt-del-char delete-length)
	  ;; We can't get it right here, because the characters aren't in the
	  ;; buffer, so we overshoot a little.
	  (tt-erase-to-eol)
	  (log-message e "Smoot ~a ~a ~a" width delete-length (screen-col e))
	  (when (< (- width delete-length) (screen-col e))
	    (setf to-delete (- delete-length (- width (screen-col e))))
	    (tt-down 1)
	    (incf (screen-row e))
	    (tt-move-to-col 0)
	    (setf (screen-col e) 0)
	    (tt-erase-to-eol)
	    (loop :while (> to-delete 0) :do
	       (tt-down 1)
	       (incf (screen-row e))
	       (tt-erase-to-eol)
	       (log-message e "Bloot ~a" to-delete)
	       (decf to-delete width))))))))

(defun update-for-insert (e)
  "Assuming we just inserted something, redisplay the rest of the buffer."
  (with-slots (buf point) e
    (when (not (= point (length buf)))	; At the end of the buffer.
      ;; Do this an horribly inefficient and cheating way:
      ;; just rewrite the whole thing relying on terminal wrap around.
      ;;
      ;; @@@ We should really fix this someday to do it the right way, and
      ;; just fiddle the beginnings and ends of the lines. Or even better.
      (let ((old-row (screen-row e))
	    (old-col (screen-col e)))
	(display-buf e point)
	(if (< old-row (screen-row e))
	    (tt-up (- (screen-row e) old-row))
	    (tt-down (- old-row (screen-row e))))
	(tt-beginning-of-line)
	(tt-forward old-col)
	(setf (screen-row e) old-row
	      (screen-col e) old-col)))))

(defun erase-display (e)
  "Erase the display of the buffer, but not the buffer itself."
  (with-slots (buf) e
    (beginning-of-line e)
    (tt-erase-to-eol)
    (let* ((cols (terminal-window-columns (line-editor-terminal e)))
	   ;;(buf-len (length buf))
	   (buf-len (display-length buf))
	   ;;(lines-to-clear (truncate (+ (screen-col e) buf-len) 80)))
	   (lines-to-clear (truncate (+ (screen-col e) buf-len) cols)))
      (when (> (+ buf-len (screen-col e)) cols)
	(loop :for i :from 1 :to lines-to-clear
	      :do (tt-down 1)
	      (tt-erase-line))
	(tt-up lines-to-clear)))))

(defun replace-buffer (e str)
  "Replace the buffer with the given string STR."
  (declare (type string str))
  (with-slots (buf point) e
    (erase-display e)
    (setf point (length str))
    (buffer-delete e 0 (length buf))
    (buffer-insert e 0 str)
    (display-buf e)))

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
	(i 0))
    (loop :while (< i (length buffer)) :do
       (if (char= (aref buffer i) #\newline)
	   (progn
	     (push (cons (1- i) last-col) endings)
	     (setf last-col col)
	     (setf col 0))
	   (progn
	     (setf char-width (display-length (aref buffer i)))
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

(defun move-over (e n &key (start (point e)) (buffer (buf e)))
  "Move over N characters in the buffer, from START. N is negative for backward,
positive for forward. Doesn't adjust POINT, but does move the cursor and update
the SCREEN-COL and SCREEN-ROW."
  (with-slots (screen-row screen-col) e
    (let ((cols (terminal-window-columns (line-editor-terminal e)))
	  (col screen-col)
	  (row screen-row)
	  (endings (calculate-line-endings e))
	  len)
      (log-message e "move-over ~a ~a" n start)
      (if (minusp n)
	  ;; backward
	  (progn
	    (loop :for i :from start :above (+ start n) :do
	       ;;(tt-move-to 20 5)
	       ;;(tt-format "--> ~a ~a ~a ~a" row col i endings)
	       ;;(tt-move-to row col)
	       ;;(tt-get-key)
	       (setf len (display-length (aref buffer (1- i))))
	       (if (< (- col len) 0)
		   (progn
		     ;;(log-message e "mooo ~a ~a ~a" i col endings)
		     ;;(setf col (- cols len))
		     (assert (assoc (1- i) endings))
		     (setf col (cdr (assoc (1- i) endings)))
		     (decf row))
		   (progn
		     (decf col len))))
	    ;;(tt-move-to 20 5)
	    ;;(tt-erase-to-eol)
	    ;;(tt-format "--> DONE ~a ~a" row col)
	    ;;(tt-move-to row col)
	    (log-message e "row=~a col=~a" row col)
	    #| because we tt-move-to in the debug code |#
	    (tt-up (- screen-row row))
	    (if (< col screen-col)
		(progn
		  ;; could optimize by:
		  ;;(tt-beginning-of-line)
		  ;;(tt-forward col)
		  (tt-backward (- screen-col col)))
		(progn
		  (tt-forward (- col screen-col))))
	    ;;(tt-move-to row col)
	    (setf screen-col col
		  screen-row row))
	  ;; forward
	  (progn
	    (loop :for i :from start :below (+ start n) :do
	       (setf len (display-length (aref buffer i)))
	       (cond
		 ((assoc i endings)
		  (setf col 0)
		  (incf row))
		 ((> (+ col len) cols)
		  (setf col len)
		  (incf row))
		 (t
		  (incf col len))))
	    (log-message e "row=~a col=~a" row col)
	    ;; (tt-beginning-of-line)
	    ;; (tt-forward col)
	    ;; (tt-down (- screen-row row))
	    (tt-down (- row screen-row))
	    (if (< col screen-col)
		(progn
		  ;; could optimize by:
		  ;;(tt-beginning-of-line)
		  ;;(tt-forward col)
		  (tt-backward (- screen-col col)))
		(progn
		  (tt-forward (- col screen-col))))
	    (setf screen-col col
		  screen-row row))))))

(defun move-backward (e n)
  "Move backward N columns on the screen. Properly wraps to previous lines.
Updates the screen coordinates."
  (move-over e (- n)))

(defun move-forward (e n)
  "Move forward N columns on the screen. Properly wraps to subsequent lines.
Updates the screen coordinates."
  (move-over e n))

#|
;;; @@@ Consider the issues of merging this with display-length.
;;; @@@ Consider that this is quite wrong, especially since it would have to
;;; do everything a terminal would do.
(defun display-cols (str)
  "Return the column the cursor is at after outputting STR."
  (let ((sum 0))
    (map nil
	 #'(lambda (c)
	     (cond
	       ((graphic-char-p c)
		(incf sum))
	       ((eql c #\tab)
		(incf sum (- 8 (mod sum 8))))
	       ((eql c #\newline)
		(setf sum 0))
	       ((eql c #\backspace)
		(decf sum))
	       ((eql c #\escape)
		#| here's where we're screwed |#)
	       (t
		(if (control-char-graphic c)
		    2			; ^X
		    4)			; \000
		)))
	 s)
    sum))
|#

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
;; characters, but not only is annoying, but it won't always work.
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

(defun do-prefix (e prompt-str)
  "Output a prefix."
  (finish-all-output)
  (let (row col start-row)
    (multiple-value-setq (row col)
      (terminal-get-cursor-position (line-editor-terminal e)))
    (setf start-row row)
    (tt-write-string prompt-str)
    ;;(finish-all-output)
    (tt-finish-output)
    ;; (eat-typeahead e)
    (multiple-value-setq (row col)
      (terminal-get-cursor-position (line-editor-terminal e)))
    (setf (screen-row e) row
	  (screen-col e) col
	  ;; save end of the prefix as the starting column
	  (start-col e) col
	  (start-row e) row
	  (prompt-height e) (- row start-row))
    (log-message e "prompt-height = ~s" (prompt-height e))))

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
    (log-message e "do-prompt only-last-line = ~s" only-last-line)
    (log-message e "do-prompt last-newline = ~s"
		 (position #\newline s :from-end t))
    (when (and only-last-line
	       (setf last-newline (position #\newline s :from-end t)))
      (setf s (subseq s (1+ last-newline)))
      (log-message e "partial prompt ~s" s))
    (log-message e "do-prompt s = ~s ~s" (length s) s)
    (do-prefix e s)))

(defun redraw (e)
  "Erase and redraw the whole line."
  (tt-move-to-col 0)
  (tt-erase-to-eol)
  (setf (screen-col e) 0)
  (do-prompt e (prompt e) (prompt-func e) :only-last-line t)
  (finish-output (terminal-output-stream (line-editor-terminal e)))
  (display-buf e)
  (with-slots (point buf) e
    (when (< point (length buf))
      ;;(let ((disp-len (display-length (subseq buf point))))
      ;;  (move-backward e disp-len))))
      (move-over e (- (length buf) point) :start (length buf))))
  (setf (need-to-redraw e) nil))

(defun tmp-prompt (e fmt &rest args)
  (tt-move-to-col 0)
  (tt-erase-to-eol)
  (setf (screen-col e) 0)
  (do-prefix e (apply #'format `(nil ,fmt ,@args))))

(defun tmp-message (e fmt &rest args)
  (apply #'tmp-prompt e fmt args)
  (setf (need-to-redraw e) t))

;; @@@ should probably be called something else, like "clear under area"?
(defun clear-completions (e)
  "Erase completions, if there are any."
  (when (did-under-complete e)
    (without-messing-up-cursor (e)
      (when (< (screen-row e)
	       (1- (terminal-window-rows (line-editor-terminal e))))
	(tt-down 1)
	(incf (screen-row e))
	(tt-beginning-of-line)
	(setf (screen-col e) 0)
	(tt-erase-below)))))

;; EOF
