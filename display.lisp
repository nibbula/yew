;;;
;;; display.lisp
;;;

(in-package :rl)

(declaim #.`(optimize ,.(getf rl-config::*config* :optimization-settings)))

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
  (with-slots (buf) e
    (with-context ()
      (setf point 0)
      (buffer-delete e 0 (olength buf) point)
      (buffer-insert e 0 str point)
      (setf point (olength str)))))

(defun use-hist (e)
  "Replace the current line with the current history line."
  ;; @@@ Problems:
  ;; - The current line is lost.
  ;; - The undo history (and all other buffer properties) are not
  ;;   retained.
  (without-undo (e)
    (if (history-current (history-context e))
	(replace-buffer e (history-current (history-context e)))
	(replace-buffer e ""))))

;; Perhaps we could consider caching or memoizing this? Espeically when
;; the buffer hasn't changed.

(defun %calculate-line-endings (buffer start cols spots col-spots)
  "Return a list of pairs of character positions and columns, in reverse order
of character position, which should be the end of the displayed lines in the
buffer. SPOTS is an alist of character indexes to set the line and column of.
COL-SPOTS is an alist of line and column pairs to set the character indexes of."
  (let (endings
	(col start)			; Start after the prompt
	(char-width 0)
	(last-col start)
	(line 0)
	(i 0)
	c cc spot)
    (dbugf :rl "endings start ~s~%" start)
    ;; (if-dbugf (:rl)
    ;; 	      (symbol-call :deblarg :debugger-wacktrace 20))
    (flet ((set-spot (x)
	     (when (and spots (setf spot (assoc i spots)))
	       (dbugf :rl "set-spot ~a~%" x)
	       (rplacd spot (cons line col)))
	     (when (and col-spots (setf spot (assoc `(,line ,col) col-spots
						    :test #'equal)))
	       (rplacd spot i))))
      (loop :while (< i (olength buffer))
	 :do
	   (setf c (oelt buffer i)
		 cc (if (fatchar-p c) (fatchar-c c) c))
	   (if (char= cc #\newline)
	       (progn
		 (push (cons (1- i) last-col) endings)
		 (setf last-col col)
		 ;; (when (< col (1- cols))
		 ;;   (incf col))		; just for the cursor?
		 (set-spot "NL")
		 (incf line)
		 (setf col 0)
		 ;;(set-spot "NL")
		 )
	       (progn
		 (setf char-width (display-length (oelt buffer i)))
		 (if (> (+ col char-width) cols)
		     (progn
		       (push (cons (1- i) last-col) endings)
		       (set-spot "wrap")
		       (setf last-col col)
		       (setf col char-width)
		       (incf line))
		     (progn
		       (set-spot "normal")
		       (setf last-col col)
		       (incf col char-width)))))
	   (incf i))

      ;; Make sure we get the last one
      (when (> (+ col char-width) cols)
	(push (cons (1- i) last-col) endings))

      ;; Spot in empty buffer
      (when (and spots (zerop (olength buffer)) (setf spot (assoc 0 spots)))
	(rplacd spot (cons line col)))

      ;; Spot after end
      (when (> (+ col char-width) cols)
	(incf line)
	(setf col 0))
      (set-spot "End")
      endings)))

(defun calculate-line-endings (e &key
				   (buffer (buf e))
				   (start (start-col e))
				   (cols (terminal-window-columns
					  (line-editor-terminal e)))
				   spots col-spots)
  "Return a list of pairs of character positions and columns, in reverse order
of character position, which should be the end of the displayed lines in the
buffer.
  BUFFER  The string to compute endings for.
  START   The column number of the first character in BUFFER.
  COLS    The number columns in the terminal window, after which it wraps."
  (%calculate-line-endings buffer start cols spots col-spots))

(defun line-ending (pos endings)
  "Return the line ending at character position POS, from the line ENDINGS,
or NIL is there is none."
  (cdr (assoc pos endings)))

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

(defun make-prompt (e string function &key no-default)
  "Return the prompt as a string."
  (let* ((prompt string)
	 (output-prompt-func function)
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
		  (if (and (not prompt) (not no-default))
		      (default-output-prompt e prompt)
		      prompt)
		  ))))
    #| last-newline |#
    ;; (log-message e "do-prompt only-last-line = ~s" only-last-line)
    ;; (log-message e "do-prompt last-newline = ~s"
    ;; 		 (oposition #\newline s :from-end t))
    ;; (when (and only-last-line
    ;; 	       (setf last-newline (oposition #\newline s :from-end t)))
    ;;   (setf s (osubseq s (1+ last-newline)))
    ;;   (log-message e "partial prompt ~s" s))
    ;; (log-message e "do-prompt s = ~s ~s" (olength s) s)
    s))

(defun regions-p (e)
  "Return true if there are regions."
  (some (_ (inator-mark _)) (inator-contexts e)))

;; @@@ or we could just modify it?
(defun highlightify (e string &key style)
  "Highlight the region in string with style. A new fatchar-string is returned.
If style isn't given it uses the theme value: (:rl :selection :style)"
  (assert (typep string '(or string fatchar-string fat-string)))
  (with-slots ((contexts inator::contexts) highlight-region region-active) e
    (if (or (> (length contexts) 1)
	    (and highlight-region (regions-p e)))
	;; @@@ We should really cache some of the stuff in here.
	(progn
	  (let* ((array (etypecase string
			 (fat-string
			  (copy-fatchar-string (fat-string-string string)))
			 (fatchar-string
			  (copy-fatchar-string string))
			 (string
			  (make-fatchar-string string))))
		 (style-char
		  (make-array 1 :element-type 'fatchar
			      :fill-pointer t
			      :initial-element (make-fatchar))))
	    (span-to-fatchar-string
	     (append (or style
			 (theme-value *theme* '(:program :selection :style))
			 (theme-value *theme* '(:rl :selection :style))
			 '(:standout))
			 (list #\x))
	     :fatchar-string style-char)
	    ;; Highlight selection regions
	    (when region-active
	      (do-contexts (e)
		(with-context ()
		  (when mark
		    (loop
		       :for i
		       :from (min mark point)
		       :below (min (max mark point) (length array))
		       :do
		       (when (< i (length array))
			 (copy-fatchar-effects (aref style-char 0)
					       (aref array i))))))))
	    ;; Multiple cursors
	    (when (> (length contexts) 1)
	      (span-to-fatchar-string
	       (append (or style
			   (theme-value *theme* '(:program :cursor :style))
			   (theme-value *theme* '(:rl :cursor :style))
			   '(:standout))
		       (list #\x))
	       :fatchar-string style-char)
	      (loop :for c :across contexts :do
		 (when (< (inator-point c) (length array))
		   (copy-fatchar-effects (aref style-char 0)
					 (aref array
					       (inator-point c))))))
	    array))
	string)))

(defun relative-move-to-row (old new)
  (if (< new old)
      (progn
	(dbugf :rl "up ~s~%" (- old new))
	(tt-up (- old new)))
      (progn
	(dbugf :rl "down ~s~%" (- new old))
	(tt-down (- new old)))))

;; An update that probably requires an optimizing terminal to be at all
;; efficient.

(defun ends-with-newline-p (string)
  "Return true if =string= end with a newline."
  (and (ochar-equal #\newline (ochar string (1- (olength string)))) t))

;; Now with more ploof!
(defun redraw-display (e &key erase)
  (declare (ignore erase)) ; @@@
  (with-slots ((contexts inator::contexts)
	       buf-str buf prompt-height start-row start-col
	       screen-relative-row last-line temporary-message region-active
	       max-message-lines) e
    (dbugf :rl "----------------~%")
    ;; Make sure buf-str uses buf.
    (when (not (eq (fat-string-string buf-str) buf))
      (setf (fat-string-string buf-str) buf))
    (let* ((prompt (make-prompt e (prompt-string e) (prompt-func e)))
	   (right-prompt
	    (make-prompt e (and (ostringp (right-prompt e))
				(right-prompt e))
			 (and (or (functionp (right-prompt e))
				  (and (symbolp (right-prompt e))
				       (fboundp (right-prompt e))))
			      (right-prompt e))
			 :no-default t))
	   (cols (terminal-window-columns (line-editor-terminal e)))
	   ;; Prompt figuring
	   (prompt-end (max 0 (1- (olength prompt))))
	   (prompt-spots (list `(,prompt-end . ())))
	   (prompt-endings (calculate-line-endings
			    e
			    :buffer prompt
			    :start 0 :cols cols
			    :spots prompt-spots))
	   (prompt-lines (length prompt-endings))
	   (prompt-last-col (cddr (assoc prompt-end prompt-spots)))
	   ;; Line figuring
	   (line-end (max 0 (1- (olength buf-str))))
	   (first-point (inator-point (aref contexts 0)))
	   (spots (list `(,first-point . ())
			`(,line-end . ())))
	   (endings (calculate-line-endings e :start (1+ prompt-last-col)
					    :spots spots))
	   (buf-lines     (length endings))
	   (msg-endings   (and temporary-message
			       (calculate-line-endings
				e :buffer temporary-message
				:start 0 :cols cols)))
	   (msg-lines     (if temporary-message
			      (+
			       ;; (if (ends-with-newline-p temporary-message)
			       ;; 	   1 1)
			       1
			       (length msg-endings))
			      0))
	   (total-lines   (+ prompt-lines buf-lines msg-lines))
	   (line-last-col (cddr (assoc line-end spots)))
	   (spot          (assoc first-point spots))
	   (point-line    (cadr spot))
	   (point-col     (cddr spot))
	   ;; (quaqua (and (dbugf :rl "FLooP FLooP ~s ~s ~s ~s ~s~%"
	   ;; 		       buf-lines point-line first-point spots
	   ;; 		       contexts) 2/3))
	   (point-offset  (- buf-lines point-line))
	   (right-prompt-start (and right-prompt
				    (- (tt-width)
				       (display-length right-prompt) 1)))
	   new-last-line erase-lines old-col relative-top)
      (declare (ignorable old-col))
      (flet ((eol-compensate () ;; @@@ This is bullcrap. Maybe "fix" ansi?
	       (when (and endings
			  (= line-last-col (1- cols))
			  (terminal-has-autowrap-delay
			   (line-editor-terminal e)))
		 (dbugf :rl "wrap compensation~%")
		 (decf relative-top)
		 (tt-write-char #\newline))))

	(relative-move-to-row screen-relative-row 0)
	(setf relative-top (- screen-relative-row))
	;; (dbugf :rl "start-row = ~s~%~
        ;;             screen-relative-row = ~s~%~
        ;;             spots = ~s~%"
	;;        start-row screen-relative-row spots)
	;; (dbugf :rl "-> prompt-spots ~s~%" prompt-spots)
	;; (dbugf :rl "-> prompt-last-col ~s~%" prompt-last-col)
	;; (dbugf :rl "-> line-last-col ~s~%" line-last-col)

	;; (dbugf :rl "-> spot ~s~%" spot)
	;; (dbugf :rl "-> point-line ~s~%" point-line)
	;; (dbugf :rl "-> point-col ~s~%" point-col)
	;; (dbugf :rl "-> point-offset ~s~%" point-offset)
	(multiple-value-setq (start-row old-col)
	  (terminal-get-cursor-position (line-editor-terminal e)))

	;; If there's not enough room to display the lines, make some.
	;; (when (> (+ start-row relative-top total-lines) (tt-height))
	;;   (let ((offset (- (+ start-row relative-top total-lines) (tt-height))))
	;;     (dbugf :rl "scroll down ~s~%" offset)
	;;     (tt-scroll-down offset)
	;;     (decf relative-top offset)))
	(setf max-message-lines (- (tt-height) prompt-lines buf-lines 2))

	;; Write the prompt
	(tt-move-to-col 0)
	(tt-erase-below)
	;; (tt-erase-to-eol)
	(tt-write-string prompt)
	(tt-erase-to-eol)
	;; (when (and right-prompt (< (+ point-col 2) right-prompt-start))
	;;   (tt-move-to-col right-prompt-start)
	;;   (tt-write-string right-prompt)
	;;   (tt-move-to-col prompt-last-col)
	;;   )
	(setf prompt-height prompt-lines
	      new-last-line total-lines
	      start-col prompt-last-col)

	(dbugf :rl "buf-lines ~s prompt-lines ~s last-line ~s start-col ~s~%~
                    buf = ~s~%"
	       buf-lines prompt-lines last-line start-col (buf-str e))
	;; Write the line
	;;(if (or (and (regions-p e) region-active) (> (length contexts) 1))
	(if (or (and (regions-p e) region-active) (> (length contexts) 1))
	    (progn
	      (let ((s (make-fat-string :string (highlightify e buf))))
		(tt-write-string s)
		(dbugf :rl "highlighted = ~a~%" s)))
	    (tt-write-string (buf-str e)))
	(eol-compensate)
	(tt-erase-to-eol)
	(dbugf :rl "right-prompt ~s ~s~%" right-prompt-start right-prompt)
	(when (and right-prompt (< point-col right-prompt-start)
		   (= 0 buf-lines))
	  (dbugf :rl "right-prompt again ~s ~s~%" right-prompt-start
		 prompt-last-col)
	  (tt-move-to-col right-prompt-start)
	  (tt-write-string right-prompt)
	  ;; (tt-move-to-col prompt-last-col)
	  )
	(when temporary-message
	  (tt-write-char #\newline)
	  (tt-write-string temporary-message))
	(tt-erase-to-eol)
	;; Erase junk after the line
	(when last-line
	  (setf erase-lines (max 0 (- last-line new-last-line)))
	  (dbugf :rl "erase-lines = ~s~%" erase-lines)
	  (loop :repeat erase-lines
	     :do
	       (tt-down)
	       (tt-move-to-col 0)
	       (tt-erase-to-eol))
	  (when (not (zerop erase-lines))
	    (tt-up erase-lines)))

	;; Move to the point.
	(when (not (zerop (+ point-offset msg-lines)))
	  (tt-up (+ point-offset msg-lines)))
	(tt-move-to-col point-col)
	(setf screen-relative-row (+ prompt-lines point-line)
	      last-line new-last-line)
	(dbugf :rl "new screen-relative-row ~s~%" screen-relative-row)
	))))

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

#|
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
|#

(defun tmp-message (e fmt &rest args)
  (with-slots (temporary-message keep-message) e
    (setf temporary-message (with-output-to-fat-string (fs)
			      (apply #'format fs fmt args))
	  keep-message nil)
    (redraw-display e)))

(defmethod message ((e line-editor) fmt &rest args)
  (apply #'tmp-message e fmt args))

;; @@@ should probably be called something else, like "clear-under-area"?
;; "flense-undercarriage" "scrub-nether-region", hmmm... or maybe this is fine.
(defun clear-completions (e)
  "Erase completions, if there are any."
  (setf (temporary-message e) nil (keep-message e) nil)
  ;; (when (did-under-complete e)
  ;;   (without-messing-up-cursor (e)
  ;;     (when (< (screen-relative-row e)
  ;;   	       (1- (terminal-window-rows (line-editor-terminal e))))
  ;;   	(tt-down 1)
  ;;   	(incf (screen-relative-row e))
  ;;   	(tt-beginning-of-line)
  ;;   	(setf (screen-col e) 0)
  ;;   	(tt-erase-below))))
  )

;; EOF
