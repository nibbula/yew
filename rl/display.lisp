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
     (tt-move-to (+ 10 i) 40)	; The “only” place we should call move-to
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
  (with-names (old-row old-col)
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

(defgeneric draw-mode-line (editor)
  (:documentation "Draw the mode line."))

(defmethod draw-mode-line ((e line-editor))
  (with-slots (show-mode-line mode-line start-row screen-relative-row) e
    (when (and show-mode-line mode-line)
      (unwind-protect
	   (progn
	     (tt-save-cursor)
	     (log-message e "mode-line ~s ~s" start-row screen-relative-row)
	     (let ((momo (or (and (ostringp mode-line) mode-line)
			     (princ-to-string mode-line))))
	       (tt-write-string-at
		(if (= (+ start-row screen-relative-row) (1- (tt-height)))
		    0 (1- (tt-height)))
		0 (osubseq momo 0 (min (olength momo) (tt-width))))))
	(tt-restore-cursor)))))

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
      ;; Maybe not the same length as str, since it's grapheme-ized.
      (setf point (olength buf)))))

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

(defgeneric editor-calculate-line-endings (editor &key
						    buffer
						    start-column
						    end-column
						    spots
						    column-spots
						    autowrap-delay)
  (:documentation
   "Return a list of pairs of character positions and columns, in reverse order
of character position, which should be the end of the displayed lines in the
buffer.
  BUFFER        The string to compute endings for.
  START-COLUMN  The column number of the first character in BUFFER.
  END-COLUMN    The number columns in the view, after which it wraps.
  SPOTS         An alist of character indexes to set the line and column of.
  COLUMN-SPOTS  An alist of line and column pairs to set the character
                indexes of."))

(defmethod editor-calculate-line-endings
    ((editor line-editor)
     &key
       (buffer (buf editor))
       (start-column
	;; With gutters, start at zero, otherwise the end of the prompt.
	(if (gutter-char editor) 0 (start-col editor)))
       (end-column
	(- (terminal-window-columns (line-editor-terminal editor))
	   ;; With gutters, the end is shortened by the gutter i.e. start-col.
	   (if (gutter-char editor) (start-col editor) 0)))
       spots column-spots
       (autowrap-delay
	(terminal-has-autowrap-delay
	 (line-editor-terminal editor))))
  (calculate-line-endings buffer start-column end-column spots column-spots
			  autowrap-delay))

(defun line-ending (pos endings)
  "Return the line ending at character position POS, from the line ENDINGS,
or NIL is there is none."
  (cdr (assoc pos endings)))

;; XXX this sucks, and is probably the wrong approach
(defun in-emacs-p ()
  "Return true if we're being run under Emacs, like probably in SLIME."
  (and (nos:env "EMACS") t))

(defun dumb-terminal-p ()
  "Return true if we're in some kind of dumb terminal which we shouldn't expect
to act like a real terminal."
  (or (typep *terminal* 'terminal-dumb:terminal-dumb)
      (in-emacs-p)))

(defun finish-all-output ()
  "Makes all output be in Finnish."
  (when (not (dumb-terminal-p))
    ;;#+ccl (ccl::auto-flush-interactive-streams) ;; Jiminy Crickets!
    (finish-output *standard-output*)
    (finish-output *terminal-io*)
    (finish-output t)
    (finish-output)
    ;(finish-output *standard-input*)
    )
  (tt-finish-output))

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
		  ;; (log-message e "make-prompt output-prompt-func -> ~s"
		  ;; 	       output-prompt-func)
		  (or (ignore-errors (funcall output-prompt-func e prompt))
		      "Your prompt Function failed> "))
		(progn
		  ;; (log-message e "make-prompt default-output-prompt")
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
			   (theme-value *theme* '(:rl :cursor :style))
			   (theme-value *theme* '(:program :cursor :style))
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

(defun ends-with-newline-p (string)
  "Return true if =string= end with a newline."
  (and (ochar-equal #\newline (ochar string (1- (olength string)))) t))

;; @@@ currently unused
(defun write-multiline-string (e string endings)
  "Write the STRING to the terminal given the line ENDINGS. Properly account
for multi-line strings which end exactly on the right side on terminals with
auto-wrap and no autowrap delay."
  (let ((cols (terminal-window-columns (line-editor-terminal e))))
    (cond
      ;; A single line less than the terminal width or no autowrap-delay.
      ((or (not endings)
	   (terminal-has-autowrap-delay (line-editor-terminal e))
	   )
       (dbugf :zarp "no endings or no a-w-d ~a ~a~%" cols endings)
       (tt-write-string string))

      ;; At least one exactly on the edge.
      ((find (1- cols) endings :key #'cdr)
       ;; @@@ Another way to do this would be to just remove the edge newlines.
       ;; I'm assuming this way is faster. It certainly avoids copying the
       ;; string at least.
       (let ((pieces ;; Collect the endings that hit the edge.
	      (loop
		 :for (char-pos . col) :in (reverse endings)
		 :when (= col (1- cols))
		 :collect char-pos))
	     (start 0))
	 (setf pieces (coerce pieces 'vector))
	 ;; Write the pieces of the string.
	 (loop
	    :for i :from 0 :below (length pieces)
	    :do
	    (tt-write-string string
			     :start start
			     :end (1+ (aref pieces i)))
	    (dbugf :zarp "start ~s end ~s~%" start (1+ (aref pieces i)))
	    (setf start (+ (aref pieces i) 2))) ;; 1+ for skipping the newline
	 ;; Do the last piece
	 (when (< start (olength string))
	   (tt-write-string string :start start))
	 (dbugf :zarp "start ~s END = ~s~%" start (olength string))
	 ))
      (t ;; No lines that end on the right edge.
       (dbugf :zarp "no edge lines~%")
       (tt-write-string string)))))

(defun write-with-gutter (string endings gutter-char width cols)
  "Write the ‘string’ to the terminal with a gutter of ‘gutter-char’.
‘endings’ are the line endings. ‘width’ is the width of the gutter.
‘cols’ are the number of columns in the window."
  (tt-move-to-col width)
  (let ((start 0))
    (flet ((write-gutter ()
	     (when (not (zerop start))
	       (dotimes (i width) (tt-write-char gutter-char))))
	   (next-line (col char-pos)
	     (when (and (< char-pos (olength string))
			(plusp char-pos)
			(< (+ col (display-length (ochar string char-pos)))
			   (- cols width)))
	       (tt-write-char #\newline)
	       ;; skip over the newline, if it's there
	       (when (ochar= (ochar string start) #\newline)
		 (incf start)))))
      ;; Write the pieces of the string.
      (loop
        :for (char-pos . col) :in (reverse endings)
        :do
        (write-gutter)
        (tt-write-string string :start start :end (1+ char-pos))
        (setf start (+ char-pos 1))
	(next-line col char-pos))
      ;; Do the last piece
      (when (< start (olength string))
	(write-gutter)
	(tt-write-string string :start start)))))

;; This is an horrible, horrible thing.
(defun pre-read (e)
  "When prompt-start-at-left is true, output a real #\return to get to the
start of the line. If partial-line-indicator is set, output it first and just
enough spaces to get to the next line if we weren't at the beginning of the
line already. If we were already at the beginning of the line, the
partial-line-idicator is overwritten by the prompt, so we don't see it."
  (when (not (dumb-terminal-p))
    (with-slots (prompt-start-at-left partial-line-indicator) e
      (when prompt-start-at-left
	(let ((tty (or (and (typep *terminal* 'terminal-wrapper)
			    (terminal-wrapped-terminal *terminal*))
		       *terminal*)))
	  (if (and partial-line-indicator (tt-width))
	      (terminal-format tty
			       "~a~va~c~va~c" partial-line-indicator
			       (- (tt-width)
				  (display-length partial-line-indicator))
			       "" #\return
			       (display-length partial-line-indicator)
			       "" #\return)
	      (terminal-write-char tty #\return))
	  ;; (terminal-finish-output tty)
	  )))))

(defun fake-width (e)
  (or (terminal-window-columns (line-editor-terminal e)) 80))

(defun fake-height (e)
  (or (terminal-window-rows (line-editor-terminal e)) 24))

;; An update that probably requires an optimizing terminal to be at all
;; efficient.

(defun redraw-display (e &key erase)
  (declare (ignore erase)) ; @@@
  (with-slots ((contexts inator::contexts)
	       buf-str buf prompt-height start-row start-col
	       screen-relative-row last-line temporary-message region-active
	       max-message-lines message-lines message-top message-endings
	       auto-suggest-p suggestion auto-suggest-style gutter-char
	       blank-line-func) e
    ;; (dbugf :rl "----------------~%")
    ;; Make sure buf-str uses buf.
    (when (not (eq (fat-string-string buf-str) buf))
      (setf (fat-string-string buf-str) buf))
    ;; point should be in the buffer or right after the buffer, or else
    ;; something did something wrong.
    (assert (<= (inator-point (aref contexts 0))
		(olength buf-str)))

    (let* ((prompt (make-prompt e (prompt-string e) (prompt-func e)))
	   (right-prompt
	    (make-prompt e (and (ostringp (right-prompt e))
				(right-prompt e))
			 (and (or (functionp (right-prompt e))
				  (and (symbolp (right-prompt e))
				       (fboundp (right-prompt e))))
			      (right-prompt e))
			 :no-default t))
	   (cols (fake-width e))
	   ;; Prompt figuring
	   (prompt-end (max 0 (1- (olength prompt))))
	   (prompt-spots (list `(,prompt-end . ())))
	   (prompt-endings (editor-calculate-line-endings
			    e
			    :buffer prompt
			    :start-column 0 :end-column cols
			    :spots prompt-spots))
	   (prompt-lines (length prompt-endings))
	   (prompt-last-col (cddr (assoc prompt-end prompt-spots)))
	   ;; Line figuring
	   line-end first-point spots endings buf-lines
	   ;; message
	   actual-message total-lines last-displayed-message-line
	   message-lines-displayed more-lines
	   ;; point
	   line-last-col spot point-line point-col point-offset
	   right-prompt-start
	   new-last-line
	   erase-lines
	   old-col
	   #|relative-top|#
	   buffer
	   suggest-p)
      (declare (ignorable old-col))
      ;; Line figuring
      (setf suggest-p (and auto-suggest-p suggestion
			   (eobp e) (not (zerop (olength buf))))
	    ;; First highlight the regions
	    buffer (if (or (and (regions-p e) region-active)
			   (> (length contexts) 1))
		       (make-fat-string :string (highlightify e buf))
		       buf-str)
	    ;; Then add the suggestion
	    buffer (if suggest-p
		       (make-compound-string buffer
					     (styled-string auto-suggest-style
							    suggestion))
		       buffer)
	    ;; Set up some spots that we'd like to know the location of.
            line-end (max 0 (1- (olength buffer)))
	    first-point (inator-point (aref contexts 0))
	    spots (list `(,first-point . ()) `(,line-end . ()))
	    start-col (if (>= (1+ prompt-last-col) (fake-width e))
			  0
			  (1+ prompt-last-col))
	    ;; Figure out where things are
	    endings
	    (editor-calculate-line-endings e :spots spots :buffer buffer)
	    buf-lines (length endings))
      ;; Message figuring
      (setf
            ;; message-endings (nreverse (and temporary-message
	    ;; 				   (editor-calculate-line-endings
	    ;; 				    e :buffer temporary-message
	    ;; 				    :start-column 0 :end-column cols)))
	    message-lines (if temporary-message
			      (+
			       ;; (if (ends-with-newline-p
			       ;;      temporary-message)
			       ;; 	   1 1)
			       ;; 1
			       (length message-endings))
			      0)
	    line-last-col (cddr (assoc line-end spots))
	    spot          (assoc first-point spots)
	    point-line    (cadr spot)
	    point-col     (cddr spot)
	    ;; @@@ Maybe we should ensure point-line isn't NIL???
	    point-offset (- buf-lines (or point-line 0))
	    right-prompt-start (and right-prompt
				    (- (fake-width e)
				       (display-length right-prompt))))
      (when gutter-char
	(incf point-col start-col))
      (flet (#|
	     (eol-compensate () ;; @@@ This is bullcrap. Maybe "fix" ansi?
	       (when (and endings
			  (= line-last-col (1- cols))
			  (terminal-has-autowrap-delay
			   (line-editor-terminal e)))
		 (dbugf :rl "wrap compensation~%")
		 (decf relative-top)
		 (tt-write-char #\newline)))
	     |# )

	(relative-move-to-row screen-relative-row 0)
	(setf start-row screen-relative-row old-col 0)

	;; If there's not enough room to display the lines, make some.
	;; (when (> (+ start-row relative-top total-lines) (tt-height))
	;;   (let ((offset (- (+ start-row relative-top total-lines)
	;; 		   (tt-height))))
	;;     (dbugf :rl "scroll down ~s~%" offset)
	;;     (tt-scroll-down offset)
	;;     (decf relative-top offset)))
	(setf max-message-lines
	      (- (fake-height e) 1 prompt-lines buf-lines)

	      last-displayed-message-line
	      (max (min (+ message-top (1- max-message-lines)) message-lines) 0)

	      more-lines
	      (max 0 (- message-lines last-displayed-message-line))

	      message-lines-displayed
	      (max 0 (- last-displayed-message-line message-top)))

	;; Add one for the [.. more] line
	(setf total-lines
	      (+ prompt-lines buf-lines
		 (- last-displayed-message-line message-top)
		 (if (plusp more-lines) 1 0)))

	(let* (start end)
	  (when (and (plusp message-top) message-endings)
	    (setf start (car (nth
			      ;; the line ending right before the top
			      (if (plusp message-top) (1- message-top) 0)
			      message-endings))))
	  (when (and (plusp last-displayed-message-line) message-endings)
	    (setf end (car (nth
			    ;; (+ last-displayed-message-line
			    ;;    (if (plusp message-top) 1 0))
			    (1- last-displayed-message-line)
			    message-endings))))
	  (setf actual-message
		(if (or start end)
		    (osubseq temporary-message
			     ;; +1 for the newline
			     ;; +1 to be at the first char of the next line
			     (or (and start (+ 2 start)) 0)
			     ;; +1 for the newline
			     (or (and end (1+ end))
				 (olength temporary-message)))
		    temporary-message)))

	;; Write the prompt
	(tt-move-to-col 0)
	(tt-erase-below)
	(when blank-line-func
	  (let ((row (terminal-get-cursor-position *terminal*)))
	    (with-saved-cursor (*terminal*)
	      (loop :repeat (- (tt-height) row 1)
		    :do (funcall blank-line-func) (tt-newline)))))

	(tt-write-string prompt)
	(tt-erase-to-eol)
	(setf prompt-height prompt-lines
	      new-last-line total-lines
	      start-col prompt-last-col)

	;; Write the line
	(if gutter-char
	    (write-with-gutter buffer endings gutter-char (1+ start-col) cols)
	    (tt-write-string buffer))
	;; (eol-compensate)
	(tt-erase-to-eol)

	;; Write out the right prompt
	(if (and right-prompt
		 (< point-col right-prompt-start)
		 (not endings)
		 (and (< line-last-col right-prompt-start)))
	    (progn
	      (tt-move-to-col (1- right-prompt-start))
	      (tt-write-string right-prompt)
	      (tt-move-to-col (1- right-prompt-start))
	      (tt-insert-char 1)
	      ;; (tt-move-to-col prompt-last-col)
	      )
	    ;; Erase when no right prompt
	    (tt-erase-to-eol))

	;; Write out the message
	(when actual-message
	  (tt-write-char #\newline)
	  (tt-write-string actual-message)
	  (when (plusp more-lines)
	    (tt-newline)
	    (tt-erase-to-eol)
	    (tt-format "[~d more lines]" more-lines))
	  (tt-erase-to-eol))

	;; Erase junk after the line
	(when last-line
	  (setf erase-lines (max 0 (- last-line new-last-line)))
	  ;; (dbugf :rlp "erase-lines = ~s~%" erase-lines)
	  (loop :repeat erase-lines
	     :do
	       (tt-down)
	       (tt-move-to-col 0)
	       (tt-erase-to-eol)
	       (when blank-line-func
		 (funcall blank-line-func)))
	  (when (not (zerop erase-lines))
	    (tt-up erase-lines)))

	;; Move the cursor to the point.
	(when (not (zerop (+ point-offset message-lines)))
	  (tt-up (+ point-offset message-lines-displayed
		    (if (not (zerop more-lines)) 1 0))))
	(tt-move-to-col point-col)
	(draw-mode-line e)

	;; Adjust the relative row and save the last-line
	(setf screen-relative-row (+ prompt-lines point-line)
	      last-line new-last-line)))))

(defmethod update-display ((e line-editor))
  (redraw-display e))

(defun tmp-prompt (e fmt &rest args)
  (apply #'tmp-message e fmt args)
  (redraw-display e))

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
	    endings (editor-calculate-line-endings
		     e :buffer (fat-string-string output-string)
                       :start-column 0))
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
  (with-slots (temporary-message keep-message message-top message-endings) e
    (setf temporary-message (with-output-to-fat-string (fs)
			      (apply #'format fs fmt args)
			      ;; @@@ It has to end in a newline?
			      ;; but we should fix the display code so it
			      ;; doesn't have to.
			      (when (and (not (zerop (olength fs)))
					 (ochar/= (oelt fs (1- (olength fs)))
						  #\newline))
				(write-char #\newline fs)))
	  keep-message nil
	  message-top 0
	  message-endings (nreverse (and temporary-message
					 (editor-calculate-line-endings
					  e :buffer temporary-message
					  :start-column 0
					  :end-column
					  (terminal-window-columns
					   (line-editor-terminal e))))))
    ;; (redraw-display e)
    ))

(defun message-prepend (e fmt &rest args)
  (tmp-message e "~a~a" (with-output-to-fat-string (fs)
			  (apply #'format fs fmt args))
	       (temporary-message e)))

(defmethod message ((e line-editor) fmt &rest args)
  (apply #'tmp-message e fmt args))

;; @@@ should probably be called something else, like "clear-under-area"?
;; "flense-undercarriage" "scrub-nether-region", hmmm... or maybe this is fine.
(defun clear-completions (e)
  "Erase completions, if there are any."
  (setf (temporary-message e) nil
	(max-message-lines e) 0
	(message-lines e) 0
	(message-endings e) nil
	(message-top e) 0
	(keep-message e) nil)
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
