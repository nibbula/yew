;;;
;;; commands.lisp - Editing commands
;;;

(in-package :rl)

(declaim #.`(optimize ,.(getf rl-config::*config* :optimization-settings)))

(defmacro with-external ((e) &body body)
  "Do ‘body’ outside the editor ‘e’, making sure that the terminal and display
are in proper condition."
  (with-names (result)
    `(let (,result)
       ;;(finish-output (terminal-output-stream (line-editor-terminal ,e)))
       (terminal-end (line-editor-terminal ,e))
       (setf ,result (progn ,@body))
       (terminal-start (line-editor-terminal ,e))
       (redraw-command ,e) 			; maybe could do better?
       ,result)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pre-name (name)
    (symbolify (s+ name "-PRE") :package (symbol-package name)))
  (defun post-name (name)
    (symbolify (s+ name "-POST") :package (symbol-package name)))

  (defmacro defmulti (name args &body body)
    "Define a command that should be called for each editing context.
The slots of the editing context are bound in the body, as in ‘with-context’.
A clause starting with :pre or :post at the beginning of the ‘body’ are run
before and after the body, respectively."
    (with-decls-and-body (body)
      (let (pre post)
	(when (and (keywordp (car fixed-body)) (eq (car fixed-body) :pre))
	  (setf pre (list
		     `(progn
			(defun ,(pre-name name) ,args
			  ,@doc-and-decls
			  ,(second fixed-body))
			(setf (get ',name 'pre) ',(pre-name name))))
		fixed-body (nthcdr 2 fixed-body)))
	(when (and (keywordp (car fixed-body)) (eq (car fixed-body) :post))
	  (setf post (list
		      `(progn
			 (defun ,(post-name name) ,args
			   ,@doc-and-decls
			   ,(second fixed-body))
			 (setf (get ',name 'post) ',(post-name name))))
		fixed-body (nthcdr 2 fixed-body)))
	`(progn
	   (defun ,name ,args
	     ,@doc-and-decls
	     (with-context ()
	       ,@fixed-body))
	   (setf (get ',name 'multiple) t)
	   ,@pre
	   ,@post))))

  (defmacro defmulti-method (name args &body body)
    "Define a command that should be called for each editing context.
The slots of the editing context are bound in the body."
    (with-decls-and-body (body)
      `(progn
	 (defmethod ,name ,args
	   ,@doc-and-decls
	   (with-context ()
	     ,@fixed-body))
	 (setf (get ',name 'multiple) t))))

  (defmacro defsingle (name args &body body)
    "Define a command that should be called once for all editing contexts."
    `(progn
       (defun ,name ,args ,@body)
       (setf (get ',name 'multiple) nil)))

  (defmacro defsingle-method (name args &body body)
    "Define a command that should be called once for all editing contexts."
    `(progn
       (defmethod ,name ,args ,@body)
       (setf (get ',name 'multiple) nil))))

(defmethod call-command ((e line-editor) function args)
  "Command invoker that handles calling commands for multiple editing contexts."
  (maybe-record-command e function args)
  (typecase function
    (symbol
     (if (get function 'multiple)
	 (progn
	   (when (get function 'pre)
	     (apply (pre-name function) e args))
	   (do-contexts (e)
	     (apply function e args))
	   (when (get function 'post)
	     (apply (post-name function) e args)))
	 (apply function e args)))
    (function
     ;; I guess non-symbol functions are on their own for mutltiple contexts.
     (apply function e args))
    (t
     (error "I don't know how to call a ~a ~a." (type-of function) function))))

(defun add-to-clipboard (string)
  "Add ‘string’ the to global clipboard."
  (push string *clipboard*))

(defun clear-clipboard ()
  "Clear the global clipboard."
  (setf *clipboard* nil))

(defun clipboard-as-string ()
  "Return the clipboard as a string."
  (with-output-to-string (str)
    (labels ((do-str (s)
	       (princ (fatchar-string-to-string s) str)))
      (typecase *clipboard*
	(string
	 (do-str *clipboard*))
	(list
	 (mapc (_ (do-str _) (princ #\newline str)) *clipboard*))
	(t
	 (format str "(~s ~s)" (type-of *clipboard*) *clipboard*))))))

;; @@@ Perhaps this should be merged with one in completion?
(defun scan-over (e dir &key func not-in action)
  "If FUNC is provied move over characters for which FUNC is true.
If NOT-IN is provied move over characters for which are not in it.
DIR is :forward or :backward. E is a line-editor.
If ACTION is given, it's called with the substring scanned over and replaces
it with ACTION's return value."
  (when (and (not func) not-in)
    (setf func #'(lambda (c) (not (oposition c not-in)))))
  (with-slots (buf) e
    (with-context ()
      (let (cc)
	(if (eql dir :backward)
	    ;; backward
	    (loop :while (and (> point 0)
			      (funcall func (buffer-char buf (1- point))))
	       :do
	       (when action
		 (when (setf cc (funcall action (buffer-char buf (1- point))))
		   (buffer-replace e (1- point) cc point)))
	       (decf point))
	    ;; forward
	    (let ((len (length buf))
		  (did-one nil))
	      (loop :while (and (< point len)
				(funcall func (buffer-char buf point)))
		 :do
		 (when action
		   (when (setf cc (funcall action (buffer-char buf point)))
		     (buffer-replace e point cc point)
		     (setf did-one t)))
		 (incf point))
	      (when did-one (decf point))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement commands

;; @@@ Should follow a more 'unicode' algorithm for finding word breaks.
(defmulti backward-word (e)
  "Move the insertion point to the beginning of the previous word or the
beginning of the buffer if there is no word."
  (with-slots (non-word-chars keep-region-active) e
    (scan-over e :backward :func #'(lambda (c) (oposition c non-word-chars)))
    (scan-over e :backward :not-in non-word-chars)
    (setf keep-region-active t)))

(defmulti mark-backward-word (e)
  "Set the mark if it's not already set or the region is not active,
and move backward a word."
  (with-slots (region-active) e
    (when (or (not region-active) (not mark))
      (set-mark e))
    (backward-word e)))

(defmulti-method backward-multiple ((e line-editor))
  (backward-word e))

(defmulti forward-word (e)
  "Move the insertion point to the end of the next word or the end of the
buffer if there is no word."
  (with-slots (non-word-chars keep-region-active) e
    (scan-over e :forward :func #'(lambda (c) (oposition c non-word-chars)))
    (scan-over e :forward :not-in non-word-chars)
    (setf keep-region-active t)))

(defmulti forward-word-or-accept-suggestion (e)
  "Move the insertion point to the end of the next word or the end of the
buffer if there is no word. If at the end of the buffer and there is a
suggestion, insert one word from it."
  (with-slots (non-word-chars keep-region-active buf suggestion) e
    (cond
      ((< point (fill-pointer buf))
       (scan-over e :forward :func (_ (oposition _ non-word-chars)))
       (scan-over e :forward :not-in non-word-chars))
      (suggestion
       (let ((pos 0))
	 (setf pos (scan-over-string suggestion pos :forward
				     :function (_ (oposition _ non-word-chars)))
	       pos (scan-over-string suggestion pos :forward
				     :not-in non-word-chars))
	 (insert e (osubseq suggestion 0 (clamp pos 0 (olength suggestion))))
	 (incf point pos))))
    (setf keep-region-active t)))

(defmulti mark-forward-word (e)
  "Set the mark if it's not already set or the region is not active,
and move forward a word."
  (with-slots (region-active) e
    (when (or (not region-active) (not mark))
      (set-mark e))
    (forward-word e)))

(defmulti-method forward-multiple ((e line-editor))
  (forward-word e))

(defmulti backward-char (e)
  "Move the insertion point backward one character in the buffer."
  (with-slots (keep-region-active) e
    (when (> point 0)
      (decf point))
    (setf keep-region-active t)))

(defmulti mark-backward-char (e)
  "Set the mark if it's not already set or the region is not active,
and move backward a character."
  (with-slots (region-active) e
    (when (or (not region-active) (not mark))
      (set-mark e))
    (backward-char e)))

(defmulti-method backward-unit ((e line-editor))
  (backward-char e))

(defmulti forward-char (e)
  "Move the insertion point forward one character in the buffer."
  (with-slots (buf keep-region-active) e
    (when (< point (fill-pointer buf))
      (incf point))
    (setf keep-region-active t)))

(defmulti forward-char-or-accept-suggestion (e)
  "Move the insertion point forward one character in the buffer, or if at the
end of a line and there is an auto-suggestion, accept it."
  (with-slots (buf keep-region-active suggestion) e
    (if (< point (fill-pointer buf))
	(incf point)
	(when suggestion
	  (insert e suggestion)
	  ;; The buffer won't necessarily be expaned by the same length as the
	  ;; suggestion, since it's converted to graphemes.
	  (setf point (fill-pointer buf))))
    (setf keep-region-active t)))

(defmulti mark-forward-char (e)
  "Set the mark if it's not already set or the region is not active,
and move forward a character."
  (with-slots (region-active) e
    (when (or (not region-active) (not mark))
      (set-mark e))
    (forward-char e)))

(defmulti-method forward-unit ((e line-editor))
  (forward-char e))

(defmulti beginning-of-line (e)
 "Move the insertion point to the beginning of the line."
  (with-slots (buf keep-region-active) e
    (when (> point 0)
      (let* ((end point)
	     (pos (oposition #\newline buf :end end :test #'ochar=
			     :from-end t)))
	(when pos
	  (incf pos))
	(setf point (or pos 0))))
    (setf keep-region-active t)))

(defmulti beginning-of-buffer (e)
  "Move the point to the beginning of the editor buffer."
  (with-slots (keep-region-active) e
    (when (> point 0)
      (setf point 0))
    (setf keep-region-active t)))

(defmulti-method move-to-beginning ((e line-editor))
  (beginning-of-line e))

(defmulti end-of-line (e)
  "Move the insertion point to the end of the line."
  (with-slots (buf keep-region-active) e
    (when (< point (fill-pointer buf))
      (let* ((start (if (ochar= #\newline (aref buf point)) (1+ point) point))
	     (pos (oposition #\newline buf :start start :test #'ochar=)))
	(setf point (or pos (fill-pointer buf)))))
    (setf keep-region-active t)))

(defmulti end-of-buffer (e)
  "Move the point to the end of the editor buffer."
  (with-slots (buf keep-region-active) e
    (when (< point (fill-pointer buf))
      (setf point (fill-pointer buf)))
    (setf keep-region-active t)))

(defmulti-method move-to-end ((e line-editor))
  (end-of-line e))

(defsingle-method next-page ((e line-editor))
  (with-slots (temporary-message max-message-lines message-lines message-top) e
    (when (and temporary-message (plusp message-lines))
      (when (> message-lines (+ message-top max-message-lines))
	(setf message-top (min (1- message-lines)
			       (+ message-top (1- max-message-lines))))))))

(defsingle-method previous-page ((e line-editor))
  (with-slots (temporary-message max-message-lines message-lines message-top) e
    (when (and temporary-message (plusp message-lines))
      (setf message-top (max 0 (- message-top max-message-lines))))))

(defsingle message-home (e)
  "Scroll the message to the beginning."
  (setf (message-top e) 0))

(defsingle message-end (e)
  "Scroll the message to the end."
  (with-slots (message-top max-message-lines message-lines) e
    (when (and message-lines (plusp message-lines)
	       max-message-lines)
      (setf message-top (- message-lines max-message-lines)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement commands

(defsingle previous-history (e)
  "Go to the previous history entry."
  (use-first-context (e)
    (history-put (buffer-string (buf e)) :context (history-context e))
    (history-prev (history-context e))
    (use-hist e)))

;; @@@ This is stupid. We should actually blow this thing up.
(defun point-coords (e point)
  "Return the line and column of ‘point’."
  (let* ((spots `((,point . ())))
	 (endings (editor-calculate-line-endings e :spots spots)))
    ;;(dbugf :roo "in point-coords:~%spots = ~s endings = ~s~%" spots endings)
    (values (cdr (assoc point spots))
	    endings)))

(defun index-of-coords (e line col)
  "Return the buffer index of ‘line’ and ‘col’."
  (let* ((pair `(,line ,col))
	 (spots `((,pair . ())))
	 (endings (editor-calculate-line-endings e :column-spots spots)))
    ;;(dbugf :roo "in index-of-coords:~%spots = ~s endings = ~s~%" spots endings)
    (values (cdr (assoc pair spots :test #'equal))
	    endings)))

(defmulti forward-line (e &key (n 1))
  "Move the point N lines, in the same column, or the end of the destination
line. Return NIL and do nothing if we can't move that far, otherwise return
the new point."
  ;; @@@ This is wasteful since it does calculate-line-endings twice. I'm not
  ;; sure that's entirely avoidable, but perhaps it could be quicker by making
  ;; it be a more generic buffer position iterator, and then bailing out as
  ;; soon as we get our thing.
  ;;
  ;; Also, we could prevent double traversal (at least here), by changing
  ;; calculate-line-endings to make the spot (in column-spot) parameters
  ;; functions, so that what column we wanted set in column-spots would be
  ;; gotten from the index in spots, as soon at it was set. Unfortunately that
  ;; wouldn't work for previous lines, only next lines.
  (let* ((coords (point-coords e point))
	 (line (car coords))
	 (col (cdr coords))
	 to-index endings)
    (setf (values to-index endings) (index-of-coords e (+ line n) col))
    (if to-index
	(setf point to-index)
	;; If we didn't find the same column on the previous line,
	;; try to use index of the end of the previous line, or do nothing.
	(when (and endings (>= (+ line n) 0) (< (+ line n) (length endings))
		   (setf to-index (nth (+ line n) (reverse endings))))
	  (setf point (1+ (car to-index)))))))

(defgeneric previous-line (editor)
  (:documentation "Move the point to the previous line."))

(defmulti-method previous-line (e)
  (setf (line-editor-keep-region-active e) t)
  (forward-line e :n -1))

(defmulti previous-line-or-history (e)
  "Go to the previous line, or the previous history entry if we're already at
the first line."
  ;;(if (find #\newline (simplify-string (buf e)))
  (when (not (previous-line e))
    (previous-history e)))

(defmulti-method previous ((e line-editor))
  (previous-line-or-history e))

(defgeneric next-line (editor)
  (:documentation "Move the point to the next line."))

(defmulti-method next-line ((e line-editor))
  (setf (line-editor-keep-region-active e) t)
  (forward-line e))

(defsingle next-history (e)
  "Go to the next history entry."
  (use-first-context (e)
    (history-put (buffer-string (buf e)) :context (history-context e))
    (history-next (history-context e))
    (use-hist e)))

(defmulti next-line-or-history (e)
  "Go to the next line, or the next history entry if we're at the last line."
  ;; (let ((simple-buf (simplify-string (buf e))))
  ;;   (if (find #\newline simple-buf)
  (when (not (next-line e))
    (next-history e)))

(defmulti-method next ((e line-editor))
  (next-line-or-history e))

(defsingle beginning-of-history (e)
  "Go to the beginning of the history."
  (use-first-context (e)
    (history-put (buffer-string (buf e)) :context (history-context e))
    (history-go-to-first (history-context e))
    (use-hist e)))

(defsingle-method move-to-top ((e line-editor))
  (beginning-of-history e))

(defsingle end-of-history (e)
  "Go to the end of the history."
  (use-first-context (e)
    (history-put (buffer-string (buf e)) :context (history-context e))
    (history-go-to-last (history-context e))
    (use-hist e)))

(defsingle-method move-to-bottom ((e line-editor))
  (end-of-history e))

(defun add-to-history-p (e buf-str)
  "Returns true if we should add the current line to the history. Don't add it
if it's blank or the same as the previous line."
  (with-slots (history-context allow-history-blanks allow-history-duplicates) e
    (let* ((cur (history-current-get history-context))
	   (prev (dl-next cur)))
      (flet ((is-blank ()
	       (and buf-str (zerop (olength buf-str))))
	     (is-dup ()
	       (and prev (dl-content prev) (history-line prev)
		    (ostring= (history-line prev) buf-str))))
	(and (or (not (is-blank)) allow-history-blanks)
	     (or (not (is-dup)) allow-history-duplicates))))))

(defsingle accept-line (e &key string)
  "Accept the buffer as input. If STRING is given, use that instead of the
current buffer."
  (with-slots (buf buf-str quit-flag history-context accept-does-newline) e
    (history-go-to-last history-context)
    (if (add-to-history-p e (or string buf-str))
	(history-put (or string (buffer-string buf)) :context history-context)
	(history-delete-last history-context))
    (setf quit-flag t)))

(defsingle-method accept ((e line-editor))
  (accept-line e))

(defmulti copy-region (e)
  "Copy the text between the insertion point and the mark to the clipboard."
  (declare (ignorable e))
  :pre (clear-clipboard)
  :post
  (with-slots (copy-region-sets-selection) e
    (when copy-region-sets-selection
      ;; @@@ Trying to copy the attributes is stupid right?
      (setf (tt-selection) (clipboard-as-string))))
  (with-slots (buf) e
    (let* ((start (min mark point))
	   (end (min (max mark point) (fill-pointer buf))))
      (setf clipboard (subseq buf start end))
      (add-to-clipboard (subseq buf start end)))))

(defmulti-method copy ((e line-editor))
  (copy-region e))

(defmulti set-mark (e)
  "Set the mark to be the current point."
  (with-slots (region-active keep-region-active) e
    (let ((toggle (not (eq 'set-mark (inator-last-command e)))))
      (setf mark point
	    region-active toggle
	    keep-region-active toggle))
    mark))

(defmulti-method select ((e line-editor))
  (set-mark e))

(defmulti kill-region (e)
  "Delete the text between the insertion point and the mark, and put it in
the clipboard."
  (declare (ignorable e))
  :pre (clear-clipboard)
  (with-slots (buf) e
    (let* ((start (min mark point))
	   (end (min (max mark point) (fill-pointer buf))))
      (setf clipboard (subseq buf start end)
	    point start)
      (add-to-clipboard (subseq buf start end))
      (buffer-delete e start end point))))

(defmulti exchange-point-and-mark (e)
  "Move point to the mark. Set the mark at the old point."
  (with-slots (keep-region-active) e
    (setf keep-region-active t)
    (when mark
      (rotatef point mark))))

(defsingle redraw-command (e)
  "Clear the screen and redraw the prompt and the input line."
  (with-slots (prompt-string prompt-func buf need-to-redraw
	       keep-region-active) e
    (tt-clear) (tt-home)
    (setf (screen-col e) 0 (screen-relative-row e) 0)
    (update-display e)
    (setf need-to-redraw nil
	  keep-region-active t)))

(defsingle-method redraw ((e line-editor))
  "Clear the screen and redraw the prompt and the input line."
  (redraw-command e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer editing

(defmulti insert (e thing)
  "Insert thing into the buffer at point."
  (buffer-insert e point thing point))

(defmulti delete-region (e start end)
  "Delete the region of the buffer between the positions start and end.
Don't update the display."
  (with-slots (buf) e
    (buffer-delete e start end point)
    ;; Make sure the point stays in the buffer.
    (when (> point (fill-pointer buf))
      (setf point (fill-pointer buf)))))

(defmulti delete-backward-char (e)
  "Backward delete a character from buf at point"
  (with-slots (buf) e
    (when (> point 0)
      (buffer-delete e (1- point) point point)
      (decf point))))

(defmulti delete-char (e)
  "Delete the character following the cursor."
  (with-slots (buf) e
    (if (= point (fill-pointer buf))
	(beep e "End of buffer")
	(buffer-delete e point (1+ point) point))))

(defmulti delete-char-or-exit (e)
  "At the beginning of a blank line, exit, otherwise delete-char."
  (with-slots (buf last-command quit-flag exit-flag history-context) e
    (if (and (= point 0) (= (length buf) 0)
	     (not (eql last-command (ctrl #\d))))
	;; At the beginning of a blank line, we exit,
	;; so long as the last input wasn't ^D too.
	(progn
	  (setf quit-flag t
		exit-flag t)
	  ;; Don't leave nil entires in the history.
	  ;; @@@ we should probably make this the responsibility of the calling
	  ;; code, not the command, like this and accept-line, etc.
	  (history-go-to-last history-context)
	  (history-delete-last history-context))
	(delete-char e))))

;;; Higher level editing functions

(defmulti backward-kill-word (e)
  (with-slots (buf non-word-chars) e
    (let ((start point))
      (scan-over e :backward :func #'(lambda (c) (oposition c non-word-chars)))
      (scan-over e :backward :not-in non-word-chars)
      (let ((region-str (subseq buf point start)))
	(setf clipboard region-str)
	(buffer-delete e point start point)))))

(defmulti kill-word (e)
  (with-slots (buf non-word-chars) e
    (let ((start point))
      (scan-over e :forward :func #'(lambda (c) (oposition c non-word-chars)))
      (scan-over e :forward :not-in non-word-chars)
      (when (< point (length buf))
	(incf point))
      (let ((region-str (subseq buf start point)))
	(setf clipboard region-str)
	(buffer-delete e point start point)
	(setf point start)))))

(defmulti kill-line (e)
  (with-slots (buf) e
    (let ((end (or (oposition #\newline buf :start point :test #'ochar=)
		   (fill-pointer buf))))
      ;; If we're sitting on a newline, kill that.
      (when (and (= end point)
		 (< point (fill-pointer buf))
		 (ochar= #\newline (aref buf point)))
	(incf end))
      (setf clipboard (if (eq (inator-last-command e) 'kill-line)
			  (oconcatenate clipboard (osubseq buf point end))
			  (osubseq buf point end)))
      (buffer-delete e point end point))))

(defmulti backward-kill-line (e)
  (with-slots (buf) e
    (let ((start (or (oposition #\newline buf :from-end t :end point)
		     0)))
      (when (> point 0)
	(when (not (zerop start))
	  (incf start))
	(setf clipboard (subseq buf start point))
	;; (if (zerop start)
	;;     (replace-buffer e (subseq buf point))
	(buffer-delete e start point point)
	(setf point start)
	;;(beginning-of-line e)
	)
      (clear-completions e))))

(defmulti yank (e)
  (let ((clip (or clipboard (clipboard-as-string))))
    (when clip
      (let ((len (length clip)))
	(insert e clip)
	(incf point len)))))

(defmulti-method paste ((e line-editor))
  (yank e))

(defun forward-word-action (e action)
  (with-context ()
    (with-slots (buf non-word-chars) e
      (scan-over e :forward :func #'(lambda (c) (oposition c non-word-chars)))
      (scan-over e :forward :not-in non-word-chars :action action)
      (when (< point (length buf))
	(incf point)))))

(defun apply-char-action-to-region (e char-action &optional beginning end)
  "Apply a function that takes a character and returns a character, to
every character in the region delimited by BEGINING and END. If BEGINING
and END aren't given uses the the current region, or gets an error if there
is none."
  (with-slots (buf) e
    (with-context ()
      (when (and (not mark) (or (not beginning) (not end)))
	(error "Mark must be set if beginning or end not given."))
      (when (not beginning)
	(setf beginning (min mark point)))
      (when (not end)
	(setf end (max mark point)))
      (when (> beginning end)
	(rotatef end beginning))
      (let ((old-mark mark)
	    (old-point point))
	(unwind-protect
	     (progn
	       ;;(setf mark beginning)
	       ;;(rotatef point mark)
	       ;;(exchange-point-and-mark e)
	       (setf point beginning)
	       ;;(scan-over e :forward :func (constantly t) :action char-action))
	       (log-message e "point = ~s end = ~s" point end)
	       (scan-over e :forward :func (_ (< point end))
			  :action
			  char-action
			  ;; (_ (let ((r (funcall char-action _)))
			  ;;      (message-pause e "~s -> ~s" _ r)
			  ;;      r))
			  )
	       (if (< point (length buf)) (incf point)))
	  (setf mark old-mark
		point old-point))))))

(defmulti downcase-region (e &optional beginning end)
  (apply-char-action-to-region e #'ochar-downcase
			       (or beginning mark)
			       (or end point)))

(defmulti upcase-region (e &optional beginning end)
  (apply-char-action-to-region e #'ochar-upcase
			       (or beginning mark)
			       (or end point)))

(defmulti downcase-word (e)
  (forward-word-action e #'(lambda (c) (ochar-downcase c))))

(defmulti upcase-word (e)
  (forward-word-action e #'(lambda (c) (ochar-upcase c))))

(defmulti capitalize-word (e)
  (let (bonk)
    (forward-word-action e #'(lambda (c)
			       (if (not bonk)
				   (progn (setf bonk t) (ochar-upcase c))
				   (ochar-downcase c))))))

(defmulti un-studly-cap (e)
  "Convert from StupidVarName to stupid-var-name."
  (with-slots (buf) e
    (record-undo e 'boundary)
    (let (c start)
      (loop :do
	 (setf start point)
	 (setf c (buffer-char buf point))
	 (scan-over
	  e :forward
	  :func #'(lambda (c) (and (oalpha-char-p c) (oupper-case-p c))))
	 ;;(message-pause e "first point = ~s ~s" point c)
	 (scan-over
	  e :forward
	  :func #'(lambda (c) (and (oalpha-char-p c) (olower-case-p c))))
	 (when (>= point (olength buf))
	   (downcase-region e start (1- point))
	   (return))
	 (setf c (buffer-char buf point))
	 ;;(message-pause e "second point = ~s ~s" point c)
	 (downcase-region e start point)
	 ;;(message-pause e "downcase ~s ~s" start point)
	 (when (>= point (olength buf))
	   (return))
	 (setf c (buffer-char buf point))
	 ;;(message-pause e "third point ~s ~s" point c)
	 (when (and (oalpha-char-p c) (oupper-case-p c))
	   (insert e #\-)
	   (incf point))
	 (when (>= point (olength buf))
	   (return))
	 (setf c (buffer-char buf point))
	 ;;(message-pause e "fourth point ~s ~s" point c)
	 :while (and (oalpha-char-p c) (oupper-case-p c)))
      (record-undo e 'boundary))))

(defmulti delete-horizontal-space (e)
  "Delete space before and after the cursor."
  (with-slots (buf) e
    (let ((origin point) start end)
      (setf origin point)
      (scan-over e :forward
		 :func #'(lambda (c) (oposition c dlib::*whitespace*)))
      (setf end point
	    point origin)
      (scan-over e :backward
		 :func #'(lambda (c) (oposition c dlib::*whitespace*)))
      (setf start point)
      (delete-region e start end))))

(defmulti transpose-characters (e)
  "Swap the character before the cursor with the one it's on, and advance the
cursor. At the beginning or end of the buffer, adjust the point so it works.
Don't do anything with less than 2 characters."
  (with-slots (buf) e
    (when (>= (length buf) 2)
      (cond
	((zerop point)
	 (incf point))
	((= point (fill-pointer buf))
	 (decf point)))
      (let ((first-char (buffer-char buf (1- point))))
	(buffer-replace e (1- point) (buffer-char buf point) point)
	(buffer-replace e point first-char point))
      (incf point))))

(defmulti quote-region (e)
  "Put double quotes around the region, escaping any double quotes inside it."
  (with-slots (buf) e
    (when (and mark (/= point mark))
      (let* ((start (min point mark))
	     (end (max point mark))
	     (new (with-output-to-string (str)
		    (write-char #\" str)
		    (loop :for i :from start :below end
		       :do
			 (if (ochar= (buffer-char buf i) #\")
			     (progn
			       (write-char #\\ str)
			       (write-char #\" str))
			     (write-char (buffer-char buf i) str)))
		    (write-char #\" str))))
	(buffer-delete e start end point)
	(buffer-insert e start new point)
	;; Adjust the point to be at the end of the replacement.
	(when (= point end)
	  (incf point (- (length new) (- end start))))))))

(defmethod maybe-record-command ((e line-editor) function args)
  "Record ‘event’ if we have event recording turned on."
  (with-slots (recording-p recording) e
    (when recording-p
      (when (not recording)
	(setf recording (make-stretchy-vector 80)))
      ;; (message e "Recorded ~s" event)
      (when (and (not (equal function #'stop-recording))
		 (not (equal function 'stop-recording)))
	(stretchy-append recording (list (list* function args)
					 (last-event e)))))))

(defsingle start-recording (e)
  "Start recording events."
  ;; Discard the old one and make a new one.
  (if (recording-p e)
      (tmp-message e "Recording already started.")
      (progn
	(setf (recording e) (make-stretchy-vector 80)
	      (recording-p e) t)
	(message e "Starting recording..."))))

(defsingle stop-recording (e)
  "Stop recording events."
  (with-slots (recording-p recording) e
    (if recording-p
	(progn
	  (message e "Stopping recording.")
	  (compile-recording e))
	(message e "Recording wasn't in progress."))
    (setf recording-p nil)))

(defsingle input-counter (e)
  "Use the replay counter as input."
  (loop :for c :across (format nil "~d" (replay-count e))
	:do (push c (queued-input e))))

(defmulti replay-recording (e)
  "Replay recorded events."
  (typecase (recording e)
    (null
      (message e "There isn't anything recorded."))
    (function
     (funcall (recording e) e))
    (t
     (message e "The recording is an unknown type: ~s."
	      (type-of (recording e)))))
  (incf (replay-count e)))

(defsingle name-last-recording (e)
  "Name the last recording."
  (typecase (recording e)
    (null
     (message e "There isn't anything recorded."))
    (function
     (setf (fdefinition (ask-expr "Name: ")) (recording e)))
    (t
     (set (ask-expr "Name: ") (recording e)))))

(defmacro with-input ((e event) &body body)
  `(progn (setf (last-event ,e) ,event) ,@body))

(defmacro playback ((e recording) &body body)
  "A dumb hack to get self-insert to work right."
  (with-names (rec)
    `(let ((,rec ,recording))
       ,@(loop
	   :for expr :in body
	   :for i :from 0
	   :collect `(with-input (,e (oelt ,rec ,i))
		       ,expr)))))

(defun recording-to-expr (e recording &key (editor-name '*line-editor*))
  (declare (ignore e))
  (let ((events (loop :for c :across recording :collect (second c)))
	(commands (loop :for c :across recording :collect (first c))))
    `(let ((e ,editor-name))
       (playback (e ',events)
         ,@(loop :for c :in commands
		 :collect `(,(car c) e ,@(cdr c)))))))

(defsingle compile-recording (e)
  "Replace the recording with a function that performs the recordings actions
with the current keymaps."
  (typecase (recording e)
    (null
     (message e "There is no recording to compile."))
    (function
     (message e "The recording is already compiled."))
    (t
     (multiple-value-bind (func warns fails)
	 (compile nil `(lambda (e)
			 ,(recording-to-expr e (recording e) :editor-name 'e)))
       (declare (ignore warns))
       (when (or (not func) fails)
	 (error "Failed to compile a recording: ~s~%" (recording e)))
       ;; @@@ I feel like it's not cool that we lose the original recording, but
       ;; do we really want to make it more complicated? To make it a true
       ;; recording of all interaction, we'd probably have to do it at the
       ;; terminal level anyway.
       (setf (recording e) func)))))

;; (defmulti dabbrev-expand ()
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hacks for typing Lisp.

(defparameter *paren-match-style* :flash
  "Style of parentheses matching. :FLASH, :HIGHLIGHT or :NONE.")

(defparameter *matched-pairs* "()[]{}"
  "Matching character pairs. Must be an even length string of paired characters
in order, \"{open}{close}...\".")

(defun is-matched-char (char)
  (oposition char *matched-pairs*))

(defun is-open-char (char)
  (let ((pos (oposition char *matched-pairs*)))
    (and pos (evenp pos))))

(defun is-close-char (char)
  (let ((pos (oposition char *matched-pairs*)))
    (and pos (oddp pos))))

(defun match-char (char)
  (let ((pos (oposition char *matched-pairs*)))
    (and pos (char *matched-pairs*
		   (if (evenp pos) (1+ pos) (1- pos))))))

(defun flash-paren (e)
  "Move the cursor momentarily to the matching opening character for the one
just before the cursor."
  (with-slots (buf-str) e
    (with-context ()
      (multiple-value-bind (ppos complain)
	  (matching-paren-position buf-str :position point
				   :char (last-event e))
	(when ppos
	  (let ((saved-point point))
	    (setf point ppos)
	    (update-display e)
	    (tt-finish-output)
	    (tt-listen-for .5)
	    (setf point saved-point)))
	(when complain
	  (beep e "No match."))))))

(defun highlight-matching-parentheses (e)
  "Hightlight the match of the current character if it's an opening character,
 or of the previous, if it's a closing character."
  (with-slots (buf) e
    (do-contexts (e)
      (with-context ()
	(when (eq *paren-match-style* :highlight)
	  (log-message e "highlight paren")
	  (cond
	    ((and (< point (length buf)) (is-open-char (aref buf point)))
	     (highlight-paren e point))
	    ((and (plusp point)
		  (is-close-char (aref buf (1- point))))
	     (highlight-paren e (1- point)))))))))

(defun unhighlight-matching-parentheses (e)
  "If a parenthese is highlighted, un-highlight it."
  (with-slots (matching-char-pos) e
    (when matching-char-pos
      (highlight-paren e matching-char-pos :state nil))))

(defun highlight-paren (e pos &key (state t))
  "Hightlight the character at ‘pos’, or un-highlight if ‘state’ is nil."
  (with-slots (buf-str) e
    (let* ((ppos 0))
      (when (< pos (olength buf-str))
	(setf ppos (matching-paren-position
		    buf-str
		    :position pos
		    :char (osimplify (ochar buf-str pos))))
	(log-message e "pos = ~s ppos = ~s" pos ppos)
	(when ppos
	  (with-slots (matching-char-pos saved-matching-char) e
	    (cond
	      (state
	       ;; (add-attr :bold (aref (buf e) ppos))
	       (setf saved-matching-char (copy-fatchar (aref (buf e) ppos))
		     matching-char-pos pos)
	       (styled-char (theme:theme-value
			     theme:*theme*
			     '(:program :editor :paren-highlight :style))
			    (aref (buf e) ppos)))
	      (t
	       ;; (remove-attr :bold (aref (buf e) ppos))
	       (setf matching-char-pos nil
		     (aref (buf e) ppos) saved-matching-char
		     saved-matching-char nil)))))))))

(defsingle finish-line (e)
  "Add any missing close parentheses and accept the line."
  (with-slots (buf) e
    (loop :while (matching-paren-position (buffer-string buf))
       :do
	 (insert e #\))
	 ;; (display-char e #\))
	 )
    (accept-line e)))

(defmulti insert-last-argument (e)
  "Insert the last word of the previous history line."
  (with-slots (non-word-chars) e
    (let ((x (dl-next (history-head (get-history (history-context e))))))
      (when x
	(setf x (osplit-if (_ (oposition _ non-word-chars))
			   (history-entry-line (dl-content x))))
	(when (setf x (last x))
	  (insert e (car x))))))
  (end-of-line e))

;; @@@ This shouldn't be in here. Maybe it should be in tiny-repl or lish itself?
(defsingle pop-to-lish (e)
  "If we're inside lish, throw to a quick exit. If we're not in lish, enter it."
  (let* ((lish-package (find-package :lish))
	 (level-symbol (intern "*LISH-LEVEL*" lish-package)))
    (when lish-package
      (if (and (boundp level-symbol) (numberp (symbol-value level-symbol)))
	  (funcall (find-symbol "LISHITY-SPLIT" :lish))
	  (progn
	    (tt-beginning-of-line)
	    (tt-erase-line)
	    ;;(finish-output (terminal-output-stream (line-editor-terminal e)))
	    ;;(terminal-set-input-mode (line-editor-terminal e) :line)
	    (terminal-end (line-editor-terminal e))
	    (if (line-editor-terminal-device-name e)
		(funcall (find-symbol "LISH" :lish)
			 :terminal-name (line-editor-terminal-device-name e))
		(funcall (find-symbol "LISH" :lish)))
	    (tt-beginning-of-line)
	    (tt-erase-line)
	    (setf (screen-col e) 0)
	    ;; (with-slots (prompt-string prompt-func point buf) e
	    ;;   (do-prompt e prompt-string prompt-func)
	    ;;   (display-buf e)
	    ;;   (when (< point (length buf))
	    ;; 	(move-backward e (string-display-length (subseq buf point)))))
	    ;;(terminal-set-input-mode (line-editor-terminal e) :line)
	    (setf (terminal-input-mode (line-editor-terminal e)) :line)
	    (terminal-start (line-editor-terminal e)))))))

(defsingle abort-command (e)
  "Invoke the debugger from inside."
  (declare (ignore e))
  ;; Maybe this should just flash the screen?
  ;; (with-simple-restart (continue "Continue RL")
  ;;   (invoke-debugger (make-condition
  ;; 		      'simple-condition
  ;; 		      :format-control "Abort command")))
  (abort))

(defsingle toggle-debugging (e)
  "Toggle debugging output."
  (with-slots (debugging) e
    (setf debugging (not debugging))))

(defsingle quoted-insert (e)
  "Insert the next character input without interpretation."
  (let ((c (get-a-char e)))
    (do-contexts (e)
      (self-insert e c :quoted t))))

(defmulti self-insert (e event &key quoted)
  (with-slots (command buf) e
    (when (not event)
      (setf event (last-event e)))
    (cond
      ((not (characterp event))
       ;; @@@ Perhaps we should get a real error, since this is probably a bug
       ;; not just a mis-configuration?
       ;;(cerror "Go on" "~a is not a character." event)
       (beep e "~a is not a character." event))
      ((and (not (graphic-char-p event)) (not quoted))
       (beep e "~a is unbound." event))
      (t
       ;; a normal character
       (if (= (length buf) point)
	   ;; end of the buf
	   (progn
	     (insert e event)
	     ;; flash paren and keep going
	     (when (and (eq *paren-match-style* :flash) (is-close-char event))
	       (flash-paren e))
	     (incf point))
	   ;; somewhere in the middle
	   (progn
	     (when (and (eq *paren-match-style* :flash) (is-close-char event))
	       (flash-paren e))
	     (insert e event)
	     (incf point)))))))

(defgeneric self-insert-command (line-editor)
  (:documentation "Try to insert a character into the buffer."))

(defmulti-method self-insert-command ((e line-editor))
  "Try to insert a character into the buffer."
  (self-insert e nil))

;; @@@ Is this reasonable?
(defmulti-method default-action ((e line-editor) &optional event)
  (self-insert e event))

(defmulti newline (e)
  "Insert a newline."
  (self-insert e #\newline :quoted t))

;; @@@ we can probably just use the one in terminal-inator?
;; (defmethod read-key-sequence ((e line-editor) &optional keymap)
;;   "Read a key sequence from the user. Descend into keymaps.
;;  Return a key or sequence of keys."
;;   (get-key-sequence (λ () (get-a-char e)) (or keymap (inator-keymap e))))

(defun ask-expr (&optional (prompt "Expression: "))
  "Prompt for an thing and return it."
  (let* ((str (rl :prompt prompt :history-context :ask-function-name
		  :recursive-p t :accept-does-newline nil))
	 (expr (and str (stringp str)
		    (ignore-errors (safe-read-from-string str)))))
    expr))

(defun ask-function-name (&optional (prompt "Function: "))
  "Prompt for a function name and return symbol of a function."
  (let ((function (ask-expr prompt)))
    (and (symbolp function) (fboundp function) function)))

(defun local-keymap (e)
  "Return the local keymap for ‘e’. Make one if it doesn't have one and put
it in the list of keymaps."
  (or (inator-local-keymap e)
      (prog1 (setf (inator-local-keymap e)
		   (make-instance 'keymap:keymap :name "rl local keymap"))
	(pushnew (inator-local-keymap e) (inator-keymap e)))))

(defsingle set-key-command (e)
  "Bind a key interactively."
  (tmp-prompt e "Set key: ")
  (let* ((key-seq (read-key-sequence e))
	 (cmd (ask-function-name (format nil "Set key ~a to command: "
					 (key-sequence-string key-seq)))))
    (clear-completions e)
    (redraw-display e)
    (if cmd
	(set-key key-seq cmd (local-keymap e))
	(tmp-message e "Not a function."))))

(defsingle-method describe-key-briefly ((e line-editor))
  "Tell what function a key invokes."
  (tmp-prompt e "Describe key: ")
  (let* ((key-seq (read-key-sequence e))
	 def)
    (cond
      ((not key-seq)
       (tmp-message e "You pressed an unknown key."))
      (t
       (setf def (key-sequence-binding key-seq (line-editor-keymap e)))
       (if def
	   (tmp-message e "~w is bound to ~a"
			(key-sequence-string key-seq) def)
	   (tmp-message e "~w is not bound"
			(key-sequence-string key-seq)))))
    (setf (line-editor-keep-region-active e) t)))

(defsingle what-cursor-position (e)
  "Describe the cursor position."
  (with-slots ((contexts inator::contexts)
	       buf #| screen-relative-row screen-col |# keep-region-active) e
    (with-slots (point) (aref contexts 0)
      (let* ((fc (and (< point (length buf))
		      (aref buf point)))
	     ;; (char (and fc (fatchar-c fc)))
	     (code (and fc (multiple-value-list (ochar-code fc))))
	     (coords (point-coords e point))
	     (row (car coords))
	     (col (cdr coords)))
	(if fc
	    (tmp-message e "~s of ~s Row: ~s Column: ~s Char: '~a' ~
                            ~{~a~^ ~} ~{~s~^ ~} ~{#x~x~^ ~}"
			 point (length buf)
			 ;; screen-relative-row screen-col
			 row col
			 fc
			 (and fc (multiple-value-list (ochar-name fc)))
			 code code)
	    (tmp-message e "~s of ~s Row: ~s Column: ~s"
			 point (length buf)
			 ;; screen-relative-row screen-col
			 row col
			 ))
	(setf keep-region-active t)))))

(defsingle exit-editor (e)
  "Stop editing."
  (with-slots (quit-flag exit-flag) e
    (setf quit-flag t
	  exit-flag t)))

(defsingle-method quit ((e line-editor))
  (exit-editor e))

;; This is mostly for binding to purposely meaningless commands.
(defsingle beep-command (e)
  "Just ring the bell or something."
  (beep e "Woof! Woof!"))

(defun find-ansi-terminal (term)
  "Return a TERMINAL-ANSI that is *terminal* or wrapped by *terminal*, or NIL."
  (loop
     :for i :from 0
     :while (and (not (typep term 'terminal-ansi))
		 (typep term 'terminal-wrapper)
		 (< i 10))
     :if (typep term 'terminal-ansi)
     :do (return term)
     :else
     :do (setf term (terminal-wrapped-terminal term)))
  (when (typep term 'terminal-ansi)
    term))

;; This is a defsingle, because we want to read a single paste, even though we
;; want to paste it in multiple places.
(defsingle bracketed-paste (e)
  (let* ((term (or (find-ansi-terminal (line-editor-terminal e))
		   (error "I don't know how to read a bracketed paste on ~
                             a ~a." (type-of *terminal*))))
	 (paste (make-fat-string :string (read-bracketed-paste term)))
	 (len (olength paste)))
    (do-contexts (e)
      (with-context ()
	(insert e (if (translate-return-to-newline-in-bracketed-paste e)
		      (osubstitute #\newline #\return paste :test #'ochar=)
		      paste))
	(incf point len)))))

(defsingle char-picker-command (e)
  "Pick unicode (or whatever) characters."
  (let ((result
	 (with-external (e)
	   (when (not (find-package :char-picker))
	     (asdf:load-system :char-picker))
	   (symbol-call :char-picker :char-picker))))
    (if result
	(do-contexts (e)
	  (self-insert e result :quoted t))
	(beep e "char-picker failed"))))

(defsingle unipose-command (e)
  "Compose unicode characters."
  (let ((first-ccc (get-a-char e)) second-ccc result)
    (setq second-ccc (get-a-char e))
    (setq result (unipose first-ccc second-ccc))
    (if result
	(do-contexts (e)
	  (self-insert e result :quoted t))
	(beep e "unipose ~c ~c unknown" first-ccc second-ccc))))

(defsingle insert-file (e)
  "Insert a file into the line editor's buffer."
  (let* ((file (read-filename :prompt "Insert-file: ")))
    (use-first-context (e)
      (with-context ()
	(buffer-insert e point (slurp file) point)))))

(defsingle save-line-command (e)
  "Save the current line to a file."
  (let* ((file (read-filename :prompt "Save line to file: "
			      :allow-nonexistent t)))
    (if file
	(use-first-context (e)
          (with-context ()
	    (with-open-file (stream file :direction :output
				    :if-does-not-exist :create)
	      (write-string (buffer-string (buf e)) stream)
	      (terpri stream))))
	(tmp-message e "Not a function."))))

(defsingle add-cursor-on-next-line (e)
  "Add a cursor where next-line would take us."
  (with-slots ((contexts inator::contexts)) e
    (let ((c (copy-editing-context (aref contexts (1- (length contexts))))))
      (use-context (c)
        (forward-line e))
      (add-context e (inator-point c) nil))))

(defsingle just-one-context (e)
  "Get rid of all the contexts except one."
  (with-slots ((contexts inator::contexts)) e
    (when (> (length contexts) 1)
      ;; (setf contexts (subseq contexts 0 1)))))
      (setf contexts (make-contexts :copy-from (subseq contexts 0 1))))))

;; @@@ Consider adding:
;; With negative ARG, delete the last one instead.
;; With zero ARG, skip the last one and mark next.

(defsingle next-like-this (e)
  "If the selection is active, add another cursor and selection matching the
current selection. Otherwise, add a cursor on the next line."
  (with-slots ((contexts inator::contexts)) e
    (let ((c (copy-editing-context (aref contexts (1- (length contexts))))))
      (use-context (c)
        (forward-line e))
      (add-context e (inator-point c) nil))))

;; This seems like a more useful thing to be on ^G than abort-command.
(defsingle reset-stuff (e)
  "Reset some stuff."
  (with-slots ((contexts inator::contexts) region-active temporary-message) e
    (cond
      (region-active
       (setf region-active nil))
      ((> (length contexts) 1)
       (just-one-context e))
      (temporary-message
       (clear-completions e)))))

(defmacro with-filename-in-buffer ((e string-var position-var) &body body)
  (with-names (str i buf)
    `(with-slots ((,buf buf)) ,e
       (let* ((,str (fatchar-string-to-string ,buf))
	      ;; (,i (1- (length ,str)))
	      (,i (1- (first-point e)))
	      ,string-var ,position-var)
	 (declare (ignorable ,string-var ,position-var))
	 (loop ;; back up until a double quote or a / or a ~ preceded by a space
	    :while (and (not (zerop ,i))
			(char/= (char ,str ,i) #\")
			(not (and (> ,i 0)
				  (or (char= (char ,str ,i) #\/)
				      (char= (char ,str ,i) #\~))
				  (char= (char ,str (1- ,i)) #\space))))
	    :do (decf ,i))
	 (log-message e "i = ~s str = ~s" ,i (subseq ,str ,i))
	 (setf ,string-var (if (zerop ,i)
			       ,str
			       (subseq ,str (1+ ,i) (first-point e)))
	       ,position-var (1+ ,i))
	 ,@body))))

(defsingle complete-filename-command (e)
  "Filename completion. This useful for when you want to explicitly complete a
filename instead of whatever the default completion is. Convenient for a key
binding."
  (with-filename-in-buffer (e str pos)
    (complete e :function #'completion::complete-filename
	      :start-from pos)))

(defsingle show-filename-completions-command (e)
  "Filename completion. This useful for when you want to explicitly complete a
filename instead of whatever the default completion is. Convenient for a key
binding."
  (with-filename-in-buffer (e str pos)
    (show-completions e :func #'completion::complete-filename
		      :string str)))

(defmulti dictionary-complete (e)
  "Complete a word from the dictionary."
  (with-slots (non-word-chars str) e
    (let ((start-pos point))
      ;; (scan-over e :backward :not-in non-word-chars)
      (backward-word e)
      (rotatef start-pos point)
      (complete e :function #'completion::complete-dictionary-word
		  :start-from start-pos))))

(defun history-prefix-matches (e &key line)
  "Return a list of the endings and the length for editor ‘e’ and ‘line’ or the
current line."
  (when (not line)
    (setf line (get-buffer-string e)))
  (let ((pos 0) match
	(table (make-hash-table :test #'equal)))
    (map-history-backward
       #'(lambda (entry)
	   (when (and (history-entry-line entry)
		      (setf pos (osearch line (history-entry-line entry)))
		      (zerop pos)
		      (> (olength (history-entry-line entry)) (olength line)))
	     (setf match (history-entry-line entry)
		   (gethash match table) t))))
    (let ((len 0))
      (values
       (with-collecting ()
	 (omapk (_ (collect (aref _ 0)) (incf len)) table))
       len))))

(defun history-prefix-match-ending (e &key line)
  "Return the first ending of the most recent line from history that begins with
the current line, or NIL if there is none."
  (when (not line)
    (setf line (get-buffer-string e)))
  (let ((pos 0))
    (block nil
      (map-history-backward
       #'(lambda (entry)
	   (when (and (history-entry-line entry)
		      (setf pos (osearch line (history-entry-line entry)))
		      (zerop pos)
		      (> (olength (history-entry-line entry)) (olength line)))
	     (return
	       (osubseq (history-entry-line entry) (olength line)))))))))

(defun complete-history (context pos all)
  (complete-list context pos all
		 (history-prefix-matches *line-editor*
					 :line (osubseq context 0 pos))))

(defsingle complete-history-command (e)
  "History completion."
  (complete e :function #'complete-history))

(defgeneric auto-suggest-command (e)
  (:documentation "Calculate a suggested ending for the current line."))

(defsingle-method auto-suggest-command (e)
  "Pick a suggestion from the history, using history-prefix-match-ending."
  (with-slots (buf auto-suggest-style suggestion) e
    (setf suggestion (history-prefix-match-ending e)
	  auto-suggest-style
	  (or (theme-value *theme* '(:program :suggestion :style))
	      auto-suggest-style))))

#|
(defun history-previous-match (e &key line)
  "Return the most recent line from history that contains the current line,
or NIL if there is none."
  (with-slots (match-element) e
    (when (not line)
      (setf line (get-buf-str e)))
    (let (pos entry)
      (block nil
	(map-history-backward-from
	  #'(lambda (element)
	      (setf entry (dl-content element))
	      (when (and (history-entry-line entry)
			 (setf pos (osearch line (history-entry-line entry))))
		(return element)))
	  match-element)))))
|#

(defsingle dwim-up (e)
  "If we're not at the first line, go to the previous line.
If we're at the first line and it's blank go to the previous history entry.
If we're at the first and only non-blank line, go to the next most recent
matching history entry."
  (use-first-context (e)
    (with-context ()
      (let ((line (get-buf-str e)) (eol 0))
	(cond
	  ((or (zerop (olength line))			  ; Empty line
	       (and (setf eol (oposition #\newline line)) ; Not on first line
		    (>= point eol)))
	   (previous-line-or-history e))
	  (t
	   #|
	   (let ((m (history-previous-match e))
		 line)
	     (when m
	       (setf (match-element e) m
		     line (history-entry-line (ofirst m)))))
	   |#
	   (go-to-matching-history e (osimplify (get-buf-str e)))))))))

(defgeneric toggle-mode-line (e)
  (:documentation "Toggle displaying the modeline."))

(defsingle-method toggle-mode-line (e)
  "Toggle displaying the modeline."
  (with-slots (show-mode-line) e
    (setf show-mode-line (not show-mode-line))))

(defsingle re-indent (e)
  "Try to re-indent the code."
  (use-first-context (e)
    (with-context ()
      (let ((code (ignore-errors (read-from-string (simplify-string (buf e))))))
	(when code
	  (let ((saved-point point)
		(re-code
		 (with-output-to-string (str)
		   (let ((*print-case* :downcase))
		     (pprint code str)))))
	    (setf point 0)
	    (buffer-delete e 0 (olength (buf e)) point)
	    (buffer-insert e 0 re-code point)
	    (setf point (min saved-point (olength re-code)))))))))

(defun in-initial-whitespace-p (e)
  "Return true if point is between the start of the line and the first
non-whitespace character."
  (use-first-context (e)
    (with-context ()
      (save-excursion (e)
	(scan-over e :backward
		   :func (lambda (c)
			   (and (ochar/= c #\newline)
				(oposition c dlib::*whitespace*))))
	(and (not (zerop point))
	     (ochar= (buffer-char (buf e) (1- point)) #\newline))))))

(defun indent-a-line (e)
  (with-context ()
    (with-slots (buf) e
      (let* ((str (buffer-string buf))
	     (ppos (matching-paren-position str :position point))
	     indent)
	(log-message e "ppos = ~s" ppos)
	(save-excursion (e)
	  (setf point ppos)
	  (beginning-of-line e)
	  (setf indent (- ppos point)
		point ppos)
	  (scan-over e :forward :not-in dlib::*whitespace*)
	  (setf indent point))

	(log-message e "indent = ~s" indent)
	(beginning-of-line e)
	;; Move over or insert spaces until indent.
	(let ((len (olength buf))
	      (i point))
	  (loop :while (< i indent)
	     :do
	       (cond
		 ((or (= point len)
		      (not (oposition (buffer-char buf point)
				      dlib::*whitespace*)))
		  (buffer-insert e point #\space point)
		  (incf point))
		 (t
		  (incf point)))
	       (incf i)))))))

(defsingle indent-single-line (e)
  (use-first-context (e)
    (indent-a-line e)))

(defmulti indent-line (e)
  (indent-single-line e))

(defsingle magic-tab (e)
  (if (in-initial-whitespace-p e)
      (do-contexts (e)
	(indent-single-line e))
      (complete e)))

;; This is vaugely like recursive edit.
(defun push-buffer (e)
  (use-first-context (e)
    (with-context ()
      (with-slots (pushed-buffers buf) e
	(push (cons point (buffer-copy buf)) pushed-buffers)))))

(defun pop-buffer (e)
  (let ((b (pop (pushed-buffers e))))
    (use-first-context (e)
      (with-context ()
	(setf point (car b)
	      (buf e) (cdr b))))))

(defsingle park-it (e)
  "Go edit another line, and come back to this one next time the editor is
invoked."
  (push-buffer e)
  (let ((saved-relative-row (screen-relative-row e)))
    (freshen e)
    (setf (screen-relative-row e) saved-relative-row)))

(defsingle eval-expression (e)
  "Evaluate an expression in the editor."
  (let* ((expr (ask-expr)))
    (tmp-message e "⇒ ~s" (eval expr))))

(defmulti insert-expression (e)
  "Read and evaluate an expression in the editor and insert it in the buffer."
  (let* ((expr (ask-expr)))
    (insert e (prin1-to-string (eval expr)))))

;; End
