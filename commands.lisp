;;
;; commands.lisp
;;

(in-package :rl)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(defmacro with-external ((e) &body body)
  "Do BODY outside the editor E, making sure that the terminal and display are
in proper condition."
  (with-unique-names (result)
    `(let (,result)
       ;;(finish-output (terminal-output-stream (line-editor-terminal ,e)))
       (terminal-end (line-editor-terminal ,e))
       (setf ,result (progn ,@body))
       (terminal-start (line-editor-terminal ,e))
       (redraw-command ,e) 			; maybe could do better?
       ,result)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defmulti (name args &body body)
    "Define a command that should be called for each editing context.
The slots of the editing context are bound in the body."
    (with-decls-and-body (body)
      `(progn
	 (defun ,name ,args
	   ,@doc-and-decls
	   (with-context ()
	     ,@fixed-body))
	 (setf (get ',name 'multiple) t))))

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
  (if (get function 'multiple)
      (do-contexts (e)
	(apply function e args))
      (apply function e args)))

;; @@@ Perhaps this should be merged with one in completion?
(defun scan-over (e dir &key func not-in action)
  "If FUNC is provied move over characters for which FUNC is true.
If NOT-IN is provied move over characters for which are not in it.
DIR is :forward or :backward. E is a line-editor.
If ACTION is given, it's called with the substring scanned over and replaces
it with ACTION's return value."
  (when (and (not func) not-in)
    (setf func #'(lambda (c) (not (position c not-in)))))
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
    (scan-over e :backward :func #'(lambda (c) (position c non-word-chars)))
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
    (scan-over e :forward :func #'(lambda (c) (position c non-word-chars)))
    (scan-over e :forward :not-in non-word-chars)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement commands

(defsingle previous-history (e)
  "Go to the previous history entry."
  (use-first-context (e)
    (history-put (buffer-string (buf e)) (history-context e))
    (history-prev (history-context e))
    (use-hist e)))

(defun point-coords (e)
  "Return the line and column of point."
  (with-context ()
    (let* ((spots `((,point . ())))
	   (endings (calculate-line-endings e :spots spots)))
      (dbugf :roo "in point-coords:~%spots = ~s endings = ~s~%" spots endings)
      (values (cdr (assoc point spots))
	      endings))))

(defun index-of-coords (e line col)
  (let* ((pair `(,line ,col))
	 (spots `((,pair . ())))
	 (endings (calculate-line-endings e :col-spots spots)))
    (dbugf :roo "in index-of-coords:~%spots = ~s endings = ~s~%" spots endings)
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
  (dbugf :roo "FIPPPY~%")
  (let* ((coords (point-coords e))
	 (line (car coords))
	 (col (cdr coords))
	 to-index endings)
    (dbugf :roo "FOOOOPY~%line = ~a col = ~a~%" line col)
    (setf (values to-index endings) (index-of-coords e (+ line n) col))
    (dbugf :roo "to-index = ~a endings = ~a~%" to-index endings)
    (if to-index
	(setf point to-index)
	;; If we didn't find the same column on the previous line,
	;; try to use index of the end of the previous line, or do nothing.
	(when (and endings (>= (+ line n) 0) (< (+ line n) (length endings))
		   (setf to-index (nth (+ line n) (reverse endings))))
	  (setf point (1+ (car to-index)))))))

(defmulti previous-line (e)
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

(defmulti next-line (e)
  (setf (line-editor-keep-region-active e) t)
  (forward-line e))

(defsingle next-history (e)
  "Go to the next history entry."
  (use-first-context (e)
    (history-put (buffer-string (buf e)) (history-context e))
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
    (history-put (buffer-string (buf e)) (history-context e))
    (history-go-to-first (history-context e))
    (use-hist e)))

(defsingle-method move-to-top ((e line-editor))
  (beginning-of-history e))

(defsingle end-of-history (e)
  "Go to the end of the history."
  (use-first-context (e)
    (history-put (buffer-string (buf e)) (history-context e))
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

(defsingle accept-line (e)
  (with-slots (buf buf-str quit-flag history-context accept-does-newline) e
    (history-go-to-last history-context)
    (if (add-to-history-p e buf-str)
	(history-put (buffer-string buf) history-context)
	(history-delete-last history-context))
    (setf quit-flag t)))

(defsingle-method accept ((e line-editor))
  (accept-line e))

(defmulti copy-region (e)
  "Copy the text between the insertion point and the mark to the clipboard."
  (with-slots (buf) e
    (let* ((start (min mark point))
	   (end (min (max mark point) (fill-pointer buf))))
      (setf clipboard (subseq buf start end)))))

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
  (with-slots (buf) e
    (let* ((start (min mark point))
	   (end (min (max mark point) (fill-pointer buf))))
      (setf clipboard (subseq buf start end)
	    point start)
      (buffer-delete e start end point))))

(defmulti exchange-point-and-mark (e)
  "Move point to the mark. Set the mark at the old point."
  (with-slots (keep-region-active) e
    (setf keep-region-active t)
    (when mark
      (rotatef point mark))))

(defsingle isearch-backward (e)
  "Incremental search backward."
  (isearch e :backward))

(defsingle-method search-command ((e line-editor))
  (isearch-backward e))

(defsingle isearch-forward (e)
  "Incremental search forward."
  (isearch e :forward))

(defun display-search (e str start end prompt)
  "Display the current line with the search string highlighted."
  (with-slots (buf history-context contexts temporary-message) e
    (setf temporary-message
	  (s+ prompt str))
      (if (and start (history-current history-context))
	  (save-excursion (e)
            ;; (setf point start
	    ;; 	  mark end		; for the highlightify
	    ;; 	  buf (highlightify
	    ;; 	       e (make-fatchar-string (history-current history-context))
	    ;; 	       :style
	    ;; 	       (or (theme-value *theme* '(:program :search-match :style))
	    ;; 		   (theme-value *theme* '(:rl :search-match :style))
	    ;; 		   '(:underline)))
	    ;; 	  mark nil)		; for the redraw-display
	    (setf contexts (make-contexts)
		  (context-point (aref contexts 0)) start
		  (context-mark (aref contexts 0)) end
		  buf (highlightify
		       e (make-fatchar-string (history-current history-context))
		       :style
		       (or (theme-value *theme* '(:program :search-match :style))
			   (theme-value *theme* '(:rl :search-match :style))
			   '(:underline))))
	    (redraw-display e))
	  (redraw-display e))))

(defun search-start-forward (history-context)
  (or (history-current-get history-context)
      (history-head (get-history history-context))))

(defun search-start-backward (history-context)
  (or (history-current-get history-context)
      (history-tail (get-history history-context))))

(defun backward-start-pos (str pos)
  (min (length str)
       (or pos (length str))))

(defun forward-start-pos (str pos)
  (cond
    ((not pos)
     0)
    ((< pos (1- (length str)))
     (1+ pos))
    (t (length str))))

(defun test-for-string (string)
  "Return the test function we should use for STRING. This 'smart' case folding,
where using any upper case triggers exact matching, and all lower case means,
be case insensitive."
  (if (some #'upper-case-p string) #'char= #'char-equal))

(defun search-history (e str direction start-from search-pos)
  (with-slots (history-context) e
    (let ((hist (get-history history-context))
	  (first-time t))
	;; (dbug "yoyo history-context ~w ~w~%" histroy-context hist)
      (if (eq direction :backward)
	  (progn
	    ;; (dbug "starting-at ~w~%" start-from)
	    (dl-list-do-element
	     start-from
	     #'(lambda (x)
		 (when (dl-content x)
		   ;; (dbug "(search ~w ~w :end2 ~w) search-pos = ~w~%"
		   ;; 	 str (dl-content x)
		   ;; 	 (backward-start-pos (dl-content x) search-pos)
		   ;; 	 search-pos)
		   (let (pos)
		     (if first-time
			 (setf pos (search str (history-line x)
					   :from-end t
					   :end2 (backward-start-pos
						  (history-line x) search-pos)
					   :test (test-for-string str)
					   )
			       first-time nil)
			 (setf pos (search str (history-line x) :from-end t
					   :test (test-for-string str))))
		     (when pos
		       ;; (dbug "found pos = ~w in ~w (~w) x=~a~%"
		       ;; 	     pos (dl-content x) str x)
		       (setf (history-cur hist) x)
		       (return-from search-history pos)))))))
	  (dl-list-do-backward-element
	   start-from
	   #'(lambda (x)
	       (when (dl-content x)
		 (let (pos)
		   (if first-time
		       (setf pos (search str (history-line x)
					 :start2 (forward-start-pos
						  (history-line x) search-pos)
					 :test (test-for-string str))
			     first-time nil)
		       (setf pos (search str (history-line x)
					 :test (test-for-string str))))
		   (when pos
		     (setf (history-cur hist) x)
		     (return-from search-history pos)))))))))
  nil)

(defparameter *isearch-prompt* "~:[~;failed ~]~:[>~;<~] i-search: ")

(defun isearch (e &optional (direction :backward))
  "Incremental search which updates the search position as the user types. The
search can be ended by typing a control character, which usually performs a
command, or Control-G which stops the search and returns to the start.
Control-R searches again backward and Control-S searches again forward."
  (with-slots (buf command history-context temporary-message last-search
		   contexts) e
    (with-slots (point) (aref contexts 0)
      (let ((quit-now nil)
	    (start-point point)
	    (start-hist (history-current-get history-context))
	    (search-string (make-stretchy-string *initial-line-size*))
	    (start-from (or (history-current-get history-context)
			    (history-head (get-history history-context))))
	    (pos point) end c #| added |# failed)
	(labels ((redisp ()
		   (display-search e search-string point end
				   (format nil *isearch-prompt*
					   failed (eq direction :backward))))
		 (resync ()
		   (buffer-delete e 0 (length buf) point)
		   (buffer-insert e 0 (or (history-current
					   (history-context e)) "")
				  point)
		   (setf point (min (or pos (length buf)) (length buf))
			 temporary-message nil
			 last-search search-string)
		   (redraw-display e)))
	  (loop :while (not quit-now)
	     :do
	     ;; (when (debugging e)
	     ;;   (debug-message e "pos = ~a start-from = ~a" pos start-from))
	     (redisp)
	     (tt-finish-output)
	     (setf c (get-a-char e))
	     ;; (setf added nil)
	     (cond
	       ((eql c (ctrl #\G))
		(setf point start-point)
		(setf (history-current history-context) start-hist)
		(use-hist e)
		(setf quit-now t))
	       ((eql c (ctrl #\S))
		(when (and (zerop (length search-string)) last-search)
		  (stretchy-append search-string last-search))
		(setf direction :forward
		      start-from (search-start-forward history-context)))
	       ((eql c (ctrl #\R))
		(when (and (zerop (length search-string)) last-search)
		  (stretchy-append search-string last-search))
		(setf direction :backward
		      start-from (search-start-backward history-context)))
	       ((eql c (ctrl #\L))
		(redisp))
	       ((or (eql c (ctrl #\h)) (eql c #\backspace) (eql c #\rubout))
		(stretchy-truncate search-string
				   (max 0 (1- (length search-string)))))
	       ((or (and (characterp c)
			 (or (control-char-p c) (meta-char-p (char-code c))))
		    (not (characterp c)))
		(resync)
		(return-from isearch c))
	       (t
		(stretchy-append search-string c)
		;; (setf added t)
		))
	     (if (setf pos (search-history
			    e search-string direction start-from pos))
		 (progn
		   (setf end (+ pos (length search-string))
			 point pos
			 failed nil))
		 (progn
		   (setf failed t)
		   ;; (when added
		   ;;   (stretchy-truncate search-string
		   ;; 		      (max 0 (1- (length search-string))))
		   ;;   (setf pos old-pos))
		   ;; (beep e "Not found")
		   )))
	  (resync))))))

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
  (with-slots (buf last-command quit-flag exit-flag) e
    (if (and (= point 0) (= (length buf) 0)
	     (not (eql last-command (ctrl #\d))))
	;; At the beginning of a blank line, we exit,
	;; so long as the last input wasn't ^D too.
	(setf quit-flag t
	      exit-flag t)
	(delete-char e))))

;;; Higher level editing functions

(defmulti backward-kill-word (e)
  (with-slots (buf non-word-chars) e
    (let ((start point))
      (scan-over e :backward :func #'(lambda (c) (position c non-word-chars)))
      (scan-over e :backward :not-in non-word-chars)
      (let ((region-str (subseq buf point start)))
	(setf clipboard region-str)
	(buffer-delete e point start point)))))

(defmulti kill-word (e)
  (with-slots (buf non-word-chars) e
    (let ((start point))
      (scan-over e :forward :func #'(lambda (c) (position c non-word-chars)))
      (scan-over e :forward :not-in non-word-chars)
      (when (< point (length buf))
	(incf point))
      (let ((region-str (subseq buf start point)))
	(setf clipboard region-str)
	(buffer-delete e point start point)
	(setf point start)))))

(defmulti kill-line (e)
  (with-slots (buf) e
    (let ((end (or (position #\newline buf :start point :key #'fatchar-c)
		   (fill-pointer buf))))
      ;; If we're sitting on a newline, kill that.
      (when (and (= end point)
		 (< point (fill-pointer buf))
		 (char= #\newline (simplify-char (aref buf point))))
	(incf end))
      (setf clipboard (if (eq (inator-last-command e) 'kill-line)
			  (oconcatenate clipboard (subseq buf point end))
			  (subseq buf point end)))
      (buffer-delete e point end point))))

(defmulti backward-kill-line (e)
  (with-slots (buf) e
    (let ((start (or (position #\newline buf
			       :from-end t :end point :key #'fatchar-c)
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
  (when clipboard
    (let ((len (length clipboard)))
      (insert e clipboard)
      (incf point len))))

(defmulti-method paste ((e line-editor))
  (yank e))

(defun forward-word-action (e action)
  (with-context ()
    (with-slots (buf non-word-chars) e
      (scan-over e :forward :func #'(lambda (c) (position c non-word-chars)))
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
  (apply-char-action-to-region e #'char-downcase
			       (or beginning mark)
			       (or end point)))

(defmulti upcase-region (e &optional beginning end)
  (apply-char-action-to-region e #'char-upcase
			       (or beginning mark)
			       (or end point)))

(defmulti downcase-word (e)
  (forward-word-action e #'(lambda (c) (char-downcase c))))

(defmulti upcase-word (e)
  (forward-word-action e #'(lambda (c) (char-upcase c))))

(defmulti capitalize-word (e)
  (let (bonk)
    (forward-word-action e #'(lambda (c)
			       (if (not bonk)
				   (progn (setf bonk t) (char-upcase c))
				   (char-downcase c))))))

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
	  :func #'(lambda (c) (and (alpha-char-p c) (upper-case-p c))))
	 ;;(message-pause e "first point = ~s ~s" point c)
	 (scan-over
	  e :forward
	  :func #'(lambda (c) (and (alpha-char-p c) (lower-case-p c))))
	 (setf c (buffer-char buf point))
	 ;;(message-pause e "second point = ~s ~s" point c)
	 (downcase-region e start point)
	 ;;(message-pause e "downcase ~s ~s" start point)
	 (setf c (buffer-char buf point))
	 ;;(message-pause e "third point ~s ~s" point c)
	 (when (and (alpha-char-p c) (upper-case-p c))
	   (insert e #\-)
	   (incf point))
	 (setf c (buffer-char buf point))
	 ;;(message-pause e "fourth point ~s ~s" point c)
	 :while (and (alpha-char-p c) (upper-case-p c)))
      (record-undo e 'boundary))))

(defmulti delete-horizontal-space (e)
  "Delete space before and after the cursor."
  (with-slots (buf) e
    (let ((origin point) start end)
      (setf origin point)
      (scan-over e :forward
		 :func #'(lambda (c) (position c dlib::*whitespace*)))
      (setf end point
	    point origin)
      (scan-over e :backward
		 :func #'(lambda (c) (position c dlib::*whitespace*)))
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
			 (if (char= (buffer-char buf i) #\")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hacks for typing Lisp.

(defparameter *paren-match-style* :flash
  "Style of parentheses matching. :FLASH, :HIGHLIGHT or :NONE.")

(defparameter *matched-pairs* "()[]{}"
  "Matching character pairs. Must be an even length string of paired characters
in order, \"{open}{close}...\".")

(defun is-matched-char (char)
  (position char *matched-pairs*))

(defun is-open-char (char)
  (let ((pos (position char *matched-pairs*)))
    (and pos (evenp pos))))

(defun is-close-char (char)
  (let ((pos (position char *matched-pairs*)))
    (and pos (oddp pos))))

(defun match-char (char)
  (let ((pos (position char *matched-pairs*)))
    (and pos (char *matched-pairs*
		   (if (evenp pos) (1+ pos) (1- pos))))))

(defun flash-paren (e)
  (with-slots (buf) e
    (with-context ()
      (let* ((str (buffer-string buf))
	     (ppos (matching-paren-position str :position point)))
	(if ppos
	    (let ((saved-point point))
	      (setf point ppos)
	      (update-display e)
	      (tt-finish-output)
	      (tt-listen-for .5)
	      (setf point saved-point))
	    (beep e "No match."))))))

(defun highlight-matching-parentheses (e)
  (with-slots (buf) e
    (do-contexts (e)
      (with-context ()
	(if (eq *paren-match-style* :highlight)
	    (cond
	      ((is-open-char (aref buf point))
	       (highlight-paren e point))
	      ((and (plusp point)
		    (is-close-char (aref buf (1- point))))
	       (highlight-paren e (1- point)))))))))

(defun highlight-paren (e pos)
  (let* ((str (buffer-string (buf e)))
	 (ppos (matching-paren-position str :position pos :char (aref str pos)))
	 #|offset offset-back |#)
    (log-message e "pos = ~s ppos = ~s" pos ppos)
    (if ppos
	;; (let ((saved-col (screen-col e)))
	;;   (declare (ignore saved-col))
	;;   (cond
	;;     ((> ppos pos)
	;;      (setf offset (- ppos pos)
	;; 	   offset-back (- (+ offset 1))))
	;;     (t
	;;      (setf offset (- (1+ (- pos ppos)))
	;; 	   offset-back (- pos ppos))))
	;;   (move-over e offset :start (point e))
	;;   (tt-bold t)
	;;   ;;(tt-write-char (match-char (aref str pos)))
	;;   (display-char e (match-char (aref str pos)))
	;;   (tt-bold nil)
	;;   (move-over e offset-back :start (1+ ppos)))
	(pushnew :bold (fatchar-attrs (aref (buf e) ppos)))
	;; @@@ but how/when to un-bold??
	)))

(defsingle finish-line (e)
  "Add any missing close parentheses and accept the line."
  (with-slots (buf) e
    (loop :while (matching-paren-position (buffer-string buf))
       :do
	 (insert e #\))
	 ;; (display-char e #\))
	 )
    (accept-line e)))

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
      (self-insert e t c))))

(defmulti self-insert (e &optional quoted char)
  (with-slots (command last-event buf) e
    (when (not char)
      (setf char last-event))
    (cond
      ((not (characterp char))
       ;; @@@ Perhaps we should get a real error, since this is probably a bug
       ;; not just a mis-configuration?
       ;;(cerror "Go on" "~a is not a character." char)
       (beep e "~a is not a character." char))
      ((and (not (graphic-char-p char)) (not quoted))
       (beep e "~a is unbound." char))
      (t
       ;; a normal character
       (if (= (length buf) point)
	   ;; end of the buf
	   (progn
	     (insert e char)
	     ;; flash paren and keep going
	     (when (and (eq *paren-match-style* :flash) (is-close-char char))
	       (flash-paren e))
	     (incf point))
	   ;; somewhere in the middle
	   (progn
	     (when (and (eq *paren-match-style* :flash) (is-close-char char))
	       (flash-paren e))
	     (insert e char)
	     (incf point)))))))

(defgeneric self-insert-command (line-editor)
  (:documentation "Try to insert a character into the buffer."))

(defmulti-method self-insert-command ((e line-editor))
  "Try to insert a character into the buffer."
  (self-insert e))

;; @@@ Is this reasonable?
(defmulti-method default-action ((e line-editor))
  (self-insert e))

(defmulti newline (e)
  "Insert a newline."
  (self-insert e t #\newline))

;; @@@ we can probably just use the one in terminal-inator?
;; (defmethod read-key-sequence ((e line-editor) &optional keymap)
;;   "Read a key sequence from the user. Descend into keymaps.
;;  Return a key or sequence of keys."
;;   (get-key-sequence (λ () (get-a-char e)) (or keymap (inator-keymap e))))

(defun ask-function-name (&optional (prompt "Function: "))
  "Prompt for a function name and return symbol of a function."
  (let* ((str (rl :prompt prompt :history-context :ask-function-name))
	 (cmd (and str (stringp str)
		   (ignore-errors (safe-read-from-string str)))))
    (and (symbolp cmd) (fboundp cmd) cmd)))

(defsingle set-key-command (e)
  "Bind a key interactively."
  (tmp-prompt e "Set key: ")
  (let* ((key-seq (read-key-sequence e))
	 (cmd (ask-function-name (format nil "Set key ~a to command: "
					 (key-sequence-string key-seq)))))
    (if cmd
	(set-key key-seq cmd (line-editor-local-keymap e))
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
    ;; (redraw-line e)
    (setf (line-editor-keep-region-active e) t)))

(defsingle what-cursor-position (e)
  "Describe the cursor position."
  (with-slots (contexts buf screen-relative-row screen-col keep-region-active) e
    (with-slots (point) (aref contexts 0)
      (let* ((fc (and (< point (length buf))
		      (aref buf point)))
	     (char (and fc (fatchar-c fc)))
	     (code (and char (char-code char))))
	(if fc
	    (tmp-message e "~s of ~s Row: ~s Column: ~s Char: '~a' ~a ~s #x~x"
			 point (length buf) screen-relative-row screen-col
			 fc
			 (and char (char-name char))
			 code code)
	    (tmp-message e "~s of ~s Row: ~s Column: ~s"
			 point (length buf) screen-relative-row screen-col))
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
	 (paste (read-bracketed-paste term))
	 (len (length paste)))
    (do-contexts (e)
      (with-context ()
	(insert e (if (translate-return-to-newline-in-bracketed-paste e)
		      (substitute #\newline #\return paste)
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
	  (self-insert e t result))
	(beep e "char-picker failed"))))

(defsingle unipose-command (e)
  "Compose unicode characters."
  (let ((first-ccc (get-a-char e)) second-ccc result)
    (setq second-ccc (get-a-char e))
    (setq result (unipose first-ccc second-ccc))
    (if result
	(do-contexts (e)
	  (self-insert e t result))
	(beep e "unipose ~c ~c unknown" first-ccc second-ccc))))

(defsingle insert-file (e)
  "Insert a file into the line editor's buffer."
  (let* ((file (read-filename :prompt "Insert-file: ")))
    (use-first-context (e)
      (with-context ()
	(buffer-insert e point (slurp file) point)))))

(defsingle add-cursor-on-next-line (e)
  "Add a cursor where next-line would take us."
  (with-slots (contexts) e
    (let ((c (copy-context (aref contexts (1- (length contexts))))))
      (use-context (c)
        (forward-line e))
      (add-context e (context-point c) nil))))

(defsingle just-one-context (e)
  "Get rid of all the contexts except one."
  (with-slots (contexts) e
    (when (> (length contexts) 1)
      ;; (setf contexts (subseq contexts 0 1)))))
      (setf contexts (make-contexts :copy-from (subseq contexts 0 1))))))

;; @@@ Consider adding:
;; With negative ARG, delete the last one instead.
;; With zero ARG, skip the last one and mark next.

(defsingle next-like-this (e)
  "If the selection is active, add another cursor and selection matching the
current selection. Otherwise, add a cursor on the next line."
  (with-slots (contexts) e
    (let ((c (copy-context (aref contexts (1- (length contexts))))))
      (use-context (c)
        (forward-line e))
      (add-context e (context-point c) nil))))

;; This seems like a more useful thing to be on ^G than abort-command.
(defsingle reset-stuff (e)
  "Reset some stuff."
  (with-slots (region-active contexts temporary-message) e
    (cond
      (region-active
       (setf region-active nil))
      ((> (length contexts) 1)
       (just-one-context e))
      (temporary-message
       (clear-completions e)))))

(defmacro with-filename-in-buffer ((e string-var position-var) &body body)
  (with-unique-names (str i buf)
    `(with-slots ((,buf buf)) ,e
       (let* ((,str (fatchar-string-to-string ,buf))
	      (,i (1- (length ,str)))
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
	 (log-message e "str = ~s" (subseq ,str ,i))
	 (setf ,string-var (if (zerop ,i)
			       ,str
			       (subseq ,str (1+ ,i)))
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

;; EOF
