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

;; @@@ Perhaps this should be merged with one in completion?
(defun scan-over (e dir &key func not-in action)
  "If FUNC is provied move over characters for which FUNC is true.
If NOT-IN is provied move over characters for which are not in it.
DIR is :forward or :backward. E is a line-editor.
If ACTION is given, it's called with the substring scanned over and replaces
it with ACTION's return value."
  (when (and (not func) not-in)
    (setf func #'(lambda (c) (not (position c not-in)))))
  (with-slots (point buf) e
    (let (cc)
      (if (eql dir :backward)
	  ;; backward
	  (loop :while (and (> point 0)
			   (funcall func (buffer-char buf (1- point))))
	    :do
	    (when action
	      (when (setf cc (funcall action (buffer-char buf (1- point))))
		(buffer-replace e (1- point) cc)))
	     (decf point))
	  ;; forward
	  (let ((len (length buf))
		(did-one nil))
	    (loop :while (and (< point len)
			     (funcall func (buffer-char buf point)))
	      :do
	       (when action
		 (when (setf cc (funcall action (buffer-char buf point)))
		   (buffer-replace e point cc)
		   (setf did-one t)))
	       (incf point))
	    (when did-one (decf point)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement commands

;; @@@ Should follow a more 'unicode' algorithm for finding word breaks.
(defun backward-word (e)
  "Move the insertion point to the beginning of the previous word or the
beginning of the buffer if there is no word."
  (with-slots (point non-word-chars) e
    (scan-over e :backward :func #'(lambda (c) (position c non-word-chars)))
    (scan-over e :backward :not-in non-word-chars)))

(defmethod backward-multiple ((e line-editor))
  (backward-word e))

(defun forward-word (e)
  "Move the insertion point to the end of the next word or the end of the
buffer if there is no word."
  (with-slots (point non-word-chars) e
    (scan-over e :forward :func #'(lambda (c) (position c non-word-chars)))
    (scan-over e :forward :not-in non-word-chars)))

(defmethod forward-multiple ((e line-editor))
  (forward-word e))

(defun backward-char (e)
  "Move the insertion point backward one character in the buffer."
  (with-slots (point) e
    (when (> point 0)
      (decf point))))

(defmethod backward-unit ((e line-editor))
  (backward-char e))

(defun forward-char (e)
  "Move the insertion point forward one character in the buffer."
  (with-slots (point buf) e
    (when (< point (fill-pointer buf))
      (incf point))))

(defmethod forward-unit ((e line-editor))
  (forward-char e))

(defun beginning-of-line (e)
 "Move the insertion point to the beginning of the line."
  (with-slots (point buf) e
    (when (> point 0)
      (let* ((end point)
	     (pos (oposition #\newline buf :end end :test #'ochar=
			     :from-end t)))
	(when pos
	  (incf pos))
	(setf point (or pos 0))))))

(defmethod move-to-beginning ((e line-editor))
  (beginning-of-line e))

(defun end-of-line (e)
  "Move the insertion point to the end of the line."
  (with-slots (point buf) e
    (when (< point (fill-pointer buf))
      (let* ((start (if (ochar= #\newline (aref buf point)) (1+ point) point))
	     (pos (oposition #\newline buf :start start :test #'ochar=)))
	(setf point (or pos (fill-pointer buf)))))))

(defmethod move-to-end ((e line-editor))
  (end-of-line e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement commands

(defun previous-history (e)
  "Go to the previous history entry."
  (history-put (buffer-string (buf e)) (context e))
  (history-prev (context e))
  (use-hist e))

(defun point-coords (e)
  "Return the line and column of point."
  (let* ((spots `((,(point e) . ())))
	 (endings (calculate-line-endings e :spots spots)))
    (dbugf :roo "in point-coords:~%spots = ~s endings = ~s~%" spots endings)
    (values (cdr (assoc (point e) spots))
	    endings)))

(defun index-of-coords (e line col)
  (let* ((pair `(,line ,col))
	 (spots `((,pair . ())))
	 (endings (calculate-line-endings e :col-spots spots)))
    (dbugf :roo "in index-of-coords:~%spots = ~s endings = ~s~%" spots endings)
    (values (cdr (assoc pair spots :test #'equal))
	    endings)))

(defun forward-line (e &key (n 1))
  "Move the point N lines, in the same column, or the end of the destination
line. Return NIL and do nothing if we can't move that far, otherwise return
the new point."
  ;; @@@ This is wasteful since it does calculate-line-endings twice. I'm not
  ;; sure that's entirely avoidable, but perhaps it could be quicker by making
  ;; it be a more generic buffer position iterator, and then bailing out as
  ;; soon as we get our thing.
  (with-slots (point) e
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
	    (setf point (1+ (car to-index))))))))

(defun previous-line (e)
  (forward-line e :n -1))

(defun previous-line-or-history (e)
  "Go to the previous line, or the previous history entry if we're already at
the first line."
  ;;(if (find #\newline (simplify-string (buf e)))
  (when (not (previous-line e))
    (previous-history e)))

(defmethod previous ((e line-editor))
  (previous-line-or-history e))

(defun next-line (e)
  (forward-line e))

(defun next-history (e)
  "Go to the next history entry."
  (history-put (buffer-string (buf e)) (context e))
  (history-next (context e))
  (use-hist e))

(defun next-line-or-history (e)
  "Go to the next line, or the next history entry if we're at the last line."
  ;; (let ((simple-buf (simplify-string (buf e))))
  ;;   (if (find #\newline simple-buf)
  (when (not (next-line e))
    (next-history e)))

(defmethod next ((e line-editor))
  (next-line-or-history e))

(defun beginning-of-history (e)
  "Go to the beginning of the history."
  (history-put (buffer-string (buf e)) (context e))
  (history-first (context e))
  (use-hist e))

(defmethod move-to-top ((e line-editor))
  (beginning-of-history e))

(defun end-of-history (e)
  "Go to the end of the history."
  (history-put (buffer-string (buf e)) (context e))
  (history-last (context e))
  (use-hist e))

(defmethod move-to-bottom ((e line-editor))
  (end-of-history e))

(defun add-to-history-p (e buf)
  "Returns true if we should add the current line to the history. Don't add it
if it's blank or the same as the previous line."
  (with-slots (context) e
    (let* ((cur (history-current-get context))
	   (prev (dl-next cur)))
#|
      (dbug "add-to-history-p = ~w~%  buf = ~w~%  (length buf) = ~w~%~
  cur = ~w~%  prev = ~w~%  (dl-content prev) = ~w~%"
	    (not (or (and buf (= (length buf) 0))
		     (and prev (dl-content prev)
			  (equal (dl-content prev) buf))))
	    buf (length buf) cur prev (dl-content prev))
|#
      (not (or (and buf (= (length buf) 0))
	       (and prev (dl-content prev) (equal (dl-content prev) buf)))))))

(defun accept-line (e)
  (with-slots (buf buf-str point quit-flag context accept-does-newline) e
    (history-last context)
    (if (add-to-history-p e buf)
	(history-put (buffer-string buf) context)
	(history-delete-last context))
    (when accept-does-newline
      (tt-write-string buf-str :start point)
      (tt-write-char #\newline)
      (tt-write-char #\return)
      (when (did-under-complete e)
	(tt-erase-below)))
    (setf quit-flag t)))

(defmethod accept ((e line-editor))
  (accept-line e))

(defun copy-region (e)
  "Copy the text between the insertion point and the mark to the clipboard."
  (with-slots (point mark buf clipboard) e
    (setf clipboard (subseq buf mark point))))

(defmethod copy ((e line-editor))
  (copy-region e))

(defun set-mark (e)
  "Set the mark to be the current point."
  (with-slots (point mark) e
    (setf mark point)))

(defmethod select ((e line-editor))
  (set-mark e))

(defun exchange-point-and-mark (e)
  "Move point to the mark. Set the mark at the old point."
  (with-slots (point mark) e
    (when mark
      (rotatef point mark))))

(defun isearch-backward (e)
  "Incremental search backward."
  ;; (isearch e :backward)
  (and e t)
  )

(defmethod search-command ((e line-editor))
  (isearch-backward e))

(defun isearch-forward (e)
  "Incremental search forward."
  ;; (isearch e :forward)
  (and e t)
  )

;; Sadly ASCII / UTF-8 specific. @@@ And should be moved to char-util?
;; (defun control-char-p (c)
;;   (let ((code (char-code c)))
;;     (or (< code 32) (= code 128))))

(defparameter *isearch-prompt* "isearch: ")

#|
(defun display-search (e str pos)
  "Display the current line with the search string highlighted."
  (with-slots (buf point) e
    ;;(setf point (min (or pos (length buf)) (length buf)))
    (erase-display e)
    ;;(tt-move-to-col 0)
    ;;(tt-erase-to-eol)
    ;;(setf (screen-col e) 0)
    ;;(do-prefix e *isearch-prompt*)
    (when (and str pos)
      (without-undo (e)
	;;(erase-display e)
	(setf point 0)
	(buffer-delete e 0 (length buf))
	(buffer-insert e 0 (or (history-current (context e)) ""))
	(move-over e (min (or pos (length buf)) (length buf)))
	(setf point (min (or pos (length buf)) (length buf)))
	)
      )
    #|
      (loop :with end = (if pos (+ pos (length str)) nil)
	   :for c :across buf :and i = 0 :then (1+ i) :do
	   (cond
	     ((and pos (= i pos))
	      (tt-underline t))
	     ((and end (= i end))
	      (tt-underline nil)))
	   (display-char e c))
	(tt-underline nil))
    (tt-finish-output)
    |#
    ;;(display-buf e 0 pos)
    ;;(tt-underline t)
    ;;(display-buf e pos (+ pos (length str)))
    ;;(tt-underline nil)
    ;;(display-buf e (+ pos (length str))
    ))
|#

#|
(defun display-search (e str pos)
  "Display the current line with the search string highlighted."
  (with-slots (point context) e
    (erase-display e)
    (do-prefix e *isearch-prompt*)
    ;;(log-message e "buf = ~s" buf)
    (when (history-current context)
      (loop :with end = (if pos (+ pos (length str)) nil)
	 :for c :across (history-current context)
	 :and i = 0 :then (1+ i) :do
	 (cond
	   ((and pos (= i pos))
	    (tt-underline t))
	   ((and end (= i end))
	    (tt-underline nil)))
	 (display-char e c)))
    (tt-underline nil)
    (tt-finish-output)))

(defun search-start-forward (context)
  ;; (or (and (history-current-get context)
  ;; 	   (dl-prev (history-current-get context)))
  (or (history-current-get context)
      (history-head (get-history context))))

(defun search-start-backward (context)
  ;; (or (and (history-current-get context)
  ;; 	   (dl-next (history-current-get context)))
  (or (history-current-get context)
      (history-tail (get-history context))))

(defun backward-start-pos (str pos)
  ;; (cond
  ;;   ((not pos)
  ;;    (length str))
  ;;   ((> pos 0)
  ;;    (min (1- pos) (length str)))
  ;;   (t 0)))
  (min (length str)
       (or pos (length str))))

(defun forward-start-pos (str pos)
  (cond
    ((not pos)
     0)
    ((< pos (1- (length str)))
     (1+ pos))
    (t (length str))))

(defun search-history (e str direction start-from search-pos)
  (with-slots (point context) e
    (let ((hist (get-history context))
	  (first-time t))
;      (dbug "yoyo context ~w ~w~%" context hist)
      (if (eq direction :backward)
	  (progn
;	    (dbug "starting-at ~w~%" start-from)
	    (dl-list-do-element
	     start-from
	     #'(lambda (x)
		 (when (dl-content x)
		   (dbug "(search ~w ~w :end2 ~w) search-pos = ~w~%"
			 str (dl-content x)
			 (backward-start-pos (dl-content x) search-pos)
			 search-pos)
		   (let (pos)
		     (if first-time
			 (setf pos (search str (dl-content x)
					   :from-end t
					   :end2 (backward-start-pos
						  (dl-content x) search-pos))
			       first-time nil)
			 (setf pos (search str (dl-content x) :from-end t)))
		     (when pos
		       (dbug "found pos = ~w in ~w (~w) x=~a~%"
			     pos (dl-content x) str x)
		       (setf (history-cur hist) x)
		       (return-from search-history pos)))))))
	  (dl-list-do-backward-element
	   start-from
	   #'(lambda (x)
	       (when (dl-content x)
		 (let (pos)
		   (if first-time
		       (setf pos (search str (dl-content x)
					 :start2 (forward-start-pos
						  (dl-content x) search-pos))
			     first-time nil)
		       (setf pos (search str (dl-content x))))
		   (when pos
		     (setf (history-cur hist) x)
		     (return-from search-history pos)))))))))
  nil)

(defun isearch (e &optional (direction :backward))
  "Incremental search which updates the search position as the user types. The
search can be ended by typing a control character, which usually performs a
command, or Control-G which stops the search and returns to the start.
Control-R searches again backward and Control-S searches again forward."
  (with-slots (point buf command context) e
    (let ((quit-now nil)
	  (start-point point)
	  (start-hist (history-current-get context))
	  (search-string (make-stretchy-string *initial-line-size*))
	  (start-from (or (history-current-get context)
			  (history-head (get-history context))))
	  (pos point) old-pos c added)
      (labels ((redisp ()
		 (display-search e search-string pos))
	       (resync ()
		 (buffer-delete e 0 (length buf))
		 (buffer-insert e 0 (or (history-current (context e)) ""))
		 (setf point (min (or pos (length buf)) (length buf)))))
	(redisp)
	(loop :while (not quit-now)
	   :do
	   (when (debugging e)
	     (debug-message e "pos = ~a start-from = ~a" pos start-from))
	   (display-search e search-string pos)
	   (setf c (get-a-char e)
		 added nil)
	   (cond
	     ((eql c (ctrl #\G))
	      (setf point start-point)
	      (setf (history-current context) start-hist)
	      (use-hist e)
	      (setf quit-now t))
	     ((eql c (ctrl #\S))
	      (setf direction :forward
		    start-from (search-start-forward context)))
	     ((eql c (ctrl #\R))
	      (setf direction :backward
		    start-from (search-start-backward context)))
	     ((eql c (ctrl #\L))
	      (redisp))
	     ((or (eql c (ctrl #\h)) (eql c #\backspace) (eql c #\rubout))
	      (stretchy-truncate search-string
				 (max 0 (1- (length search-string)))))
	     ((or (control-char-p c) (meta-char-p (char-code c)))
	      (resync)
	      (redraw-line e)
	      (return-from isearch c))
	     (t
	      (stretchy-append search-string c)
	      (setf added t)))
	   (if (setf pos (search-history
			  e search-string direction start-from pos))
	       (progn
		 (setf old-pos pos
		       point pos))
	       (progn
		 (when added
		   (stretchy-truncate search-string
				      (max 0 (1- (length search-string))))
		   (setf pos old-pos))
		 (beep e "Not found"))))
	(resync)
	(redraw-line e)))))
|#

;; @@@ Consider calling redraw?
(defun redraw-command (e)
  "Clear the screen and redraw the prompt and the input line."
  (with-slots (prompt-string prompt-func point buf need-to-redraw) e
    (tt-clear) (tt-home)
    (setf (screen-col e) 0 (screen-relative-row e) 0)
    (update-display e)
    (setf need-to-redraw nil)))

(defmethod redraw ((e line-editor))
  (redraw-command e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer editing

(defun insert-char (e c)
  "Insert a character into the buffer at point. Don't update the display."
  (declare (type character c))
  (buffer-insert e (point e) c))

(defun insert-string (e s)
  "Insert a string into the buffer at point. Don't update the display."
;  (declare (type string s))
  (buffer-insert e (point e) s))

(defun delete-region (e start end)
  "Delete the region of the buffer between the positions start and end.
Don't update the display."
  (with-slots (point buf) e
    (buffer-delete e start end)
    ;; Make sure the point stays in the buffer.
    (when (> point (fill-pointer buf))
      (setf point (fill-pointer buf)))))

(defun delete-backward-char (e)
  "Backward delete a character from buf at point"
  (with-slots (point buf) e
    (when (> point 0)
      (buffer-delete e (1- point) point)
      (decf point))))

(defun delete-char (e)
  "Delete the character following the cursor."
  (with-slots (point buf) e
    (if (= point (fill-pointer buf))
	(beep e "End of buffer")
	(buffer-delete e point (1+ point)))))

(defun delete-char-or-exit (e)
  "At the beginning of a blank line, exit, otherwise delete-char."
  (with-slots (point buf last-command quit-flag exit-flag) e
    (if (and (= point 0) (= (length buf) 0)
	     (not (eql last-command (ctrl #\d))))
	;; At the beginning of a blank line, we exit,
	;; so long as the last input wasn't ^D too.
	(setf quit-flag t
	      exit-flag t)
	(delete-char e))))

;;; Higher level editing functions that DO update the display

(defun backward-kill-word (e)
  (with-slots (buf point non-word-chars clipboard) e
    (let ((start point))
      (scan-over e :backward :func #'(lambda (c) (position c non-word-chars)))
      (scan-over e :backward :not-in non-word-chars)
      (let ((region-str (subseq buf point start)))
	(setf clipboard region-str)
	(buffer-delete e point start)))))

(defun kill-word (e)
  (with-slots (buf point non-word-chars clipboard) e
    (let ((start point))
      (scan-over e :forward :func #'(lambda (c) (position c non-word-chars)))
      (scan-over e :forward :not-in non-word-chars)
      (when (< point (length buf))
	(incf point))
      (let ((region-str (subseq buf start point)))
	(setf clipboard region-str)
	(buffer-delete e point start)
	(setf point start)))))

(defun kill-line (e)
  (with-slots (clipboard buf point) e
    (let ((end (or (position #\newline buf :start point :key #'fatchar-c)
		   (fill-pointer buf))))
      ;; If we're sitting on a newline, kill that.
      (when (and (= end point)
		 (< point (fill-pointer buf))
		 (char= #\newline (simplify-char (aref buf point))))
	(incf end))
      (setf clipboard (subseq buf point end))
      (buffer-delete e point end))))

(defun backward-kill-line (e)
  (with-slots (point clipboard buf) e
    (let ((start (or (position #\newline buf
			       :from-end t :end point :key #'fatchar-c)
		     0)))
      (when (> point 0)
	(when (not (zerop start))
	  (incf start))
	(setf clipboard (subseq buf start point))
	;; (if (zerop start)
	;;     (replace-buffer e (subseq buf point))
	(buffer-delete e start point)
	(setf point start)
	;;(beginning-of-line e)
	)
      (clear-completions e))))

(defun yank (e)
  (with-slots (clipboard point) e
    (when clipboard
      (let ((len (length clipboard)))
	(insert-string e clipboard)
	(incf point len)))))

(defmethod paste ((e line-editor))
  (yank e))

(defun forward-word-action (e action)
  (with-slots (point buf non-word-chars) e
    (scan-over e :forward :func #'(lambda (c) (position c non-word-chars)))
    (scan-over e :forward :not-in non-word-chars :action action)
    (when (< point (length buf))
      (incf point))))

(defun apply-char-action-to-region (e char-action &optional beginning end)
  "Apply a function that takes a character and returns a character, to
every character in the region delimited by BEGINING and END. If BEGINING
and END aren't given uses the the current region, or gets an error if there
is none."
  (with-slots (point mark buf) e
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
	     (scan-over e :forward :func (_ (< (point e) end))
			:action
			char-action
			;; (_ (let ((r (funcall char-action _)))
			;;      (message-pause e "~s -> ~s" _ r)
			;;      r))
			)
	     (if (< point (length buf)) (incf point)))
	(setf mark old-mark
	      point old-point)))))

(defun downcase-region (e &optional (begining (mark e)) (end (point e)))
  (apply-char-action-to-region e #'char-downcase begining end))

(defun upcase-region (e &optional begining end)
  (apply-char-action-to-region e #'char-upcase begining end))

(defun downcase-word (e)
  (forward-word-action e #'(lambda (c) (char-downcase c))))

(defun upcase-word (e)
  (forward-word-action e #'(lambda (c) (char-upcase c))))

(defun capitalize-word (e)
  (let (bonk)
    (forward-word-action e #'(lambda (c)
			       (if (not bonk)
				   (progn (setf bonk t) (char-upcase c))
				   (char-downcase c))))))

(defun un-studly-cap (e)
  "Convert from StupidVarName to stupid-var-name."
  (with-slots (point buf) e
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
	   (insert-char e #\-)
	   (incf point))
	 (setf c (buffer-char buf point))
	 ;;(message-pause e "fourth point ~s ~s" point c)
	 :while (and (alpha-char-p c) (upper-case-p c)))
      (record-undo e 'boundary))))

(defun delete-horizontal-space (e)
  "Delete space before and after the cursor."
  (with-slots (buf point) e
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hacks for typing lisp

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
  (with-slots (point buf) e
    (let* ((str (buffer-string buf))
	   (ppos (matching-paren-position str :position point)))
      (if ppos
	  (let ((saved-point point))
	    (setf point ppos)
	    (update-display e)
	    (tt-finish-output)
	    (tt-listen-for .5)
	    (setf point saved-point))
	  (beep e "No match.")))))

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

(defun finish-line (e)
  "Add any missing close parentheses and accept the line."
  (with-slots (buf) e
    (loop :while (matching-paren-position (buffer-string buf))
       :do
	 (insert-char e #\))
	 ;; (display-char e #\))
	 )
    (accept-line e)))

(defun pop-to-lish (e)
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

(defun abort-command (e)
  "Invoke the debugger from inside."
  (declare (ignore e))
  ;; Maybe this should just flash the screen?
  ;; (with-simple-restart (continue "Continue RL")
  ;;   (invoke-debugger (make-condition
  ;; 		      'simple-condition
  ;; 		      :format-control "Abort command")))
  (abort))

(defun toggle-debugging (e)
  "Toggle debugging output."
  (with-slots (debugging) e
    (setf debugging (not debugging))))

(defun quoted-insert (e)
  "Insert the next character input without interpretation."
  (let ((c (get-a-char e)))
    (self-insert e t c)))

(defun self-insert (e &optional quoted char)
  (with-slots (command last-event buf point) e
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
	     (insert-char e char)
	     ;; flash paren and keep going
	     (when (and (eq *paren-match-style* :flash) (is-close-char char))
	       (flash-paren e))
	     (incf point))
	   ;; somewhere in the middle
	   (progn
	     (when (and (eq *paren-match-style* :flash) (is-close-char char))
	       (flash-paren e))
	     (insert-char e char)
	     (incf point)))))))

(defgeneric self-insert-command (line-editor)
  (:documentation "Try to insert a character into the buffer.")
  (:method ((e line-editor))
    (self-insert e)))

;; @@@ Is this reasonable?
(defmethod default-action ((e line-editor))
  (self-insert e))

(defun newline (e)
  "Insert a newline."
  (self-insert e #\newline))

;; @@@ we can probably just use the one in terminal-inator?
;; (defmethod read-key-sequence ((e line-editor) &optional keymap)
;;   "Read a key sequence from the user. Descend into keymaps.
;;  Return a key or sequence of keys."
;;   (get-key-sequence (λ () (get-a-char e)) (or keymap (inator-keymap e))))

(defun ask-function-name (&optional (prompt "Function: "))
  "Prompt for a function name and return symbol of a function."
  (let* ((str (rl :prompt prompt :context :ask-function-name))
	 (cmd (and str (stringp str)
		   (ignore-errors (safe-read-from-string str)))))
    (and (symbolp cmd) (fboundp cmd) cmd)))

(defun set-key-command (e)
  "Bind a key interactively."
  (tmp-prompt e "Set key: ")
  (let* ((key-seq (read-key-sequence e))
	 (cmd (ask-function-name (format nil "Set key ~a to command: "
					 (key-sequence-string key-seq)))))
    (if cmd
	(set-key key-seq cmd (line-editor-local-keymap e))
	(tmp-message e "Not a function."))))

(defmethod describe-key-briefly ((e line-editor))
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
    ))

(defun what-cursor-position (e)
  "Describe the cursor position."
  (with-slots (point buf screen-relative-row screen-col) e
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
      ;;(redraw-line e)
      )))

(defun exit-editor (e)
  "Stop editing."
  (with-slots (quit-flag exit-flag) e
    (setf quit-flag t
	  exit-flag t)))

(defmethod quit ((e line-editor))
  (exit-editor e))

;; This is mostly for binding to purposely meaningless commands.
(defun beep-command (e)
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

(defun bracketed-paste (e)
  (with-slots (point) e
    (let* ((term (or (find-ansi-terminal (line-editor-terminal e))
		     (error "I don't know how to read a bracketed paste on ~
                             a ~a." (type-of *terminal*))))
	   (paste (read-bracketed-paste term))
	   (len (length paste)))
      (insert-string e paste)
      (incf point len))))

(defun char-picker-command (e)
  "Pick unicode (or whatever) characters."
  (let ((result
	 (with-external (e)
	   (when (not (find-package :char-picker))
	     (asdf:load-system :char-picker))
	   (symbol-call :char-picker :char-picker))))
    (if result
	(self-insert e t result)
	(beep e "char-picker failed"))))

(defun unipose-command (e)
  "Compose unicode characters."
  (let ((first-ccc (get-a-char e)) second-ccc result)
    (setq second-ccc (get-a-char e))
    (setq result (unipose first-ccc second-ccc))
    (if result
	(self-insert e t result)
	(beep e "unipose ~c ~c unknown" first-ccc second-ccc))))

(defun insert-file (e)
  "Insert a file into the line editor's buffer."
  (let* ((file (read-filename :prompt "Insert-file: ")))
    (insert-string e (slurp file))))

;; EOF
