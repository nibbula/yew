;;
;; commands.lisp
;;

(in-package :rl)

(defmacro with-external ((e) &body body)
  "Do BODY outside the editor E, making sure that the terminal and display are
in proper condition."
  (with-unique-names (result)
    `(let (,result)
       (finish-output (terminal-output-stream (line-editor-terminal ,e)))
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
  (if (and (not func) not-in)
      (setf func #'(lambda (c) (not (position c not-in)))))
  (with-slots (point buf) e
    (let (cc)
      (if (eql dir :backward)
	  ;; backward
	  (loop :while (and (> point 0)
			   (funcall func (aref buf (1- point))))
	    :do
	    (when action
	      (when (setf cc (funcall action (aref buf (1- point))))
		(buffer-replace e (1- point) cc)))
	    (decf point))
	  (let ((len (length buf))
		(did-one nil))
	    (loop :while (and (< point len)
			     (funcall func (aref buf point)))
	      :do
	      (progn
		(when action
		  (when (setf cc (funcall action (aref buf point)))
		    (buffer-replace e point cc)
		    (setf did-one t)))
		(incf point)))
	    (when did-one (decf point)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement commands

(defun backward-word (e)
  "Move the insertion point to the beginning of the previous word or the
beginning of the buffer if there is no word."
  (with-slots (point buf non-word-chars) e
    (let ((start point))
      (scan-over e :backward :func #'(lambda (c) (position c non-word-chars)))
      (scan-over e :backward :not-in non-word-chars)
      (move-over e (- (- start point)) :start start))))

(defun forward-word (e)
  "Move the insertion point to the end of the next word or the end of the
buffer if there is no word."
  (with-slots (point buf non-word-chars) e
    (let ((start point))
      (scan-over e :forward :func #'(lambda (c) (position c non-word-chars)))
      (scan-over e :forward :not-in non-word-chars)
      (move-over e (- point start) :start start))))

(defun backward-char (e)
  "Move the insertion point backward one character in the buffer."
  (with-slots (point) e
    (when (> point 0)
      (move-over e -1)
      (decf point))))

(defun forward-char (e)
  "Move the insertion point forward one character in the buffer."
  (with-slots (point buf) e
    (when (< point (fill-pointer buf))
      (move-over e 1)
      (incf point))))

(defun beginning-of-line (e)
 "Move the insertion point to the beginning of the line (actually the buffer)."
  (with-slots (point buf) e
    (move-over e (- point))
    (setf point 0)))

(defun end-of-line (e)
  "Move the insertion point to the end of the line (actually the buffer)."
  (with-slots (point buf) e
    (move-over e (- (length buf) point))
    (setf point (fill-pointer buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement commands

(defun previous-history (e)
  "Go to the previous history entry."
  (history-put (context e) (buf e))
  (history-prev (context e))
  (use-hist e))

(defun next-history (e)
  "Go to the next history entry."
  (history-put (context e) (buf e))
  (history-next (context e))
  (use-hist e))

(defun beginning-of-history (e)
  "Go to the beginning of the history."
  (history-put (context e) (buf e))
  (history-first (context e))
  (use-hist e))

(defun end-of-history (e)
  "Go to the end of the history."
  (history-put (context e) (buf e))
  (history-last (context e))
  (use-hist e))

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
  (with-slots (buf quit-flag context accept-does-newline) e
    (history-last context)
    (if (add-to-history-p e buf)
	(history-put context buf)
	(history-delete-last context))
    (when accept-does-newline
      (move-over e (- (length (buf e)) (point e)))
      (tt-write-char #\newline)
      (tt-write-char #\return)
      (when (did-under-complete e)
	(tt-erase-below))
      (tt-finish-output))
    (setf quit-flag t)))

(defun copy-region (e)
  "Copy the text between the insertion point and the mark to the clipboard."
  (with-slots (point mark buf clipboard) e
    (setf clipboard (subseq buf mark point))))

(defun set-mark (e)
  "Set the mark to be the current point."
  (with-slots (point mark) e
    (setf mark point)))

(defun exchange-point-and-mark (e)
  "Move point to the mark. Set the mark at the old point."
  (with-slots (point mark) e
    (when mark
      (cond
	((< mark point)
	 ;;(move-backward e (- point mark))
	 (move-over e (- (- point mark)) :start point))
	((> mark point)
	 ;;(move-forward e (- mark point))
	 (move-over e (- mark point))))
      (rotatef point mark))))

(defun isearch-backward (e)
  "Incremental search backward."
  (isearch e :backward))

(defun isearch-forward (e)
  "Incremental search forward."
  (isearch e :forward))

;; Sadly ASCII / UTF-8 specific. @@@ And should be moved to char-util?
(defun control-char-p (c)
  (let ((code (char-code c)))
    (or (< code 32) (= code 128))))

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

(defun display-search (e str pos)
  "Display the current line with the search string highlighted."
  (with-slots (point context) e
    ;;(setf point (min (or pos (length buf)) (length buf)))
    (tt-move-to-col 0)
    (erase-display e)
    ;; (tt-erase-to-eol)
    (setf (screen-col e) 0)
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
	 ;;(display-char e c)
	 (tt-write-char c)
	 ))
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
  (with-slots (point buf cmd context) e
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
	     (message e "pos = ~a start-from = ~a" pos start-from))
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
	      (redraw e)
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
	(redraw e)))))


;; @@@ Consider calling redraw?
(defun redraw-command (e)
  "Clear the screen and redraw the prompt and the input line."
  (with-slots (prompt prompt-func point buf need-to-redraw) e
    (tt-clear) (tt-home)
    (setf (screen-col e) 0 (screen-row e) 0)
    (do-prompt e prompt prompt-func)
    (finish-output (terminal-output-stream
		    (line-editor-terminal e)))
    (display-buf e)
    (when (< point (length buf))
      (move-over e (- (- (length buf) point)) :start (length buf)))
    (setf need-to-redraw nil)))

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
      (let ((del-len (display-length (aref buf (1- point)))))
	(move-over e -1)
	(buffer-delete e (1- point) point)
	(decf point)
	(tt-del-char del-len)
	(update-for-delete e del-len 1)))))

(defun delete-char (e)
  "Delete the character following the cursor."
  (with-slots (point buf) e
    (if (= point (fill-pointer buf))
	(beep e "End of buffer")
	(progn
	  (let ((del-len (display-length (aref buf point))))
	    (buffer-delete e point (1+ point))
	    (tt-del-char del-len)
	    (update-for-delete e del-len 1))))))

(defun delete-char-or-exit (e)
  "At the beginning of a blank line, exit, otherwise delete-char."
  (with-slots (point buf last-input quit-flag exit-flag) e
    (if (and (= point 0) (= (length buf) 0)
	     (not (eql last-input (ctrl #\d))))
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
      (let* ((region-str (subseq buf point start))
	     (del-len (display-length region-str)))
	(move-over e (- (- start point)) :start start)
	(tt-del-char del-len)
	(setf clipboard region-str)
	(buffer-delete e point start)
	(update-for-delete e del-len (- start point))))))

(defun kill-word (e)
  (with-slots (buf point non-word-chars clipboard) e
    (let ((start point))
      (scan-over e :forward :func #'(lambda (c) (position c non-word-chars)))
      (scan-over e :forward :not-in non-word-chars)
      (if (< point (length buf)) (incf point))
      (let* ((region-str (subseq buf start point))
	     (del-len (display-length region-str)))
	(tt-del-char del-len)
	(setf clipboard region-str)
	(buffer-delete e point start)
	(setf point start)
	(update-for-delete e del-len (- start point))))))

(defun kill-line (e)
  (with-slots (clipboard buf point screen-row screen-col) e
    (setf clipboard (subseq buf point))
    (let ((saved-row screen-row)
	  (saved-col screen-col))
      (move-over e (- (length buf) point))
      (tt-erase-to-eol)
      (loop :with row = screen-row
	 :while (> row saved-row) :do
	 (tt-beginning-of-line)
	 (tt-erase-to-eol)
	 (tt-up 1)
	 (decf row))
      (setf screen-row saved-row
	    screen-col saved-col)
      (tt-move-to screen-row screen-col)
      (tt-erase-to-eol)
      (buffer-delete e point (fill-pointer buf)))))

(defun backward-kill-line (e)
  (with-slots (point clipboard buf) e
    (when (> point 0)
      (setf clipboard (subseq buf 0 point))
      (replace-buffer e (subseq buf point))
      (beginning-of-line e))
    (clear-completions e)))

(defun yank (e)
  (with-slots (clipboard point) e
    (when clipboard
      (let ((len (length clipboard))
	    #| (disp-len (display-length clipboard)) |#)
	(insert-string e clipboard)
	;; (tt-ins-char disp-len)
	(display-buf e point (+ point len))
	(incf point len)
	(update-for-insert e)
	))))

(defun forward-word-action (e action)
  (with-slots (point buf non-word-chars) e
    (let ((start point))
      (scan-over e :forward :func #'(lambda (c) (position c non-word-chars)))
      (scan-over e :forward :not-in non-word-chars :action action)
      (if (< point (length buf)) (incf point))
;      (move-forward e (display-length (subseq buf start point))))))
      (display-buf e start point))))

(defun apply-char-action-to-region (e char-action &optional beginning end)
  "Apply a function that takes a character and returns a character, to
every character in the region delimited by BEGINING and END. If BEGINING
and END aren't given uses the the current region, or gets an error if there
is none."
  (with-slots (point mark) e
    (if (or (not beginning) (not end))
	(error "Mark must be set if beginning or end not given."))
    (if (not beginning)
	(setf beginning (min mark point)))
    (when (not end)
      (setf end (max mark point)))
    (when (> end beginning)
      (rotatef end beginning))
    (let ((old-mark mark))
      (unwind-protect
	   (progn
	     (setf mark beginning)
	     (exchange-point-and-mark e)
	     (scan-over e :forward :func (constantly t) :action char-action))
	(setf mark old-mark)))))

(defun downcase-region (e &optional begining end)
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

;; This is just an experiment to see how I would do it.
(defun un-studly-cap (e)
  "Convert from StupidVarName to stupid-var-name."
  (with-slots (point buf) e
    (record-undo e 'boundary)
    (let ((overall-start point) c start)
      (loop :do
	 (setf start point)
	 (scan-over
	  e :forward
	  :func #'(lambda (c) (and (alpha-char-p c) (upper-case-p c))))
	 (scan-over
	  e :forward
	  :func #'(lambda (c) (and (alpha-char-p c) (lower-case-p c))))
	 (downcase-region e start point)
	 (setf c (aref buf point))
	 (when (and (alpha-char-p c) (upper-case-p c))
	   (insert-char e #\-))
	 :while (and (alpha-char-p c) (upper-case-p c)))
      (record-undo e 'boundary)
      (display-buf e overall-start point))))

(defun delete-horizontal-space (e)
  "Delete space before and after the cursor."
  (with-slots (buf point) e
    (let ((origin point)
	  start end del-len disp-len deleted-string
	  #| first-half-disp-len |#)
      (setf origin point)
      (scan-over e :forward
		 :func #'(lambda (c) (position c dlib::*whitespace*)))
      (setf end point
	    point origin)
      (scan-over e :backward
		 :func #'(lambda (c) (position c dlib::*whitespace*)))
      (setf start point
	    del-len (- end start)
	    deleted-string (subseq buf start end)
	    ;;first-half-disp-len (display-length (subseq buf start origin))
	    disp-len (display-length deleted-string))
      (delete-region e start end)
      ;;(move-backward e first-half-disp-len)
      (move-over e (- (- origin start)) :start origin)
      (tt-del-char disp-len)
      (update-for-delete e disp-len del-len))))

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
  (let* ((str (buf e))
	 (point (point e))
	 (ppos (matching-paren-position str :position point))
	 (offset (and ppos (1+ (- point ppos)))))
    (if ppos
	(let ((saved-col (screen-col e)))
	  (declare (ignore saved-col))
	  ;; The +1 to point is because we haven't incremented point for the
	  ;; close paren yet.
	  (move-over e (- offset) :start (1+ point))
	  (tt-finish-output)
	  (tt-listen-for .5)
	  (move-over e offset :start (- (1+ point) offset)))
	(beep e "No match."))))

(defun highlight-paren (e pos)
  (let* ((str (buf e))
	 (ppos (matching-paren-position str :position pos :char (aref str pos)))
	 offset offset-back)
    (log-message e "pos = ~s ppos = ~s" pos ppos)
    (if ppos
	(let ((saved-col (screen-col e)))
	  (declare (ignore saved-col))
	  (cond
	    ((> ppos pos)
	     (setf offset (- ppos pos)
		   offset-back (- (+ offset 1))))
	    (t
	     (setf offset (- (1+ (- pos ppos)))
		   offset-back (- pos ppos))))
	  (move-over e offset :start (point e))
	  (tt-bold t)
	  ;;(tt-write-char (match-char (aref str pos)))
	  (display-char e (match-char (aref str pos)))
	  (tt-bold nil)
	  (move-over e offset-back :start (1+ ppos))))))

(defun finish-line (e)
  "Add any missing close parentheses and accept the line."
  (with-slots (buf) e
    (loop :while (matching-paren-position buf)
       :do (insert-char e #\)) (display-char e #\)))
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
	    (finish-output (terminal-output-stream (line-editor-terminal e)))
	    (terminal-end (line-editor-terminal e))
	    (if (line-editor-terminal-device-name e)
		(funcall (find-symbol "LISH" :lish)
			 :terminal-name (line-editor-terminal-device-name e))
		(funcall (find-symbol "LISH" :lish)))
	    (tt-beginning-of-line)
	    (tt-erase-line)
	    (setf (screen-col e) 0)
	    (with-slots (prompt prompt-func point buf) e
	      (do-prompt e prompt prompt-func)
	      (display-buf e)
	      (when (< point (length buf))
		(move-backward e (display-length (subseq buf point)))))
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
  (setf (cmd e) (get-a-char e))
  (self-insert e t))

(defun self-insert (e &optional quoted char)
  "Try to insert a character into the buffer."
  (with-slots (cmd buf point screen-col) e
    (when (not char)
      (setf char cmd))
    (cond
      ((not (characterp char))
       (beep e "~a is not a character." char))
      ((and (not (graphic-char-p char)) (not quoted))
       (beep e "~a is unbound." char))
      (t
       ;; a normal character
       (if (= (length buf) point)
	   ;; end of the buf
	   (progn
	     (display-char e char)
	     (insert-char e char)
	     ;; flash paren and keep going
	     (when (and (eq *paren-match-style* :flash) (is-close-char char))
	       (flash-paren e))
	     (incf point))
	   ;; somewhere in the middle
	   (progn
	     (let* ((endings (calculate-line-endings e))
		    (end (line-ending point endings))
		    (len (display-length char))
		    (at-len (display-length (aref buf point)))
		    (cols (terminal-window-columns (line-editor-terminal e)))
		    prev-end)
	       (cond
		 ;; We're at a wide char at EOL and inserting a smaller one.
		 ((and (and end (< end screen-col))
		       (< at-len len))
		  (tt-erase-to-eol)
		  (tt-down 1)
		  (tt-beginning-of-line)
		  (tt-ins-char len))
		 ;; We're at the beginning with an empty col on the previous
		 ((and (= screen-col 0)
		       (setf prev-end (line-ending (1- point) endings))
		       (< prev-end cols))
		  (tt-backward 1))
		 (t
		  ;; normal
		  (tt-ins-char len)))
	       (display-char e char)
	       (when (and (eq *paren-match-style* :flash) (is-close-char char))
		 (flash-paren e))
	       (insert-char e char)
	       (incf point)
	       ;; do the dumb way out
	       (update-for-insert e))))))))

(defun read-key-sequence (e &optional keymap)
  "Read a key sequence from the user. Descend into keymaps.
 Return a key or sequence of keys."
  (let* ((c (get-a-char e))
	 (action (key-definition c (or keymap (line-editor-keymap e)))))
    (if (and (symbolp action) (boundp action) (keymap-p (symbol-value action)))
	(let ((result-seq (read-key-sequence e (symbol-value action))))
	  (if (listp result-seq)
	      (append (list c) result-seq)
	      (list c result-seq)))
	c)))

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

(defun describe-key-briefly (e)
  "Tell what function a key invokes."
  (tmp-prompt e "Describe key: ")
  (let* ((key-seq (read-key-sequence e))
	 (def (key-sequence-binding key-seq (line-editor-keymap e))))
    (if def
	(tmp-message e "Describe key: ~w is bound to ~a"
		     (key-sequence-string key-seq) def)
	(tmp-message e "Describe key: ~w is not bound"
		     (key-sequence-string key-seq)))))

(defun exit-editor (e)
  "Stop editing."
  (with-slots (quit-flag exit-flag) e
    (setf quit-flag t
	  exit-flag t)))

;; @@@ Consider calling redraw?
#| Redundant
(defun redraw-command (e)
  "Clear the screen and redraw the prompt and the input line."
  (with-slots (prompt prompt-func point buf need-to-redraw) e
    (tt-clear) (tt-home)
    (setf (screen-col e) 0 (screen-row e) 0)
    (do-prompt e prompt prompt-func)
    (finish-output (terminal-output-stream
		    (line-editor-terminal e)))
    (display-buf e)
    (when (< point (length buf))
      (move-over e (- (- (length buf) point)) :start (length buf)))
    (setf need-to-redraw nil)))
|#

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

;; EOF
