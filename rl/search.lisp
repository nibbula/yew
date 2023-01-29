;;;
;;; search.lisp - Search commands
;;;

(in-package :rl)

(declaim #.`(optimize ,.(getf rl-config::*config* :optimization-settings)))

(defvar *match-style* nil
  "Cached style for displaying the matched string for the search instance.")

(declaim (inline match-style))
(defun match-style ()
  (or *match-style*
      (setf *match-style*
	    (or (theme-value *theme* '(:program :search-match :style))
		(theme-value *theme* '(:rl :search-match :style))
		'(:underline)))))

(defun display-search (e str start end prompt)
  "Display the current line with the search string highlighted."
  (with-slots ((contexts inator::contexts)
	       buf history-context temporary-message) e
    (tmp-message e "~a~a" prompt str)
    (if (and start (history-current history-context))
	(save-excursion (e)
	  (let ((saved-point (inator-point (aref contexts 0))))
	    (setf contexts (make-contexts)
		  (inator-point (aref contexts 0)) start
		  (inator-mark (aref contexts 0)) end
		  (line-editor-region-active e) t
		  ;; @@@ The way this works could use a little re-design.
		  buf (highlightify
		       e (make-fatchar-string (history-current history-context))
		       :style (match-style))
		  (inator-point (aref contexts 0)) saved-point
		  (inator-mark (aref contexts 0)) nil))
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

(defun use-case-insensitive-p (string)
  "Return true if we should do case insensitive search on this string."
  (not (some #'upper-case-p string)))

(defun test-for-string (string)
  "Return the test function we should use for STRING. This is so-called 'smart'
case folding, where using any upper case triggers exact matching, and all lower
case means, be case insensitive."
  (if (use-case-insensitive-p string) #'char= #'char-equal))

(defun search-history (e str direction start-from search-pos &key regexp-scanner)
  "Find the position of STR, in the history of editor E. Return three values:
the number of the matching history item, the start and end character positions
in that item.
  DIRECTION       Which way to search, either :backward or :forward.
  START-FROM      The from history element to start from.
  SEARCH-POS      The position in the line.
  REGEXP-SCANNER  The scanner for regular expression search."
  (with-slots (history-context) e
    (let (#| (hist (get-history history-context)) |#
	  (first-time t) start end from-pos matches)
      (flet ((do-search-backward (x)
	       (setf from-pos (backward-start-pos (history-line x) search-pos))
	       (if regexp-scanner
		   (progn
		     (if first-time
			 (setf matches (ppcre:all-matches regexp-scanner
							  (history-line x)
							  :end from-pos)
			       first-time nil)
			 (setf matches (ppcre:all-matches regexp-scanner
							  (history-line x))))
		     (when matches
		       (let* ((mu (last matches 2)))
			 (setf start (first mu) end (second mu)))))
		   (progn
		     (if first-time
			 (setf start (search str (history-line x)
					     :from-end t
					     :end2 from-pos
					     :test (test-for-string str))
			       first-time nil)
			 (setf start (search str (history-line x) :from-end t
					     :test (test-for-string str))))
		     (setf end (and start (+ start (length str)))))))
	     (do-search-forward (x)
	       (setf from-pos (forward-start-pos (history-line x) search-pos))
	       (if regexp-scanner
		   (if first-time
		       (progn
			 (setf first-time nil)
			 (multiple-value-setq (start end)
			   (ppcre:scan regexp-scanner
				       (history-line x) :start from-pos)))
		       (multiple-value-setq (start end)
			 (ppcre:scan regexp-scanner (history-line x))))
		   (progn
		     (if first-time
			 (setf start (search str (history-line x)
					     :start2 from-pos
					     :test (test-for-string str))
			       first-time nil)
			 (setf start (search str (history-line x)
					     :test (test-for-string str))))
		     (setf end (and start (+ start (length str))))))))
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
		     (do-search-backward x)
		     (when start
		       (return-from search-history (values x start end)))))))
	    (dl-list-do-backward-element
	     start-from
	     #'(lambda (x)
		 (when (dl-content x)
		   (do-search-forward x)
		   (when start
		     (return-from search-history (values x start end)))))))
	nil))))

(defparameter *isearch-prompt* "~:[~;failed ~]~:[>~;<~] ~:[~;re-~]i-search: ")

(defkeymap *default-isearch-keymap* (:default-binding 'isearch-default)
  `((,(ctrl #\G)      . quit)
    (,(ctrl #\S)      . isearch-again)
    (,(ctrl #\R)      . isearch-again-reverse)
    (,(ctrl #\L)      . redraw)
    (,(meta-char #\r) . isearch-toggle-regexp)
    (:f1	      . help)))

(defclass search-inator (terminal-inator)
  ((editor
    :initarg :editor :accessor search-inator-editor
    :documentation "The editor we're searching in.")
   (search-string
    :initarg :search-string :accessor search-inator-search-string
    :type string :initform (make-stretchy-string *initial-line-size*)
    :documentation "The string being searched for.")
   (start-point
    :initarg :start-point :accessor search-inator-start-point
    :documentation "Point saved before we start searching.")
   (start-hist
    :initarg :start-hist :accessor search-inator-start-hist
    :documentation "Starting history item before entering search.")
   (start-from
    :initarg :start-from :accessor search-inator-start-from
    :documentation "Current history item to start searching from.")
   (failed
    :initarg :failed :accessor search-inator-failed :type boolean :initform nil
    :documentation "True if the search failed.")
   (direction
    :initarg :direction :accessor search-inator-direction
    :type (member :backward :forward) :initform :backward
    :documentation "Direction we are searching in.")
   (regexp-p
    :initarg :regexp-p :accessor search-inator-regexp-p
    :type boolean :initform nil
    :documentation
    "True if search-string should be considered a regular expression.")
   (scanner
    :initarg :scanner :accessor search-inator-scanner :initform nil
    :documentation "Rexexp scanner, or nil if there isn't one.")
   (scanner-string
    :initarg :scanner-string :accessor search-inator-scanner-string
    :initform nil
    :documentation "String used for the regexp scanner.")
   (match-start
    :initarg :match-start :accessor search-inator-match-start :initform nil
    :documentation "Starting position of the match.")
   (match-end
    :initarg :match-end :accessor search-inator-match-end :initform nil
    :documentation "Ending position of the match."))
  (:default-initargs :default-keymap *default-isearch-keymap*)
  (:documentation "A search sub-system."))

(defmethod initialize-instance
    :after ((o search-inator) &rest initargs &key &allow-other-keys)
  "Initialize a search-inator."
  (declare (ignore initargs))
  (assert (slot-value o 'editor))
  (dbugf :nis "init keymap ~s default-keymap ~s~%"
	 (if (slot-boundp o 'inator::keymap)
	     (slot-value o 'inator::keymap)
	     '|<unbound>|)
	 (if (slot-boundp o 'inator::default-keymap)
	     (slot-value o 'inator::default-keymap)
	     '|<unbound>|))

  (with-slots (editor) o
    (let ((hist (history-context editor)))
      (setf (slot-value o 'start-point)
	    (inator-point (ofirst (inator-contexts editor)))

            (slot-value o 'start-hist)
	    (history-cur (get-history hist))

	    (slot-value o 'start-from)
	    (or (history-current-get hist) (history-head hist)))))
  (values))

(defmethod quit ((o search-inator))
  (with-slots ((e editor) start-hist start-point) o
    (with-slots ((contexts inator::contexts) history-context temporary-message) e
      (with-slots ((point inator::point)) (aref contexts 0)
	(setf temporary-message nil)
	(use-first-context (e)
	  ;; Reset history item and point back to what they were when we started.
	  (setf (history-cur (get-history history-context)) start-hist
		point start-point))))))

(defmethod await-event ((o search-inator))
  (tt-finish-output)
  (prog1 (await-event (search-inator-editor o))
    (dbugf :nis "await-event ~s~%" (last-event (search-inator-editor o)))))

(defmethod update-display ((o search-inator))
  (with-slots (search-string failed direction regexp-p match-end (e editor)) o
    (with-slots ((contexts inator::contexts)) e
      (with-slots ((point inator::point)) (aref contexts 0)
	(dbugf :nis "update-display ~s ~s ~s ~s~%" search-string point match-end
	       (inator-keymap o))
	(display-search e search-string point match-end
			(format nil *isearch-prompt*
				failed (eq direction :backward)
				regexp-p))))))

(defmethod redraw ((o search-inator))
  (update-display o))

(defmethod help ((o search-inator))
  (message (search-inator-editor o)
	   "~a" (with-output-to-string (s)
		  (let ((km (inator-keymap o)))
		    (if (atom km)
			(describe-keymap (inator-keymap o))
			(mapcar #'describe-keymap km))))))

(defgeneric isearch-again (o)
  (:documentation "")
  (:method ((o search-inator))
    (with-slots (search-string last-search direction start-from (e editor)) o
      (with-slots (last-search history-context) e
	(when (and (zerop (length search-string)) last-search)
	  (stretchy-append search-string last-search))
	(setf direction :forward
	      start-from (search-start-forward history-context))))))

(defgeneric isearch-again-reverse (o)
  (:documentation "")
  (:method ((o search-inator))
    (with-slots (search-string last-search direction start-from (e editor)) o
      (with-slots (last-search history-context) e
	(when (and (zerop (length search-string)) last-search)
	  (stretchy-append search-string last-search))
	(setf direction :backward
	      start-from (search-start-backward history-context))))))

(defgeneric isearch-toggle-regexp (o)
  (:documentation "Toggle regular expression search.")
  (:method ((o search-inator))
    (with-slots (regexp-p scanner scanner-string) o
      (setf regexp-p (not regexp-p)
	    scanner nil
	    scanner-string ""))))

(defgeneric isearch-resync (o)
  (:documentation "Make the editor be where we searched to.")
  (:method ((o search-inator))
    (with-slots ((e editor) search-string match-start) o
      (with-slots ((contexts inator::contexts) buf history-context last-search) e
	(with-slots ((point inator::point)) (aref contexts 0)
	  (setf point 0)
	  (buffer-delete e 0 (length buf) point)
	  (buffer-insert e 0 (or (history-current (history-context e)) "")
			 point)
	  (setf point (min (or match-start (length buf)) (length buf))
		;; temporary-message nil
		last-search search-string)
	  (clear-completions e)
	  (redraw-display e))))))

(defun add-to-search (o c)
  (with-slots (search-string regexp-p match-start match-end direction
	       (e editor) failed scanner scanner-string start-from) o
    (with-slots ((contexts inator::contexts) history-context) e
      (with-slots ((point inator::point)) (aref contexts 0)
	(stretchy-append search-string c)
	(when (and regexp-p (string/= search-string scanner-string))
	  (when (setf scanner (ignore-errors
			       (ppcre:create-scanner
				search-string
				:case-insensitive-mode
				(use-case-insensitive-p search-string))))
	    (setf scanner-string (copy-seq search-string))))
	(let (new-hist)
	  (multiple-value-setq (new-hist match-start match-end)
	    (search-history e search-string direction start-from match-start
			    :regexp-scanner scanner))
	  (if new-hist
	      (setf (history-cur (get-history (history-context e))) new-hist
		    point match-start
		    failed nil)
	      (progn
		(setf failed t))))))))

;; @@@ or should we use inator:default-action ?
(defgeneric isearch-default (o)
  (:documentation "The default action for events not in the keymap.")
  (:method ((o search-inator))
    (let* ((e (search-inator-editor o))
	   (c (last-event e)))
      (dbugf :nis "default ~s~%" c)
      (cond
	;; Exit on "command" characters
	((or (and (characterp c)
		  (or (control-char-p c) (meta-char-p (char-code c))))
	     (not (characterp c)))
	 (isearch-resync o)
	 (when (equal c #\escape)
	   (push #\escape (queued-input e)))
	 (push c (queued-input e))
	 (setf (inator-quit-flag o) t))
	(t
	 ;; Append non-command characters to the search.
	 (add-to-search o c))))))

(defun isearch2 (e &key (direction :backward) regexp-p)
  "Incremental search which updates the search position as the user types. The
search can be ended by typing a control character, which usually performs a
command, or Control-G which stops the search and returns to the start.
Control-R searches again backward and Control-S searches again forward."
  (let (*match-style*)
    (invoke 'search-inator :editor e
			   :direction direction
			   :regexp-p regexp-p)))

(defun isearch (e &key (direction :backward) regexp-p)
  "Incremental search which updates the search position as the user types. The
search can be ended by typing a control character, which usually performs a
command, or Control-G which stops the search and returns to the start.
Control-R searches again backward and Control-S searches again forward."
  (with-slots ((contexts inator::contexts)
	       buf command history-context temporary-message last-search) e
    (with-slots ((point inator::point)) (aref contexts 0)
      (let* ((quit-now nil)
	     (start-point point)
	     (hist (get-history history-context))
	     (start-hist (history-cur hist))
	     (search-string (make-stretchy-string *initial-line-size*))
	     (start-from (or (history-current-get history-context)
			     (history-head hist)))
	     (pos point) end c #| added |# failed escape new-hist
	     scanner-string scanner dont-search)
	(labels ((redisp ()
		   (display-search e search-string point end
				   (format nil *isearch-prompt*
					   failed (eq direction :backward)
					   regexp-p)))
		 (resync ()
		   (setf point 0)
		   (buffer-delete e 0 (length buf) point)
		   (buffer-insert e 0 (or (history-current
					   (history-context e)) "")
				  point)
		   (setf point (min (or pos (length buf)) (length buf))
			 ;; temporary-message nil
			 last-search search-string)
		   (clear-completions e)
		   (redraw-display e)))
	  (loop :while (not quit-now)
	     :do
	     ;; (when (debugging e)
	     ;;   (debug-message e "pos = ~a start-from = ~a" pos start-from))
	     (redisp)
	     (tt-finish-output)
	     (setf c (get-a-char e))
	     ;; (setf added nil)
	     (setf dont-search nil)
	     (cond
	       ((eql c (ctrl #\G))
		(setf temporary-message nil)
		(use-first-context (e)
		  (setf (history-cur hist) start-hist)
                  (setf point start-point)
		  (return-from isearch c)))
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
		(setf dont-search t)
		(redisp))
	       ((or (eql c (ctrl #\h)) (eql c #\backspace) (eql c #\rubout))
		(stretchy-truncate search-string
				   (max 0 (1- (length search-string)))))
	       ((eql c #\escape)
		(setf escape t
		      dont-search t))
	       ((or (eql c (meta-char #\r)) (and escape (eql c #\r)))
		;; toggle regexp search
		(setf regexp-p (not regexp-p)
		      scanner nil
		      scanner-string ""))
	       ((or (and (characterp c)
			 (or (control-char-p c) (meta-char-p (char-code c))))
		    (not (characterp c)))
		(resync)
		(when escape
		  (push #\escape (queued-input e)))
		(push c (queued-input e))
		(return-from isearch c))
	       (t
		(setf escape nil)
		(stretchy-append search-string c)
		;; (setf added t)
		))
	     (when (and regexp-p (string/= search-string scanner-string))
	       (when (setf scanner (ignore-errors
				     (ppcre:create-scanner
				      search-string
				      :case-insensitive-mode
				      (use-case-insensitive-p search-string))))
		 (setf scanner-string (copy-seq search-string))))
	     (when (not dont-search)
	       (multiple-value-setq (new-hist pos end)
		 (search-history e search-string direction start-from pos
				 :regexp-scanner scanner))
	       (if new-hist
		   (setf (history-cur hist) new-hist
			 point pos
			 failed nil)
		   (progn
		     (setf failed t)
		     ;; (when added
		     ;;   (stretchy-truncate search-string
		     ;; 		     (max 0 (1- (length search-string))))
		     ;;   (setf pos old-pos))
		     ;; (beep e "Not found")
		     )))))))))

(defsingle isearch-backward (e)
  "Incremental search backward."
  (isearch e :direction :backward))

(defsingle-method search-command ((e line-editor))
  (isearch-backward e))

(defsingle isearch-forward (e)
  "Incremental search forward."
  (isearch e :direction :forward))

(defun go-to-matching-history (e string &key regexp-p)
  "Go to a previous history that matches ‘string’."
  (with-slots ((contexts inator::contexts) history-context) e
    (with-slots ((point inator::point)) (aref contexts 0)
      (let* ((hist (get-history history-context))
	     (start-from (or (history-current-get history-context)
			     (history-head hist)))
	     (pos point)
	     end new-hist scanner)

	(setf start-from (search-start-backward history-context))

	(when regexp-p
	  (setf scanner (ignore-errors
			 (ppcre:create-scanner
			  string
			  :case-insensitive-mode
			  (use-case-insensitive-p string)))))
	(multiple-value-setq (new-hist pos end)
	  (search-history e string :backward start-from pos
			  :regexp-scanner scanner))
	(if new-hist
	    (setf (history-cur hist) new-hist
		  point pos))))))

;; End
