;;
;; history.lisp - Line editor history.
;;

(in-package :rl)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; History
;;;
;;; History is a forward and backward navigable list of strings. We maintain
;;; separate history contexts, so that line editor instances can have separate
;;; histories or share the same history.
;;;
;;; *history* is an alist of (CONTEXT . HISTORY), where CONTEXT is a symbol
;;; and HISTORY is a history structure, which contains a dl-list of strings.
;;;
;;; I know it's confusing, but the tail is the oldest entry and the head
;;; is the most recent entry. We push new entries on to the head, but we
;;; print the list backwards from tail to the head. So, for example, the
;;; previous history item is accessed by dl-next.
;;;
;;; The lines are editable, and the last line is usually the one we're working
;;; on. When we go back and edit a line and accept it (hit enter) we don't
;;; change the history, we just add it at the bottom. If you go back and
;;; accept a history line, the line you started with gets lost. That's how
;;; people expect to work, so c'est la vie.
;;;
;;; To have things work the way that's expected, we:
;;;   - Always add a history line initially.
;;;   - When accepting go to the last line and put it (if it meets criteria).
;;;   - When moving, put and move.
;;;   - When not meeting the criteria, delete the last line.

(defstruct history
  "Line editor history."
  (head nil :type dl-node)		; Start of history list
  (tail nil :type dl-node)		; End of history list
  (cur nil :type dl-node))		; Current node

(defstruct history-entry		; @@@ not used yet
  "An entry in the line editor history."
  time					; a universal time
  line					; a string
  (modified nil :type boolean))		; true if the entry has be edited

(defvar *history* '() "Line history of some sort.")
(defvar *history-context* nil "The current dynamic context.")

(defun get-history (&optional (context *history-context*))
  "Get the history records for CONTEXT, which should probably be a keyword."
  (cdr (assoc context *history*)))

(defun history-init (&optional (context *history-context*))
  "Initialize history for CONTEXT."
  (let ((context-hist (assoc context *history*)))
    (when (not context-hist)
      (let ((l (make-dl-list)))
	(setf *history* (acons context (make-history :head l :tail l :cur l)
			       *history*))))))

(defun history-clear (&optional (context *history-context*))
  "Clear all of the history for the context."
  (let ((hist (get-history context))
	(lst (make-dl-list)))
    (setf (history-head hist) lst
	  (history-tail hist) lst
	  (history-cur hist) lst)))

(defun history-add (buf &optional (context *history-context*))
  "Adds the content BUF as the most recent history item."
  (let* ((hist (get-history context)))
    (dl-push (history-head hist) (copy-seq buf))
    (when (not (history-cur hist))
      (setf (history-cur hist) (history-head hist)))
    (when (not (history-tail hist))
      (setf (history-tail hist) (history-head hist)))))

(defun history-delete-last (&optional (context *history-context*))
  "Delete the last history entry."
  (dl-pop (history-head (get-history context))))

(defun history-put (buf &optional (context *history-context*))
  "Save the BUF in the current history item."
  (let* ((hist (get-history context))
	 (cur (history-cur hist)))
    (when cur
      (setf (dl-content cur) (copy-seq buf)))))

(defun history-prev (&optional (context *history-context*))
  "Move the current history to the next oldest."
  (let* ((hist (get-history context))
	 (cur (history-cur hist))
	 (head (history-head hist))
	 (next (if cur (dl-next cur) head)))
    (when next
      (setf (history-cur hist) next))))

(defun history-next (&optional (context *history-context*))
  "Move the current history to the next most recent."
  (let* ((hist (get-history context))
	 (cur (history-cur hist)))
    (when (and cur (dl-prev cur))
      (setf (history-cur hist) (dl-prev cur)))))

(defun history-first (&optional (context *history-context*))
  "Move the current history to the oldest history item."
  (let ((hist (get-history context)))
    (setf (history-cur hist) (history-tail hist))))

(defun history-last (&optional (context *history-context*))
  "Move the current history to the most recent history item."
  (let ((hist (get-history context)))
    (setf (history-cur hist) (history-head hist))))

(defun history-current (&optional (context *history-context*))
  "Return the content of the current item."
  (let* ((hist (get-history context))
	 (cur (history-cur hist)))
    (if cur (dl-content cur) nil)))

(defun history-current-get (&optional (context *history-context*))
  "Return the current history node."
  (history-cur (get-history context)))

;;(defun history-current-set (newval &optional (context *history-context*))
(defun history-current-set (newval &optional (context *history-context*))
  "Set the current history node to NEWVAL."
  (setf (history-cur (get-history context)) newval))

(defsetf history-current (&optional (context *history-context*)) (store)
  "SETF form for the current history node."
  `(history-current-set ,store ,context))

;; This is quite inefficient because dl-list's suck.
(defun history-nth (n &optional (context *history-context*))
  "Return the Nth element of the history for CONTEXT, or nil."
  (let* ((h (get-history context))
	 list element)
    (and h
	 (setf list (history-head h))
	 (if (minusp n)
	     (setf element (dl-nth (abs n) list))
	     (setf element (dl-nth (- (dl-length list) n) list)))
	 (dl-content element))))

(defun show-history (&optional (context *history-context*))
  "Print the history with numbers."
  (let ((hist (get-history context))
	(i 1))
    (dl-list-do-backward
     (history-tail hist)
     #'(lambda (x)
	 (format t "~4d  ~a~%" i x)	;; @@@ zorp
	 (incf i))))
  (values))

(defun history-file-name (&optional (context *history-context*))
  (merge-pathnames
   (make-pathname :name (format nil ".~(~a~)_history" (string context)))
   (user-homedir-pathname)))

;; Increment for every incompatible change.
(defparameter *history-version* 1
  "Version number of history format file.")

(defun history-save (&optional (context *history-context*))
  (let ((hist (get-history context)))
    (with-open-file (str (history-file-name context)
			 :direction :output
			 :if-exists :supersede)
      ;; write version
      (format str "trlh ~a~%" *history-version*)
      ;; history list
      (princ #\()
      (dl-list-do (history-head hist)
		  #'(lambda (x)
		      (format str "~s~%" x))) ;; @@@ zorp
      (princ #\) (terpri)))))

(defun history-load (&optional (context *history-context*))
  (declare (ignore context)))
  ;; (let ((hist (get-history context))
  ;; 	(s (make-string 4)) i)
  ;;   (with-open-file (stm (history-file-name context)
  ;; 			 :direction :input)
  ;;     (when (string/= (read-sequence s stm :end 4) "trlh")
  ;; 	(error "Bad magic tag ~a in history file." s))
  ;;     (when (/= *history-version*
  ;; 		(setq i (parse-integer (setq s (read-line stm)))))
  ;; 	(error "Bad version number ~a in history file." s))
  ;;     ;; @@@ SECURITY ALERT !!!
  ;;     (let ((*read-eval* nil))
  ;;       (setq s (read stm)))
  ;;     (when (not (listp s))
  ;; 	(error "Malformed history list in history file: ~a." s))
  ;;     (setf (history-current hist)
  ;; 	    (make-dl-list :from-list s)))))

;; EOF
