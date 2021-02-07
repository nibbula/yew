;;;
;;; history-store.lisp - Storage for command history.
;;;

(in-package :rl)

(declaim #.`(optimize ,.(getf rl-config::*config* :optimization-settings)))

;; This would be mostly unnecessary if we had a proper non-filesystem O/S,
;; but until then...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic history storage

(defclass history-store ()
  ()
  (:documentation "A place to store command history."))

(defclass history-file-store (history-store)
  ((file-name
    :initarg :file-name :accessor history-store-file-name :initform nil 
    :documentation "Name of the file."))
  (:documentation "History stored in a file."))

(defgeneric history-store-save (store style &key update history-context)
  (:documentation
   "Save the history from HISTORY-CONTEXT to STORE in STYLE."))

(defgeneric history-store-load (store style &key update history-context)
  (:documentation
   "Load the history to HISTORY-CONTEXT from STORE in STYLE."))

(defgeneric history-store-start (store style)
  (:documentation "Start using a history store."))

(defgeneric history-store-done (store style)
  (:documentation "Indicate we're done with using a history store."))

(defgeneric history-store-default-file-name (store &optional history-context)
  (:documentation
   "Return the default pathname where the history for HISTORY-CONTEXT in STORE
is saved."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Storage for format :text-file

(defclass text-history-store (history-file-store)
  ()
  (:documentation "Text file history format."))

(defmethod history-store-default-file-name ((store text-history-store)
					    &optional
					      (context *history-context*))
  (nos:path-append (nos:data-dir (string-downcase context))
		   "command-history.txt"))

(defmethod initialize-instance
    :after ((o text-history-store) &rest initargs &key &allow-other-keys)
  "Initialize a text-history-store."
  (declare (ignore initargs))
  (when (not (and (slot-boundp o 'file-name)
		  (slot-value o 'file-name)))
    (setf (slot-value o 'file-name)
	  (history-store-default-file-name o))))

(defmethod history-store-start ((store text-history-store) style)
  "Start using a text history store, of any style."
  (declare (ignore store style)))

(defmethod history-store-done ((store text-history-store) style)
  "Indicate we're done with using a text history store, of any style."
  (declare (ignore store style)))

;;;;;;;;;;;;;;;;;
;; Fancy style

;; Increment for every incompatible change. Woe be unto them that don't
;; increment this appropriately.
(defparameter *text-history-version* 3
  "Version number of history format file.")

(defparameter *text-history-magic* "trlh"
  "Magic identifier for the fancy style.")

(defun check-version (stream &key no-error)
  "Check STREAM for the fancy magic and version number. Signal an error if it
doesn't match, or if NO-ERROR is true, return a keyword indicating what
happened (:magic-bad :version-bad :ok). Assumes that stream is open for reading
and positioned at the beginning of the file."
  (block nil
    (let ((s (make-string 4)) i)
      (read-sequence s stream :end 4)
      (when (string/= s *text-history-magic*)
	(if no-error
	    (return :bad-magic)
	    (error "Bad magic tag ~s in history file." s)))
      (when (/= *text-history-version*
		(setq i (parse-integer (setq s (read-line stream)))))
	(if no-error
	    (return :bad-version)
	    (error "Bad version number ~s in history file." s))))
    :ok))

(defun fancy-hist-p (file-name)
  "Return true if the file named FILE-NAME is probably in fancy format."
  (with-open-file (stream file-name :direction :input)
    (let ((result (check-version stream :no-error t)))
      (if (eq result :bad-magic)
	  nil
	  result))))

(defun should-save-p (hist file-pos)
  (and (or (dl-length-at-least-p (history-start hist) 1)
	   (zerop file-pos))
       (or (dl-prev (history-start hist))
	   (dl-length-at-least-p (history-head hist) 1))))

(defmethod history-store-save ((store text-history-store)
			       (style (eql :fancy))
			       &key update (history-context *history-context*))
  "Simple text file saving."
  (declare (ignore update)) ;; @@@ is it really even necessary?
  (let ((hist (get-history history-context)) pos)
    (with-slots (file-name) store
      (ensure-directories-exist file-name)
      (loop
	 (if (and (file-exists file-name)
		  (not (eq :ok (fancy-hist-p file-name))))
	     (progn
	       (cerror "Rename it and try again."
		       "The history file exists, but is probably not in fancy ~
                        format, so I'm not overwriting it.")
	       (rename-file file-name (s+ file-name ".non-fancy")))
	     (return)))
      (with-open-file (stream file-name
			      :direction :output
			      :if-does-not-exist :create
			      :if-exists :append)
	(when (zerop (setf pos (file-position stream)))
	  ;; Write version
	  (format stream "~a ~a~%" *text-history-magic* *text-history-version*))
	;; We have to have at least one history entry or we're starting with a
	;; blank file, and we have added at least one line
	;; (when (and (or (dl-length-at-least-p (history-start hist) 1)
	;; 	       (zerop pos))
	;; 	   (or (dl-prev (history-start hist))
	;; 	       (dl-length-at-least-p (history-head hist) 1)))
	(when (should-save-p hist pos)
	  (dl-list-do-backward
	   (if (zerop pos)
	       (history-tail hist)
	       (dl-prev (history-start hist)))
	   #'(lambda (x)
	       ;; Write the history-entry struct.
	       ;; But, don't bother writing NIL for the empty current.
	       (when (not (and (eq x (dl-content (history-head hist)))
			       (not (history-entry-line x))))
		 (write x :stream stream)
		 (terpri stream))))
	  ;; Move the start to the end.
	  (setf (history-start hist) (history-head hist)))))))

(defmethod history-store-load ((store text-history-store)
			       (style (eql :fancy))
			       &key update
				 (history-context *history-context*))
  "Load the history to HISTORY-CONTEXT from STORE in STYLE."
  (declare (ignore update)) ;; @@@ implement update
  (let ((hist (get-history history-context)))
    (with-slots (file-name) store
      (ensure-directories-exist file-name)
      (when (file-exists file-name) ;; Don't fail if the file doesn't exist.
	(with-open-file (stream file-name :direction :input)
	  (when (not (eq :ok (check-version stream :no-error t)))
	    (error "History format is fancy, but the history file isn't in ~
                    fancy format."))
	  (let ((*read-eval* nil))
	    (setf (history-head hist)
		  (make-dl-list
		   (nreverse ;; <<-- N.B.
		    (loop :with s
		       :while (setq s (safe-read stream nil))
		       :do
		       (when (not (history-entry-p s))
			 (error "Malformed history entry in history file: ~a."
				s))
		       :collect s)))
		  (history-tail hist)  (dl-last (history-head hist))
		  (history-start hist) (history-head hist)
		  (history-cur hist)   (history-head hist))))))))

;;;;;;;;;;;;;;;;;
;; Simple style

;; Let's try to keep this one compatible with other shells.

(defmethod history-store-save ((store text-history-store)
			       (style (eql :simple))
			       &key update (history-context *history-context*))
  "Simple text file saving."
  (declare (ignore update)) ;; @@@ is it really even necessary?
  (let ((hist (get-history history-context)) #|have-any|# pos)
    (with-slots (file-name) store
      (ensure-directories-exist file-name)
      (loop
	 (if (and (file-exists file-name) (fancy-hist-p file-name))
	     (progn
	       (cerror "Rename it and try again."
		       "The history file exists, but is probably is in fancy ~
                        format, so I'm not overwriting it.")
	       (rename-file file-name (s+ file-name ".fancy")))
	     (return)))
      (with-open-file (stream file-name
			      :direction :output
			      :if-does-not-exist :create
			      :if-exists :append)
	;; Assume if file-position is zero, we just created it, so
	;; output the whole history even if update is true.
	(setf pos (file-position stream))
	;; We have to have at least one history entry or we're starting with a
	;; blank file, and we have added at least one line
	(when (and (or (dl-length-at-least-p (history-start hist) 1)
		       (zerop pos))
		   ;;(dl-prev (history-start hist)))
		   (or (dl-prev (history-start hist))
		       (dl-length-at-least-p (history-head hist) 1)))
	  ;; (format t "pos = ~d~%" pos)
	  (dl-list-do-backward
	   (if (zerop pos)
	       (history-tail hist)
	       (dl-prev (history-start hist)))
	   #'(lambda (x)
	       ;; Don't bother writing a blank for the empty current.
	       (when (not (and (eq x (dl-content (history-head hist)))
			       (not (history-entry-line x))))
		 ;; (format t "--> ~s <--~%" (history-entry-line x))
		 (write-line
		  (or (history-entry-line x) "") ;; @@@ workaround for NIL
		  stream))))
	  ;; Move the start to the end.
	  (setf (history-start hist) (history-head hist)))))))

(defmethod history-store-load ((store text-history-store)
			       (style (eql :simple))
			       &key update
				 (history-context *history-context*))
  "Simple text file history loading."
  (declare (ignore update)) ;; @@@ implement update
  (let ((hist (get-history history-context)))
    (with-slots (file-name) store
      (when (file-exists file-name) ;; Don't fail if the file doesn't exist.
	(when (fancy-hist-p file-name)
	  (error "The history file is probably in fancy format, but we're ~
                  we're trying to load simple format."))
	(with-open-file (stream file-name :direction :input)
	  (setf (history-head hist)
		(make-dl-list (nreverse ;; <<-- N.B.
			       (loop :with line
				  :while (setf line (read-line stream nil))
				  :collect (make-history-entry :line line))))
		(history-tail hist)  (dl-last (history-head hist))
		(history-start hist) (history-head hist)
		(history-cur hist)   (history-head hist)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Storage for format :database

#+t-rl-config-use-sqlite
(progn
;; Turn on SQL symbolic syntax.
  #.(clsql:file-enable-sql-reader-syntax)

(defclass db-history-store (history-file-store)
  ((connection
    :initarg :connection :accessor db-history-store-connection :initform nil 
    :documentation "Database connection."))
  (:documentation "Database history format."))

(defmethod history-store-default-file-name ((store db-history-store)
					    &optional
					      (context *history-context*))
  (nos:path-append (nos:data-dir (string-downcase context))
		   "command-history.sqlite"))

(defmethod initialize-instance
    :after ((o db-history-store) &rest initargs &key &allow-other-keys)
  "Initialize a db-history-store."
  (declare (ignore initargs))
  (when (not (and (slot-boundp o 'file-name)
		  (slot-value o 'file-name)))
    (setf (slot-value o 'file-name)
	  (history-store-default-file-name o))))

;;;;;;;;;;;;;;;;;
;; Fancy style

;; Increment for every incompatible schema change.
(defparameter *db-history-version* 1
  "Version number of history database.")
;; @@@ maybe we should have a version table ?

(defgeneric create-history-table (store version)
  (:documentation "Create the history table.")
  (:method ((store db-history-store) (version (eql 1)))
    (with-slots (connection) store
      (clsql:create-table [history]
			  '(([id]       integer :primary-key :autoincrement)
			    ([context]  varchar)
			    ([time]     integer)
			    ([line]     varchar)
			    ([modified] integer)
			    ([extra]    varchar))
			  :database connection)
      ;; @@@ create version table and add record
      )))

(defun ensure-history-table (store)
  (declare (ignore store))
  ;; @@@
  #|
  (when (not (clsql table-exists [version]))
  (when (not (clsql table-exists [history]))
  (create-history-table store *db-history-version*)
  |#
  )

(define-condition create-db-error (simple-error) ()
  (:default-initargs
   :format-control "Failed to create the history database."))

(defun ensure-history-db (store)
  (with-slots (file-name connection) store
    (ensure-directories-exist file-name)
    (let ((db-name (list file-name)))
      (when (not (nos:file-exists file-name))
	(clsql:create-database db-name :database-type :sqlite3))
      (setf connection (clsql:connect db-name :database-type :sqlite3))
      (ensure-history-table store))))

(defmethod history-store-save ((store db-history-store)
			       (style (eql :fancy))
			       &key update (history-context *history-context*))
  "Simple text file saving."
  (with-slots (file-name connection) store
    (let ((hist (get-history history-context)))
      (omapn #'(lambda (x)
		 (when (not (and (eq x (dl-content (history-head hist)))
				 (not (history-entry-line x))))
		   (clsql:insert-records
		    :into [history]
		    :av-pairs `(([context]  ,(string-downcase history-context))
				([time]     ,(history-entry-time x))
				([line]     ,(history-entry-line x))
				([modified]
				 ,(if (history-entry-modified x) 1 0))
				([extra]    ,(history-entry-extra x)))
		    :database connection)))
	     (if update
		 (history-start hist)
		 (history-head hist)))
      ;; Move the start to the end.
      (setf (history-start hist) (history-tail hist)))))

(defmethod history-store-load ((store db-history-store)
			       (style (eql :fancy))
			       &key update
				 (history-context *history-context*))
  "Load the history to HISTORY-CONTEXT from STORE in STYLE."
  (declare (ignore update)) ;; @@@ implement update
  (let ((hist (get-history history-context)))
    (with-slots (file-name connection) store
      ;; @@@ make error types, so we can ignore them
      ;; or instead of getting an error here, just create it?
      (when (not (nos:file-exists file-name))
	(error "Command history database does not exist."))
      (when (not connection)
	(error "Not connected to history database."))

      ;; @@@ If update is set, we should probably only get with a time greater
      ;; than last time.
      (let ((records
	     (loop :for item
		:in (clsql:select [time] [line] [modified]
				  :from [history]
				  :where [= [context]
					    (string-downcase history-context)]
				  :flatp t
				  :database connection)
		:collect (make-history-entry :time (first item)
					     :line (second item)
					     :modified nil))))
	(when records
	  (setf (history-head hist)    (make-dl-list records)
		(history-tail hist)    (dl-last (history-head hist))
		(history-start hist)   (history-tail hist)
		(history-cur hist)     (history-tail hist)))))))

(defmethod history-store-start ((store db-history-store) style)
  "Start using a text history store, of any style."
  (declare (ignore style))
  (ensure-history-db store))

(defmethod history-store-done ((store db-history-store) style)
  "Indicate we're done with using a text history store, of any style."
  (declare (ignore style))
  (with-slots (connection) store
    (clsql:disconnect :database connection)))

) ;; t-rl-config-use-sqlite progn

;; There is no database simple style.

;; EOF
