;;
;; history-store.lisp - Storage for command history.
;;

(in-package :rl)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

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

;; Increment for every incompatible change.
(defparameter *text-history-version* 2
  "Version number of history format file.")

(defmethod history-store-save ((store text-history-store)
			       (style (eql :fancy))
			       &key update (history-context *history-context*))
  "Simple text file saving."
  (declare (ignore update)) ;; @@@ is it really even necessary?
  (let ((hist (get-history history-context)) pos)
    (with-slots (file-name) store
      (ensure-directories-exist file-name)
      (with-open-file (stream file-name
			      :direction :output
			      :if-does-not-exist :create
			      :if-exists :append)
	(when (zerop (setf pos (file-position stream)))
	  ;; Write version
	  (format stream "trlh ~a~%" *text-history-version*))
	(when (or (> (dl-length (history-start hist)) 1)
		  (zerop pos))
	  (dl-list-do-backward
	   (if (zerop pos)
	       (history-tail hist)
	       (dl-prev (history-start hist)))
	   #'(lambda (x)
	       ;; Write the history-entry struct.
	       ;; But, don't bother writing NIL for the empty current.
	       (when (not (and (eq x (dl-content (history-head hist)))
			       (not (history-entry-line x))))
		 (write-line x stream))))
	  ;; Move the start to the end.
	  (setf (history-start hist) (history-head hist)))))))

(defmethod history-store-load ((store text-history-store)
			       (style (eql :fancy))
			       &key update
				 (history-context *history-context*))
  "Load the history to HISTORY-CONTEXT from STORE in STYLE."
  (declare (ignore update)) ;; @@@ implement update
  (let ((hist (get-history history-context))
  	(s (make-string 4)) i)
    (with-slots (file-name) store
      (with-open-file (stream file-name :direction :input)
	(read-sequence s stream :end 4)
	(when (string/= s "trlh")
	  (error "Bad magic tag ~a in history file." s))
	(when (/= *text-history-version*
		  (setq i (parse-integer (setq s (read-line stream)))))
	  (error "Bad version number ~a in history file." s))
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
		(history-cur hist)   (history-head hist)))))))

;;;;;;;;;;;;;;;;;
;; Simple style

;; Let's try to keep this one compatible with other shells.

(defmethod history-store-save ((store text-history-store)
			       (style (eql :simple))
			       &key update (history-context *history-context*))
  "Simple text file saving."
  (declare (ignore update)) ;; @@@ is it really even necessary?
  (let ((hist (get-history history-context)) pos)
    (with-slots (file-name) store
      (ensure-directories-exist file-name)
      (with-open-file (stream file-name
			      :direction :output
			      :if-does-not-exist :create
			      :if-exists :append)
	;; Assume if file-position is zero, we just created it, so
	;; output the whole history even if update is true.
	(setf pos (file-position stream))
	;; We have to have at least
	(when (or (> (dl-length (history-start hist)) 1)
		  (zerop pos))
	  (dl-list-do-backward
	   (if (zerop pos)
	       (history-tail hist)
	       (dl-prev (history-start hist)))
	   #'(lambda (x)
	       ;; Don't bother writing a blank for the empty current.
	       (when (not (and (eq x (dl-content (history-head hist)))
			       (not (history-entry-line x))))
		 (write-line
		  (or (history-entry-line x) "") ;; @@@ workaround for NIL
		  stream))))
	  ;; Move the start to the end.
	  (setf (history-start hist) (history-head hist))))))
  (values))

(defmethod history-store-load ((store text-history-store)
			       (style (eql :simple))
			       &key update
				 (history-context *history-context*))
  "Simple text file history loading."
  (declare (ignore update)) ;; @@@ implement update
  (let ((hist (get-history history-context)))
    (with-slots (file-name) store
      (with-open-file (stream file-name :direction :input)
	(setf (history-head hist)
	      (make-dl-list (nreverse ;; <<-- N.B.
			     (loop :with line
				:while (setf line (read-line stream nil))
				:collect (make-history-entry :line line))))
	      (history-tail hist)  (dl-last (history-head hist))
	      (history-start hist) (history-head hist)
	      (history-cur hist)   (history-head hist)))))
  (values))

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
				 ,(if (history-entry-modified x) 1 0)))
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
      (when (not (nos:file-exists file-name))
	;; @@@ make an error type, so we can ignore it
	(error "Command history database does not exist."))
      ;; @@@ If update is set, we should probably only get with a time greater
      ;; than last time.
      (let ((records
	     (loop :for item
		:in (clsql:select [*]
				  :from [history]
				  :where [= [context] history-context]
				  :flatp t
				  :database connection)
		:collect (make-history-entry :time (first item)
					     :line (second item)
					     :modified nil))))
	(when records
	  (setf (history-current hist)
		(make-dl-list records)
		(history-tail hist)  (last (history-head hist))
		(history-start hist) (history-tail hist)
		(history-cur hist)   (history-tail hist)))))))

(defmethod history-store-start ((store db-history-store) style)
  "Start using a text history store, of any style."
  (declare (ignore style))
  (with-slots (file-name connection) store
    (let ((db-name (list file-name))
	  new)
      (ensure-directories-exist file-name)
      (when (not (nos:file-exists file-name))
	(clsql:create-database db-name :database-type :sqlite3)
	(setf new t))
      (setf connection (clsql:connect db-name :database-type :sqlite3))
      (when new
	(clsql:create-table [history]
			    '(([context]  #| text |# varchar)
			      ([time]     integer)
			      ([line]     #| text |# varchar)
			      ([modified] integer))
			    :database connection)))))

(defmethod history-store-done ((store db-history-store) style)
  "Indicate we're done with using a text history store, of any style."
  (declare (ignore style))
  (with-slots (connection) store
    (clsql:disconnect :database connection)))

) ;; t-rl-config-use-sqlite progn

;; There is no database simple style.

;; EOF
