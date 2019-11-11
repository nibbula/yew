;;
;; locate.lisp - locate files
;;

(defpackage :locate
  (:documentation "Try to somehow locate files.")
  (:use :cl :dlib :dlib-misc :ppcre #| :trie |#)
  (:export
   #:locate
   #:!locate
   ))
(in-package :locate)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))
;; (declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))
;; (declaim (optimize (speed 3) (safety 0) (debug 2) (space 0)))

(defparameter *magic-str* "mlocate")
(defparameter *magic* (make-array (length *magic-str*)
				  :initial-contents
				  (map 'vector (_ (char-code _)) "mlocate")
				  :element-type '(unsigned-byte 8))
  "Magic number.")
(declaim (type (simple-array (unsigned-byte 8)) *magic*))

(defparameter *version* 0
  "Version number of the database format.")
(declaim (type fixnum *version*))

(defstruct dir
  (time	(make-dtime) :type dtime)
  (name "" :type string)
  ;;(entries #() :type vector)
  entries
  )

;; @@@ maybe we don't need to save the type?
;; (defstruct file
;;   (type nil :type (or (member :file :directory) null))
;;   (name "" :type string))

(defstruct db
  file
  (config-size 0 :type (unsigned-byte 32))
  (require-visibility nil :type boolean)
  root
  prune-bind-mounts
  prune-fs
  prune-paths
  directories
  (count 0 :type integer))

(declaim (ftype (function ((vector (unsigned-byte 8))) (unsigned-byte 32))
		get-be32))
(defun get-be32 (buf)
  "Return a big endian 32 bit integer from the buffer of bytes."
  (declare (type (simple-array (unsigned-byte 8)) buf))
  (logior (ash (aref buf 0) (* 3 8))
	  (ash (aref buf 1) (* 2 8))
	  (ash (aref buf 2) (* 1 8))
	  (aref buf 3)))

(declaim (ftype (function ((vector (unsigned-byte 8))) (unsigned-byte 64))
		get-be64))
(defun get-be64 (buf)
  "Return a big endian 64 bit integer from the buffer of bytes."
  (declare (type (simple-array (unsigned-byte 8)) buf))
  (logior (ash (aref buf 0) (* 7 8))
	  (ash (aref buf 1) (* 6 8))
	  (ash (aref buf 2) (* 5 8))
	  (ash (aref buf 3) (* 4 8))
	  (ash (aref buf 4) (* 3 8))
	  (ash (aref buf 5) (* 2 8))
	  (ash (aref buf 6) (* 1 8))
	       (aref buf 7)))

(defun read-string (stream)
  "Read a null terminated string of bytes from STREAM."
  (with-output-to-string (str)
    (loop :with byte
       :while (not (zerop (setf byte (read-byte stream))))
       :do (princ (code-char byte) str))))

(defun read-locate-header (stream buf db)
  "Read the locate header from STREAM into DB, using the byte buffer BUF."
  (declare (type (simple-array (unsigned-byte 8)) buf))
  (when (/= (read-byte stream) 0)
    (error "Missing starting zero. Probably not an mlocate file."))
  (read-sequence buf stream :end (length *magic*))
  (when (not (equalp (subseq buf 0 (length *magic*)) *magic*))
    (error "Bad magic string. It's probably not an mlocate database?"))
  (read-sequence buf stream :end 4)
  (setf (db-config-size db) (get-be32 buf))
  (when (/= (read-byte stream) *version*)
    (error "Wrong format version."))

  (let ((byte (read-byte stream)))
    (setf (db-require-visibility db)
	  (case byte
	    (0 nil)
	    (1 t)
	    (t (error "require-visibility is not 1 or 0.")))))
  ;; Two bytes of padding, presumably zero
  (read-byte stream)
  (read-byte stream)
  (setf (db-root db) (read-string stream)))

(defun read-locate-config (stream db)
  (let (var-name values (byte-count 0))
    (declare (type integer byte-count))
    (flet ((read-values ()
	     (coerce
	      (loop :with str :of-type string
		 :while (not (zerop (length (setf str (read-string stream)))))
		 :do (incf byte-count (1+ (length str)))
		 :collect str)
	      'vector)))
      (declare (ftype (function () vector) read-values))
      (loop :while (< byte-count (db-config-size db))
	 :do
	 (setf var-name (read-string stream))
	 (incf byte-count (1+ (length var-name)))
	 (cond
	   ((equal var-name "prune_bind_mounts")
	    (setf values (read-values))
	    (setf (db-prune-bind-mounts db)
		  (cond
		    ((equal (aref values 0) "0") nil)
		    ((equal (aref values 0) "1") t)
		    (t (error "prune_bind_mounts is not 0 or 1")))))
	   ((equal var-name "prunefs")
	    (setf (db-prune-fs db) (read-values)))
	   ((equal var-name "prunepaths")
	    (setf (db-prune-paths db) (read-values)))
	   (t
	    ;; Typical of unix shit, don't complain about unknown variables.
	    ;; (warn "Unknown mlocate variable name ~s" var-name)
	    (read-values)))))))

(defun read-file-entries (stream)
  "Return a vector of file structs read from STREAM."
  (let* ((count 0)
	 (results
	 (loop :with type
	    :do
	    (setf type (read-byte stream))
	    (setf type
		  (case type
		    (0 :file)
		    (1 :directory)
		    (2 :eod)
		    (otherwise (error "Unknown file type ~s" type))))
	    :while (not (eq type :eod))
	    ;; :collect (make-file :type type :name (read-string stream)))
	    :collect (read-string stream)
	    :do (incf count))))
    (declare (type fixnum count))
    (make-array count :initial-contents results :element-type 'string
		:adjustable nil)))

(defvar *complete* nil
  "Make sure we don't get an EOF in the middle of a directory.")

(defun read-dir (stream buf)
  "Return a a dir struct read from STREAM, using BUF. Set *COMPLETE* so error
handlers can tell if things were okay."
  (let (secs nsecs name dir)
    (setf secs (uos:unix-to-universal-time
		(progn
		  (read-sequence buf stream :end 8) 
		  (get-be64 buf))))
    (setf *complete* nil
	  nsecs (progn
		  (read-sequence buf stream :end 4)
		  (get-be32 buf)))
    (read-sequence buf stream :end 4) ;; 4 bytes of padding
    (setf name (read-string stream))
    (setf dir (make-dir :time (make-dtime :seconds secs :nanoseconds nsecs)
			:name name
			:entries (read-file-entries stream)))
    (setf *complete* t)
    dir))

(defun read-locate-files (stream buf db)
  (let (#| (trie (make-trie)) |#
	(*complete* nil)
	dir)
    ;; (setf (db-directories db) trie)
    (setf (db-count db) 0)
    (handler-case
	(with-spin ()
	  (loop #| a simple loop |#
	     (setf dir (read-dir stream buf))
	     (push dir (db-directories db))
	     (loop :for f :across (dir-entries dir)
		:do
		;; (trie-add trie (opsys:path-append (dir-name dir)
		;; 					(file-name f))))
		;; @@@ This loses the file type and directory time
		;; (push (opsys:path-append (dir-name dir) (file-name f))
		;; 	    (db-directories db))
		(incf (db-count db))
		(when (zerop (mod (db-count db) 10000))
		  (spin)))))
      (end-of-file ()
	(when (not *complete*)
	  (warn "Reached the end of file prematurely."))))
    ;; @@@ or maybe it doesn't matter if they're reversed?
    (setf (db-directories db) (nreverse (db-directories db)))
    ))

(defun read-locate-db (file)
  (let ((buf (make-array 8 :element-type '(unsigned-byte 8) :adjustable nil))
	(db (make-db)))
    (with-open-file (stream file :element-type '(unsigned-byte 8))
      (setf (db-file db) file)
      (read-locate-header stream buf db)
      (read-locate-config stream db)
      (read-locate-files stream buf db))
    db))

(defvar *locate-db* nil
  "The currently loaded database.")

(defparameter *locate-file* "/var/lib/mlocate/mlocate.db"
  "Name of the mlocate file.")

(defun ensure-database (database file)
  (or database
      *locate-db*
      (setf *locate-db* (read-locate-db (or file *locate-file*)))))

(defun locate (regexp &key (database *locate-db*) (file *locate-file*)
			collect (print t) basename)
  (setf database (ensure-database database file))
  (let ((scanner (ppcre:create-scanner regexp)))
    (labels ((our-path-append (dir base)
	       (declare (type string dir base))
	       ;; The regular path-append is too slow here.
	       (concatenate 'string dir nos:*directory-separator-string* base))
	     (scan-dir (dir)
	       (loop :with path :and full
		  :for f :across (dir-entries dir)
		  :do
		  (setf path
			(if (not basename)
			    (setf full (our-path-append (dir-name dir) f))
			    f))
		  :when (ppcre:scan scanner path)
		  :collect (or full (our-path-append (dir-name dir) f)))))
      (declare (ftype (function (string string) string)))
      (loop
	 :with results
	 :for d :in (db-directories database)
	 :if (setf results (scan-dir d))
	   :if collect
             :nconc results
	   :end
	   :and :if print
	   :do
           (loop :for f :in results
	      :do (format t "~a~%" f))))))

(defun show-statistics (database file)
  (let ((db (ensure-database database file)))
    (let ((filename-bytes 0) #| db-bytes |#)
      (loop :for dir :in (db-directories db) :do
	 (loop :for f :across (dir-entries dir)
	    :do
	    (incf filename-bytes (length (dir-name dir)))
	    (incf filename-bytes)
	    (incf filename-bytes (length f))))
      (print-properties `("Database" ,(db-file db)
			  "Directories" ,(length (db-directories db))
			  "Files" ,(db-count db)
			  "Bytes in file names" ,filename-bytes)
			:de-lispify nil :right-justify t))))

#+lish
(lish:defargtype locate-database (lish:arg-pathname)
  "A designator for a locate database. Can be either a locate:db or a pathname."
  ())

#+lish
(lish:defcommand locate
  ((regexp string :help "Regular expression to match paths.")
   (database locate-database :short-arg #\d :help "Database to use.")
   (reload boolean :short-arg #\r :help "Reload the database.")
   (basename boolean :short-arg #\b :help "Match only the basename.")
   (collect boolean :short-arg #\c :help "Collect values to return.")
   (print boolean :short-arg #\p :default t :use-supplied-flag t
    :help
    "True to print matching paths. Print defaults to false when collect is given")
   (statistics boolean :short-arg #\S :help "Show database statistics."))
  :keys-as keys
  "Locate files."
  (when (and (not statistics) (not regexp))
    (error "Locate what?"))
  (when (and collect (not print-supplied-p))
    (setf print nil))
  (remf keys :regexp)
  (remf keys :reload)
  (when reload
    (setf *locate-db* nil))
  (when database
    (typecase database
      (db #| dont' mess with anythting |#)
      ((or string pathname)
       ;; Convert to :file keyword
       (remf keys :database)
       (setf (getf keys :file) database))
      (t
       (error "Database argument should be a database or a file name."))))

  (cond
    (statistics (show-statistics database (getf keys :file)))
    (t (setf lish:*output* (apply #'locate `(,regexp ,@keys))))))

;; EOF
