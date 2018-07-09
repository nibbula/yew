;;
;; unzip.lisp - Manipulate zip files.
;;

(defpackage :unzip
  (:documentation "Manipulate zip files.")
  (:use :cl :dlib :opsys :dlib-misc :zip :mkdir :table :grout)
  (:export
   #:unzip-command
   #:zip-command
   ))
(in-package :unzip)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(defvar *confirm-all* nil
  ":all :none or NIL if it hasn't been set.")

(defun confirm-overwrite (entry)
  "Return a file name to overwrite or NIL not overwrite."
  (case *confirm-all*
    (:all  (return-from confirm-overwrite (zipfile-entry-name entry)))
    (:none (return-from confirm-overwrite nil)))

  (loop :with done :and answer :and result
     :and name = (zipfile-entry-name entry)
     :do
     (format *query-io*
	     "Overwrite ~s?~%[y]es, [n]o, Yes to [A]ll, [N]one, [r]ename: "
	     name)
     (finish-output *query-io*)
     (setf answer (read-line *query-io* nil))
     (when (null answer) ;; EOF is equivalent to None.
       (setf answer "N"))
     (setf done t)
     (flet ((bad-input ()
	      (if (zerop (length answer))
		  (format *query-io* "You didn't seem to enter anything.~%")
		  (format *query-io* "~s isn't one of the choices.~%" answer))
	      (format *query-io* "Please choose one of: 'y' 'n' 'A' 'N' 'r'~%")
	      (setf done nil)))
       (cond
	 ((zerop (length answer))
	  (bad-input))
	 ((or (equal answer "y") (equal (trim answer) "yes"))
	  (setf result name))
	 ((or (equal answer "n") (equal (trim answer) "no"))
	  (setf result nil))
	 ((or (equal answer "A") (equalp (trim answer) "All"))
	  (setf *confirm-all* :all
		result name))
	 ((or (equal answer "N") (equalp (trim answer) "None"))
	  (setf *confirm-all* :none
		result nil))
	 ((or (equal answer "r") (equalp (trim answer) "rename"))
	  (loop :with rename-done
	     :do
	     (setf result (rl:read-filename :prompt "new name: "
					    :allow-nonexistent t
					    :string (path-directory-name name)))
	     (terpri *query-io*)
	     (cond
	       ((not result) #| aborting the rl is equivalent to saying 'n' |#)
	       ((zerop (length (trim result)))
		(format *query-io* "You didn't seem to enter anything.~%"))
	       ((file-exists (trim result))
		(format *query-io*
			"That file exists. Please use a new file name.~%"))
	       (t (setf rename-done t)))
	     :while (not rename-done)))
	 (t
	  (bad-input))))
     :while (not done)
     :finally (return result)))

(defun set-attributes (entry file-name)
  "Set the attributes from ENTRY on FILE-NAME."
  (with-slots ((mode zip::mode)
	       (made-by zip::made-by)
	       (date zip::date)) entry
    (when (eq zip::+UNIX+ made-by)
      #+unix (uos:chmod file-name (logand mode #o777))
      #-unix (warn "I don't know how to set file modes yet on this platform.")
      )
    (let ((time (make-os-time :seconds date)))
      (nos:set-file-time file-name :modification-time time))))

(defun fix-time (name entry)
  "Fix the time on the directory of ENTRY."
  (when (stringp name)
    (let ((dir (path-directory-name name))
	  (file (path-file-name name)))
      (when (and (not (zerop (length dir)))
		 (zerop (length file))
		 (file-exists dir))
	(let ((time (make-os-time :seconds (zipfile-entry-date entry))))
	  (nos:set-file-time dir :modification-time time))))))

(defun make-file (entry file-name never-overwrite)
  "Create the FILE-NAME with the contents of ENTRY. Pass the never-overwrite
flag for extra safety."
  (with-open-file-or-stream (str file-name
				 :direction :output
				 :if-exists
				 (when never-overwrite
				   ;; This is probably superfluous, but..
				   :error
				   :overwrite)
				 :if-does-not-exist :create
				 :element-type '(unsigned-byte 8))
      (zipfile-entry-contents entry str))
    (set-attributes entry file-name))

(defun extract-entry (entry &key overwrite never-overwrite pipe)
  "Extract the ENTRY. The OVERWRITE, NEVER-OVERWRITE, and PIPE flags, are as
in UNZIP-COMMAND."
  (let (out answer)
    (cond
      (pipe
       (setf out *standard-output*))
      ((and overwrite (not never-overwrite))
       (setf out (zipfile-entry-name entry)))
      ((file-exists (zipfile-entry-name entry))
       (if (and (not never-overwrite)
		(setf answer (confirm-overwrite entry)))
	   (setf out answer)
	   (return-from extract-entry nil)))
      (t ;; the file doesn't exist
       (setf out (zipfile-entry-name entry))))
    (when (not pipe)
      (format t "~a~%" out))
    (if (stringp out)
	(let* ((dir (path-directory-name out))
	       (dir-len (length dir))
	       (file (path-file-name out))
	       (file-len (length file)))
	  (when (and (not (zerop dir-len)) (not (file-exists dir)))
	    (mkdir :directories `(,dir) :errorp nil)
	    (set-attributes entry dir))
	  (if (zerop file-len)
	      ;; It has no file name.
	      (if (zerop dir-len)
		  (when (not pipe)
		    (error "Stream input file and stream output not specified."))
		  #| It's just a directory so we already made it above |#)
	      ;; It has a file name.
	      (make-file entry out never-overwrite)))
	;; It's a pipe.
	(when (or (not (zipfile-entry-name entry))
		  (and (zipfile-entry-name entry)
		       (not (zerop (length (path-file-name
					    (zipfile-entry-name entry)))))))
	  ;; It's presumably not a directory.
	  (zipfile-entry-contents entry out)))))

(defun compression-percent (uncompressed compressed)
  (if (zerop uncompressed)
      0
      (round (- 100 (* (/ compressed uncompressed) 100)))))

(defun list-entry (entry &key verbose)
  (let (result)
    (with-slots ((name zip::name)
		 (size zip::size)
		 (compressed-size zip::compressed-size)
		 (comment zip::comment)
		 (method zip::method)
		 (date zip::date)
		 (crc zip::crc)
		 (made-by zip::made-by)
		 (mode zip::mode)) entry
      (flet ((dd (time)
	       ;; (format nil "~x ~x" date time)))
	       (format-date "~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d"
			    (:year :month :date :hour :min)
			    :time time))
	     (glom (format-control &rest things)
	       (declare (ignore format-control))
	       (setf result (make-array (length things)
					:initial-contents things))
	       ;; (apply #'format t format-control things)
	       ))
	(if verbose
	    (glom "~10d  ~10d ~3d% ~16a ~a~%"
		  size compressed-size
		  (compression-percent size compressed-size)
		  (string-capitalize method)
		  (format nil "~8,'0x" crc)
		  (zip:made-by-name made-by)
		  #+unix (if (eq made-by zip::+unix+)
			     (uos:symbolic-mode mode)
			     (format nil "~6,'0o" mode))
		  #-unix (format nil "~6,'0o" mode)
		  (dd date) name)
	    (glom "~10d  ~15a ~a~%"
		  size (dd date) name))))))

(defun list-title (verbose)
  (declare (ignore verbose))
#|  
  (if verbose
      (progn
	(format t "~10@a  ~10@a ~4a ~16a ~a~%"
		"Size" "Compressed"
		"Cmpr"
		"   Date    Time" "Name")
	(format t "~10,,,'-a  ~10,,,'-a ~4,,,'-a ~10,,,'-a ~5,,,'-a ~30,,,'-a~%"
		#\- #\-
		#\- #\-
		#\- #\-))
      (progn
	(format t "~10@a  ~16a ~a~%" "Size" "   Date    Time" "Name")
	(format t "~10,,,'-a  ~10,,,'-a ~5,,,'-a ~40,,,'-a~%" #\- #\- #\- #\-)))
|#
  )

(defstruct total
  (size 0)
  (compressed-size 0)
  (count 0))

(defun list-footer (verbose total)
  (declare (ignore verbose total))
  #|
  (with-slots (size compressed-size count) total
    (if verbose
      (progn
	(format t "~10,,,'-a  ~10,,,'-a ~4,,,'-a ~10,,,'-a ~5,,,'-a ~30,,,'-a~%"
		#\- #\-
		#\- #\-
		#\- #\-)
	(format t "~10@a  ~10@a ~3d% ~15a  Files: ~d~%"
		size compressed-size
		(compression-percent size compressed-size)
		" " count))
      (progn
	(format t "~10,,,'-a  ~10,,,'-a ~5,,,'-a ~40,,,'-a~%" #\- #\- #\- #\-)
	;;(format t "~10@a  ~15a ~a~%" "Size" " Date Time " "Name")
	(format t "~10@a  ~15a  Files: ~d~%" size " " count))))
  |#
  )

(defun unzip-command
    (&key archive files exclude extract list-p verbose overwrite
       never-overwrite pipe force-utf-8)
  "Extract or list files from a ZIP archive.
  ARCHIVE is the zip archive file.
  FILES is a list of files to extract.
  EXCLUDE is a list of files to exclude.
  EXTRACT is true to extract files.
  LIST-P is true to list files.
  VERBOSE is true to list more detail about archive contents.
  OVERWRITE is true to overwrite files without asking.
  NEVER-OVERWRITE is true to never overwrite files, overridding OVERWRITE.
  PIPE is true to extract files to *STANDARD-OUTPUT*.
  FORCE-UTF-8 is true to pretend the archive contents are in UTF-8."
  ;;(declare (ignore overwrite pipe extract))
  (when verbose
    (setf list-p t))
  (let ((totals (make-total))
	(*confirm-all* nil)
	zippy results)
    (macrolet ((do-entries (() &body body)
		 `(do-zipfile-entries (name entry zippy)
		    (when (or (not files)
			      (and files (some (_ (glob:fnmatch _ name)) files)))
		      (when (or (not exclude)
				(and exclude
				     (notany (_ (glob:fnmatch _ name)) exclude)))
			,@body)))))
      (with-open-file-or-stream (stream archive
					:direction :input
					:element-type '(unsigned-byte 8))
	(unwind-protect
	     (progn
	       (when list-p
		 (list-title verbose))
	       (setf zippy (zip::open-zipfile-from-stream
			    stream :force-utf-8 force-utf-8))
	       (do-entries ()
		 (when list-p
		   (push (list-entry entry :verbose verbose) results))
		 (when extract
		   (extract-entry entry :overwrite overwrite :pipe pipe
				  :never-overwrite never-overwrite))
		 (incf (total-size totals)
		       (zipfile-entry-size entry))
		 (incf (total-compressed-size totals)
		       (zipfile-entry-compressed-size entry))
		 (incf (total-count totals)))
	       (when extract
		 ;; We have to go and fix the times on the directories, because
		 ;; they were very likely updated by putting files in them.
		 (do-entries ()
		   (fix-time name entry)))
	       (when list-p
		 (list-footer verbose totals)))
	  (when zippy
	    (close-zipfile zippy)))))
    (setf results (nreverse results))
    (with-grout ()
      (when list-p
	(grout-print-table (make-table-from
			    results
			    :column-names
			    (if verbose
				'("Size" "Compressed" "%" "Method" "CRC"
				  "Made by" "Mode" "   Date    Time" "Name")
				'("Size" "   Date    Time" "Name"))
			    ))
	#| @@@ Add the ability to do nice footers to text-table-renderer
	(with-slots (size compressed-size count) totals
	  (grout-print-table
	    (if verbose
		(make-table-from
		 `(,(list size
			  compressed-size
			  (compression-percent size compressed-size)
			  " " (s+ "Files: " count)))
		 :column-names
		 '("Size" "Compressed" "%"
		   "   Date    Time" "Name"))
		(make-table-from
		 `(,(list size " " (s+ "Files: " count)))
		 :column-names
		 '("Size" "   Date    Time" "Name")))))
	|#
	))
    results))

;; (defun zip ()
;;   )

#+lish
(lish:defcommand unzip
  ((overwrite boolean :short-arg #\o
    :help "True to overwrite files without asking.")
   (never-overwrite boolean :short-arg #\n
    :help "True to never overwrite.")
   (list-p boolean :short-arg #\l
    :help "True to list archive members in short format.")
   (verbose boolean :short-arg #\v
    :help "True to list archive members in verbose format.")
   (pipe boolean :short-arg #\p
    :help "True to extract files to standard output.")
   (exclude pathname :short-arg #\x :repeating t
    :help "Files to exclude from extracting or listing.")
   (archive pathname :optional nil
    :help "Zip archive file to operate on.")
   (files pathname :optional t :repeating t
    :help "File members to operate on."))
  :keys-as keys
  "Extract or list files from a ZIP archive."
  (when verbose
    (setf list-p t))
  (apply #'unzip-command `(,@keys :extract ,(if list-p nil t))))

;; EOF
