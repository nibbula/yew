;;
;; unzip.lisp - Manipulate zip files.
;;

(defpackage :unzip
  (:documentation "Manipulate zip files.")
  (:use :cl :dlib :dlib-misc :zip :opsys :mkdir :table :grout)
  (:export
   #:unzip-command
   #:zip-command
   ))
(in-package :unzip)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

;; (defun confirm (entry)
;;   (yes-or-no-p
;;    "Are you sure you want to overwrite ~s?" (zipfile-entry-name entry)))

(defun set-attributes (entry file-name)
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

(defun make-file (entry file-name)
  (with-open-file-or-stream (str file-name
				 :direction :output :if-exists :overwrite
				 :if-does-not-exist :create
				 :element-type '(unsigned-byte 8))
    (zipfile-entry-contents entry str))
  (set-attributes entry file-name))

;; @@@ we need to preserve the mode and time and maybe other things?
(defun extract-entry (entry &key overwrite pipe)
  (let (out)
    (cond
      (pipe
       (setf out *standard-output*))
      (overwrite
       (setf out (zipfile-entry-name entry)))
      ((file-exists (zipfile-entry-name entry))
       (if (confirm (format nil "overwrite ~s" (zipfile-entry-name entry))
		    :eof-confirms nil)
	   (setf out (zipfile-entry-name entry))
	   (return-from extract-entry nil)))
      (t
       (setf out (zipfile-entry-name entry))))
    (format t "~a~%" out)
    (when (stringp out)
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
	    (make-file entry out))))))

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
    (&key overwrite list-p verbose pipe force-utf-8 exclude archive
       files extract)
  ;;(declare (ignore overwrite pipe extract))
  (when verbose
    (setf list-p t))
  (let ((totals (make-total))
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
		   (extract-entry entry :overwrite overwrite :pipe pipe))
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
  "Extract files from a ZIP archive."
  (when verbose
    (setf list-p t))
  (apply #'unzip-command `(,@keys :extract ,(if list-p nil t))))

;; EOF
