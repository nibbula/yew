;;
;; unzip.lisp - Manipulate zip files.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patches for ZIP package
;;
;; @@@ Take these out if they get into QuickLisp.

(in-package :zip)

(setf (documentation 'zipfile-entries 'function)
      "Return a hash-table mapping filenames to entry handles for all files
+contained in the zip archive.")

(defstruct zipfile-entry
  name
  stream
  offset
  size
  compressed-size
  comment
  method
  date
  universal-time
  crc
  made-by
  made-by-version
  mode)

(setf (documentation 'zipfile-entry-name 'function)
      "Return an entry's file name as a string.")

(defparameter *compression-methods*
  `(( 0 . :none)	 ;;   - The file is stored (no compression)
    ( 1 . :shrunk)	 ;;   - The file is Shrunk
    ( 2 . :reduced-1)	 ;;   - The file is Reduced with compression factor 1
    ( 3 . :reduced-2)	 ;;   - The file is Reduced with compression factor 2
    ( 4 . :reduced-3)	 ;;   - The file is Reduced with compression factor 3
    ( 5 . :reduced-4)	 ;;   - The file is Reduced with compression factor 4
    ( 6 . :impode)	 ;;   - The file is Imploded
    ( 7 . :reserved-1)	 ;;   - Reserved for Tokenizing compression algorithm
    ( 8 . :deflate)	 ;;   - The file is Deflated
    ( 9 . :deflate64)	 ;;   - Enhanced Deflating using Deflate64(tm)
    (10 . :pk-implode)	 ;;   - PKWARE Imploding (old IBM TERSE)
    (11 . :reserved-2)	 ;;   - Reserved by PKWARE
    (12 . :bzip2)	 ;;   - File is compressed using BZIP2 algorithm
    (13 . :reserved-3)	 ;;   - Reserved by PKWARE
    (14 . :lzma)	 ;;   - LZMA (EFS)
    (15 . :reserved-4)	 ;;   - Reserved by PKWARE
    (16 . :reserved-5)	 ;;   - Reserved by PKWARE
    (17 . :reserved-6)	 ;;   - Reserved by PKWARE
    (18 . :terse)	 ;;   - File is compressed using IBM TERSE (new)
    (19 . :lz77)	 ;;   - IBM LZ77 z Architecture (PFS)
    ;; If the order would have been preserved, everyone could still be using
    ;; an array, without special casing. Keep in mind, when doing anything
    ;; that writes file formats, your temporary kludge may persist for
    ;; thousands of years.
    (97 . :wavepack) ;;   - WavPack compressed data
    (98 . :ppmd)     ;;   - PPMd version I, Rev 1
    ))

(defun compression-method-name (number)
  "Return a mildly useful keyword, given the number, or :unkown if we don't
know about it."
  (or (cdr (assoc number *compression-methods*))
      :unknown))

(defconstant +MS-DOS+         0)
(defconstant +Amiga+          1)
(defconstant +OpenVMS+        2)
(defconstant +UNIX+           3)
(defconstant +VM/CMS+         4)
(defconstant +Atari-ST+       5)
(defconstant +OS/2-HPFS+      6)
(defconstant +Macintosh+      7)
(defconstant +Z-System+       8)
(defconstant +CP/M+           9)
(defconstant +Windows-NTFS+   10)
(defconstant +MVS+            11)
(defconstant +VSE+            12)
(defconstant +Acorn-Risc+     13)
(defconstant +VFAT+           14)
(defconstant +alternate-MVS+  15)
(defconstant +BeOS+           16)
(defconstant +Tandem+         17)
(defconstant +OS/400+         18)
(defconstant +OS-X-Darwin+    19)

(defparameter *made-by-name*
  `(( 0 . "MS-DOS") ; or OS/2 FAT
    ( 1 . "Amiga")
    ( 2 . "OpenVMS")
    ( 3 . "UNIX")
    ( 4 . "VM/CMS")
    ( 5 . "Atari ST")
    ( 6 . "OS/2 HPFS")
    ( 7 . "Macintosh")
    ( 8 . "Z-System")
    ( 9 . "CP/M")
    (10 . "Windows NTFS")
    (11 . "MVS")
    (12 . "VSE")
    (13 . "Acorn Risc")
    (14 . "VFAT")
    (15 . "alternate MVS")
    (16 . "BeOS")
    (17 . "Tandem")
    (18 . "OS/400")
    (19 . "OS X (Darwin)")))

(defun made-by-name (code)
  "Given the number CODE, return a string describing the system or filesystem
that something supposedly came from, or :unkown if we don't know about it."
  (or (cdr (assoc code *made-by-name*))
      :unknown))

(defun made-by-version (code)
  "Return a cons of the major and minor version numbers, given the whole 16-bit
'version-made-by' code."
  (let ((value (logand code #xff)))
    (cons (truncate (/ value 10))
	  (truncate (mod value 10)))))

#|
Bits   Description
-----  -----------
15-9   Year 0 = 1980
 8-5    Month (1-12)
 4-0    Day (1-31)

15-11  Hours (0-23)
10-5   Minutes (0-59)
 4-0   Seconds/2 (0-29)
       The seconds is recorded only to a 2 second resolution.
|#
(defconstant +dos-year-pivot+ 1980
  "Yet more trouble, still to be dealt with.")

;; @@@ This should probably go somewhere else?
(defun dos-to-universal-time (date time)
  "Take the 16 bit date and time in DOS/FAT format, and return a"
  (let ((year (+ (logand (ash date  -9) #x7f) +dos-year-pivot+))
	(mon     (logand (ash date  -5) #x0f))
	(date    (logand      date      #x1f))
	(hour    (logand (ash time -11) #x1f))
	(min     (logand (ash time  -5) #x3f))
	;; The min 59 is not technically true because: leap seconds!!!
	;; But the Common Lisp specification says it must be 0-59 for
	;; encode-universal-time. Anyway, in this case it's mostly likely not
	;; a real leap second, but just due to the 2 second multiply.
	(sec  (min 59 (* (logand (ash time  -1) #x1f) 2))))
    (encode-universal-time sec min hour date mon year)))

(defun read-entry-object (s)
  (let* ((header (make-directory-entry s))
	 (name (make-array (cd/name-length header)
                           :element-type '(unsigned-byte 8)))
	 (comment
	  (when (plusp (cd/comment-length header))
	    (make-array (cd/comment-length header)
			:element-type '(unsigned-byte 8))))
         (utf8p (or (logtest (cd/flags header) 2048) *force-utf-8*)))
    (assert (= (cd/signature header) #x02014b50))
    (read-sequence name s)
    (setf name (decode-name name utf8p))
    (file-position s (+ (file-position s) (cd/extra-length header)))
    (when comment
      (read-sequence comment s)
      (setf comment (decode-name comment utf8p)))
    (make-zipfile-entry :name name
			:stream s
			:offset (cd/offset header)
			:size (cd/size header)
			:compressed-size (cd/compressed-size header)
			:comment comment
			:method (compression-method-name (cd/method header))
                        :date (cd/date header)
                        :universal-time (dos-to-universal-time
					 (cd/date header) (cd/time header))
			:crc (cd/crc header)
			:made-by (logand (ash (cd/version-made-by header) -8)
					 #xff)
			:made-by-version
			(made-by-version (cd/version-made-by header))
                        :mode (ash (cd/external-attributes header) -16))))

(defun open-zipfile (pathname &key force-utf-8)
  "Return a ZIPFILE structure, representing the contents of PATHNAME.
You should probably eventualy call CLOSE-ZIPFILE on the results, but maybe on
some implementations, like SBCL, it will be done for you on garbage collection.
If FORCE-UTF-8 is true, force using the UTF-8 encoding, instead of whatever
else might be the default, probably LATIN-1."
  (with-latin1 ()
    (let* ((*force-utf-8* force-utf-8)
	   (s (open pathname :element-type '(unsigned-byte 8))))
      (unwind-protect
	   (progn
	     (seek-to-end-header s)
	     (let* ((end (make-end-header s))
		    (n (end/total-files end))
		    (entries (make-hash-table :test #'equal))
		    (zipfile (make-zipfile :stream s
					   :entries entries)))
	       (file-position s (end/central-directory-offset end))
	       (dotimes (x n)
		 (let ((entry (read-entry-object s)))
		   (setf (gethash (zipfile-entry-name entry) entries) entry)))
	       #+sbcl (let ((s s)) (sb-ext:finalize zipfile (lambda ()(close s))))
	       (setf s nil)
	       zipfile))
	(when s
	  (close s))))))

(defun close-zipfile (zipfile)
  "Close the file handle."
  (close (zipfile-stream zipfile)))

(defun get-zipfile-entry (name zipfile)
  "Return an entry handle for the file called NAME."
  (gethash name (zipfile-entries zipfile)))

(defgeneric write-zipentry (zip-writer name data
			    &key file-write-date deflate file-mode)
  ;;(:documentation "Write DATA to NAME and blah blah blah")
  )

(defun zipfile-entry-contents (entry &optional stream)
  "If stream is given, extract entry to the (unsigned-byte 8) stream given as
the argument. Otherwise, return the entry contents as an
(unsigned-byte 8) vector."
  (if (pathnamep stream)
      (with-open-file (s stream
			 :direction :output
			 :if-exists :supersede
                         :element-type '(unsigned-byte 8))
	(%zipfile-entry-contents entry s))
      (%zipfile-entry-contents entry stream)))

(defmacro with-zipfile ((file pathname &key force-utf-8) &body body)
  "Evaluate BODY with FILE bound to the result of OPEN-ZIPFILE on PATHNAME.
Passes FORCE-UTF-8 to OPEN-ZIPFILE."
  `(let ((,file (open-zipfile ,pathname :force-utf-8 ,force-utf-8)))
     (unwind-protect
	 (progn ,@body)
       (close-zipfile ,file))))

(defmacro with-zipfile-stream ((file stream &key force-utf-8) &body body)
  "Evaluate BODY with FILE bound to the result of OPEN-ZIPFILE-FROM-STREAM on
STREAM. Passes FORCE-UTF-8 to OPEN-ZIPFILE-FROM-STREAM."
  `(let ((,file (open-zipfile-from-stream ,stream :force-utf-8 ,force-utf-8)))
     ,@body))

(defmacro do-zipfile-entries ((name entry zipfile) &body body)
  "Map over all entries in zipfile binding NAME and ENTRY to each
file name and entry handle in turn. Establish implicit block named NIL
around the loop."
  (setf name (or name (gensym)))
  (setf entry (or entry (gensym)))
  `(block nil
     (maphash (lambda (,name ,entry)
		(declare (ignorable ,name ,entry))
		,@body)
	      (zipfile-entries ,zipfile))))

(defun unzip (pathname target-directory &key (if-exists :error) verbose
					  force-utf-8)
  "Extract all entries from the zip archive at PATHNAME into TARGET-DIRECTORY.
IF-EXISTS as for in CL:OPEN."
  ;; <Xof> "When reading[1] the value of any pathname component, conforming
  ;;       programs should be prepared for the value to be :unspecific."
  (when (set-difference (list (pathname-name target-directory)
                              (pathname-type target-directory))
                        '(nil :unspecific))
    (error "Pathname not a directory. Perhaps it lacks a trailing slash?"))
  (with-zipfile (zip pathname :force-utf-8 force-utf-8)
    (do-zipfile-entries (name entry zip)
      (let* (#+nil (name (ppcre:regex-replace-all "[/*?]" name "_"))
             #+nil (name (subseq name 0 (min (length name) 128)))
             (filename (merge-pathnames name target-directory)))
        (ensure-directories-exist filename)
        (unless (char= (elt name (1- (length name))) #\/)
          (ecase verbose
            ((nil))
            ((t) (write-string name) (terpri))
            (:dots (write-char #\.)))
          (force-output)
          (with-open-file
              (s filename :direction :output :if-exists if-exists
               :element-type '(unsigned-byte 8))
            (zipfile-entry-contents entry s)))))))

;; package.lisp

(export '(zipfile-entry
	  zipfile-entry-compressed-size
	  zipfile-entry-method
	  zipfile-entry-date
	  zipfile-entry-universal-time
	  zipfile-entry-crc
	  zipfile-entry-made-by
	  zipfile-entry-made-by-version
	  zipfile-entry-mode
	  made-by-name)
	:zip)

(in-package :cl-user)

;; End of ZIP patches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :unzip
  (:documentation "Manipulate zip files.")
  (:use :cl :dlib :opsys :dlib-misc :zip :mkdir :table :grout)
  (:export
   #:unzip-command
   #:zip-command
   ))
(in-package :unzip)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
(defun localize-path (path)
  "Convert a ZIP path into a local operating system path."
  #+unix path
  #+windows (join-by-string (split-sequence #\/ path)
			    nos:*directory-separator*))

(defun localized-name (entry)
  "Return the localized path name from a ZIPFILE-ENTRY."
  (localize-path (zipfile-entry-name entry)))

(defvar *confirm-all* nil
  ":all :none or NIL if it hasn't been set.")

(defun confirm-overwrite (entry)
  "Return a file name to overwrite or NIL not overwrite."
  (case *confirm-all*
    (:all  (return-from confirm-overwrite (localized-name entry)))
    (:none (return-from confirm-overwrite nil)))

  (loop :with done :and answer :and result
     :and name = (localized-name entry)
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
	       (date zip::universal-time)) entry
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
	(let ((time (make-os-time
		     :seconds (zipfile-entry-universal-time entry))))
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
       (setf out (localized-name entry)))
      ((file-exists (localized-name entry))
       (if (and (not never-overwrite)
		(setf answer (confirm-overwrite entry)))
	   (setf out answer)
	   (return-from extract-entry nil)))
      (t ;; the file doesn't exist
       (setf out (localized-name entry))))
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
	(when (or (not (localized-name entry))
		  (and (localized-name entry)
		       (not (zerop (length (path-file-name
					    (localized-name entry)))))))
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
		 (date zip::universal-time)
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
	(grout-print-table
	 (setf results
	       (make-table-from
		results
		;; :column-names
		;; (if verbose
		;;     '("Size" "Compressed" "%" "Method" "CRC"
		;;       "Made by" "Mode" "   Date    Time" "Name")
		;;     '("Size" "   Date    Time" "Name"))))
		:columns
		(if verbose
		    '((:name "Size"       :type number)
		      (:name "Compressed" :type number)
		      (:name "%"          :type number)
		      (:name "Method")
		      (:name "CRC")
		      (:name "Made by")
		      (:name "Mode")
		      (:name "   Date    Time")
		      (:name "Name"))
		    '((:name "Size" :type number)
		      (:name "   Date    Time")
		      (:name "Name")
		      ))))
	 :max-width nil)
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
  (setf lish:*output*
	(apply #'unzip-command `(,@keys :extract ,(if list-p nil t)))))

;; EOF
