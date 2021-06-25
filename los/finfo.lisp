;;;
;;; finfo.lisp - Print some information about a file
;;;

(defpackage :finfo
  (:documentation "Print some information about a file.")
  (:use :cl :dlib :opsys :dlib-misc :dtime :grout #+unix :os-unix :los-config)
  (:export
   ;; Main entry point
   #:finfo
   #:!finfo
   ))
(in-package :finfo)

#+unix
(defparameter *mode-tags*
  '((is-fifo		   	"FIFO ")
    (is-character-device	"character special ")
    (is-directory		"directory ")
    (is-block-device		"block special ")
    (is-regular-file		"regular ")
    (is-symbolic-link		"symbolic link ")
    (is-socket			"socket ")
    (is-door			"door ")
    (is-whiteout		"whiteout ")
    (is-set-uid			"set-UID ")
    (is-sticky			"sticky "))
  "Sequence of test functions and strings for printing modes.")

#+unix
(defparameter *permission-tags*
  '((is-user-readable		#\r)
    (is-user-writable		#\w)
    (is-user-executable		#\x)
    (is-group-readable		#\r)
    (is-group-writable		#\w)
    (is-group-executable	#\x)
    (is-other-readable		#\r)
    (is-other-writable		#\w)
    (is-other-executable	#\x))
  "Sequence of test functions and strings for printing permission bits.")

(defparameter *output* nil "Dynamic output.")

(defun output (label format &rest args)
  "Save a line of the output."
  (setf *output* (append *output*
			 (list (cons label (apply #'format nil format args))))))

(defun output-finish (collect)
  "Output the table in two appropriately sized and colored columns."
  (let ((max-label-length (loop :for (label . nil) :in *output*
			     :maximize (length label)))
	results)
    (loop :for (label . data) :in *output* :do
       (grout-format "  ")
       (grout-color :green :default label)
       (grout-format ":~v,,,va" (max 1 (- (1+ max-label-length) (length label)))
		     #\space #\space)
       (grout-color :white :default data)
       (grout-format "~%")
       (when collect
	 (push (list label data) results)))
    results))

#+unix
(defun print-mode (mode)
  (output "Mode" "~o ~{~a~}~a~{~c~}" mode
	  (loop :for (func str) :in *mode-tags*
	     :when (apply func (list mode))
	     :collect str)
	  (if (is-set-gid mode)
	      (if (is-group-executable mode)
		  "set-GID "
		  "mandatory locking ")
	      "")
	  (loop :for (func chr) :in *permission-tags*
	     :collect (if (apply func (list mode)) chr #\-))))

#+unix
(defun fs-info (file info)
  (declare (ignore info))
  (let ((where (mount-point-of-file file)))
    (if where
	(s+ " " (car where) " " (cdr where))
	"")))

(defun format-time (time)
  (let ((ut (typecase time
	      #+unix
	      (os-unix:timespec
	       (os-unix:unix-to-universal-time
		(os-unix:timespec-seconds time)))
	      (os-time (os-time-seconds time)))))
    (format-date "~a-~2,'0d-~2,'0d ~a ~2,'0d:~2,'0d:~2,'0d"
		 (:year :month :date :day-abbrev :hour :minute :second)
		 :time ut)))

#+unix
(defun unix-file-info (file &key follow-links collect)
  (with-grout ()
    (let ((info (if follow-links (stat file) (lstat file)))
	  (*output* nil))
      (with-slots ((device os-unix::device)
		   (inode os-unix::inode)
		   (mode os-unix::mode)
		   (links os-unix::links)
		   (uid os-unix::uid)
		   (gid os-unix::gid)
		   (device-type os-unix::device-type)
		   (access-time os-unix::access-time)
		   (modify-time os-unix::modify-time)
		   (change-time os-unix::change-time)
		   (birth-time os-unix::birth-time)
		   (size os-unix::size)
		   (blocks os-unix::blocks)
		   (block-size os-unix::block-size)
		   (flags os-unix::flags)
		   (generation os-unix::generation)) info
	(grout-color :cyan :default (format nil "~a:~%" file))
	(print-mode mode)
	(output "I-Node"      "~d" inode)
	(output "Device"      "0x~x~a~a" device
		(fs-info file info)
		(if (or (is-character-device mode) (is-block-device mode)
			(is-socket mode))
		    (format nil ", 0x~x" device-type)
		    ""))
	(output "Links"       "~d" links)
	(when (is-symbolic-link mode)
	  (output "Symbolic link to" "~a" (readlink file)))
	(output "UID"         "~d ~a" uid (let ((pw (getpwuid uid)))
				    (if pw (passwd-name pw) "")))
	(output "GID"         "~d ~a" gid (let ((grp (getgrgid gid)))
				    (if grp (group-entry-name grp))))
	(output "Size"        "~d" size)
	(output "Access time" "~a" (format-time access-time))
	(output "Mod time"    "~a" (format-time modify-time))
	(output "Change time" "~a" (format-time change-time))
	(when birth-time
	  (output "Birth time"  "~a" (format-time birth-time)))
	(output "Block size"  "~d" block-size)
	(output "Blocks"      "~d" blocks)
	(when flags
	  (output "Flags"     "~a" (os-unix:flags-string flags)))
	(output-finish collect)))))

#+windows
(defun ms-file-info (file &key follow-links collect)
  (declare (ignore file follow-links collect)))

(defun generic-file-info (file &key follow-links collect)
  (with-grout ()
    (let ((info (get-file-info file :follow-links follow-links))
	  (*output* nil))
      (with-accessors ((type file-info-type)
		       (size file-info-size)
		       (flags file-info-flags)
		       (creation-time file-info-creation-time)
		       (access-time file-info-access-time)
		       (modification-time file-info-modification-time)) info
	(grout-color :cyan :default (format nil "~a:~%" file))
	(output "Type"              "~a" type)
	(output "Size"              "~s" size)
	(output "Flags"             "~s" flags)
	(output "Creation time"     "~a" (format-time creation-time))
	(output "Access time"       "~a" (format-time access-time))
	(output "Modification time" "~a" (format-time modification-time))
	(output-finish collect)))))

(defparameter *default-style*
  #+unix :unix
  ;; #+windows :ms @@@
  #+windows :generic
  #-(or unix windows) :generic)

(defun file-info (file &key follow-links (style *default-style*) collect)
  (when (not style)
    (setf style *default-style*))
  (case style
    #+unix (:unix (unix-file-info file :follow-links follow-links
				  :collect collect))
    #+windows (:ms (ms-file-info file :follow-links follow-links
				 :collect collect))
    (:generic (generic-file-info file :follow-links follow-links
				 :collect collect))
    (otherwise
     (generic-file-info file :follow-links follow-links :collect collect))))

(defun finfo (file-or-files &key follow-links style collect)
  "Print some information about a file. Argument can be a string or path or a
list of strings or paths. If FOLLOW-LINKS is true, print information about the
linked file."
  (with-grout ()
    (flet ((call-it (f)
	     (file-info f :follow-links follow-links :style style
			:collect collect)))
      (typecase file-or-files
	(list
	 (let (results)
	   (loop :for file :in file-or-files
	      :do
		(with-simple-restart (continue "Continue with the next file.")
		  (if collect
		      (push (call-it file) results)
		      (call-it file))))
	   (if (= (length results) 1)
	       (car results)
	       results)))
	(pathname
	 (call-it (namestring file-or-files)))
	(string
	 (call-it file-or-files))))))

#+lish
(lish:defcommand finfo
  ((follow-links boolean :short-arg #\l
    :help "True to give information about the linked thing, not the link." )
   (style choice :short-arg #\s :default *default-style*
    :choices '("generic" "unix" "ms")
    :help "Operating system specific output style.")
   (collect boolean :short-arg #\c :help "True to collect results.")
   (files pathname :repeating t
    :help "The path names to give information about."))
  :accepts (string pathname sequence)
  :keys-as args
  "Print information about a file."
  (remf args :files)
  (when (not files)
    (if lish:*input*
	(setf files
	      (typecase lish:*input*
		((or string pathname) (list lish:*input*))
		(list lish:*input*)
		(sequence (map 'list #'identity lish:*input*))))
	(error "But what file do you want information about?")))
  (if collect
      (setf lish:*output* (apply #'finfo files args))
      (apply #'finfo files args)))

#|
  File: /dev/pts/1
  Size: 0               Blocks: 0          IO Block: 1024   character special file
Device: 18h/24d Inode: 4           Links: 1     Device type: 88,1
Access: (0620/crw--w----)  Uid: ( 1024/     dan)   Gid: (    5/     tty)
Access: 2021-04-17 03:08:11.374197577 -0700
Modify: 2021-04-17 03:38:16.374197577 -0700
Change: 2021-03-29 18:48:41.426197576 -0700
 Birth: -
|#

#+unix
(defun stat-format-time (time)
  (if time
      (let ((ut (typecase time
		  #+unix
		  (os-unix:timespec
		   (os-unix:unix-to-universal-time
		    (os-unix:timespec-seconds time)))
		  (os-time (os-time-seconds time)))))
	(with-output-to-string (str)
          (format-date "~a-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
		       (:year :month :date :hour :minute :second)
		       :time ut :stream str)
	  (format str ".~d" (uos:timespec-nanoseconds time))
	  (format-date " ~a" (:std-zone) :time ut :stream str)))
      "-"))

(defun print-stat (file &key follow-links)
  #+unix
  ;; Just a dumb imitation of the default Linix stat
  (let ((info (if follow-links (stat file) (lstat file))))
    (with-slots ((device os-unix::device)
		 (inode os-unix::inode)
		 (mode os-unix::mode)
		 (links os-unix::links)
		 (uid os-unix::uid)
		 (gid os-unix::gid)
		 (device-type os-unix::device-type)
		 (access-time os-unix::access-time)
		 (modify-time os-unix::modify-time)
		 (change-time os-unix::change-time)
		 (birth-time os-unix::birth-time)
		 (size os-unix::size)
		 (blocks os-unix::blocks)
		 (block-size os-unix::block-size)
		 (flags os-unix::flags)
		 (generation os-unix::generation)) info
      (format t "  File: ~a~%" file)
      (format t "  Size: ~15a Blocks: ~10a IO Block: ~6a ~{~a~}~a~%"
	      size blocks block-size
	      (loop :for (func str) :in *mode-tags*
		    :when (apply func (list mode))
		    :collect str)
	      (if (is-set-gid mode)
		  (if (is-group-executable mode)
		      "set-GID "
		    "mandatory locking ")
		""))
      (format t "Device: ~2xh/~2dd Inode: ~11a Links: ~5a Device type: ~a~%"
	      device device inode links device-type)
      (format t "Access: (~4o/~a)  Uid: (~5d/~8@a)   Gid: (~5d/~8@a)~%"
	      mode (symbolic-mode mode) uid (user-name uid)
	      gid (group-name gid))
      (format t "Access: ~a~%" (stat-format-time access-time))
      (format t "Modify: ~a~%" (stat-format-time modify-time))
      (format t "Change: ~a~%" (stat-format-time change-time))
      (format t " Birth: ~a~%" (stat-format-time birth-time))))
  #-unix
  ;; Just do finfo on non-unix.
  (apply #'finfo file :follow-links follow-links))

#+lish
(lish:defcommand stat
  ((follow-links boolean :short-arg #\L
    :help "True to give information about the linked thing, not the link." )
   (help boolean :short-arg #\?
    :help "Show the help.")
   (files pathname :repeating t
    :help "The path names to give information about."))
  :accepts (string pathname sequence)
  :keys-as args
  "Print information about a file."
  (remf args :files)
  (when (not files)
    (if lish:*input*
	(setf files
	      (typecase lish:*input*
		((or string pathname) (list lish:*input*))
		(list lish:*input*)
		(sequence (map 'list #'identity lish:*input*))))
	(error "But what file do you want information about?")))
  (mapc (_ (print-stat _ :follow-links follow-links)) files))

;; EOF
