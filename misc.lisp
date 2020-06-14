;;;
;;; misc.lisp - Miscellaneous small commands.
;;;

(defpackage :misc
  (:documentation "Miscellaneous small commands.")
  (:use :cl :dlib :lish :opsys :terminal :collections)
  (:export
   #:!basename
   #:!dirname
   #:!tty
   #:!uname
   ))
(in-package :misc)

;; (defcommand za ((a string :rest t))
;;   (apply #'!= "/usr/bin/basename" a) (apply #'!= "basename" a))

;; @@@ some of the differences here are things in path-file-name that I think
;; I probably should fix.
(defcommand basename
  ((all boolean :short-arg #\a :long-arg "multiple"
    :help "All arguments are filenames.")
   (suffix string :short-arg #\s :help "Suffix to remove.")
   (collect boolean :short-arg #\c :help "Collect output as a sequence.")
   (files pathname :help "Path(s) to get the base name of." :rest t))
  :accepts (or sequence string pathname)
  "Print the file name without the directory name."
  (cond
    (suffix
     (setf all t))
    ((and (not suffix) (not all) (olength-at-least-p 2 files))
     (setf suffix (oelt files 1))))
  (when (and (not files) *input*)
    (typecase *input*
      ((or string pathname)
       (setf files (list *input*)))
      (sequence
       (setf files *input*))))
  (let (results)
    (labels ((snip (f)
	       (if suffix (remove-suffix f suffix) f))
	     (action (f)
	       (if collect
		   (push (path-file-name (snip f)) results)
		   (format t "~a~%" (path-file-name (snip f))))))
      (cond
	(all
	 (omapn (_ (action _)) files))
	(t
	 (action (first files)))))
    (when collect
      (setf *output* (nreverse results)))))

(defcommand dirname
  ((collect boolean :short-arg #\c :help "Collect output as a sequence.")
   (files pathname :help "Path(s) to get the directory name of." :rest t))
  :accepts (or sequence string pathname)
  "Print the directory name of a file."
  (when (and (not files) *input*)
    (typecase *input*
      ((or string pathname)
       (setf files (list *input*)))
      (sequence
       (setf files *input*))))
  (let (results)
    (flet ((action (f)
	     (if collect
		 (push (path-directory-name f) results)
		 (format t "~a~%" (path-directory-name f)))))
      (omapn (_ (action _)) files))
    (when collect
      (setf *output* (nreverse results)))))

(defcommand tty
  ((lisp boolean :short-arg #\l :help "Use Lisp *standard-input*.")
   (type boolean :short-arg #\t :help "Print the name and type of *terminal*."))
  "Print the file name of the terminal on standard input."
  (let (fd name)
    (if type
	(if *terminal*
	    (progn
	      (let ((real-term
		     (or (terminal-wrapped-terminal *terminal*) *terminal*)))
		(format t "*terminal* is a ~a on ~a.~%"
			(type-of *terminal*)
			(setf name (terminal-device-name real-term)))
		(setf fd (terminal-file-descriptor real-term))))
	    (progn
	      (setf name "unknown")
	      (format t "*terminal* is not set.~%")))
	(progn
	  (when (not fd)
	    (setf fd (or (and lisp (stream-system-handle *standard-input*))
			 0)))
	  (setf name
		#+unix (if (zerop (uos:isatty fd)) "not a tty" (uos:ttyname fd))
		#-unix "unknown")
	  (format t "~a~%" name)
	  (setf *output* name)))))

(defcommand uname
  ((system boolean :short-arg #\s :help "Print the operating system name.")
   (network boolean :short-arg #\n :help "Print the network name.")
   (release boolean :short-arg #\r :help "Print the kernel release.")
   (version boolean :short-arg #\v :help "Print the kernel version.")
   (machine boolean :short-arg #\m :help "Print the machine name.")
   (all boolean :short-arg #\a :help "Print all the infomation."))
  "Print system type information."
  (declare (ignorable system network version release machine))
  (when (not (or system network version release machine))
    (setf system t))
  (let ((first t))
    (macrolet ((foo (name func)
		 `(progn
		    (when (or ,name all)
		      (if first
			  (setf first nil)
			  (write-char #\space))
		      (princ (,func))))))
      (foo system  os-software-type)
      (foo network os-machine-instance)
      (foo release os-software-release)
      (foo version os-software-version)
      (foo machine os-machine-type)))
  (terpri)
  (values))

;; EOF
