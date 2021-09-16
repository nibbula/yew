;;;
;;; basename.lisp - Print the file name without the directory.
;;;

(defpackage :basename
  (:documentation "Print the file name without the directory.")
  (:use :cl :dlib :opsys :collections :lish)
  (:export
   #:!basename
   ))
(in-package :basename)

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
  (when (not files)
    (cond
      ((or (stringp *input*) (pathnamep *input*))
       (setf files (list *input*)))
      ((and (not (null *input*)) (mappable-p *input*))
       (setf files *input*))
      (t
       (error "An argument or *input* is required."))))
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

;; (defcommand za ((a string :rest t))
;;   (apply #'!= "/usr/bin/basename" a) (apply #'!= "basename" a))

;; @@@ Some of the differences here are things in path-file-name that I think
;; I probably should fix.

;; End
