;;;
;;; dirname.lisp - Print the directory name of a file.
;;;

(defpackage :dirname
  (:documentation "Print the directory name of a file.")
  (:use :cl :dlib :opsys :collections :lish)
  (:export
   #:dirname
   ))
(in-package :dirname)

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

;; End
