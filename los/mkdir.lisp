;;;
;;; mkdir.lisp - Make directories and files.
;;;

(defpackage :mkdir
  (:documentation "Make directories and files.")
  (:use :cl :opsys)
  (:export
   #:mkdir
   #:!mkdir
   ))
(in-package :mkdir)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

;; @@@ Maybe we should use nos:ensure-directory, but I wrote this first.
;; And what about the verboseness, and errorp ... ?
(defun mkdir (&key directories mode (make-parents t) verbose (errorp t))
  (loop :for d :in directories :do
     (if (file-exists d)
	 (progn
	   (when errorp
	     (error "~a already exists." d)))
	 (progn
	   (loop :with prefix = ""
	      :for component :in (nos:split-path d)
	      :do
	      (setf prefix (path-append prefix component))
	      (when (not (file-exists prefix))
		(when verbose
		  (format t "Making directory ~s" prefix))
		(when (not make-parents)
		  (cerror "Create ~s?" "Parent directory ~s doen't exist."
			  prefix))
		(make-directory prefix :mode mode)
		(when verbose
		  (write-char #\.)
		  (terpri))))))))

;; EOF
