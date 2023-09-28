;;;
;;; touch.lisp - Touch a thing in the file system.
;;;

(defpackage :touch
  (:documentation "Touch a thing in the file system.")
  (:use :cl :opsys #+unix :opsys-unix :calendar :dtime)
  (:export
   #:touch
   #:!touch
   ))
(in-package :touch)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

;; We try to use Unix open so we can be more specific about file flags and
;; permissions.
(defun open-it (name)
  #+unix (syscall (posix-open
		   name
		   (logior +O_WRONLY+ +O_CREAT+ +O_NOCTTY+ +O_NONBLOCK+)
		   #o666))
  #-unix (open name :direction :output :if-does-not-exist :create))

(defun close-it (fd)
  #+unix (when (plusp fd) (posix-close fd))
  #-unix (close fd))

(defun convert-time (time name)
  "Convert a ‘time’ named ‘name’ to an os-time."
  (typecase time
    (dtime
     (make-os-time :seconds (dtime-seconds time)
		   :nanoseconds (dtime-nanoseconds time)))
    (integer ;; assume it's a universal-time
     (make-os-time :seconds time))
    ;; (string) @@@ add when we have better date parsing in dtime
    ((or os-time null) time)
    (t (error "Unknown type for ~(~a~) time: ~s." name
	      (type-of time)))))

(defun touch (pathname &key seconds nanoseconds time
			 modification-time access-time
			 reference-file)
  (macrolet ((convert (time-var)
	       `(setf ,time-var (convert-time ,time-var ',time-var))))
    (convert modification-time)
    (convert access-time)
    (convert time))
  (when (and reference-file
	     (file-exists reference-file))
    (let ((info (file-info reference-file)))
      (setf time (file-info-creation-time info)
	    modification-time (file-info-modification-time info)
	    access-time (file-info-access-time info))))
  (let ((path (safe-namestring pathname))
	fd)
    (unwind-protect
      (progn
	(setf fd (open-it path))
	(when fd
	  (cond
	    ((and modification-time access-time)
	     (set-file-time fd :modification-time modification-time
			       :access-time access-time))
	    (modification-time
	     (set-file-time fd :modification-time modification-time))
	    (access-time
	     (set-file-time fd :access-time access-time))
	    (time
	     (set-file-time fd :modification-time time
			       :access-time time))
	    ((or seconds nanoseconds)
	     (setf time (make-os-time :seconds (or seconds 0)
				      :nanoseconds (or nanoseconds 0)))
	     (set-file-time fd :modification-time time
			       :access-time time))
	    (t
	     (set-file-time fd)))))
      (when fd
	(close-it fd)))))

;; End
