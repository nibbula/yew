;;;
;;; dlib-fancy.lisp - Dan's not so fancy junk.
;;;

;;; This is for things that have more dependencies or are otherwise quirky.

;; $Revision: 1.4 $

(defpackage :dlib-fancy
  (:documentation "Dan's not so fancy junk.")
  (:use :cl :dlib :pager :filter-stream :lish)
  (:export
   #:check-inc #:!check-inc
   #:check-def #:!check-def
   #:!snip
   #:m
   #:mm
   ))
(in-package :dlib-fancy)

#+lish
(lish:defcommand check-inc (("includes" string :repeating t))
  "See the actual results of C include files."
  (let ((incs
	 (with-output-to-string (str)
	   (loop :for i :in includes :do (format str "#include <~a>~%" i)))))
    (! "echo \"" incs "\" | cc -E - | grep -v \"(^#|^$)\" | pager")))

#+lish
(lish:defcommand snip
  (("pattern"	regexp :optional nil)
   ("source"	filename :default *standard-input*)
   ("before"	boolean :short-arg #\b :long-arg "before":default nil)
   ("after"	boolean :short-arg #\a :long-arg "after" :default nil))
  "Snip output before or after a pattern."
  (when (not (or before after))
    (error "You probably should specify either BEFORE or AFTER."))
  (let ((saw-it nil))
    (cond
      (before
       (with-lines (line source)
	 (if saw-it
	     (progn (write-string line) (terpri))
	     (when (ppcre:all-matches pattern line)
	       (setf saw-it t)))))
      (after
       (with-lines (line source)
	 (progn (write-string line) (terpri))
	 (when (ppcre:all-matches pattern line)
	   (finish-output)
	   (return)))))
    (finish-output)))

#+lish
(lish:defcommand check-def
    (("def" string :optional nil)
     ("includes" string :repeating t))
  "See what some macro is defined as in a C include file."
  (let* ((tag (format nil "Jinky~dJinky" (random #xffff)))
	 (doc
	  (with-output-to-string (str)
	    (loop :for i :in includes :do
	       (format str "#include <~a>~%" i))
	    (format str "~a~%\\\"~a\\\" = ~a~%" tag def def))))
    (! "echo \"" doc "\" | cc -E - | snip -b " tag)))

;; Pager shorthand
(defmacro m (&body body)
  (if (and (= 1 (length body)) (stringp (first body)))
      `(pager ,@body)
      `(with-pager ,@body)))

(defmacro mm (&body body)
  (if (and (= 1 (length body)) (stringp (first body)))
      `(pager ,@body)
      `(with-pager* ,@body)))

;; EOF
