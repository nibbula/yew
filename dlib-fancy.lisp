;;;
;;; dlib-fancy.lisp - Dan's not so fancy junk.
;;;

;;; This is for things that have more dependencies or are otherwise quirky.

;; $Revision: 1.3 $

(defpackage :dlib-fancy
  (:documentation "Dan's not so fancy junk.")
  (:use :cl :dlib :pager :filter-stream)
  (:export
   #:check-inc
   #:check-def
   #:m
   #:mm
   #:title
   ))
(in-package :dlib-fancy)

(defun check-inc (&rest includes)
  "See the actual results of C include files."
  (with-output-to-string (str)
    (loop :for i :in includes :do (format str "#include <~a>~%" i))
    (pager (system-command-stream
	    "bash" `("-c"
		     ,(format nil "echo '~a' | cc -E - | egrep -v '(^#|^$)'"
			      (get-output-stream-string str))))))
  (values))

(defun check-def (def &rest includes)
  "See what some macro is defined as in a C include file."
  (with-output-to-string (str)
    (loop :for i :in includes :do (format str "#include <~a>~%" i))
    (let ((tag (format nil "Jinky~dJinky" (random #xffff))))
      (format str "~a~%\"~a\" = ~a~%" tag def def)
      (pager (system-command-stream
	      "bash" `("-c"
		       ,(format nil
				"echo '~a' | cc -E - | ~
sed -En -e '/~a/,$p' -e 's/~a//g' | egrep -v '(^#|^$)'"
				(get-output-stream-string str) tag tag))))))
  (values))

;; Pager shorthand
(defmacro m (&body body)
  (if (and (= 1 (length body)) (stringp (first body)))
      `(pager ,@body)
      `(with-pager ,@body)))

(defmacro mm (&body body)
  (if (and (= 1 (length body)) (stringp (first body)))
      `(pager ,@body)
      `(with-pager* ,@body)))

(defun title (&optional string)
  "Set the title of a terminal window. The terminal is assumed to work like XTerm or something."
  (format t "~c]0;~a~c" #\escape
	  (or string (progn (princ "Title? ") (finish-output) (read-line)))
	  #\^G))

;; grep -v :
;;
;; (defun filter-lines (in)
;;   )

;; EOF
