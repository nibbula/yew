;;
;; color-stripe.lisp - Color stripes
;;

(defpackage :color-stripe
  (:documentation "Color stripes")
  (:use :cl :md5)
  (:export
   #:color-stripe
   ))
(in-package :color-stripe)

(defun color-stripe (thing)
  (if thing
      (let ((sum (etypecase thing
		   (string   (md5sum-string   thing))
		   (pathname (md5sum-file     thing))
		   (stream   (md5sum-stream   thing))
		   (sequence (md5sum-sequence thing)))))
	(with-output-to-string (str)
	  (loop :for i :from 0 :below (length sum) :by 3
	     :do
	     (format str "~c[48;2;~a;~a;~am " #\escape
		     (aref sum i) (aref sum (+ i 1)) (aref sum (+ i 2))))
	  (format str "~c[0m" #\escape)))
      "     "))

;; EOF
