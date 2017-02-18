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

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(defun color-stripe (thing)
  "Make a color strip from the MD5 hash of THING."
  (if thing
      (let* ((sum (etypecase thing
		    (string   (md5sum-string   thing))
		    (pathname (md5sum-file     thing))
		    (stream   (md5sum-stream   thing))
		    (sequence (md5sum-sequence thing))))
	     (count (truncate (length sum) 3)))
	(with-output-to-string (str)
	  ;; Unfortunately an MD5 checksum is one byte short of a full 6.
	  (loop :for i :from 0 :below (* count 3) :by 3
	     :do
	     (format str "~c[48;2;~a;~a;~am " #\escape
		     (aref sum i) (aref sum (+ i 1)) (aref sum (+ i 2))))
	  (format str "~c[0m" #\escape)))
      "     "))

;; EOF
