;;
;; color-stripe.lisp - Color stripes
;;

(defpackage :color-stripe
  (:documentation "Color stripes")
  (:use :cl :md5)
  (:export
   #:color-stripe-from-hash
   #:color-stripe
   ))
(in-package :color-stripe)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(defun color-stripe-from-hash (hash)
  (let ((byte-count (truncate (length hash) 3)))
    (with-output-to-string (str)
      ;; Unfortunately an MD5 checksum is one byte short of a full 6.
      (loop :for i :from 0 :below (* byte-count 3) :by 3
	 :do
	   (format str "~c[48;2;~a;~a;~am " #\escape
		   (aref hash i) (aref hash (+ i 1)) (aref hash (+ i 2))))
      (format str "~c[0m" #\escape))))

(defun color-stripe (thing)
  "Return a color strip from the MD5 hash of THING."
  (if thing
      (let* ((sum (etypecase thing
		    (string   (md5sum-string   thing))
		    (pathname (md5sum-file     thing))
		    (stream   (md5sum-stream   thing))
		    (sequence (md5sum-sequence thing)))))
	(color-stripe-from-hash sum))
      "     "))

;; EOF
