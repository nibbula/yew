;;;
;;; collections-l2.lisp - Collections Level 2
;;;

(defpackage :collections-l2
  (:documentation "Collections Level 2

This is for more complicated or less frequently used collection functions, and
for things with functionality not in the standard.
")
  (:use :cl :collections :dlib :dlib-misc)
  (:export
   #:orandomize
   #:obegins-with
   #:oends-with
   #:oremove-prefix
   #:oremove-suffix
   ))
(in-package :collections-l2)

;; @@@ Shouldn't osplit be in here?

(defgeneric orandomize (thing)
  (:documentation "Randomly rearrange a collection in place. Returns the
modfied collection.")
  (:method ((collection list))
    (replace collection (randomize-vector (coerce collection 'vector))))
  (:method ((collection vector))
    (randomize-vector collection))
  (:method ((collection hash-table))
    (let* ((n (hash-table-count collection))
	   (keys (let ((r (make-array n))
		       (i 0))
		   (maphash
		    (lambda (k v)
		      (declare (ignore v))
		      (setf (aref r i) k)
		      (incf i))
		    collection)
		   r)))
      (loop :for i :below n
        :do (rotatef (gethash (aref keys (random n)) collection)
		     (gethash (aref keys (random n)) collection)))
      collection))
  ;; Heh.
  ;; (:method ((collection structure-object)))
  ;; (:method ((collection standard-object)))
  )

(defmethod orandomize ((collection container))
  (orandomize (container-data collection)))

(defgeneric obegins-with (prefix thing &key test)
  (:documentation "True if ‘thing’ begins with ‘prefix’.")
  (:method (prefix thing &key test)
    (let ((pos (if test
                   (osearch prefix thing :test test)
                   (osearch prefix thing))))
      (and pos (zerop pos)))))

(defgeneric oends-with (suffix thing &key test)
  (:documentation "True if ‘thing’ ends with ‘suffix’.")
  (:method (suffix thing &key test)
    (let ((pos (if test
                   (mismatch suffix thing :test test :from-end t)
                   (mismatch suffix thing :from-end t))))
      (and pos (zerop pos)))))

(defgeneric oremove-prefix (thing prefix &key test)
  (:documentation
  "Remove ‘prefix’ from ‘thing’. ‘thing’ and ‘prefix’ are both collections.
If ‘thing’ is is not prefixed by ‘prefix’, just return ‘thing’.")
  (:method (thing prefix &key test)
    (if (if test
            (obegins-with prefix thing :test test)
            (obegins-with prefix thing))
        (osubseq thing (length prefix))
        (ocopy thing))))

(defgeneric oremove-suffix (thing suffix &key test)
  (:documentation
   "Remove ‘suffix’ from the end of ‘thing’. If ‘thing’ doesn't end in ‘suffix’,
just return ‘thing’. Elements are compared with ‘test’.")
  (:method (thing suffix &key test)
    (let ((pos (if test
                   (osearch suffix thing :from-end t :test test)
                   (osearch suffix thing :from-end t))))
      (if (and pos (= pos (- (olength thing) (olength suffix))))
	  (osubseq thing 0 pos)
	  (ocopy thing)))))

;; End
