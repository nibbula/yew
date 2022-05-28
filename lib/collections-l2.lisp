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
   ))
(in-package :collections-l2)

;; @@@ Should osplit be in here?

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

;; End
