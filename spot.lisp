;;
;; spot.lisp - Locations of objects.
;;

(defpackage :spot
  (:documentation "Generic location of objects.")
  (:use :cl)
  (:export
   #:spot
   #:object-at
   #:spot-range
   #:spot-range-start
   #:spot-range-end
   #:make-spot-range
   #:range-sequence
   ))
(in-package :spot)

(defclass spot ()
  ()
  (:documentation "A generic location for objects in inators."))

(defgeneric object-at (spot)
  (:documentation "Return the object at the location."))

(defgeneric (setf object-at) (value spot)
  (:documentation "Set the object at the location."))

(defclass spot-range ()
  ((start
    :initarg :start :accessor spot-range-start
    :documentation "Starting position of the range.")
   (end
    :initarg :end :accessor spot-range-end
    :documentation "Ending position of the range."))
  (:documentation "A range of objects in an inator delimited by spots."))

(defgeneric make-spot-range (start end)
  (:documentation "Return a spot-range from START to END."))

(defgeneric range-objects (spot-range)
  (:documentation "Return a collection of objects in the range."))

(defgeneric range-objects* (start end)
  (:documentation "Return a collection of objects in the range that would be
made by START and END."))

;; End
