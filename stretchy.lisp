;;
;; stretchy.lisp - Adjustable arrays and strings
;;

(defpackage :stretchy
  (:documentation
   "Functions for manipulating stretchy arrays and strings.

Common Lisp provides adjustable arrays, and therefore also adjustable strings,
but the mechanics of the programming interface is somewhat clumsy. This is an
attempt to make it better.

Perhaps this is misguided and we should just use WITH-OUTPUT-TO-STRING since
it seems much faster on most implementations.
")
  (:use :cl)
  (:export
   #:*default-stretch-factor*
   #:stretchy-string
   #:make-stretchy-string
   #:stretchy-vector
   #:make-stretchy-vector
   #:stretchy-append
   #:stretchy-set
   #:stretchy-truncate
   ))
(in-package :stretchy)

;; (declaim (optimize (speed 0) (safety 3) (debug 3)
;; 		   (space 0) (compilation-speed 0)))
(declaim (optimize (speed 3) (safety 0) (debug 3)
		   (space 0) (compilation-speed 0)))

(defvar *default-stretch-factor* 2/3
  "Default expansion factor for stretchy things.")

(deftype stretchy-string ()
  "An adjustable string of characters"
  '(and
    (vector character)
    (satisfies adjustable-array-p)
    (satisfies array-has-fill-pointer-p)))

(defun make-stretchy-string (n)
  "Make a stretchy string of size N. A stretchy string is an adjustable array
of characters with a fill pointer."
  (make-array n :element-type 'character :fill-pointer 0 :adjustable t))

(deftype stretchy-vector ()
  "An adjustable vector of objects."
  '(and
    (vector)
    (satisfies adjustable-array-p)
    (satisfies array-has-fill-pointer-p)))

;; I don't know if this is useful, since it's probably just as easy to call
;; make-array, which could be faster and supports initializers.
(defun make-stretchy-vector (n &key (element-type t))
  "Make a stretchy vector of size N. A stretchy vector is an adjustable array
of objects with a fill pointer."
  (make-array n :fill-pointer 0 :adjustable t :element-type element-type))

(defmacro %make-stretchy-vector (n &rest args
				   &key (element-type t)
				        (initial-element 0)
				        (inital-contents nil))
  "Make a stretchy vector of size N. A stretchy vector is an adjustable array
of with a fill pointer."
  (declare (ignorable element-type initial-element inital-contents))
  (setf (getf args :adjustable) t)
  `(make-array ,n ,@args))

;; (defun resize (array size factor)
;;   "Resize the ARRAY to SIZE, expanding by FACTOR."
;;   (declare (type (array * (*)) array)
;; 	   (type fixnum size)
;; 	   (type number factor))
;;   (setf array (adjust-array
;; 	       array (+ (array-total-size array) size
;; 			(truncate (* (array-total-size array) factor))))))

(defmacro resize (array size factor)
  "Expand the ARRAY at least by SIZE, expanding by FACTOR."
  #-ecl (declare (type (array * (*)) array)
		 (type fixnum size)
		 (type number factor))
  (let ((a (gensym "resize-a"))
	(new-size (gensym "resize-new-size")))
    `(let* ((,a ,array)
	    (,new-size (+ (array-total-size ,a) ,size
			  (truncate (* (array-total-size ,a) ,factor)))))
       (setf ,array (adjust-array ,a ,new-size)))))

(defun stretchy-append-string-or-vector (to from factor)
  "Append a vector to a stretchy vector. FACTOR is the amount of the total
size to expand by when expansion is needed."
  (declare (type (array * (*)) to from))
  (let ((from-len (length from))
	(to-len (length to)))
    (when (>= (+ from-len to-len) (array-total-size to))
      (resize to from-len factor))
    (incf (fill-pointer to) from-len)
    (setf (subseq to to-len (+ to-len from-len)) from)))

(defun stretchy-append-character (to char factor)
  "Append the character CHAR to the stretchy string TO, with FACTOR as the
amount of the total size to expand by when expansion is needed."
  #-ecl (declare (type (array character (*)) to)
		 (type character char)
		 (type number factor))
  (let ((to-len (length to)))
    (when (>= (1+ to-len) (array-total-size to))
      (resize to 1 factor))
    (incf (fill-pointer to))
    (setf (char to to-len) char)))

(defun stretchy-append-object (to object factor)
  "Append the OBJECT to the stretchy string TO, with FACTOR as the
amount of the total size to expand by when expansion is needed."
  (declare (type (array * (*)) to)
	   (type number factor))
  ;;(format *debug-io* "append object ~s ~s~%" (type-of object) object)
  (let ((to-len (length to)))
    (when (>= (1+ to-len) (array-total-size to))
      (resize to 1 factor))
    (incf (fill-pointer to))
    (setf (aref to to-len) object)))

(defun stretchy-set (vec n value &key (factor *default-stretch-factor*))
  "Put an element in a stretchy vector. Factor is the amount of the total size
to expand by when expansion is needed."
  (declare (type (array * (*)) vec)
	   (type fixnum n)
	   (type number factor))
  ;; (when (>= n (array-total-size vec))
  (when (> n (1- (array-total-size vec)))
    (resize vec (- (1+ n) (array-total-size vec)) factor))
  (when (< (fill-pointer vec) (1+ n))
    (setf (fill-pointer vec) (1+ n))) ;; should this really be done?
  (setf (aref vec n) value))

#|

;;; %%% Common Lisp fail?
;;; I'd like to make methods on a deftype but classes and types are dismorphic.
;;; Is there some way to unify them while retaining the powers, and even most of
;;; the efficiency, of both types and classes?
;;;
;;; In this case there are system classes, and a string *is* a vector, but
;;; maybe I'd like to be able to define a non-string-vector. I suppose in
;;; Dylan this could be a limited type? But I think in Dylan, limited types
;;; are not necessarily extensible? You can define a vector of a limited type,
;;; but perhaps not of "not some type". Also, how do limited types work with
;;; unboxing?

(defgeneric stretchy-append (to from &key factor)
  (:documentation "Append FROM to TO. Expand by FACTOR if necessary."))

(defmethod stretchy-append ((to stretchy-string) (from string) &key factor)
  (stretchy-append-string-or-vector to from :factor factor))

(defmethod stretchy-append ((to stretchy-string) (from stretchy-string)
			    &key factor)
  (stretchy-append-string-or-vector to from :factor factor))

(defmethod stretchy-append ((to stretchy-vector) (from vector) &key factor)
  (stretchy-append-string-or-vector to from :factor factor))

(defmethod stretchy-append ((to stretchy-vector) (from stretchy-vector)
			    &key factor)
  (stretchy-append-string-or-vector to from :factor factor))

;; @@@ Just punting and taking the easy way for now for the next two methods.
;; In the future, make these faster specific methods.
(defmethod stretchy-append ((to stretchy-string) (from character)
			    &key factor)
  (stretchy-append-string-or-vector to (string from) :factor factor))

(defmethod stretchy-append ((to stretchy-vector) from &key factor)
  "Append a single object (of any type) to a stretchy vector."
  (stretchy-append-string-or-vector to (vector from) :factor factor))

|#

(defun stretchy-append (to from &key (factor *default-stretch-factor*))
  "Append FROM to stretchy thing TO. FROM can be a character or a string or a
symbol if TO is a string, or anything if TO is a vector. FACTOR is the
amount of the total size to expand by when expansion is needed."
  (declare (type (array * (*)) to)
	   (type number factor))
  (when (not (or (typep to 'stretchy-string) (typep to 'stretchy-vector)))
    (error "Destination must be a stretchy-string or stretchy-vector"))
  (cond
    ;; @@@ Just punting and taking the easy way for now.
    ;; In the future, make these faster specific functions.
    ((or (typep from 'stretchy-string)
	 (typep from 'stretchy-vector)
	 (typep from 'string)
	 (typep from 'vector))
     (stretchy-append-string-or-vector to from factor))
    ((and (typep to 'stretchy-string)
	  (typep from 'character))
     (stretchy-append-character to from factor))
    ((and (typep to 'stretchy-string)
	  (typep from 'symbol))
     (stretchy-append-string-or-vector to (string from) factor))
    ((and (not (typep to 'stretchy-string))
	  (typep from 'vector))
     (stretchy-append-string-or-vector to from factor))
    ((not (typep to 'stretchy-string))
     (stretchy-append-object to from factor))
    (t
     (error "Can't append a ~(~a~) to a stretchy-string." (type-of from)))))

;; Might also be called shrink, but shrink implies taking up less space, which
;; it doesn't really.
(defun stretchy-truncate (s &optional (len 0))
  "Truncate a stretchy to length LEN."
  (setf (fill-pointer s) len))

;;;; TESTS!!

;; Compare performance vs. with-output-to-string

#|
(defun dork1 (n)
  (let ((ss (make-stretchy-string 100)))
    (loop :for i fixnum :from 1 :to n :do
       (stretchy:stretchy-append ss (princ-to-string i)))))

(defun dork2 (n)
  (with-output-to-string (str)
    (loop :for i fixnum :from 1 :to n :do (princ i str))))

(defun test-performance ()
  (format t "STRETCHY:~%")
  (time (dotimes (n 1000) (dork1 1000)))
  (time (dotimes (n 1000) (dork1 1000)))
  (time (dotimes (n 1000) (dork1 1000)))
  (format t "WITH-OUTPUT-TO_STRING:~%")
  (time (dotimes (n 1000) (dork2 1000)))
  (time (dotimes (n 1000) (dork2 1000)))
  (time (dotimes (n 1000) (dork2 1000))))
|#

;; EOF
