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

(defun make-stretchy-vector (n &key (element-type t))
  "Make a stretchy vector of size N. A stretchy vector is an adjustable array
of objects with a fill pointer."
  (make-array n :fill-pointer 0 :adjustable t :element-type element-type))

(defun resize (array size factor)
  "Resize the ARRAY to SIZE, expanding by FACTOR."
  (declare (type (array * (*)) array)
	   (type integer size)
	   (type number factor))
  (setf array (adjust-array
	       array (+ (array-total-size array) size
			(truncate (* (array-total-size array) factor))))))

(defun stretchy-append-string-or-vector (dst src factor)
  "Append a vector to a stretchy vector. FACTOR is the amount of the total
size to expand by when expansion is needed."
  (declare (type (array * (*)) dst src))
  (let ((src-len (length src))
	(dst-len (length dst)))
    (when (>= (+ src-len dst-len) (array-total-size dst))
      (resize dst src-len factor))
    (incf (fill-pointer dst) src-len)
    (setf (subseq dst dst-len (+ dst-len src-len)) src)))

(defun stretchy-append-character (dst char factor)
  "Append the character CHAR to the stretchy string DST, with FACTOR as the
amount of the total size to expand by when expansion is needed."
  (declare (type (array character (*)) dst)
	   (type character char)
	   (type number factor))
  (let ((dst-len (length dst)))
    (when (>= (1+ dst-len) (array-total-size dst))
      (resize dst 1 factor))
    (incf (fill-pointer dst))
    (setf (char dst dst-len) char)))

(defun stretchy-set (vec n value &key (factor *default-stretch-factor*))
  "Put an element in a stretchy vector. Factor is the amount of the total size
to expand by when expansion is needed."
  (declare (type (array * (*)) vec)
	   (type integer n)
	   (type number factor))
  (when (>= n (array-total-size vec))
    (resize vec (- n (array-total-size vec)) factor))
  (setf (fill-pointer vec) n) ;; should this really be done?
  (setf (aref vec n) value))

#|

;;; %%% Common Lisp fail:
;;; I'd like to make methods on a deftype but classes and types are dismorphic!

(defgeneric stretchy-append (dst src &key factor)
  (:documentation "Append SRC to DST. Expand by FACTOR if necessary."))

(defmethod stretchy-append ((dst stretchy-string) (src string) &key factor)
  (stretchy-append-string-or-vector dst src :factor factor))

(defmethod stretchy-append ((dst stretchy-string) (src stretchy-string)
			    &key factor)
  (stretchy-append-string-or-vector dst src :factor factor))

(defmethod stretchy-append ((dst stretchy-vector) (src vector) &key factor)
  (stretchy-append-string-or-vector dst src :factor factor))

(defmethod stretchy-append ((dst stretchy-vector) (src stretchy-vector)
			    &key factor)
  (stretchy-append-string-or-vector dst src :factor factor))

;; @@@ Just punting and taking the easy way for now for the next two methods.
;; In the future, make these faster specific methods.
(defmethod stretchy-append ((dst stretchy-string) (src character)
			    &key factor)
  (stretchy-append-string-or-vector dst (string src) :factor factor))

(defmethod stretchy-append ((dst stretchy-vector) src &key factor)
  "Append a single object (of any type) to a stretchy vector."
  (stretchy-append-string-or-vector dst (vector src) :factor factor))

|#

(defun stretchy-append (dst src &key (factor *default-stretch-factor*))
  "Append SRC to stretchy thing DST. SRC can be a character or a string or a ~
   symbol if DST is a string, or anything if DST is a vector. FACTOR is the ~
   amount of the total size to expand by when expansion is needed."
  (declare (type (array * (*)) dst)
	   (type number factor))
  (when (not (or (typep dst 'stretchy-string) (typep dst 'stretchy-vector)))
    (error "Destination must be a stretchy-string or stretchy-vector"))
  (cond
    ;; @@@ Just punting and taking the easy way for now.
    ;; In the future, make these faster specific functions.
    ((or (typep src 'stretchy-string)
	 (typep src 'stretchy-vector)
	 (typep src 'string)
	 (typep src 'vector))
     (stretchy-append-string-or-vector dst src factor))
    ((and (typep dst 'stretchy-string)
	  (typep src 'character))
     (stretchy-append-character dst src factor))
    ((and (typep dst 'stretchy-string)
	  (typep src 'symbol))
     (stretchy-append-string-or-vector dst (string src) factor))
    ((not (typep dst 'stretchy-string))
     (stretchy-append-string-or-vector dst (vector src) factor))
    (t
     (error "Can't append a ~(~a~) to a stretchy-string." (type-of src)))))

;; Might also be called shrink, but shrink implies taking up less space, which
;; it doesn't really.
(defun stretchy-truncate (s &optional (len 0))
  "Truncate a stretchy to length LEN."
  (setf (fill-pointer s) len))

;;;; TESTS!!

;; Compare performance vs. with-output-to-string

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

;; EOF
