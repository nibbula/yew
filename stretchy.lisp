;;
;; stretchy.lisp - Adjustable arrays and strings
;;

;; $Revision: 1.5 $

(defpackage :stretchy
  (:documentation
   "Functions for manipulating stretchy arrays and strings.

Common Lisp provides adjustable arrays, and therefore also adjustable strings,
but the mechanics of the programming interface is s
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
  (setf array (adjust-array
	       array (+ (array-total-size array) size
			(truncate (* (array-total-size array) factor))))))

(defun stretchy-append-string-or-vector (dst src factor)
  "Append a vector to a stretchy vector. FACTOR is the amount of the total
size to expand by when expansion is needed."
;  (declare (type stretchy-string dst))
;  (declare (type (or string stretchy-string) src))
  (let ((src-len (length src))
	(dst-len (length dst)))
    (when (>= (+ src-len dst-len) (array-total-size dst))
      (resize dst src-len factor))
    (incf (fill-pointer dst) src-len)
    (setf (subseq dst dst-len (+ dst-len src-len)) src)))

(defun stretchy-append-character (dst char factor)
  "Append the character CHAR to the stretchy string DST, with FACTOR as the
amount of the total size to expand by when expansion is needed."
  (let ((dst-len (length dst)))
    (when (>= (1+ dst-len) (array-total-size dst))
      (resize dst 1 factor))
    (incf (fill-pointer dst))
    (setf (char dst dst-len) char)))

(defun stretchy-set (vec n value &key (factor *default-stretch-factor*))
  "Put an element in a stretchy vector. Factor is the amount of the total size
to expand by when expansion is needed."
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

;; EOF
