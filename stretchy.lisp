;;
;; stretchy.lisp - Adjustable arrays and strings
;;

;; $Revision: 1.3 $

(defpackage :stretchy
  (:documentation "Functions for manipulating stretchy arrays and strings.")
  (:use :cl)
  (:export
   #:stretchy-string
   #:make-stretchy-string
   #:stretchy-vector
   #:make-stretchy-vector
   #:*default-stretch-factor*
   #:stretchy-append
   #:stretchy-set
   #:stretchy-truncate
   ))
(in-package :stretchy)

(deftype stretchy-string ()
  "An adjustable string of characters"
  '(and
    (vector character)
    (satisfies adjustable-array-p)
    (satisfies array-has-fill-pointer-p)))

;; The point of this being a macro was WHAT?
;; (defmacro make-stretchy-string (n)
;;   `(make-array ,n :element-type 'character :fill-pointer 0 :adjustable t))

(defun make-stretchy-string (n)
  (make-array n :element-type 'character :fill-pointer 0 :adjustable t))

(deftype stretchy-vector ()
  "An adjustable vector of objects"
  '(and
    (vector)
    (satisfies adjustable-array-p)
    (satisfies array-has-fill-pointer-p)))

(defun make-stretchy-vector (n)
  (make-array n :fill-pointer 0 :adjustable t))

(defvar *default-stretch-factor* 2/3
  "Default expansion factor for stretchy things.")

(defun stretchy-append-string-or-vector (dst src
					 &key (factor *default-stretch-factor*))
  "Append a vector to a stretchy vector. Factor is the amount of the total size to expand by when expansion is needed."
;  (declare (type stretchy-string dst))
;  (declare (type (or string stretchy-string) src))
  (let ((src-len (length src))
	(dst-len (length dst)))
    (when (>= (+ src-len dst-len) (array-total-size dst))
      ;; resize
      (setf dst (adjust-array
		 dst (+ (array-total-size dst) src-len
			(truncate (* (array-total-size dst) factor))))))
    (incf (fill-pointer dst) src-len)
    (setf (subseq dst dst-len (+ dst-len src-len)) src)))


(defun stretchy-set (vec n value &key (factor *default-stretch-factor*))
  "Put an element in a stretchy vector. Factor is the amount of the total size to expand by when expansion is needed."
;  (let ((len (length vec)))
    (when (>= n (array-total-size vec))
      ;; resize
      (setf vec (adjust-array
		 vec (+ (array-total-size vec) (- n (array-total-size vec))
			(truncate (* (array-total-size vec) factor))))))
    (setf (fill-pointer vec) n) ;; should this really be done?
    (setf (aref vec n) value))

;;;
;;; !!! COMMON-LISP-BADNESS-ALERT !!!
;;;
;;; I'd like to make methods on a deftype but classes and types are dismorphic!

#|
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
  (when (not (or (typep dst 'stretchy-string) (typep dst 'stretchy-vector)))
    (error "Destination must be a stretchy-string or stretchy-vector"))
  (cond
    ((or (typep src 'stretchy-string)
	 (typep src 'stretchy-vector)
	 (typep src 'string)
	 (typep src 'vector))
     (stretchy-append-string-or-vector dst src :factor factor))
    ((and (typep dst 'stretchy-string)
	  (or (typep src 'character) (typep src 'symbol)))
     ;; @@@ Just punting and taking the easy way for now.
     ;; In the future, make these faster specific functions.
     (stretchy-append-string-or-vector dst (string src) :factor factor))
    ((not (typep dst 'stretchy-string))
     ;; @@@ Just punting and taking the easy way for now.
     ;; In the future, make these faster specific functions.
     (stretchy-append-string-or-vector dst (vector src) :factor factor))
    (t
     (error "Can't append a ~(~a~) to a stretchy-string." (type-of src)))))

;; Might also be called shrink, but shrink implies taking up less space, which
;; it doesn't really.
(defun stretchy-truncate (s &optional (len 0))
  "Truncate a stretchy to length LEN."
  (setf (fill-pointer s) len))

;;;; TESTS!!

;; EOF
