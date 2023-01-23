;;;
;;; collections.lisp - Somewhere to put the junk.
;;;

;; This isn't necessarily efficient or well designed, it's just practical and
;; straightforward, because I really just need it to work. It could at least
;; probably use more macrology to make the methods and things. Of course, by
;; nature, treating various data structures uniformly has many potential
;; performance pitfalls.

;; @@@ The ‘o’ prefix is rather ugly. We should entertain other naming ideas.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dlib:without-package-variance
    (defpackage :collections
      (:documentation
"Generic collection functions. The methods defined here don't provide much over
the normal functions, but they enable defining your own methods which work
somewhat orthogonally with system classes. Be warned that using things in here
can be slow compared to the similar CL sequence functions. There's some pretty
foolish implementations in here, in the cause of orthogonality. Especially the
parts where we rather clownishly dress up hash tables and structs as sequences.
Also we really need the MOP for stuff.")
      (:use :cl)	; Please don't add any dependencies.
      (:nicknames :o)) ; too presumptuous, but maybe we could remove the 'o'?
    ))

(in-package :collections)

;; This is so the generic functions that one might want to specialize, are
;; separate, and we can use them to generate a template.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *methods*
    '(emptyp
      oelt
      olength
      olength-at-least-p
      olast
      omap
      omap-as
      omapk
      omapn
      omapcan
      omapcan-as
      mappable-p
      collection-p
      keyed-collection-p
      ordered-collection-p
      forward-collection-p
      backward-collection-p
      bi-directional-collection-p
      oiterator
      oiter-ref
      oincr
      odecr
      obeginning-p
      oend-p
      omap-into
      oevery
      oany
      osome
      onotevery
      onotany
      ocopy
      ofill
      osubseq
      oreduce
      ocount
      ocount-if
      oreverse
      onreverse
      osort
      ostable-sort
      ofind
      ofind-if
      oposition
      oposition-if
      osearch
      omismatch
      oreplace
      osubstitute
      onsubstitute
      oconcatenate
      oconcatenate-as
      omerge
      opick
      oremove
      oremove-if
      oremove-duplicates
      osplit
      osplit-if
      oreplace-subseq-as
      oreplace-subseq
      oaref
      opush-element
      opushnew-element
      opop-element
      ointersection
      onintersection
      oset-difference
      onset-difference
      oset-exclusive-or
      onset-exclusive-or
      osubsetp
      ounion
      onunion))

  (defparameter *other-exports*
    '(collection
      container
      keyed-collection
      ordered-collection
      iterator
      forward-collection
      forward-iterator
      backward-collection
      backward-iterator
      bi-directional-collection
      bi-directional-iterator
      container-data
      oitem
      ofirst osecond othird ofourth ofifth osixth oseventh oeighth oninth otenth
      oeleventh
      ofill-with
      ofill-range-with
      oslice
      oslice-from
      otake
      osort-by
      ostable-sort-by
      ofind-with-key
      osplit
      osplit-if
      opush
      opushnew
      opop
      make-collection-template))

  (export *methods*)
  (export *other-exports*))

(defclass collection ()
  ()
  (:documentation "A bag of stuff."))

;; This isn't really the way the type system is supposed to work.
;; E.g. (typep x 'collection) doesn't match up.
(defgeneric collection-p (thing)
  (:documentation "Return true if THING is a collection.")
  (:method ((thing t))                nil)
  (:method ((thing list))             t)
  (:method ((thing vector))	      t)
  (:method ((thing sequence))	      t)
  (:method ((thing hash-table))	      t)
  (:method ((thing structure-object)) t)
  (:method ((thing standard-object))  t)
  (:method ((thing collection))       t))

(defclass container (collection)
  ;; In some way, having this data slot is stupid and unnecessary, because you
  ;; can just get the data through the other methods, but it makes it easy to
  ;; define sub-types. Anyway you can just make it be IDENTITY or something?
  ((data :initarg :data
	 :accessor container-data
	 :documentation "Collection of data."))
  (:documentation "More words for things."))

(defclass keyed-collection (collection)
  ()
  (:documentation "A collection with explicit keys."))

;; I supposed we could treat a vector as a keyed collection too, but why?
(defgeneric keyed-collection-p (collection)
  (:documentation "Return true if COLLECTION is a keyed-collection.")
  (:method ((collection t))                nil)
  (:method ((collection list))             nil)
  (:method ((collection vector))	   nil)
  (:method ((collection sequence))	   nil)
  (:method ((collection hash-table))	   t)
  (:method ((collection structure-object)) t)
  (:method ((collection standard-object))  t)
  (:method ((collection keyed-collection)) t))

(defmethod keyed-collection-p ((collection container))
  (keyed-collection-p (container-data collection)))

;; aka sequence
(defclass ordered-collection (collection)
  ()
  (:documentation
   "A collection with a specific order, and thus can be indexed by the natural
numbers."))

(defgeneric ordered-collection-p (collection)
  (:documentation "Return true if COLLECTION is an ordered-collection.")
  (:method ((collection t))                  nil)
  (:method ((collection list))               t)
  (:method ((collection vector))	     t)
  (:method ((collection sequence))	     t)
  (:method ((collection hash-table))	     nil)
  (:method ((collection structure-object))   nil) ; @@@ wrong?
  (:method ((collection standard-object))    nil) ; @@@ wrong?
  (:method ((collection ordered-collection)) t))

(defmethod ordered-collection-p ((collection container))
  (ordered-collection-p (container-data collection)))

(defun check-make-collection-args
  (size
   initial-element
   initial-element-supplied-p
   element-type
   generator)
  "Check for simple problems in the arguments to ‘make-collection’, and error if
there are any."
  (declare (ignore size initial-element element-type))
  ;; I guess we can leave all the other sutff to the lower functions.
  (cond
    ((and generator initial-element-supplied-p)
     (error "generator and initial-element shouldn't both be supplied."))))

(defgeneric make-collection (type &key size initial-element element-type
				    generator)
  (:documentation
   "Return an ordered collection of ‘type’, with ‘size’ elements of
‘element-type’. The elements will be ‘initial-element’ if given, or what is
returned by successive calls to ‘generator’ if provided.")
  (:method ((type (eql 'list)) &key (size 0)
				 (initial-element nil initial-element-p)
				 element-type
				 generator)
    (check-make-collection-args size initial-element initial-element-p
				element-type generator)
    (make-list (or size 0) :initial-element initial-element))
  (:method ((type (eql 'string))
	    &key (size 0)
	      (initial-element (code-char 0) initial-element-p)
	      (element-type 'character)
	      generator)
    (check-make-collection-args size initial-element initial-element-p
				element-type generator)
    (make-string (or size 0) :initial-element initial-element
		 :element-type element-type))
  (:method ((type (eql 'vector))
	    &key size (initial-element nil initial-element-p)
	      element-type generator)
    (check-make-collection-args size initial-element initial-element-p
				element-type generator)
    (make-array (or size 0) :initial-element initial-element
		:element-type element-type))
  (:method ((type (eql 'hash-table))
	    &key size (initial-element nil initial-element-p)
	      element-type generator)
    (check-make-collection-args size initial-element initial-element-p
				element-type generator)
    (make-hash-table :size (or size 0))))

;; I know, iterators are so C++ you say? Yes, we sure don't want to use them
;; most of the time, but sometimes they are actually useful, like when you
;; want to iterate over multiple generic sequences in parallel and want it to
;; be even theoretically efficient for collections without O(1) or even O(n)
;; random access.

(defclass iterator ()
  ((location
    :initarg :location :accessor iterator-location :initform nil
    :documentation "Where it's at."))
  (:documentation
   "A thing that indicates a position in an ordered collection."))

(defclass forward-collection (ordered-collection)
  ()
  (:documentation
   "A collection that can do onext and onext-p."))

(defclass forward-iterator (iterator)
  ()
  (:documentation
   "An iterator that we can move forward through a collection with."))

(defgeneric forward-collection-p (collection)
  (:documentation "Return true if COLLECTION is an forward-collection.")
  (:method ((collection t))                  nil)
  (:method ((collection list))               t)
  (:method ((collection vector))	     t)
  (:method ((collection sequence))	     t)
  (:method ((collection hash-table))	     nil)
  (:method ((collection structure-object))   nil)
  (:method ((collection standard-object))    nil)
  (:method ((collection ordered-collection)) nil) ; but not a forward-collection
  (:method ((collection forward-collection)) t))

(defclass backward-collection (ordered-collection)
  ()
  (:documentation
   "A collection that can do oprev and oprev-p."))

(defgeneric backward-collection-p (collection)
  (:documentation "Return true if COLLECTION is an backward-collection.")
  (:method ((collection t))                  nil)
  (:method ((collection list))               nil)
  (:method ((collection vector))	     t)
  (:method ((collection sequence))	     t)
  (:method ((collection hash-table))	     nil)
  (:method ((collection structure-object))   nil)
  (:method ((collection standard-object))    nil)
  (:method ((collection ordered-collection)) nil) ; but not a backward-collection
  (:method ((collection backward-collection)) t))

(defclass backward-iterator ()
  ()
  (:documentation
   "An iterator that we can move forward through a collection with."))

(defclass bi-directional-collection (forward-collection backward-collection)
  ()
  (:documentation
   "A collection that can do what both forward-collection and
backward-collection can."))

(defgeneric bi-directional-collection-p (collection)
  (:documentation "Return true if COLLECTION is an backward-collection.")
  (:method ((collection t))
    (and (forward-collection-p collection)
	 (backward-collection-p collection))))

(defclass bi-directional-iterator (forward-iterator backward-iterator)
  ()
  (:documentation
   "An iterator that we can move forward or backward through a collection
with."))

;; With iterators there is an increased issue with stability of operations.
;; Since the biggest case of stability problems is concurency, and Common Lisp
;; doesn't include adequate facilities to address that, I suggest that
;; stability and concurency issues should be left to another package which can
;; depend on a portible concurrency library. I further suggest that a future
;; Common Lisp, with concurency primitives, would likely also include
;; facilites for generic collection operations. Nonetheless, future work on
;; this library could address mutative stability.

(defgeneric oiterator (ordered-collection &optional at)
  (:documentation "Return an iterator for the collection."))

(defgeneric oiter-ref (iterator)
  (:documentation "Access the object at the current iterator position."))

(defgeneric oincr (iterator &optional increment)
  (:documentation
   "Increment the iterator position by INCREMENT which defaults to 1."))

(defgeneric odecr (iterator &optional decrement)
  (:documentation
   "Decrement iterator position by INCREMENT which defaults to 1."))

(defgeneric obeginning-p (iterator)
  (:documentation
   "Return true if the iterator is a the beginning of the collection."))

(defgeneric oend-p (iterator)
  (:documentation
   "Return true if the iterator is at the end of the collection."))

;; List iterator

(defclass list-iterator (forward-iterator)
  ()
  (:documentation
   "A thing that indicates a position in a list."))

(defmethod oiterator ((collection list) &optional at)
  (make-instance 'list-iterator :location (or at collection)))

(defmethod oiter-ref ((i list-iterator))
  (car (iterator-location i)))

(defmethod (setf oiter-ref) (value (i list-iterator))
  (setf (car (iterator-location i)) value))

(defmethod oincr ((i list-iterator) &optional (increment 1))
  (setf (iterator-location i) (nthcdr increment (iterator-location i))))

(defmethod oend-p ((i list-iterator))
  (endp (iterator-location i)))

;; Vector iterator

(defclass vector-iterator (forward-iterator)
  ((object
    :initarg :object :accessor iterator-object
    :documentation "The vector."))
  (:default-initargs :location 0)
  (:documentation
   "A thing that indicates a position in a vector."))

(defmethod oiterator ((collection vector) &optional at)
  (make-instance 'vector-iterator :object collection :location (or at 0)))

;; We could check if setting the iterator out of bounds, but you'll get an error
;; when you try to use it when safety is on, and if safety is off, you probably
;; don't want the check. One could also imagine uses where you might want the
;; iterator out of bounds.

(defmethod oiter-ref ((i vector-iterator))
  (aref (iterator-object i) (iterator-location i)))

(defmethod (setf oiter-ref) (value (i vector-iterator))
  (setf (aref (iterator-object i) (iterator-location i)) value))

(defmethod oincr ((i vector-iterator) &optional (increment 1))
  (setf (iterator-location i) (+ (iterator-location i) increment)))

(defmethod obeginning-p ((i vector-iterator))
  (zerop (iterator-location i)))

(defmethod oend-p ((i vector-iterator))
  (>= (iterator-location i) (length (iterator-object i))))

(defmethod odecr ((i vector-iterator) &optional decrement)
  (setf (iterator-location i) (- (iterator-location i) decrement)))

;; We actually could fake hash-table struct and class iteration, by keeping
;; a vector of the keys.
#|
(defclass hash-table-iterator (forward-iterator)
  ((key-array
    :initarg :key-array :accessor hash-table-iterator-key-array :initform nil
    :documentation "The array of hash keys."))
  (:default-initargs :location 0)
  (:documentation
   "A thing that indicates a position in a vector."))

(defmethod oiterator ((collection hash-table) &optional at)
  (make-instance 'hash-table-iterator :location (or at 0)))
|#

(defmacro call-with-start-and-end (func args)
  "Call func with args and START and END keywords, assume that an environemnt
that has START and START-P and END and END-P."
  `(progn
     (if start-p
	 (if end-p
	     (,func ,@args :start start :end end)
	     (,func ,@args :start start))
	 (if end-p
	     (,func ,@args ::end end)
	     (,func ,@args)))))

;; @@@ There must be a better way to do this??
(defmacro call-with-start-end-test (func args)
  "Call func with args and START, END, TEST, and TEST-NOT keywords. Assume that
the environemnt has <arg> and <arg>-P for all those keywords."
  `(progn
     (cond
       (test-not-p
	(if start-p
	    (if end-p
		(,func ,@args :start start :end end :test-not test-not)
		(,func ,@args :start start :test-not test-not))
	    (if end-p
		(,func ,@args ::end end :test-not test-not)
		(,func ,@args :test-not test-not))))
       (test-p
	(if start-p
	    (if end-p
		(,func ,@args :start start :end end :test test)
		(,func ,@args :start start :test test))
	    (if end-p
		(,func ,@args ::end end :test test)
		(,func ,@args :test test))))
       (t
	(if start-p
	    (if end-p
		(,func ,@args :start start :end end)
		(,func ,@args :start start))
	    (if end-p
		(,func ,@args ::end end)
		(,func ,@args)))))))

(defgeneric emptyp (collection)
  (:documentation "Return true if there are no objects in the collection.")
  (:method ((thing list)) 	(null thing))
  (:method ((thing vector))     (zerop (length thing)))
  (:method ((thing sequence))   (zerop (length thing)))
  (:method ((thing hash-table)) (zerop (hash-table-count thing)))
  (:method ((thing container))  (emptyp (container-data thing)))
  (:method ((thing structure-object))
    (zerop (length (mop:class-slots (class-of thing)))))
  (:method ((thing standard-object))
    (zerop (length (mop:class-slots (class-of thing))))))

(defun slot-element (object name)
  "Return the value of the slot NAME in object, or NIL if it's doesn't exist or
isn't bound. NAME is converted to a string, as with the STRING function, and
compared with EQUALP, which is nice for avoiding symbol package problems."
  (let ((slot (find (string name) (mop:class-slots (class-of object))
		    :key #'mop:slot-definition-name
		    :test (lambda (a b) (equalp (string a) (string b))))))
    (and slot
	 (slot-boundp object (mop:slot-definition-name slot))
	 (slot-value object (mop:slot-definition-name slot)))))

;; This is probably only defined for sequences and keyed-collections
(defgeneric oelt (keyed-collection key)
  (:documentation "Return the element of COLLECTION specified by KEY.")
  ;; Keyed collections
  (:method ((thing hash-table) key)    (gethash key thing))
  (:method ((thing structure-object) (key integer))
    (let ((slot (nth key (mop:class-slots (class-of thing)))))
      (and
       (slot-boundp thing (mop:slot-definition-name slot))
       (slot-value thing (mop:slot-definition-name slot)))))
  (:method ((thing standard-object) (key integer))
    (let ((slot (nth key (mop:class-slots (class-of thing)))))
      (and
       (slot-boundp thing (mop:slot-definition-name slot))
       (slot-value thing (mop:slot-definition-name slot)))))
  (:method ((thing structure-object)   (key symbol)) (slot-element thing key))
  (:method ((thing structure-object)   (key string)) (slot-element thing key))
  (:method ((thing standard-object)    (key symbol)) (slot-element thing key))
  (:method ((thing standard-object)    (key string)) (slot-element thing key))
  ;; Ordered collections
  (:method ((thing list) key) 	       (nth key thing))
  (:method ((thing vector) key)        (aref thing key))
  (:method ((thing sequence) key)      (elt thing key)))

;; This is separate since it wants the generic to already be defined.
;; Many of the other container methods have to be separated this way.
(defmethod oelt ((thing container) key)
  (oelt (container-data thing) key))

(defun set-slot-element (object name value)
  "Set the VALUE of the slot NAME in OBJECT. Call SLOT-MISSING like SETF would,
if the slot is not found in the object."
  (let ((slot (find (string name) (mop:class-slots (class-of object))
		    :key #'mop:slot-definition-name
		    :test (lambda (a b) (equalp (string a) (string b))))))
    (when (not slot)
      (slot-missing (class-of object) object name 'setf value))
    (setf (slot-value object (mop:slot-definition-name slot)) value)))

(defgeneric (setf oelt) (value keyed-collection key)
  (:documentation
   "Set the element of KEYED-COLLECTION specified by KEY to VALUE.")
  (:method (value (thing hash-table) key)    (setf (gethash key thing) value))
  (:method (value (thing structure-object) (key integer))
    (let ((slot (nth key (mop:class-slots (class-of thing)))))
      (when (not slot)
	(slot-missing (class-of thing) thing (mop:slot-definition-name slot)
		      'setf value))
      (setf (slot-value thing (mop:slot-definition-name slot)) value)))
  (:method (value (thing standard-object) (key integer))
    (let ((slot (nth key (mop:class-slots (class-of thing)))))
      (when (not slot)
	(slot-missing (class-of thing) thing (mop:slot-definition-name slot)
		      'setf value))
      (setf (slot-value thing (mop:slot-definition-name slot)) value)))
  (:method (value (thing structure-object) (key symbol))
    (set-slot-element thing key value))
  (:method (value (thing structure-object) (key string))
    (set-slot-element thing key value))
  (:method (value (thing standard-object) (key symbol))
    (set-slot-element thing key value))
  (:method (value (thing standard-object) (key string))
    (set-slot-element thing key value))
  (:method (value (thing container) key)
    (setf (oelt (container-data thing) key) value))
  ;; Ordered collections
  (:method (value (thing list)     key) (setf (nth key thing)  value))
  (:method (value (thing vector)   key) (setf (aref thing key) value))
  (:method (value (thing sequence) key) (setf (elt thing key)  value)))

(defun oitem (key collection)
  "Return the element of COLLECTION specified by KEY. This is the same as OELT, 
but with the arguments reversed for convenient use in pipelines."
  (oelt collection key))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun define-ordinal-accessor (index)
    (let* ((name (intern (format nil "O~@:(~:r~)" (1+ index))))
	   (doc (format nil "Accessor for the ~(~a~) element of ‘collection’."
			name)))
      `(progn
	 (defgeneric ,name (collection)
	   (:documentation ,doc)
	   (:method (collection) (oelt collection ,index)))
	 (defgeneric (setf ,name) (value collection)
	   (:documentation ,doc)
	   (:method (value collection) (setf (oelt collection ,index) value))))))
  (defmacro define-ordinal-accessors ()
    `(progn ,@(loop :for i :from 0 :to 10
		    :collect (define-ordinal-accessor i)))))
(define-ordinal-accessors)

;; Palpable.
(defgeneric olength (collection)
  (:documentation "Return the length of a COLLECTION.")
  (:method ((collection list)) 	 		(length collection))
  (:method ((collection vector)) 	        (length collection))
  (:method ((collection sequence))      	(length collection))
  (:method ((collection hash-table))		(hash-table-count collection))
  (:method ((collection structure-object))
    (length (mop:class-slots (class-of collection))))
  (:method ((collection standard-object))
    (length (mop:class-slots (class-of collection)))))

(defmethod olength ((collection container))
  (olength (container-data collection)))

;; Palpable.
(defgeneric olength-at-least-p (n collection)
  (:documentation "Return true if the length of COLLECTION is at least N.")
  (:method (n (collection list))
    ;; Stolen from the CL spec entry for list-length.
    (do ((i 0 (+ i 2))	      ;; Counter.
	 (fast collection (cddr fast)) ;; Fast pointer: leaps by 2.
	 (slow collection (cdr slow))) ;; Slow pointer: leaps by 1.
	(nil)
      ;; If fast pointer hits the end, return the count.
      (when (endp fast) (return (if (>= i n) t nil)))
      (when (endp (cdr fast)) (return (if (>= (1+ i) n) t nil)))
      ;; If fast pointer eventually equals slow pointer,
      ;;  then we must be stuck in a circular list.
      ;; (A deeper property is the converse: if we are
      ;;  stuck in a circular list, then eventually the
      ;;  fast pointer will equal the slow pointer.
      ;;  That fact justifies this implementation.)
      (when (and (eq fast slow) (> i 0)) (return nil))))
  (:method (n (collection vector))     (>= (length collection) n))
  (:method (n (collection sequence))   (>= (length collection) n))
  (:method (n (collection hash-table)) (>= (hash-table-count collection) n))
  (:method (n (collection structure-object))
    (block nil
      (let ((i 0))
	(map nil (lambda (_)
		   (declare (ignore _))
		   (if (= i n) (return t) (incf i)))
	     (mop:class-slots (class-of collection))))))
  (:method (n (collection standard-object))
    (block nil
      (let ((i 0))
	(map nil (lambda (_)
		   (declare (ignore _))
		   (if (= i n) (return t) (incf i)))
	     (mop:class-slots (class-of collection)))))))

(defmethod olength-at-least-p (n (collection container))
  (olength-at-least-p (container-data collection) n))

;; This isn't exactly like LAST for lists, since it doesn't return a sub-list.
;; @@@ The list version has: &optional n, but I think I would rather make a an
;; OLASTN for n > 1, since for collections allocating a new collection is a
;; potentially much more expensive operation.
(defgeneric olast (collection)
  (:documentation
   "Return the last item of a COLLECTION. Note that this differs from the
standard LAST function for lists, in that it doesn't return a LIST. It will
probably only work for ordered-collections.")
  (:method ((collection list))
    (car (last collection)))
  (:method ((collection vector))
    (aref collection (1- (length collection))))
  (:method ((collection sequence))
    (elt collection (1- (length collection))))
  ;; @@@ We could actually make this work for structs and classes, but it's kind
  ;; of an anomaly, since it would depend on the order of class-slots, and not
  ;; very useful, since you would have to know what the last slot is.
  ;; (:method ((collection structure-object))
  ;;   (oelt collection
  ;; 	  (mop:slot-definition-name
  ;; 	   (car (last (1- (length (mop:class-slots (class-of collection)))))))))
  ;; (:method ((collection standard-object))
  ;;   (length (mop:class-slots (class-of collection))))
  )

(defmethod olast ((collection container))
  (olast (container-data collection)))

(defgeneric (setf olast) (value collection)
  (:documentation
   "Set the last element of a collection.")
  (:method (value (collection list))
    (setf (car (last collection)) value))
  (:method (value (collection vector))
    (setf (aref collection (1- (length collection))) value))
  (:method (value (collection sequence))
    (setf (elt collection (1- (length collection))) value))
  ;; @@@ Is it even useful for structs or classes?
  )

(defmethod (setf olast) (value (collection container))
  (setf (olast (container-data collection)) value))

;; (defgeneric omap (function &rest collections)
;;   (:documentation
;; "Apply FUNCTION to each object in all COLLECTIONS. Return a new COLLECTION,
;; each element of which is the result of applying FUNCTION to an object in
;; COLLECTIONS. FUNCTION is a function of one argument that returns an object
;; appropriate to be element of COLLECTION.

;; If COLLECTIONS are SEQUENCEs, the elements are applied in the sequence order."))

(defgeneric omap (function collection) ;; should be &rest collections
  (:documentation
   "Return a sequence of the results of applying FUNCTION to successive elements
of COLLECTION. The sequence returned is usually a list unless COLLECTION is a
sequence.")
  (:method (function (collection list))
    (mapcar function collection))
  (:method (function (collection vector))
    ;; @@@ Why did I have it just return the collection??
    ;; (progn (map nil function collection) collection))
    (map (type-of collection) function collection))
  ;; (:method (function (collection sequence))
  ;;   ;;(progn (map nil function collection) collection))
  ;;   (map (type-of collection) function collection))
  (:method (function (collection hash-table))
    (progn (maphash #'(lambda (k v)
			(declare (ignore k))
			(funcall function v)) collection)))
  (:method (function (collection structure-object))
    (loop :for slot :in (mop:class-slots (class-of collection))
       :collect
       (funcall function
		(and
		 (slot-boundp collection (mop:slot-definition-name slot))
		 (slot-value collection (mop:slot-definition-name slot))))))
  (:method (function (collection standard-object))
    (loop :for slot :in (mop:class-slots (class-of collection))
       :collect
       (funcall function
		(and
		 (slot-boundp collection (mop:slot-definition-name slot))
		 (slot-value collection (mop:slot-definition-name slot)))))))

(defmethod omap (function (collection container))
  (omap function (container-data collection)))

;; @@@ The has the problems of combinatoric method explosion, so we only support
;; a few values for type.

(defgeneric omap-as (type function collection)
  (:documentation
   "Apply ‘function’ to successive elements of ‘collection’. Collect return
values into a ‘type’ and return it.")
  (:method ((type (eql 'nil)) function collection)
    (omapn function collection))
  (:method ((type (eql 'list)) function collection)
    (let ((results '()))
      (omapn (lambda (_) (push (funcall function _) results)) collection)
      (nreverse results)))
  (:method ((type (eql 'vector)) function collection)
    (let ((results
	    (make-collection type :size (olength collection)
				  :element-type t)) ; @@@ yet another problem
	  (i 0))
      (omapn (lambda (_)
	       (setf (oelt results i) (funcall function _))
	       (incf i))
	     collection)
      results))
  (:method ((type (eql 'string)) function collection)
    (let ((results (make-collection type :size (olength collection)))
	  (i 0))
      (omapn (lambda (_)
	       (setf (oelt results i) (funcall function _))
	       (incf i))
	     collection)
      results)))

(defmethod omap-as (type function (collection container))
  (omap-as type function (container-data collection)))

(defgeneric omapk (function keyed-collection) ;; should be &rest collections
  (:documentation
   "Return a sequence of the results of applying FUNCTION to successive elements
of COLLECTION. If the collection is a keyed-collection, function will be passed
a pair consisiting of (key . value). The sequence returned is usually a list
unless COLLECTION is a sequence.")
  (:method (function (collection list))
    (mapcar function collection))
;; (:method (function (collection vector))
;;   (progn (map nil function collection) collection))
  (:method (function (collection sequence))
    ;;(progn (map nil function collection) collection))
    (map (type-of collection) function collection))
  (:method (function (collection hash-table))
    (progn (maphash #'(lambda (k v)
			(funcall function (vector k v))) collection)))
  (:method (function (collection structure-object))
    (loop :for slot :in (mop:class-slots (class-of collection))
       :collect
       (funcall function
		(and
		 (slot-boundp collection (mop:slot-definition-name slot))
		 (vector
		  (mop:slot-definition-name slot)
		  (slot-value collection (mop:slot-definition-name slot)))))))
  (:method (function (collection standard-object))
    (loop :for slot :in (mop:class-slots (class-of collection))
       :collect
       (funcall function
		(and
		 (slot-boundp collection (mop:slot-definition-name slot))
		 (vector
		  (mop:slot-definition-name slot)
		  (slot-value collection (mop:slot-definition-name slot))))))))

(defmethod omapk (function (collection container))
  (omapk function (container-data collection)))

(defgeneric omapn (function collection)
  (:documentation
   "Apply FUNCTION to successive elements of COLLECTION. Do not collect return
values.")
  (:method (function (collection list))
    ;; (mapcan function collection)
    (mapc function collection)
    )
;; (:method (function (collection vector))
;;   (progn (map nil function collection) collection))
  (:method (function (collection sequence))
    (map nil function collection))
  (:method (function (collection hash-table))
    (progn
      (maphash #'(lambda (k v) (funcall function (vector k v))) collection)))
  (:method (function (collection structure-object))
    (loop :for slot :in (mop:class-slots (class-of collection))
       :do
       (funcall function
		(and
		 (slot-boundp collection (mop:slot-definition-name slot))
		 (slot-value collection (mop:slot-definition-name slot))))))
  (:method (function (collection standard-object))
    (loop :for slot :in (mop:class-slots (class-of collection))
       :do
       (funcall function
		(and
		 (slot-boundp collection (mop:slot-definition-name slot))
		 (slot-value collection (mop:slot-definition-name slot)))))))

(defmethod omapn (function (collection container))
  (omapn function (container-data collection)))

;; @@@ Maybe we should make an omapkn too?

;; @@@ Note: mapcan-<x>like are not hygenic, so just for use here.
(defmacro mapcan-listlike (() &body body)
  `(let (end result item new)
      ;; Like mapcan, but doesn't require function to return a list.
      (omapn (lambda (_)
	       (when (setf item (funcall function _))
                 (setf new (if (listp item) item (list item)))
                 (when end
                   (rplacd end new))
                 (setf end (last new))
                 (when (null result)
                   (setf result new))))
             collection)
     ,@body))

(defmacro mapcan-vectorlike (() &body body)
  `(let (end result item new (len 0))
     (declare (type fixnum len))
     ;; Like the list method, but keeps track of the length, so we can make
     ;; the result array quicker.
     (omapn (lambda (_)
	      (when (setf item (funcall function _))
                (setf new (if (listp item)
			      (prog1 item
				(incf len (length item)))
			      (prog1 (list item)
				(incf len))))
                (when end
                  (rplacd end new))
                (setf end (last new))
                (when (null result)
                  (setf result new))))
            collection)
     ,@body))

(defmacro mapcan-hashlike (hash-table)
  `(let ((result ,hash-table)
	 item)
     (omapk (lambda (_)
	      (when (setf item (funcall function _))
		(setf (gethash (oelt item 0) result) (oelt item 1))))
            collection)
     result))

(defgeneric omapcan-as (type function collection)
  (:documentation
   "Apply ‘function’ to successive elements of ‘collection’, conceptually
applying ‘nconc’ to results. Simlar to mapcan, but takes collections as
arguments. Return a collection of ‘type’.")
  (:method ((type (eql 'list)) function collection)
    (mapcan-listlike () result))
  (:method ((type (eql 'vector)) function collection)
    (mapcan-vectorlike ()
      (make-array len :element-type (array-element-type collection)
		      :initial-contents result)))
  (:method ((type (eql 'string)) function collection)
    (mapcan-vectorlike ()
      ;; is this sufficient? or should we make-string?
      (make-array len :element-type 'character
		      :initial-contents result)))
;; (:method (function (collection sequence))
;;   )
  (:method ((type (eql 'hash-table)) function collection)
    (mapcan-hashlike
     (make-hash-table
      :test #'equal
      :size (olength collection)))))
;; Structure and standard object versions don't seem useful.

;; I don't think this is necessary
;; (defmethod omapcan-as (type function (collection container))
;;   (omapcan-as type function (container-data collection)))

(defgeneric omapcan (function collection)
  (:documentation
   "Apply ‘function’ to successive elements of ‘collection’, conceptually
applying ‘nconc’ to results. Simlar to mapcan, but takes collections as
arguments.")
  (:method (function (collection list))
    (mapcan-listlike () result))
  (:method (function (collection vector))
    (mapcan-vectorlike ()
      (make-array len :element-type (array-element-type collection)
		      :initial-contents result)))
  ;; (:method (function (collection sequence))
  ;;   )
  (:method (function (collection hash-table))
    (mapcan-hashlike
     (make-hash-table
      :test (hash-table-test collection)
      :size (hash-table-size collection)
      :rehash-size (hash-table-rehash-size collection)
      :rehash-threshold (hash-table-rehash-threshold collection)))))
;; Structure and standard object versions don't seem useful.

(defmethod omapcan (function (collection container))
  (omapcan function (container-data collection)))

;; This has the parallel sequence feature from normal MAP, but can't use
;; generic dispatch on the collections.
#|
(defun omapm (result-type function &rest collections)
  "Call FUNCTION with successive sets of arguments from COLLECTIONS. One
argument is obtained from each collection. Return the collected results in
collection of RESULT-TYPE."
  (if result-type
      (apply #'omap-into result-type function
      (apply #'omap result-type function
  )
|#

(defgeneric mappable-p (collection)
  (:documentation "Return true if the COLLECTION can be iterated with OMAP.")
  (:method ((collection t))                nil)
  (:method ((collection list))             t)
  (:method ((collection vector))	   t)
  (:method ((collection sequence))	   t)
  (:method ((collection hash-table))	   t)
  (:method ((collection structure-object)) t)
  (:method ((collection standard-object))  t))

(defmethod mappable-p ((collection container))
  (mappable-p (container-data collection)))

;; This is actually not as useful, since we can't specialize on ‘collections’.
(defgeneric omap-into (mutable-collection function &rest collections)
  (:documentation
"Apply FUNCTION to each object in the COLLECTIONs and store the results in
MUTABLE-COLLECTION. FUNCTION is a function of one argument that returns an
object appropriate to be element of a collection of MUTABLE-COLLECTION.
Returns MUTABLE-COLLECTION.

If COLLECTIONS are SEQUENCEs, the elements are applied in the sequence order.")
  (:method ((mutable-collection list) function &rest collections)
    (apply #'map-into mutable-collection function collections))
  (:method ((mutable-collection vector) function &rest collections)
    (apply #'map-into mutable-collection function collections))
  (:method ((mutable-collection sequence) function &rest collections)
    (apply #'map-into mutable-collection function collections))
  (:method ((mutable-collection hash-table) function &rest collections)
    (apply #'map-into mutable-collection function collections)))

(defmethod omap-into ((mutable-collection container) function &rest collections)
  (apply #'omap-into (container-data mutable-collection) function collections))

;; I think there's not really any reason why oevery, etc. have to be generic.
;; @@@ Yes, I know these don't do the multi-dimensional iteration thing yet.

(defun oevery (function &rest collections)
  "Return true if FUNCTION returns true for every element of COLLECTIONS."
  (omapn (lambda (c)
	   (omapn
	    (lambda (e)
	      (unless (funcall function e)
		(return-from oevery nil))) c))
	 collections)
  t)

(defun oany (function &rest collections)
  "Return true if FUNCTION returns true for any element of COLLECTIONS."
  (omapn (lambda (c)
	   (omapn
	    (lambda (e)
	      (when (funcall function e)
		(return-from oany t))) c))
	 collections)
  nil)

;; (defalias 'osome 'oany)
(setf (fdefinition 'osome) (fdefinition 'oany)
      (documentation 'osome 'function) (documentation 'oany 'function))

(defun onotevery (function &rest collections)
  "Return true if FUNCTION returns false for all elements of COLLECTIONS."
  (omapn (lambda (c)
	   (omapn
	     (lambda (e)
	       (unless (funcall function e)
		 (return-from onotevery t))) c))
	 collections)
  nil)

(defun onotany (function &rest collections)
  "Return true if FUNCTION returns false for all elements of COLLECTIONS."
  (omapn (lambda (c)
	   (omapn
	    (lambda (e)
	      (when (funcall function e)
		(return-from onotany nil))) c))
	 collections)
  t)

#|
(defun test-quantifying-predicates-1 ()
  (let ((v '((nil nil nil nil)
	     (nil nil   t nil)
	     (  t   t   t   t)
	     (  t   t nil   t))))
    (loop :for f :in '(every some notevery notany) :do
      (loop :for x :in v :do
        (when (not (equal (funcall f #'identity x)
			  (funcall (intern (concatenate 'string "O" (string f)))
				   #'identity x)))
	  (error "Fail ~s ~s" f x))))))
|#

;; Sequence-like functions

(defgeneric ocopy (collection)
  (:documentation
   "Return new collection with the same elements as COLLECTION.")
  (:method ((collection list)) (copy-seq collection))
  (:method ((collection vector)) (copy-seq collection))
  (:method ((collection sequence)) (copy-seq collection))
  (:method ((collection hash-table))
    ;; Of course this could have some discernable properties that are
    ;; different than the original.
    (let ((new-table
	   (make-hash-table
	    :test (hash-table-test collection)
	    :size (hash-table-size collection)
	    :rehash-size (hash-table-rehash-size collection)
	    :rehash-threshold (hash-table-rehash-threshold collection))))
      (maphash #'(lambda (key value)
		   (setf (gethash key new-table) value))
	       collection)
      new-table)))

(defmethod ocopy ((collection container))
  ;; This is one of those things that may not work for complex objects.
  ;; Callers should realize this, and object implementations should
  ;; error or something, if it doesn't make sense.

  ;; (make-instance (type-of collection)
  ;; 		 :data (ocopy (container-data collection))))
  (let ((result (dlib:shallow-copy-object collection)))
    (setf (container-data result) (ocopy (container-data collection)))
    result))

(defgeneric ofill (collection item &key start end)
  (:documentation
   "Replaces the elements of SEQUENCE bounded by START and END with ITEM.")
  (:method ((collection list) item &key start end)
    (fill collection item :start start :end end))
  (:method ((collection vector) item &key start end)
    (fill collection item :start start :end end))
  (:method ((collection sequence) item &key start end)
    (fill collection item :start start :end end))
  (:method ((collection hash-table) item &key start end)
    (declare (ignore start end))
    (maphash #'(lambda (key value)
		 (declare (ignore value))
		 (setf (gethash key collection) item))
	     collection)))

(defmethod ofill ((collection container) item &key start end)
  (ofill (container-data collection) item :start start :end end))

(defun ofill-with (item collection)
  "Replaces the elements of SEQUENCE with ITEM. This is the same as OFILL but
with the arguments reversed for convenient use in pipelines, and without the
START and END arguments. See also OFILL-RANGE-WITH."
  (ofill collection item))

(defun ofill-range-with (start end item collection)
  "Replaces the elements of SEQUENCE with ITEM. This is the same as OFILL but
with the arguments reversed for convenient use in pipelines. See also
OFILL-WITH."
  (ofill collection item :start start :end end))

;; (defgeneric make-container (collection ...)
;;   (:documentation "")
;;   (:method ((collection XX))
;; 	    ))

(defgeneric osubseq (collection start &optional end)
  (:documentation
   "OSUBSEQ creates a sequence that is a copy of the subsequence of sequence
bounded by START and END.")
  (:method ((collection list) start &optional end)
    (subseq collection start end))
  (:method ((collection vector) start &optional end)
    (subseq collection start end))
  (:method ((collection sequence) start &optional end)
    (subseq collection start end)))

(defmethod osubseq ((collection container) start &optional end)
  ;; Somewhat dubious, of course.
  ;; (make-instance (type-of collection)
  ;; 		 :data (osubseq (container-data collection) start end))

  (let ((result (dlib:shallow-copy-object collection)))
    (setf (container-data result)
	  (osubseq (container-data collection) start end))
    result))

(defgeneric (setf osubseq) (value collection start &optional end)
  (:documentation
   "OSUBSEQ creates a sequence that is a copy of the subsequence of sequence
bounded by START and END.")
  (:method (value (collection list) start &optional end)
    (setf (subseq collection start end) value))
  (:method (value (collection vector) start &optional end)
    (setf (subseq collection start end) value))
  (:method (value (collection sequence) start &optional end)
    (setf (subseq collection start end) value)))

(defmethod (setf osubseq) (value (collection container) start &optional end)
  ;; Somewhat dubious, of course.
  ;; (make-instance (type-of collection)
  ;; 		 :data (osubseq (container-data collection) start end))

  (let ((result (dlib:shallow-copy-object collection)))
    (setf (osubseq (container-data result) start end) value)
    result))

;; I wonder if the useful word "slice" is really appropriate to be even slightly
;; used up here.

(defun oslice (start end collection)
  "This is the same as OSUBSEQ, but with the arguments reversed for convenient
use in pipelines. See also OSLICE-FROM."
  (osubseq collection start end))

(defun oslice-from (start collection)
  "This is the same as OSUBSEQ, but with the arguments reversed for convenient
use in pipelines, and without the END argument. See also OSLICE."
  (osubseq collection start))

;; Yes, more stupid subseq wrappers. I know there is wisdom in not having
;; duplicate names for the same functionality, but I really just want to be
;; able to say this.

(defun otake (n collection)
  "This is the same as (osubseq 0 n collection), but is more convenient use in
pipelines, and maybe easier to remember."
  (osubseq collection 0 n))

;; @@@ Is the initial value going to work??
(defgeneric oreduce (function collection
		     &key key from-end start end initial-value)
  (:documentation
   "Apply ‘function’ to pairs of the elements of ‘collection’ and the results
of the previous application. The ‘function’ must take two arguments which are
either elements of ‘collection’ or the result of itself and an element, or no
arguments.")
  (:method (function (collection list)
	     &key key from-end (start nil start-p) (end nil end-p)
	       (initial-value nil ivp))
    (declare (ignorable start start-p end end-p))
    (if ivp
	(call-with-start-and-end reduce (function collection
				         :key key :from-end from-end
				         :initial-value initial-value))
	(call-with-start-and-end reduce (function collection
				         :key key :from-end from-end))))
  (:method (function (collection vector)
	     &key key from-end (start nil start-p) (end nil end-p)
	       (initial-value nil ivp))
    (declare (ignorable start start-p end end-p))
    (if ivp
	(call-with-start-and-end reduce (function collection
				         :key key :from-end from-end
				         :initial-value initial-value))
	(call-with-start-and-end reduce (function collection
				         :key key :from-end from-end))))
  (:method (function (collection sequence)
	     &key key from-end (start nil start-p) (end nil end-p)
	       (initial-value nil ivp))
    (declare (ignorable start start-p end end-p))
    (if ivp
	(call-with-start-and-end reduce (function collection
					 :key key :from-end from-end
					 :initial-value initial-value))
	(call-with-start-and-end reduce (function collection
					 :key key :from-end from-end))))
  ;; @@@ I think might be useless because of ordering instability.
  #|
  (:method ((function (collection hash-table)
	     &key key from-end (start nil start-p) (end nil end-p)
	       initial-value))
    (declare (ignorable start start-p end end-p)
	     (ignore from-end))		; ignore this for now
    (let ((i 0)
	  )
      (declare (type fixnum i))
    (with-hash-table-iterator (next collection)
      (multiple-value-bind (more? key value) (next)
	(unless more? (return))
	))
    collection)
  |#
  ;; Doing it for structs and objects just seems weirdly unlikely.
  )

(defmethod oreduce (function (collection container)
		    &key key from-end (start nil start-p) (end nil end-p)
		      initial-value)
  (declare (ignorable start start-p end end-p))
  (call-with-start-and-end reduce (function (container-data collection)
				   :key key :from-end from-end
				   :initial-value initial-value)))

(defgeneric ocount (item collection &key from-end start end key test test-not)
  (:documentation "Return the number of elements of COLLECTION, bounded by START
and END, that satisfy the TEST.")
  (:method (item (collection list) &key from-end key
				     (test nil test-p)
				     (test-not nil test-not-p)
				     (start nil start-p)
				     (end nil end-p))
    (call-with-start-end-test
     count (item collection :from-end from-end :key key)))
  (:method (item (collection vector) &key from-end key
				       (test nil test-p)
				       (test-not nil test-not-p)
				       (start nil start-p)
				       (end nil end-p))
    (call-with-start-end-test
     count (item collection :from-end from-end :key key)))
  (:method (item (collection sequence) &key from-end key
					 (test nil test-p)
					 (test-not nil test-not-p)
					 (start nil start-p)
					 (end nil end-p))
    (call-with-start-end-test
     count (item collection :from-end from-end :key key)))
  (:method (item (collection hash-table)
	    &key from-end start end key test test-not)
    (declare (ignore start end from-end))
    (let ((count 0))
      (labels ((test-test (k value)
		 (declare (ignore k))
		 (when (funcall test item value) (incf count)))
	       (test-test-key (k value)
		 (declare (ignore k))
		 (when (funcall test item (funcall key value)) (incf count)))
	       (test-test-not (k value)
		 (declare (ignore k))
		 (when (not (funcall test item value)) (incf count)))
	       (test-test-not-key (k value)
		 (declare (ignore k))
		 (when (not (funcall test item (funcall key value)))
		   (incf count)))
	       (test-default (k value)
		 (declare (ignore k))
		 (when (eql item value) (incf count)))
	       (test-default-key (k value)
		 (declare (ignore k))
		 (when (eql item (funcall key value)) (incf count))))
	(maphash
	 (cond
	   (test     (if key #'test-test-key     #'test-test))
	   (test-not (if key #'test-test-not-key #'test-test-not))
	   (t        (if key #'test-default-key  #'test-default)))
	 collection)
	count))))

(defmethod ocount (item (collection container) &key from-end key
						 (test nil test-p)
						 (test-not nil test-not-p)
						 (start nil start-p)
						 (end nil end-p))
  (call-with-start-end-test ocount (item (container-data collection)
					 :from-end from-end :key key)))

(defgeneric ocount-if (predicate collection &key from-end start end key)
  (:documentation "Return the number of elements of COLLECTION, bounded by START
and END, that satisfy the PREDICATE.")
  (:method (predicate (collection list) &key from-end key
				     (start nil start-p)
				     (end nil end-p))
    (call-with-start-and-end
     count-if (predicate collection :from-end from-end :key key)))
  (:method (predicate (collection vector) &key from-end key
				       (start nil start-p)
				       (end nil end-p))
    (call-with-start-and-end
     count-if (predicate collection :from-end from-end :key key)))
  (:method (predicate (collection sequence) &key from-end key
					 (start nil start-p)
					 (end nil end-p))
    (call-with-start-and-end
     count-if (predicate collection :from-end from-end :key key)))
  (:method (predicate (collection hash-table)
	    &key from-end start end key)
    (declare (ignore start end from-end))
    (let ((count 0)
	  the-test-func)
      (labels ((test-test (k value)
		 (declare (ignore k))
		 (when (funcall predicate value) (incf count)))
	       (test-test-key (k value)
		 (declare (ignore k))
		 (when (funcall predicate (funcall key value)) (incf count))))
	(setf the-test-func (if key #'test-test-key #'test-test))
	(maphash the-test-func collection)
	count))))

(defmethod ocount-if (predicate (collection container) &key from-end key
							 (start nil start-p)
							 (end nil end-p))
  (call-with-start-and-end ocount-if (predicate (container-data collection)
						:from-end from-end
						:key key)))

(defgeneric oreverse (ordered-collection)
  (:documentation
   "Return a new sequence containing the same elements but in reverse order.")
  (:method ((collection list)) 	 		(reverse collection))
  (:method ((collection vector)) 	        (reverse collection))
  (:method ((collection sequence))      	(reverse collection)))

(defmethod oreverse ((ordered-collection container))
  (oreverse (container-data ordered-collection)))

(defgeneric onreverse (ordered-collection)
  (:documentation
   "Return a sequence of the same elements in reverse order, possibly modifying
or destroying the argument.")
  (:method ((collection list)) 	 		(nreverse collection))
  (:method ((collection vector)) 	        (nreverse collection))
  (:method ((collection sequence))      	(nreverse collection)))

(defmethod onreverse ((ordered-collection container))
  (oreverse (container-data ordered-collection)))

(defgeneric osort (collection predicate &key key)
  (:documentation
   "Destructively sort a collection according to the order determined by the
predicate function. ‘predicate’ should return non-NIL if the first argument is
to precede the second argument. As in other collection functions, ‘key’ is a
function that is given the collection element, and should return a value to be
given to ‘predicate’.")
  (:method ((collection list) predicate &key key)
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (sort collection predicate :key key))
  (:method ((collection vector) predicate &key key)
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (sort collection predicate :key key))
  (:method ((collection sequence) predicate &key key)
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (sort collection predicate :key key)))

(defgeneric ostable-sort (collection predicate &key key)
  (:documentation
   "Destructively sort a collection according to the order determined by the
predicate function, with guaranteed stability. ‘predicate’ should return
non-NIL if the first argument is to precede the second argument. As in other
collection functions, ‘key’ is a function that is given the collection
element, and should return a value to be given to ‘predicate’.")
  (:method ((collection list) predicate &key key)
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (stable-sort collection predicate :key key))
  (:method ((collection vector) predicate &key key)
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (stable-sort collection predicate :key key))
  (:method ((collection sequence) predicate &key key)
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (stable-sort collection predicate :key key)))

(defmethod osort ((collection container) predicate &key key)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (setf (container-data collection)
	(osort (container-data collection) predicate :key key))
  collection)

(defmethod ostable-sort ((collection container) predicate &key key)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (setf (container-data collection)
	(ostable-sort (container-data collection) predicate :key key))
  collection)

;; @@@ Should we make sort-by-key also?
(defun osort-by (predicate collection)
  "Destructively sort a collection according to the order determined by the
predicate function. This is just like OSORT but with the arguments reversed for
convenience in pipelines."
  (osort collection predicate))

(defun ostable-sort-by (predicate collection)
  "Destructively sort a collection according to the order determined by the
predicate function, with guaranteed stability. This is just like ‘ostable-sort’
but with the arguments reversed for convenience in pipelines."
  (ostable-sort collection predicate))

(defgeneric ofind (item collection &key from-end start end key test test-not)
  (:documentation
   "Search for an element of the COLLECTION bounded by START and END that
satisfies the test TEST or TEST-NOT, as appropriate.")
  (:method (item (collection list) &key from-end (start 0) end key test test-not)
    (find item collection :from-end from-end :start start :end end :key key
	  :test test :test-not test-not))
  (:method (item (collection vector) &key from-end (start 0) end key test
				       test-not)
    (find item collection :from-end from-end :start start :end end :key key
	  :test test :test-not test-not))
  (:method (item (collection sequence) &key from-end (start 0) end key test
					 test-not)
    (find item collection :from-end from-end :start start :end end :key key
	  :test test :test-not test-not))
  (:method (item (collection hash-table) &key from-end (start 0) end key test
					   test-not)
    ;; This is certainly not perfect, but...
    (declare (ignore from-end start end))
    (let ((found-item (gethash (if key (funcall key item) item) collection)))
      (cond
	((and test (funcall test item found-item)) found-item)
	((and test-not (not (funcall test item found-item))) found-item))
      found-item)))

(defmethod ofind (item (collection container)
		  &key from-end (start 0) end key test test-not)
  (ofind item (container-data collection) :from-end from-end :start start
	 :end end :key key :test test :test-not test-not))

(defgeneric ofind-if (predicate collection
		      &key from-end start end key)
  (:documentation
   "Search for an element of the COLLECTION bounded by START and END that
satisfies the PREDICATE.")
  (:method (predicate (collection list) &key from-end (start 0) end key)
    (find-if predicate collection :from-end from-end :start start :end end
	     :key key))
  (:method (predicate (collection vector) &key from-end (start 0) end key)
    (find predicate collection :from-end from-end :start start :end end :key key))
  (:method (predicate (collection sequence) &key from-end (start 0) end key)
    (find predicate collection :from-end from-end :start start :end end :key key))
  (:method (predicate (collection hash-table) &key from-end (start 0) end key)
    ;; This is certainly not perfect, but...
    (declare (ignore from-end start end))
    (maphash (lambda (k value)
	       (declare (ignore k))
	       (when (funcall predicate (if key (funcall key value) value))
		 (return-from ofind-if value)))
	       collection)))

(defmethod ofind-if (predicate (collection container)
		     &key from-end (start 0) end key)
  (ofind-if predicate (container-data collection)
	    :from-end from-end :start start
	    :end end :key key))

(defun ofind-with-key (item key collection)
  "Like ofind with supplying a KEY argument, but convenient for use in
pipelines."
  (ofind item collection :key key))

(defgeneric oposition (item collection &key from-end test test-not start end key)
  (:documentation
   "Return the index of the element that satisfies the TEST in COLLECTION.
Return the leftmost if FROM-END is true, or of the rightmost if FROM-END is
false. If no element satifies the test, NIL is returned.")
  (:method (item (collection list)
	    &key from-end
	      (test nil test-p)
	      (test-not nil test-not-p)
	      key
	      (start nil start-p)
	      (end nil end-p))
    (declare (ignorable start start-p end end-p))
    (call-with-start-end-test position (item collection
					     :from-end from-end :key key)))
  (:method (item (collection vector)
	    &key from-end
	      (test nil test-p)
	      (test-not nil test-not-p)
	      key
	      (start nil start-p)
	      (end nil end-p))
    (declare (ignorable start start-p end end-p))
    (call-with-start-end-test position (item collection
					     :from-end from-end :key key)))
  (:method (item (collection sequence)
	    &key from-end
	      (test nil test-p)
	      (test-not nil test-not-p)
	      key
	      (start nil start-p)
	      (end nil end-p))
    (declare (ignorable start start-p end end-p))
    (call-with-start-end-test position (item collection
					    :from-end from-end :key key))))

(defmethod oposition (item (collection container)
		      &key from-end
			(test nil test-p)
			(test-not nil test-not-p)
			key
			(start nil start-p)
			(end nil end-p))
  (declare (ignorable start start-p end end-p))
  (call-with-start-end-test position (item (container-data collection)
					   :from-end from-end :key key)))

(defgeneric oposition-if (predicate collection &key from-end start end key)
  (:documentation
   "Return the index of the element that satisfies the TEST in COLLECTION.
Return the leftmost if FROM-END is true, or of the rightmost if FROM-END is
false. If no element satifies the test, NIL is returned.")
  (:method (predicate (collection list)
	    &key from-end key (start nil start-p) (end nil end-p))
    (declare (ignorable start start-p end end-p))
    (call-with-start-and-end position-if
			     (predicate collection
					:from-end from-end :key key)))
  (:method (predicate (collection vector)
	    &key from-end key (start nil start-p) (end nil end-p))
    (declare (ignorable start start-p end end-p))
    (call-with-start-and-end position-if (predicate
					  collection
					  :from-end from-end :key key)))
  (:method (predicate (collection sequence)
	    &key from-end key (start nil start-p) (end nil end-p))
    (declare (ignorable start start-p end end-p))
    (call-with-start-and-end position-if (predicate
					  collection
					  :from-end from-end :key key))))

(defmethod oposition-if (predicate (collection container)
			 &key from-end key (start nil start-p) (end nil end-p))
  (declare (ignorable start start-p end end-p))
  (call-with-start-and-end position-if (predicate
					(container-data collection)
					:from-end from-end :key key)))

(defgeneric osearch (collection-1 collection-2
		     &rest args ;; Yeah. I give up with this one.
		     &key from-end test test-not key start1 start2 end1 end2)
  (:documentation
   "Searches collection-2 for a subsequence that matches collection-1.
If from-end is true, the index of the leftmost element of the rightmost
matching subsequence is returned.")
  (:method ((collection-1 list) (collection-2 list) &rest args
	     &key from-end test test-not key start1 start2 end1 end2)
    (declare (ignorable from-end test test-not key start1 start2 end1 end2))
    (apply #'search collection-1 collection-2 args))
  (:method ((collection-1 vector) (collection-2 vector) &rest args
	     &key from-end test test-not key start1 start2 end1 end2)
    (declare (ignorable from-end test test-not key start1 start2 end1 end2))
    (apply #'search collection-1 collection-2 args))
  (:method ((collection-1 sequence) (collection-2 sequence) &rest args
	     &key from-end test test-not key start1 start2 end1 end2)
    (declare (ignorable from-end test test-not key start1 start2 end1 end2))
    (apply #'search collection-1 collection-2 args)))

(defmethod osearch ((collection-1 container) (collection-2 container)
		    &rest args
		    &key from-end test test-not key start1 start2 end1 end2)
  (declare (ignorable from-end test test-not key start1 start2 end1 end2))
  (apply #'osearch
	 (container-data collection-1) (container-data collection-2) args))

#|
(defgeneric omismatch (collection ...)
  (:documentation "")
  (:method ((collection XX))
	    ))

(defgeneric oreplace (collection ...)
  (:documentation "")
  (:method ((collection XX))
	    ))

|#

(defgeneric osubstitute (new-item old-item collection
			 &key from-end test test-not start end count key)
  (:documentation
   "Return a copy of the collection in which each element that satisfies the
test has been replaced with ‘new-item’.")
  (:method (new-item old-item (collection list)
	    &key from-end
	      (test nil test-p)
	      (test-not nil test-not-p)
	      (start nil start-p)
	      (end nil end-p)
	      count key)
    (call-with-start-end-test substitute
      (new-item old-item collection :from-end from-end :count count :key key)))
  (:method (new-item old-item (collection vector)
	    &key from-end
	      (test nil test-p)
	      (test-not nil test-not-p)
	      (start nil start-p)
	      (end nil end-p)
	      count key)
    (call-with-start-end-test substitute
      (new-item old-item collection :from-end from-end :count count :key key)))
  (:method (new-item old-item (collection sequence)
	    &key from-end
	      (test nil test-p)
	      (test-not nil test-not-p)
	      (start nil start-p)
	      (end nil end-p)
	      count key)
    (call-with-start-end-test substitute
      (new-item old-item collection :from-end from-end :count count :key key)))
  (:method (new-item old-item (collection hash-table)
	    &key from-end test test-not start end count key)
    (declare (ignore from-end start end count)) ;; @@@ shouldn't ignore count
    (let ((result (dlib:copy-hash-table collection))
	  (val))
      (with-hash-table-iterator (get-entry collection)
	(loop
	  (multiple-value-bind (ok hash-key value) (get-entry)
	    (if ok
		(progn
		  (setf val (if key
				(funcall key value)
				value))
		  (when (cond
			  (test (funcall test old-item val))
			  (test-not (not (funcall test-not old-item val)))
			  (t
			   (funcall (fdefinition (hash-table-test collection))
				    old-item val)))
		    (setf (gethash hash-key result) new-item)))
		(return)))))
      result))
  ;; I think for structs it's not too unreasonable to do a shallow copy.
  (:method (new-item old-item (collection structure-object)
	    &key from-end test test-not start end count key)
    (declare (ignore from-end start end count)) ;; @@@ shouldn't ignore these
    (let ((result (dlib:shallow-copy-object collection)))
      (loop
	:with value
	:for slot :in (mop:class-slots (class-of collection))
	:when (slot-boundp collection (mop:slot-definition-name slot))
	:do
	   (setf value (slot-value collection (mop:slot-definition-name slot))
		 value (if key
			   (funcall key value)
			   value))
	   (when (cond
		   (test (funcall test old-item value))
		   (test-not (not (funcall test-not old-item value)))
		   (t (eql old-item value)))
	     (setf (slot-value result (mop:slot-definition-name slot))
		   new-item)))
      result))
  ;; I don't think these copying methods make sense for class-oids
  #|
  (:method (new-item old-item (collection standard-object)
	    &key from-end test test-not start end count key)
    (declare (ignore from-end start end count)) ;; @@@ shouldn't ignore these
    (let (val)
      (loop :for slot :in (mop:class-slots (class-of collection))
        :when (slot-boundp collection (mop:slot-definition-name slot))
        :do
	(setf value (slot-value collection (mop:slot-definition-name slot))
	      value (if key
		      (funcall key value)
		      value))
	  (when (cond
		  (test (funcall test old-item val))
		  (test-not (not (funcall test-not old-item val)))
		  (t (eql old-item val)))
	    (setf (slot-value collection (mop:slot-definition-name slot))
		  new-item)))
      collection))
  |#
  )

#|
(defmethod osubstitute (new-item old-item (collection container)
			&key from-end
			  (test nil test-p)
			  (test-not nil test-not-p)
			  (start nil start-p)
			  (end nil end-p)
			  count key)
  (call-with-start-end-test osubstitute
    (new-item old-item (container-data collection)
     :from-end from-end :count count :key key)))
|#

(defgeneric onsubstitute (new-item old-item collection
			  &key from-end test test-not start end count key)
  (:documentation
   "Return a copy of the collection in which each element that satisfies the
test has been replaced with ‘new-item’.")
  (:method (new-item old-item (collection list)
	    &key from-end
	      (test nil test-p)
	      (test-not nil test-not-p)
	      (start nil start-p)
	      (end nil end-p)
	      count key)
    (call-with-start-end-test nsubstitute
      (new-item old-item collection :from-end from-end :count count :key key)))
  (:method (new-item old-item (collection vector)
	    &key from-end
	      (test nil test-p)
	      (test-not nil test-not-p)
	      (start nil start-p)
	      (end nil end-p)
	      count key)
    (call-with-start-end-test nsubstitute
      (new-item old-item collection :from-end from-end :count count :key key)))
  (:method (new-item old-item (collection sequence)
	    &key from-end
	      (test nil test-p)
	      (test-not nil test-not-p)
	      (start nil start-p)
	      (end nil end-p)
	      count key)
    (call-with-start-end-test nsubstitute
      (new-item old-item collection :from-end from-end :count count :key key)))
  (:method (new-item old-item (collection hash-table)
	    &key from-end test test-not start end count key)
    (declare (ignore from-end start end count)) ;; @@@ shouldn't ignore count
    (let (val)
      (with-hash-table-iterator (get-entry collection)
	(loop
	  (multiple-value-bind (ok hash-key value) (get-entry)
	    (if ok
		(progn
		  (setf val (if key
				(funcall key value)
				value))
		  (when (cond
			  (test (funcall test old-item val))
			  (test-not (not (funcall test-not old-item val)))
			  (t
			   (funcall (fdefinition (hash-table-test collection))
				    old-item val)))
		    (setf (gethash hash-key collection) new-item)))
		(return)))))
      collection))
  (:method (new-item old-item (collection structure-object)
	    &key from-end test test-not start end count key)
    (declare (ignore from-end start end count)) ;; @@@ shouldn't ignore these
    (loop
      :with value
      :for slot :in (mop:class-slots (class-of collection))
      :when (slot-boundp collection (mop:slot-definition-name slot))
      :do
	 (setf value (slot-value collection (mop:slot-definition-name slot))
	       value (if key
			 (funcall key value)
			 value))
	 (when (cond
		 (test (funcall test old-item value))
		 (test-not (not (funcall test-not old-item value)))
		 (t (eql old-item value)))
	   (setf (slot-value collection (mop:slot-definition-name slot))
		 new-item)))
      collection)
  (:method (new-item old-item (collection standard-object)
	    &key from-end test test-not start end count key)
    (declare (ignore from-end start end count)) ;; @@@ shouldn't ignore these
    (loop
      :with value
      :for slot :in (mop:class-slots (class-of collection))
      :when (slot-boundp collection (mop:slot-definition-name slot))
      :do
      (setf value (slot-value collection (mop:slot-definition-name slot))
	    value (if key
		      (funcall key value)
		      value))
      (when (cond
	      (test (funcall test old-item value))
	      (test-not (not (funcall test-not old-item value)))
	      (t (eql old-item value)))
	(setf (slot-value collection (mop:slot-definition-name slot))
	      new-item)))
    collection))

(defmethod onsubstitute (new-item old-item (collection container)
			 &key from-end
			   (test nil test-p)
			   (test-not nil test-not-p)
			   (start nil start-p)
			   (end nil end-p)
			   count key)
  (call-with-start-end-test onsubstitute
    (new-item old-item (container-data collection)
     :from-end from-end :count count :key key)))

(defgeneric oconcatenate (first-collection &rest collections)
  (:documentation
   "Return a collection containing all the individual elements of all the
collections. If they are ordered collections, the elements will be in the order
that they were supplied. The resulting collection is of the same type as the
first collection. For some")
  (:method ((first-collection list) &rest collections)
    (when (not (every #'(lambda (_) (typep _ 'sequence)) collections))
      (error
       "I don't know how to concatenate things that aren't sequences ~
        to a list."))
    (apply #'concatenate 'list first-collection collections))
  (:method ((first-collection vector) &rest collections)
    (when (not (every #'(lambda (_) (typep _ 'sequence)) collections))
      (error
       "I don't know how to concatenate things that aren't sequences ~
        to a vector."))
    (apply #'concatenate 'vector first-collection collections))
  (:method ((first-collection sequence) &rest collections)
    (when (not (every #'(lambda (_) (typep _ 'sequence)) collections))
      (error
       "I don't know how to concatenate things that aren't sequences ~
        to a strange sequence type."))
    (apply #'concatenate (type-of first-collection)
	   first-collection collections))
  (:method ((first-collection hash-table) &rest collections)
    (when (not (every #'keyed-collection-p collections))
      (error
       "I don't know how to concatenate things that aren't keyed-collections ~
        to a hash-table."))
    (let ((new-table (ocopy first-collection)))
      (loop :for c :in collections :do
	   (maphash #'(lambda (key value)
			(setf (gethash key new-table) value))
		   c))
      new-table))
  ;; We could do something ridiculous, like defining a new structure or
  ;; class that has the slots of all the collections. But it's not only crazy
  ;; sauce, it isn't even the same type as the arguments.
  ;; (:method ((first-collection structure-object) &rest collections) @@@)
  ;; (:method ((collection standard-object)) @@@)
  )

(defmethod oconcatenate ((first-collection container) &rest collections)
  ;; @@@ Is this right? What should we check about collections?
  (apply #'oconcatenate (container-data first-collection)
	 (loop :for c :in collections
	    :collect (container-data c))))

(defgeneric oconcatenate-as (result-type &rest collections)
  (:documentation
   "Return a collection containing all the individual elements of all the
collections. If they are ordered collections, the elements will be in the order
that they were supplied. The resulting collection is of type RESULT-TYPE.")
  (:method ((result-type (eql 'list)) &rest collections)
    (when (not (every #'(lambda (_) (typep _ 'sequence)) collections))
      (error
       "I don't know how to concatenate things that aren't sequences~
        to a list."))
    (apply #'concatenate 'list collections))
  (:method ((result-type (eql 'string)) &rest collections)
    (when (not (every #'(lambda (_) (typep _ 'sequence)) collections))
      (error
       "I don't know how to concatenate things that aren't sequences~
        to a vector."))
    (apply #'concatenate 'string collections))
  (:method ((result-type (eql 'vector)) &rest collections)
    (when (not (every #'(lambda (_) (typep _ 'sequence)) collections))
      (error
       "I don't know how to concatenate things that aren't sequences~
        to a vector."))
    (apply #'concatenate 'vector collections))
  ;; You'll have to make your own method for a non-standard sequence.
  (:method ((result-type (eql 'hash-table)) &rest collections)
    (when (not (every #'keyed-collection-p collections))
      (error
       "I don't know how to concatenate things that aren't keyed-collections~
        to a hash-table."))
    (let ((new-table
	   ;; Use the first hash table found as a template.
	   (let ((hh (find-if #'hash-table-p collections)))
	     (if hh
		 (make-hash-table
		  :test (hash-table-test hh)
		  :size (hash-table-size hh)
		  :rehash-size (hash-table-rehash-size hh)
		  :rehash-threshold (hash-table-rehash-threshold hh))
		 ;; There's no hash tables. We can just make any old one, but
		 ;; there could be problems.
		 (progn
		   (cerror "I'm fine with that."
			   ;; @@@ This should really be an error type so
			   ;; someone could theoretically auto-continue.
			   "There are no hash tables in collections that I can ~
                          copy. You might lose something.")
		   (make-hash-table))))))
      (loop :for c :in collections :do
	   (maphash #'(lambda (key value)
			(setf (gethash key new-table) value))
		    c))
      new-table))
  ;; We could do something ridiculous, like defining a new structure or
  ;; class that has the slots of all the collections. But it's not only crazy
  ;; sauce, it isn't even the same type as the arguments.
  ;; (:method ((first-collection structure-object) &rest collections) @@@)
  ;; (:method ((collection standard-object)) @@@)
  )

(defmethod oconcatenate-as ((result-type (eql 'container)) &rest collections)
  ;; @@@ Is this right? What should we check about collections?
  (when collections
    (apply #'oconcatenate (container-data (first collections))
	   (loop :for c :in collections
	      :collect (container-data c)))))

#|
(defgeneric omerge (collection ...)
  (:documentation "")
  (:method ((collection XX))
	    ))

(defgeneric oremove-duplicates (collection ...)
  (:documentation "")
  (:method ((collection XX))
	    ))

;; @@@ I think these are really just intersection.

(defgeneric ochoose (value-collection collection)
  (:documentation
   "Returns elements of COLLECTION that satisfy TEST.")
  (:method ((collection XX))
	    ))

(defgeneric ochoose-by (test value-collection collection)
  (:documentation
   "Returns elements of COLLECTION that correspond to those VALUE-COLLECTION
that satisfy TEST."
  (:method ((collection XX))
	    )))

|#

;; This is really another name for remove-if-not.
(defgeneric opick (predicate collection
		   &key from-end start end count key)
  (:documentation
   "Return a new collection with only elemnets of COLLECTION that satisfy
PREDICATE.")
  (:method (predicate (collection list)
	    &key from-end key (start nil start-p) (end nil end-p) count)
    (declare (ignorable start start-p end end-p))
    (call-with-start-and-end remove-if-not
			     (predicate collection
					:from-end from-end
					:count count
					:key key)))
  (:method (predicate (collection vector)
	    &key from-end key (start nil start-p) (end nil end-p) count)
    (declare (ignorable start start-p end end-p))
    (call-with-start-and-end remove-if-not (predicate
					    collection
					    :from-end from-end
					    :count count
					    :key key)))
  (:method (predicate (collection sequence)
	    &key from-end key (start nil start-p) (end nil end-p) count)
    (declare (ignorable start start-p end end-p))
    (call-with-start-and-end remove-if-not (predicate
					    collection
					    :from-end from-end
					    :count count
					    :key key))))

(defmethod opick (predicate (collection container)
		  &key from-end key (start nil start-p) (end nil end-p) count)
  (declare (ignorable start start-p end end-p))
  (call-with-start-and-end opick (predicate
				  (container-data collection)
				  :from-end from-end
				  :count count
				  :key key)))

(defgeneric oremove (item collection
		     &key from-end test test-not start end count key)
  (:documentation "Return a copy of the collection with ITEM removed.")
  (:method (item (collection list)
	    &key from-end key
	      (test nil test-p)
	      (test-not nil test-not-p)
	      (start nil start-p)
	      (end nil end-p)
	      count)
    (declare (ignorable start start-p end end-p test test-not))
    (call-with-start-end-test remove
			      (item collection
				    :from-end from-end
				    :count count
				    :key key)))
  (:method (item (collection vector)
	    &key from-end key
	      (test nil test-p)
	      (test-not nil test-not-p)
	      (start nil start-p)
	      (end nil end-p)
	      count)
    (declare (ignorable start start-p end end-p test test-not))
    (call-with-start-end-test remove (item
				      collection
				      :from-end from-end
				      :count count
				      :key key)))
  (:method (item (collection sequence)
	    &key from-end key
	      (test nil test-p)
	      (test-not nil test-not-p)
	      (start nil start-p)
	      (end nil end-p)
	      count)
    (declare (ignorable start start-p end end-p test test-not))
    (call-with-start-end-test remove (item
				      collection
				      :from-end from-end
				      :count count
				      :key key))))

(defmethod oremove (item (collection container)
		    &key from-end key
		      (test nil test-p)
		      (test-not nil test-not-p)
		      (start nil start-p)
		      (end nil end-p)
		      count)
  (declare (ignorable start start-p end end-p test test-not))
  (call-with-start-end-test remove (item
				    (container-data collection)
				    :from-end from-end
				    :count count
				    :key key)))

(defgeneric oremove-if (predicate collection
			&key from-end start end count key)
  (:documentation
   "Return a copy of the collection with elements removed for which PREDICATE
is true.")
  (:method (predicate (collection list)
	    &key from-end key (start nil start-p) (end nil end-p) count)
    (declare (ignorable start start-p end end-p))
    (call-with-start-and-end remove-if (predicate
					collection
					:from-end from-end
					:count count
					:key key)))
  (:method (predicate (collection vector)
	    &key from-end key (start nil start-p) (end nil end-p) count)
    (declare (ignorable start start-p end end-p))
    (call-with-start-and-end remove-if (predicate
					collection
					:from-end from-end
					:count count
					:key key)))
  (:method (predicate (collection sequence)
	    &key from-end key (start nil start-p) (end nil end-p) count)
    (declare (ignorable start start-p end end-p))
    (call-with-start-and-end remove-if (predicate
					collection
					:from-end from-end
					:count count
					:key key))))

(defmethod oremove-if (predicate (collection container)
		       &key from-end key (start nil start-p) (end nil end-p)
			 count)
  (declare (ignorable start start-p end end-p))
  (call-with-start-and-end remove-if (predicate
				      (container-data collection)
				      :from-end from-end
				      :count count
				      :key key)))

(defgeneric osplit (separator ordered-collection
		    &key omit-empty start end test key #| count |# bag)
  (:documentation
   "Split the ORDERED-COLLECTION into subsequences separated by SEPARATOR.
Return an ORDERED-COLLECTION of the subsequences. SEPARATOR can be a
ORDERED-COLLECTION itself, which means the whole sequence is the separator.
The returned collection might not be the same type you passed in. For example it
might always be a list. But the pieces in that collection should be of the same
type as the one given.
  OMIT-EMPTY - If true, then don't return empty subsequnces.
  START      - Element number to start gathering from. Defaults to 0.
  END        - Element number to end gathering at. Defaults to the end of the
               collection.
  TEST       - A function called with an element which should return true if
               that element is a separator. The SEPARATOR argument is ignored
               if TEST is supplied and non-NIL.
  KEY        - A function called with each element, which should return an
               element to be tested.
")
  (:method (separator (collection list)
	    &key omit-empty test key bag
	      (start nil start-p)
	      (end nil end-p))
    (call-with-start-and-end
     dlib:split-sequence
     (separator collection
		:omit-empty omit-empty :test test :key key :bag bag)))
  (:method (separator (collection vector)
	    &key omit-empty test key bag
	      (start nil start-p)
	      (end nil end-p))
    (call-with-start-and-end
     dlib:split-sequence
     (separator collection
		:omit-empty omit-empty :test test :key key :bag bag)))
  (:method (separator (collection sequence)
	    &key omit-empty test key bag
	      (start nil start-p)
	      (end nil end-p))
    (call-with-start-and-end
     dlib:split-sequence
     (separator collection
		:omit-empty omit-empty :test test :key key :bag bag))))

(defmethod osplit (separator (collection container)
		   &key omit-empty test key bag
		     (start nil start-p)
		     (end nil end-p))
  (call-with-start-and-end
   osplit (separator (container-data collection)
		     :omit-empty omit-empty :test test :key key :bag bag)))

(defgeneric osplit-if (predicate ordered-collection
		       &key omit-empty start end key #| count |#)
  (:documentation
   "Split the ORDERED-COLLECTION into subsequences separated by elements for
which PREDICATE returns true. Return an ORDERED-COLLECTION of the subsequences.
The returned collection might not be the same type you passed in. For example it
might always be a list. But the pieces in that collection should be of the same
type as the one given.
  OMIT-EMPTY - If true, then don't return empty subsequnces.
  START      - Element number to start gathering from. Defaults to 0.
  END        - Element number to end gathering at. Defaults to the end of the
               collection.
  KEY        - A function called with each element, which should return an
               element to be tested.
")
  (:method (predicate (collection list)
	    &key omit-empty key
	      (start nil start-p)
	      (end nil end-p))
    (call-with-start-and-end
     dlib:split-sequence-if
     (predicate collection :omit-empty omit-empty :key key)))
  (:method (predicate (collection vector)
	    &key omit-empty key
	      (start nil start-p)
	      (end nil end-p))
    (call-with-start-and-end
     dlib:split-sequence-if
     (predicate collection :omit-empty omit-empty :key key)))
  (:method (predicate (collection sequence)
	    &key omit-empty key
	      (start nil start-p)
	      (end nil end-p))
    (call-with-start-and-end
     dlib:split-sequence
     (predicate collection :omit-empty omit-empty :key key))))

(defmethod osplit-if (predicate (collection container)
		      &key omit-empty key
			(start nil start-p)
			(end nil end-p))
  (call-with-start-and-end
   osplit-if (predicate (container-data collection)
			:omit-empty omit-empty :key key)))

;; Collection-ified version of replace-subseq from dlib.
(defun oreplace-subseq-as (type target replacement sequence &key count)
  "Return a copy of SEQUECE as a TYPE, but with sub-sequences of TARGET replaced
REPLACEMENT."
  (if (and (> (olength target) 0) (or (not count) (> count 0)))
      (let ((pos 0)
	    (i 0)
	    (n 0)
	    new)
	(loop :while (setf pos (osearch target sequence :start2 i))
	   :do
	   ;;(format t "i = ~a pos = ~a new = ~a~%" i pos new)
	   (setf new (nconc new (list (osubseq sequence i pos) replacement)))
	   (setf i (+ pos (olength target)))
	   ;;(format t "i = ~a pos = ~a new = ~a~%" i pos new)
	   (incf n)
	   :until (and count (>= n count)))
	(setf new (nconc new (list (osubseq sequence i))))
	;;(apply #'concatenate (append '(string) new)))
	;;(apply #'oconcatenate-as (type-of sequence) new)
	(apply #'oconcatenate-as type new)
	)
      (ocopy sequence)))

;; @@@ This is horrible.
(defgeneric oreplace-subseq (target replacement sequence &key count)
  (:documentation
   "Return a copy of SEQUECE but with sub-sequences of TARGET replaced
REPLACEMENT.")
  (:method (target replacement (sequence list) &key count)
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (apply #'oreplace-subseq-as 'list
	   target replacement sequence (list :count count)))
  (:method (target replacement (sequence string) &key count)
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (apply #'oreplace-subseq-as 'string
	   target replacement sequence (list :count count)))
  (:method (target replacement (sequence vector) &key count)
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (apply #'oreplace-subseq-as 'vector
	   target replacement sequence (list :count count))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Array-like

(defgeneric oaref (array &rest subscripts)
  (:documentation "Return an element of an array.")
  (:method ((array vector) &rest subscripts)
    (apply #'aref array subscripts))
  (:method ((array array) &rest subscripts)
    (apply #'aref array subscripts)))

;; We could do elt on lists, sequences, and even struct & classes, but
;; is there a point? Why not just use oelt or something? The performance will
;; likely be worse with this.

(defmethod oaref ((array container) &rest subscripts)
  ;; @@@ I suppose this will only work if the container data is an array, but
  ;; should we do a check here so perhaps we could get a better error?
  (apply #'oaref (container-data array) subscripts))

(defgeneric (setf oaref) (value array &rest subscripts)
  (:documentation
   "Set the element of ARRAY specified by SUBSCRIPTS to VALUE.")
  (:method (value (array vector) &rest subscripts)
    (setf (apply #'aref array subscripts) value))
  (:method (value (array array) &rest subscripts)
    (setf (apply #'aref array subscripts) value)))

(defmethod (setf oaref) (value (array container) &rest subscripts)
  ;; @@@ I suppose this will only work if the container data is an array, but
  ;; should we do a check here so perhaps we could get a better error?
  (setf (apply #'oaref (container-data array) subscripts) value))

(defun oarray-ref (&rest args)
  "Like OAREF, but with the arguments reversed for convenience in pipelines."
  ;; @@@ Is it really permissible to do this? How crappy is the performace?
  (oaref (last args) (nbutlast args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stack-like

;; @@@ Will these even work right?
;; @@@ Why did I make the args reversed from normal push??

(defgeneric opush-element (collection item)
  (:documentation "Add ITEM to COLLECTION and return COLLECTION.")
  (:method ((thing list) item)   (push item thing))
  ;; @@@ Hmmm. This isn't prepending, it's appending. Prepending is too
  ;; expensive for a vector, but appending is too expensive for a list, which
  ;; is maybe why this is a bad idea in the first place.
  (:method ((thing vector) item) (vector-push-extend item thing) thing))

(defmethod opush-element ((collection container) item)
  (opush-element (container-data collection) item))

(defmacro opush (collection item)
  "Prepend ITEM to COLLECTION and return COLLECTION."
  `(setf ,collection (opush-element ,collection ,item)))

(defgeneric opushnew-element (collection item &rest key-args
			      &key key test test-not)
  (:documentation
   "Add ITEM to COLLECTION only if it isn't already the same as any
existing element, and return COLLECTION.")
  (:method ((thing list) item &rest key-args &key key test test-not)
    (declare (ignore key-args))
    (cond
      (test
       (if key
	   (pushnew item thing :key key :test test)
	   (pushnew item thing :test test)))
      (test-not
       (if key
	   (pushnew item thing :key key :test-not test-not)
	   (pushnew item thing :test-not test-not)))))
  (:method ((thing vector) item &rest key-args &key key test test-not)
    (declare (ignorable key test test-not))
    (when (not (apply #'find item thing key-args))
      (vector-push-extend item thing))
    thing))

(defmacro opushnew (collection item &rest key-args &key key test test-not)
  (declare (ignorable key test test-not))
  `(setf ,collection
	 (apply #'opushnew-element ,collection ,item ,key-args)))

(defgeneric opop-element (collection)
  (:documentation
   "Remove the element from COLLECTION and return the element AND
the modified COLLECTION.")
  (:method ((thing list))   (values (pop thing) thing))
  (:method ((thing vector)) (values (vector-pop thing) thing)))

(defmacro opop (collection)
  "Remove the first element from COLLECTION and return the element."
  (dlib:with-names (val new)
    `(multiple-value-bind (,val ,new) (opop-element ,collection)
       (setf ,collection ,new)
       ,val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set-like

;; This is so we can get the decisions outside the body of loop, which should
;; presumably be faster. Also it's nice to factor out the argument choosing.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-member-func ((func &key (finder 'find) type) &body body)
    "Evaluate the body with FUNC set an appropriate member function given KEY,
TEST, and TEST-NOT. Use finder as the name of the member testing function,
which defaults to FIND. Note that this macro is un-hygenic."
    `(labels
	 ((memb (x seq)
	    (declare (type ,type seq))
	    (,finder x seq))
	  (memb-key (x seq)
	    (declare (type ,type seq)
		     (type (function (t) t) key))
	    (,finder x seq :key key))
	  (memb-test (x seq)
	    (declare (type ,type seq)
		     (type (function (t t) t) test))
	    (,finder x seq :test test))
	  (memb-test-not (x seq)
	    (declare (type ,type seq)
		     (type (function (t t) t) test-not))
	    (,finder x seq :test-not test-not))
	  (memb-test-key (x seq)
	    (declare (type ,type seq)
		     (type (function (t) t) key)
		     (type (function (t t) t) test))
	    (,finder x seq :key key :test test))
	  (memb-test-not-key (x seq)
	    (declare (type ,type seq)
		     (type (function (t) t) key)
		     (type (function (t t) t) test-not))
	    (,finder x seq :key key :test-not test-not)))
       (declare (ftype (function (t ,type) t)
		       memb memb-key memb-test memb-test-not memb-test-key
		       memb-test-not-key))
       (let ((,func
	      (cond
		((and test test-not)
		 (error "Both :test and :test-not provided.")
		 #'memb)
		(test
		 (if key #'memb-test-key #'memb-test))
		(test-not
		 (if key #'memb-test-not-key #'memb-test-not))
		(t
		 (if key #'memb-key #'memb)))))
	 (declare (type (function (t ,type) t) ,func))
	 ,@body))))

#|
(defun oint1 (collection-1 collection-2 &key key test test-not)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type vector collection-1)
	   (type vector collection-2)
	   (dynamic-extent key test test-not))
  (let ((count 0) result)
    (declare (type fixnum count)
	     ;; (type (function (t) t) key)
	     ;; (type (function (t t) t) test test-not)
	     )
    (with-member-func (memb :type vector)
      (setf result
	    (loop
	      :for i :from 0 :below (min (length collection-1)
					 (length collection-2))
	      :when (funcall memb (aref collection-1 i) collection-2)
	      :collect (if key
			   (funcall key (aref collection-1 i))
			   (aref collection-1 i))
	      :and :do (incf count))))
      (when (plusp count)
	(make-array count :element-type (array-element-type collection-1)
		    :initial-contents result))))

(defun oint2 (collection-1 collection-2 &key key test test-not)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (dynamic-extent key test test-not))
  (let* ((count 0)
	 (len (min (length collection-1) (length collection-2)))
	 (result (make-array len
			     :element-type (array-element-type collection-1)
			     ;; :initial-element (aref collection-1 0)
			     :adjustable t))
	 (thingy (if key
		     (lambda (a i) (funcall key (aref a i)))
		     (lambda (a i) (aref a i)))))
    (declare (type fixnum count len)
	     (type vector result)
	     (type (function (vector fixnum) t) thingy))
    (with-member-func (memb :type vector)
      (loop
	:for i :from 0 :below len
	:when (funcall memb (aref collection-1 i) collection-2)
	:do
	   (setf (aref result i) (funcall thingy collection-1 i))
	   (incf count))
      (when (plusp count)
	(adjust-array result count
		      :element-type (array-element-type collection-1))))))

It doesn't seem to make much difference if we pre-make an ajustable array,
rather than to accumulate into a list.

So why is this 1 or 2 orders of magnitude slower than the list version?

(defun test-1 (n l)
  (time
   (dotimes (i (round n))
     (let ((l1 (coerce (loop repeat l collect (random 10)) 'vector))
	   (l2 (coerce (loop repeat l collect (+ (random 10) 5)) 'vector)))
       (oint1 l1 l2))))
  (time
   (dotimes (i (round n))
     (let ((l1 (coerce (loop repeat l collect (random 10)) 'vector))
	   (l2 (coerce (loop repeat l collect (+ (random 10) 5)) 'vector)))
       (oint2 l1 l2))))
  (time
   (dotimes (i (round n))
     (let ((l1 (loop repeat l collect (random 10)))
	   (l2 (loop repeat l collect (+ (random 10) 5))))
       (intersection l1 l2)))))
|#

(defgeneric ointersection (collection-1 collection-2 &key key test test-not)
  (:documentation
   "Return a collection that contains every element that occurs in both
COLLECTION-1 and COLLECTION-2.")
  (:method ((collection-1 list)
	    (collection-2 list) &key key test test-not)
    (cond
      (test
       (intersection collection-1 collection-2 :key key :test test))
      (test-not
       (intersection collection-1 collection-2 :key key :test-not test-not))
      (t
       (intersection collection-1 collection-2 :key key))))
  (:method ((collection-1 vector)
	    (collection-2 vector) &key key test test-not)
    (let ((count 0) result)
      (with-member-func (memb :type vector)
	(setf result
	      (loop
		 :for i :from 0 :below (min (length collection-1)
					    (length collection-2))
		 :when (funcall memb (aref collection-1 i) collection-2)
		 :collect (if key
			      (funcall key (aref collection-1 i))
			      (aref collection-1 i))
		 :and :do (incf count))))
      (when (plusp count)
	(make-array count :element-type (array-element-type collection-1)
		    :initial-contents result))))
  (:method ((collection-1 hash-table)
	    (collection-2 hash-table) &key key test test-not)
    (let ((count 0) result)
      (flet ((hash-member (k tab &key key test test-not)
	       (declare (ignore test test-not))
	       ;; We can't really do anything with test/test-not because it's
	       ;; part of the hash-table.
	       (if key (gethash (funcall key k) tab) (gethash k tab))))
	(with-member-func (memb :finder hash-member :type hash-table)
	  (setf result
		(loop
		   :for k :being :the :hash-keys :of collection-1
		   :when (funcall memb k collection-2)
		   :collect (cons k (gethash k collection-1))
		   :and :do (incf count)))))
      (when (plusp count)
	(let ((new-table
	       (make-hash-table
		:test (hash-table-test collection-1)
		:size count
		:rehash-size (hash-table-rehash-size collection-1)
		:rehash-threshold (hash-table-rehash-threshold collection-1))))
	  (loop :for (k . v) :in result
	     :do (setf (gethash k new-table) v))
	  new-table)))))

(defmethod ointersection ((collection-1 container)
			  (collection-2 container) &key key test test-not)
  (ointersection (container-data collection-1)
		 (container-data collection-1)
		 :key key :test test :test-not test-not))

(defgeneric onintersection (collection-1 collection-2 &key key test test-not)
  (:documentation
   "Return COLLECTION-1 destructively modified to contain every element that
occurs in both COLLECTION-1 and COLLECTION-2."))

(defgeneric oset-difference (collection-1 collection-2 &key key test test-not)
  (:documentation
   "Returns a collection of elements of COLLECTION-1 that do not appear in
COLLECTION-2."))

(defgeneric onset-difference (collection-1 collection-2 &key key test test-not)
  (:documentation
   "Returns COLLECTION-1 possibly destructively modified to remove elements
that do not appear in COLLECTION-2."))

(defgeneric oset-exclusive-or (collection-1 collection-2 &key key test test-not)
  (:documentation
   "Return a collection of elements that appear in exactly one of COLLECTION-1
and COLLECTION-2"))

(defgeneric onset-exclusive-or (collection-1 collection-2 &key key test test-not)
  (:documentation
   "Return a COLLECTION-1 possibly destructively modified to contain only
elements that appear in exactly one of COLLECTION-1 and COLLECTION-2"))

(defgeneric osubsetp (collection-1 collection-2 &key key test test-not)
  (:documentation
   "Return true if every elemnt of COLLECTION-1 matches eome element of
COLLECTION-2."))

(defgeneric ounion (collection-1 collection-2 &key key test test-not)
  (:documentation
   "Return a collection that contains every element that occurs in either
COLLECTION-1 or COLLECTION-2.")
  (:method ((collection-1 list)
	    (collection-2 list) &key key test test-not)
    (cond
      (test
       (union collection-1 collection-2 :key key :test test))
      (test-not
       (union collection-1 collection-2 :key key :test-not test-not))
      (t
       (union collection-1 collection-2 :key key))))
  (:method ((collection-1 vector)
	    (collection-2 vector) &key key test test-not)
    ;; @@@ we should probably just use a hash table if the vectors are long
    ;; enough
    (let ((count 0) to-add (result #()))
      (declare (type fixnum count)
	       (type vector result)
	       (type list to-add))
      (multiple-value-bind (shorter longer)
	  (if (< (length collection-1) (length collection-2))
	      (values collection-1 collection-2)
	      (values collection-2 collection-1))
	(declare (type vector shorter longer))
	(with-member-func (memb :type vector)
	  (setf to-add
		(loop
		  :for i :from 0 :below (length longer)
		  :when (not (funcall memb (aref longer i) shorter))
		  :collect (if key
			       (funcall key (aref longer i))
			       (aref longer i))
		  :and :do (incf count))))
	(when (plusp count)
	  (setf result
		(make-array (+ count (length shorter))
			    :element-type (array-element-type collection-1)))
	  (setf (subseq result 0) to-add
		(subseq result count) shorter))
	result)))
  (:method ((collection-1 hash-table)
	    (collection-2 hash-table) &key key test test-not)
    (when test-not
      (error "TEST-NOT can't be used for hash tables."))
    ;; We could use ‘test’ for the new table's test, but that would be a
    ;; would really be a somewhat different meaning, that might no be intuitive.
    (when test
      (error "TEST can't be used for hash tables."))
    (let ((result (dlib:copy-hash-table collection-1)))
      (macrolet
	  ((looper (value-expr)
	     `(with-hash-table-iterator (get-entry collection-2)
		(loop
		  (multiple-value-bind (ok hash-key value) (get-entry)
		    (if ok
			(setf (gethash hash-key result) ,value-expr)
			(return)))))))
	(if key
	    (looper (funcall key value))
	    (looper value)))
      result)))

(defmethod ounion ((collection-1 container)
		   (collection-2 container) &key key test test-not)
  (ounion (container-data collection-1)
	  (container-data collection-1)
	  :key key :test test :test-not test-not))

(defgeneric onunion (collection-1 collection-2 &key key test test-not)
  (:documentation
   "Return COLLECTION-1 possibly destructively modfied so that contains every
element that occurs in either COLLECTION-1 or COLLECTION-2."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a template

(defun make-collection-template (name)
  "Write a file named 'NAME.lisp' that is a template for making a new 
collection type."
  (let ((file-name (dlib:s+ name ".lisp")))
    (with-open-file (stream file-name :direction :output)
      (format stream ";;
;; ~(~a~).lisp - Template for making a new collection type.
;;

;; 1. Decide if your collection is a container:
;;
;;    A container is a box around another collection type which is already
;;    implemented.
;;
;;    If so, just subclass it from container, make sure to put something in
;;    in the container-data slot, or override maybe even the container-data
;;    method, and you're probably done.
;;

(defclass ~(~:*~a~) (container ?maybe-something-else?)
  ....)

;; 2. Decide if your collection is a:
;;
;;      keyed-collection     A collection with explicit keys.
;;                           Example: hash-table.
;;
;;      ordered-collection   A collection with a specific order, and thus can be
;;   			     indexed by the natural numbers.
;;                           Example: vector.

(defclass ~(~:*~a~) (keyed-collection)
  ...
  (:documentation \"A collection of ~:*~@:(~a~)s...\"))

;; 3. Implement all the methods you want.
;;
;; Certain methods only make sense for certain type of collections. But, some
;; methods that aren't stable (i.e. give different results for the same input)
;; are still useful. For example, map and concatentate of un-ordered
;; collections are not necessarily stable, but can be useful.
;;
;; Note that the docstrings are mostly here for reference. You could get rid of
;; them, since they're on the generic function, or even customize them if
;; there's something unusual about the method.
;;
;; My opinion is you should only implement the methods you think you will use.
;; Writing code that never gets used, is not only pointless, but adds future
;; maintenance overhead. On the other hand, for a wide audience, you may not
;; know how it's going to be used, so it's nice to implement as many as you can.

" name)
;; (defun x ()
;;   (progn
;;     (progn
;;       (progn
	;; It's pathetic how crap Emacs is formatting Lisp code after 30 years!
	(loop :for sym :in *methods* :do
	   (when (and (fboundp sym)
		      (typep (symbol-function sym) 'generic-function))
	     (let ((ll (dlib:argument-list sym))
		   (*print-pretty* nil)
		   (*print-escape* nil))
	     (loop :for cc :in '(collection ordered-collection keyed-collection
				 collection-1 collection-2)
		:do
		(setf ll (substitute `(,cc ,name) cc ll)))
	     (format stream "(defmethod ~(~s~) ~(~w~)~%  ~s~%  )~%~%"
		     sym ll (documentation sym 'function)))))
	(format stream ";; End~%"))
      (format t "Wrote ~a.~%" file-name)))

;; EOF
