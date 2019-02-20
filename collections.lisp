;;
;; collections.lisp - Piles of junk.
;;

;; This isn't necessarily efficient or well designed, it's just practical and
;; straightforward, because I really just need it to work. It could at least
;; probably use more macrology to make the methods and things. Of course, by
;; nature, treating various data structures uniformly has many potential
;; performance pitfalls.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro stfu-defpackage (name &body body)
    `(if (find-package ,name)
	 (let (#+sbcl (*on-package-variance* '(:warn (,name) :error t)))
	   (dlib:without-warning
	       (defpackage ,name
		 ,@body)))
	 (dlib:without-warning
	     (defpackage ,name
	       ,@body)))))

;; @@@ The ‘o’ prefix is rather ugly. We should entertain other naming ideas.

(eval-when (:compile-toplevel :load-toplevel :execute)
(stfu-defpackage :collections
  (:documentation
   "Generic collection functions. These aren't so much for the methods defined
in here, but it's really so you can define your own methods which work
somewhat orthogonally with system classes. Be warned that using things in here
can be very slow compared to the similar CL sequence functions. There's some
pretty foolish implementations in here, in the cause of orthogonality.
Especially the parts where we rather clownishly dress up hash tables and
structs as sequences. Also we really need the MOP for stuff.")
  (:use :cl)	   ; Please don't add any dependencies.
  (:nicknames :o)) ; too presumptuous, but maybe we could remove the 'o'?
)

(in-package :collections)

;; This is so the generic functions that one might want to specialize, are
;; separate, and we can use them to generate a template.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *methods*
    '(emptyp
      oelt
      olength
      omap
      omapk
      omapn
      mappable-p
      collection-p
      keyed-collection-p
      ordered-collection-p
      omap-into
      oevery
      oany
      ocopy
      ofill
      osubseq
      oreduce
      ocount
      oreverse
      osort
      ofind
      ofind-if
      oposition
      oposition-if
      osearch
      omismatch
      oreplace
      osubstitute
      oconcatenate
      omerge
      oremove
      oremove-duplicates
      osplit
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
      container-data
      oitem
      ofill-with
      ofill-range-with
      oslice
      oslice-from
      osort-by
      ofind-with-key
      osplit
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
  (:method ((collection structure-object))   nil)
  (:method ((collection standard-object))    nil)
  (:method ((collection ordered-collection)) t))

(defmethod ordered-collection-p ((collection container))
  (ordered-collection-p (container-data collection)))

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
isn't bound."
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
;; (:method (function (collection vector))
;;   (progn (map nil function collection) collection))
  (:method (function (collection sequence))
    ;;(progn (map nil function collection) collection))
    (map (type-of collection) function collection))
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
		 (slot-value collection (mop:slot-definition-name slot))
		 ))))
  (:method (function (collection standard-object))
    (loop :for slot :in (mop:class-slots (class-of collection))
       :collect
       (funcall function
		(and
		 (slot-boundp collection (mop:slot-definition-name slot))
		 (slot-value collection (mop:slot-definition-name slot)))))))

(defmethod omap (function (collection container))
  (omap function (container-data collection)))

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
    (mapcan function collection))
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

;; @@@ I think I would actually like an omapkn too. And maybe an onapkin.

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

(defgeneric oevery (function &rest collections)
  (:documentation
"Return true if FUNCTION returns true for every element of COLLECTIONS."))

(defgeneric oany (function &rest collections)
  (:documentation
"Return true if FUNCTION returns true for any element of COLLECTIONS."))

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

#|
@@@ You know the business...

(defgeneric oreduce (collection ...)
  (:documentation "")
  (:method ((collection XX))
	    ))
|#

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

#|
(defgeneric oreverse (collection ...)
  (:documentation "")
  (:method ((collection XX))
	    ))
|#

(defgeneric osort (collection predicate &key key)
  (:documentation
   "Destructively sort a collection according to the order determined by the
predicate function. PREDICATE should return non-NIL if ARG1 is to precede ARG2.
As in other collection functions, KEY is a function that is given the collection
element, and should return a value to be given to PREDICATE.")
  (:method ((collection list) predicate &key key)
    (sort collection predicate :key key))
  (:method ((collection vector) predicate &key key)
    (sort collection predicate :key key))
  (:method ((collection sequence) predicate &key key)
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (sort collection predicate :key key)))

(defmethod osort ((collection container) predicate &key key)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (setf (container-data collection)
	(osort (container-data collection) predicate :key key))
  collection)

;; @@@ Should we make sort-by-key also?
(defun osort-by (predicate collection)
  "Destructively sort a collection according to the order determined by the
predicate function. This is just like OSORT but with the arguments reversed for
convenience in pipelines."
  (osort collection predicate))

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
	    &key from-end test test-not key
	      (start nil start-p)
	      (end nil end-p))
    (declare (ignorable start start-p end end-p))
    (call-with-start-and-end position (item collection
					    :from-end from-end :test test
					    :test-not test-not :key key)))
  (:method (item (collection vector)
	    &key from-end test test-not key
	      (start nil start-p)
	      (end nil end-p))
    (declare (ignorable start start-p end end-p))
    (call-with-start-and-end position (item collection
					    :from-end from-end :test test
					    :test-not test-not :key key)))
  (:method (item (collection sequence)
	    &key from-end test test-not key
	      (start nil start-p)
	      (end nil end-p))
    (declare (ignorable start start-p end end-p))
    (call-with-start-and-end position (item collection
					    :from-end from-end :test test
					    :test-not test-not :key key))))

(defmethod oposition (item (collection container)
		      &key from-end test test-not key
			(start nil start-p)
			(end nil end-p))
  (declare (ignorable start start-p end end-p))
  (call-with-start-and-end position (item (container-data collection)
					  :from-end from-end :test test
					  :test-not test-not :key key)))

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

#|
(defgeneric osearch (collection ...)
  (:documentation "")
  (:method ((collection XX))
	    ))

(defgeneric omismatch (collection ...)
  (:documentation "")
  (:method ((collection XX))
	    ))

(defgeneric oreplace (collection ...)
  (:documentation "")
  (:method ((collection XX))
	    ))

(defgeneric osubstitute (collection ...)
  (:documentation "")
  (:method ((collection XX))
	    ))
|#

(defgeneric oconcatenate (first-collection &rest collections)
  (:documentation
   "Return a collection containing all the individual elements of all the
collections. If they are ordered collections, the elements will be in the order
that they were supplied. The resulting collection is of the same type as the
first collection. For some")
  (:method ((first-collection list) &rest collections)
    (when (not (every #'(lambda (_) (typep _ 'sequence)) collections))
      (error
       "I don't know how to concatenate things that aren't sequences~
        to a list."))
    (apply #'concatenate 'list first-collection collections))
  (:method ((first-collection vector) &rest collections)
    (when (not (every #'(lambda (_) (typep _ 'sequence)) collections))
      (error
       "I don't know how to concatenate things that aren't sequences~
        to a vector."))
    (apply #'concatenate 'vector first-collection collections))
  (:method ((first-collection sequence) &rest collections)
    (when (not (every #'(lambda (_) (typep _ 'sequence)) collections))
      (error
       "I don't know how to concatenate things that aren't sequences~
        to a strange sequence type."))
    (apply #'concatenate (type-of first-collection)
	   first-collection collections))
  (:method ((first-collection hash-table) &rest collections)
    (when (not (every #'keyed-collection-p collections))
      (error
       "I don't know how to concatenate things that aren't keyed-collections~
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

(defgeneric oremove (collection ...)
  (:documentation "")
  (:method ((collection XX))
	    ))

(defgeneric oremove-duplicates (collection ...)
  (:documentation "")
  (:method ((collection XX))
	    ))
|#

(defgeneric osplit (separator ordered-collection
		    &key omit-empty start end test key #| count |#)
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
	    &key omit-empty test key
	      (start nil start-p)
	      (end nil end-p))
    (call-with-start-and-end
     dlib:split-sequence
     (separator collection
		:omit-empty omit-empty :test test :key key)))
  (:method (separator (collection vector)
	    &key omit-empty test key
	      (start nil start-p)
	      (end nil end-p))
    (call-with-start-and-end
     dlib:split-sequence
     (separator collection
		:omit-empty omit-empty :test test :key key)))
  (:method (separator (collection sequence)
	    &key omit-empty test key
	      (start nil start-p)
	      (end nil end-p))
    (call-with-start-and-end
     dlib:split-sequence
     (separator collection
		:omit-empty omit-empty :test test :key key))))

(defmethod osplit (separator (collection container)
		   &key omit-empty test key
		     (start nil start-p)
		     (end nil end-p))
  (call-with-start-and-end
   osplit (separator (container-data collection)
		     :omit-empty omit-empty :test test :key key)))

#|
(defgeneric opick (collection &rest keys)
  (:documentation
   "Return a collection, with only the elemnets of COLLECTION indicated by
KEYS.")
|#


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

;; @@@ will these even work right?

(defgeneric opush-element (collection item)
  (:documentation "Add ITEM to COLLECTION and return COLLECTION.")
  (:method ((thing list) item)   (push item thing))
  (:method ((thing vector) item) (vector-push-extend item thing) thing))

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
  (dlib:with-unique-names (val new)
    `(multiple-value-bind (,val ,new) (opop-element ,collection)
       (setf ,collection ,new)
       ,val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set-like

(defgeneric ointersection (collection-1 collection-2 &key key test test-not)
  (:documentation
   "Return a collection that contains every element that occurs in both
COLLECTION-1 and COLLECTION-2."))

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
COLLECTION-1 or COLLECTION-2."))

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
	     (let ((ll (dlib:lambda-list sym))
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
