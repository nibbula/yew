;;
;; collections.lisp - Piles of junk.
;;

;; This isn't necessarily efficient or well designed, it's just practical and
;; straightforward, because I really just need it to work. It could at least
;; probably use more macrology to make the methods and things.  Of course, by
;; nature, treating various data structures uniformly has many potential
;; performance pitfalls.

(defpackage :collections
  (:documentation
   "So it seems like I'm doing this again. These aren't so much for the methods
defined in here, but it's *really* so you can define your own methods which work
somewhat orthogonally with system classes. The ‘o’ prefix is rather ugly.
We should entertain other naming ideas.")
  (:use :cl)				; Please don't add any dependencies.
  (:nicknames :o)			; probably too presumptuous
  (:export
   #:collection
   #:container #:container-data
   #:emptyp
   #:oelt
   #:oitem
   #:olength
   #:omap
   #:omapn
   #:omapk
   #:mappable
   #:keyed-collection-p
   #:omap-into
   #:oevery
   #:oany
   #:ocopy
   #:ofill
   #:ofill-with
   #:ofill-range-with
   #:osubseq
   #:oslice
   #:oslice-from
   #:oreduce
   #:ocount
   #:oreverse
   #:osort
   #:osort-by
   #:ofind
   #:ofind-with-key
   #:oposition
   #:osearch
   #:omismatch
   #:oreplace
   #:osubstitute
   #:oconcatenate
   #:omerge
   #:oremove
   #:oremove-duplicates
   #:oaref
   #:opush
   #:opushnew
   #:opop
   #:ointersection #:onintersection
   #:oset-difference #:onset-difference
   #:oset-exclusive-or #:onset-exclusive-or
   #:osubsetp
   #:ounion #:onunion
  ))
(in-package :collections)

(defclass collection ()
  ()
  (:documentation "A bag of stuff."))

(defclass container (collection)
  ;; In some way having this data slot is stupid and unnecessary, because you
  ;; can just get the data through the other methods, but it makes it easy to
  ;; define sub-types. Anyway you can just make it be IDENTITY or something?
  ((data :initarg :data
	 :accessor container-data
	 :documentation "Collection of data."))
  (:documentation "More words for things."))

(defclass keyed-collection (collection)
  ()
  (:documentation "A collection with explicit keys."))

;; aka sequence
(defclass ordered-collection (collection)
  ()
  (:documentation
   "A collection with a specific order, and thus can be indexed by the natural
numbers."))

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

(defgeneric mappable (collection)
  (:documentation "Return true if the COLLECTION can be iterated with OMAP.")
  (:method ((collection list))             t)
  (:method ((collection vector))	   t)
  (:method ((collection sequence))	   t)
  (:method ((collection hash-table))	   t)
  (:method ((collection structure-object)) t)
  (:method ((collection standard-object))  t))

(defmethod mappable ((collection container))
  (mappable (container-data collection)))

(defgeneric keyed-collection-p (collection)
  (:method ((collection list))             nil)
  (:method ((collection vector))	   nil)
  (:method ((collection sequence))	   nil)
  (:method ((collection hash-table))	   t)
  (:method ((collection structure-object)) t)
  (:method ((collection standard-object))  t))

(defmethod keyed-collection-p ((collection container))
  (keyed-collection-p (container-data collection)))

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
  (make-instance (type-of collection)
		 :container-data (ocopy (container-data collection))))

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
  (make-instance (type-of collection)
		 :container-data
		 (osubseq (container-data collection) start end)))

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

(defgeneric ocount (collection ...)
  (:documentation "")
  (:method ((collection XX))
	    ))

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
  (osort (container-data collection) predicate :key key))

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
    (apply 'position
	   `(,item ,collection
	     :from-end ,from-end :test ,test :test-not ,test-not :key ,key
	     ,@(when start-p `(:start ,start))
	     ,@(when end-p `(:end ,end)))))
  (:method (item (collection vector)
	    &key from-end test test-not key
	      (start nil start-p)
	      (end nil end-p))
    (apply 'position
	   `(,item ,collection
	     :from-end ,from-end :test ,test :test-not ,test-not :key ,key
	     ,@(when start-p `(:start ,start))
	     ,@(when end-p `(:end ,end)))))
  (:method (item (collection sequence)
	    &key from-end test test-not key
	      (start nil start-p)
	      (end nil end-p))
    (apply 'position
	   `(,item ,collection
	     :from-end ,from-end :test ,test :test-not ,test-not :key ,key
	     ,@(when start-p `(:start ,start))
	     ,@(when end-p `(:end ,end))))))

(defmethod oposition (item (collection container)
		      &key from-end test test-not key
			(start nil start-p)
			(end nil end-p))
  (apply #'oposition
	 `(,item (container-data ,collection)
		 :from-end ,from-end :test ,test :test-not ,test-not :key ,key
		 ,@(when start-p `(:start ,start))
		 ,@(when end-p `(:end ,end)))))

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

(defgeneric oconcatenate (collection ...)
  (:documentation "")
  (:method ((collection XX))
	    ))

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

(defun oarray-ref (&rest args)
  "Like OAREF, but with the arguments reversed for convenience in pipelines."
  ;; @@@ Is it really permissible to do this? How crappy is the performace?
  (oaref (last args) (nbutlast args)))

(defgeneric opush (collection item)
  (:documentation ""))

(defgeneric opushnew (collection item)
  (:documentation ""))

(defgeneric opop (collection)
  (:documentation ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-like

;; intersection, nintersection
;; set-difference, nset-difference
;; set-exclusive-or, nset-exclusive-or
;; subsetp
;; union, nunion

;; EOF
