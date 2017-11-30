;;
;; collections.lisp - Piles of junk.
;;

(defpackage :collections
  (:documentation
   "So it seems like I'm doing this again. These aren't so much for the methods
defined in here, but it's *really* so youcan define your own methods which work
somewhat orthogonally with system classes. The ‘o’ prefix is rather ugly.
We should entertain other naming ideas.")
  (:use :cl)
  (:nicknames :o)
  (:export
   #:collection
   #:container #:container-data
   #:emptyp
   #:oelt
   #:olength
   #:omap
   #:omapn
   #:mappable
   #:omap-into
   #:oevery
   #:oany
   #:ofill
   #:osubseq
   #:oreduce
   #:ocount
   #:oreverse
   #:osort
   #:ofind
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

(defgeneric emptyp (collection)
  (:documentation "Return true if there are no objects in the collection.")
  (:method ((thing list)) 	(null thing))
  (:method ((thing vector))     (zerop (length thing)))
  (:method ((thing sequence))   (zerop (length thing)))
  (:method ((thing hash-table)) (zerop (hash-table-count thing)))
  (:method ((thing container))  (emptyp (container-data thing))))

(defun slot-element (object name)
  "Return the value of the slot NAME in object, or NIL if it's doesn't exist or
isn't bound."
  (let ((slot (find (string name) (mop:class-slots (class-of object))
		    :test #'equalp)))
    (and slot
	 (slot-boundp object (mop:slot-definition-name slot))
	 (slot-value object (mop:slot-definition-name slot)))))

;; This is probably only defined for sequences and keyed-collections
(defgeneric oelt (collection key)
  (:documentation "Return the element of COLLECTION specified by KEY.")
  (:method ((thing list) key) 	       (nth key thing))
  (:method ((thing vector) key)        (aref thing key))
  (:method ((thing sequence) key)      (elt thing key))
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
  (:method ((thing container) key)     (oelt (container-data thing) key)))

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
    (length (mop:class-slots (class-of collection))))
  (:method ((collection container))
    (olength (container-data collection))))

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
    (progn (maphash #'(lambda (k v) (funcall function (vector k v))) collection)))
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
		 (slot-value collection (mop:slot-definition-name slot))))))
  (:method (function (collection container))
    (omap function (container-data collection))))

(defgeneric omapn (function collection)
  (:documentation "Apply FUNCTION to successive elements of COLLECTION.")
  (:method (function (collection list))
    (mapcan function collection))
;; (:method (function (collection vector))
;;   (progn (map nil function collection) collection))
  (:method (function (collection sequence))
    (map nil function collection))
  (:method (function (collection hash-table))
    (progn (maphash #'(lambda (k v) (funcall function (vector k v))) collection)))
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
		 (slot-value collection (mop:slot-definition-name slot))))))
  (:method (function (collection container))
    (omapn function (container-data collection))))

(defgeneric mappable (collection)
  (:documentation "Return true if the COLLECTION can be iterated with OMAP.")
  (:method ((collection list))             t)
  (:method ((collection vector))	   t)
  (:method ((collection sequence))	   t)
  (:method ((collection hash-table))	   t)
  (:method ((collection structure-object)) t)
  (:method ((collection standard-object))  t)
  (:method ((collection container)) (mappable (container-data collection))))

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
    (apply #'map-into mutable-collection function collections))
  (:method ((mutable-collection container) function &rest collections)
    (apply #'map-into (container-data mutable-collection) function collections)))

(defgeneric oevery (function &rest collections)
  (:documentation
"Return true if FUNCTION returns true for every element of COLLECTIONS."))

(defgeneric oany (function &rest collections)
  (:documentation
"Return true if FUNCTION returns true for any element of COLLECTIONS."))

;; Sequence-like functions

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
	     collection))
  (:method ((collection container) item &key start end)
    (fill (container-data collection) item :start start :end end))
  )

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
    (subseq collection start end))
  (:method ((collection container) start &optional end)
    (subseq (container-data collection) start end)))

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

(defgeneric osort (collection ...)
  (:documentation "")
  (:method ((collection XX))
	    ))

(defgeneric ofind (collection ...)
  (:documentation "")
  (:method ((collection XX))
	    ))

(defgeneric oposition (collection ...)
  (:documentation "")
  (:method ((collection XX))
	    ))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Array-like

(defgeneric oaref (array &rest subscripts)
  (:documentation "Return an element of an array."))

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
;; subsetp::
;; union, nunion

;; EOF
