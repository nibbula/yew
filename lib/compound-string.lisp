;;;
;;; compound-string.lisp - Compound string.
;;;

(defpackage :compound-string
  (:documentation
   "A compound string is a string made of of other strings. This provides a
compound-string class and collection and string methods for it.")
  (:use :cl :dlib :collections :ochar :ostring :char-util :terminal)
  (:export
   #:compound-string
   #:compound-string-pieces
   #:make-compound-string
   ))
(in-package :compound-string)

;; This is just very simple and basic, and isn't designed for high performance
;; with a lot of pieces. It's probably best to think of it as relatively
;; immutable, although nothing enforces that.

(defclass compound-string (ordered-collection ostring)
  ((pieces
    :initarg :pieces :accessor compound-string-pieces :initform nil :type list
    :documentation "A list of the pieces."))
  (:documentation "A string which can be stored in separate pieces."))

(defun make-compound-string (&rest args)
  "Return compound string with the pieces. The arguments can be strings or
lists of strings."
  (make-instance 'compound-string :pieces (flatten args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collection methods

(defmethod emptyp ((cs compound-string))
  ;; "Return true if there are no objects in the collection."
  (null (compound-string-pieces cs)))

(defmethod olength ((collection compound-string))
  ;; "Return the length of a COLLECTION."
  ;; This means that you had better not make a compound string a piece of itself!
  (reduce #'+ (compound-string-pieces collection) :key #'olength))

(defun piece-index (cs index)
  "Return the piece, the index in that piece for the overall index, and the
offset of the start of the piece."
  (loop :with offset = 0
     :for p :in (compound-string-pieces cs)
     :do
     (if (< index (+ offset (olength p)))
	 (return-from piece-index (values p (- index offset) offset))
	 (incf offset (olength p))))
  (error "Array index ~d for ~s, is too big. It should be a non-negative ~
          integer below ~s." index cs (olength cs)))

(defmethod oelt ((cs compound-string) key)
  (check-type key integer)
  (multiple-value-bind (piece index) (piece-index cs key)
    (oelt piece index)))

(defmethod olength-at-least-p (n (cs compound-string))
  (>= (olength cs) n))

(defmethod olast ((cs compound-string))
  (let ((piece (car (last (compound-string-pieces cs)))))
    (oelt piece (1- (olength piece)))))

(defmethod omap (function (cs compound-string))
  (make-instance 'compound-string
		 :pieces (loop :for p :in (compound-string-pieces cs)
			       :collect (omap function p))))

(defmethod omapk (function (cs compound-string))
  (omap function cs))

(defmethod omapn (function (cs compound-string))
  (prog1 nil
    (loop :for p :in (compound-string-pieces cs)
      :do (omap function p))))

(defmethod mappable-p ((collection compound-string)) t)
(defmethod collection-p ((collection compound-string)) t)
(defmethod keyed-collection-p ((collection compound-string)) nil)
(defmethod ordered-collection-p ((collection compound-string)) t)

#|
(defmethod omap-into (mutable-collection function &rest collections)
  "Apply FUNCTION to each object in the COLLECTIONs and store the results in
MUTABLE-COLLECTION. FUNCTION is a function of one argument that returns an
object appropriate to be element of a collection of MUTABLE-COLLECTION.
Returns MUTABLE-COLLECTION.

If COLLECTIONS are SEQUENCEs, the elements are applied in the sequence order."
  )

(defmethod oevery (function &rest collections)
  "Return true if FUNCTION returns true for every element of COLLECTIONS."
  )

(defmethod oany (function &rest collections)
  "Return true if FUNCTION returns true for any element of COLLECTIONS."
  )

|#

(defmethod ocopy ((cs compound-string))
  "Return new collection with the same elements as COLLECTION."
  (make-instance 'compound-string
		 :pieces (loop :for p :in (compound-string-pieces cs)
			    :collect (ocopy p))))

;; (defmethod ofill ((collection compound-string) item &key start end)
;;   "Replaces the elements of SEQUENCE bounded by START and END with ITEM."
;;   )

(defmethod osubseq ((cs compound-string) start &optional end)
  (let* ((len (olength cs))
	 (sub-len (- (or end len) start)))
    (make-instance
     'compound-string
     :pieces
     (loop
	:with offset = 0
	:and i = start
	:and result
	:for piece :in (compound-string-pieces cs)
	:do
	(when (< i (+ offset (olength piece)))
	  (if (<= (olength piece) sub-len)
	      (return (list (osubseq piece start (+ start sub-len))))
	      (push (osubseq piece (- i offset)) result)))
	(incf offset (olength piece))
	:finally (return (nreverse result))))))

#|
(defmethod ocount (item (collection compound-string) &key from-end start end key test test-not)
  "Return the number of elements of COLLECTION, bounded by START
and END, that satisfy the TEST."
  )

(defmethod ocount-if (predicate (collection compound-string) &key from-end start end key)
  "Return the number of elements of COLLECTION, bounded by START
and END, that satisfy the PREDICATE."
  )

(defmethod osort ((collection compound-string) predicate &key key)
  "Destructively sort a collection according to the order determined by the
predicate function. PREDICATE should return non-NIL if ARG1 is to precede ARG2.
As in other collection functions, KEY is a function that is given the collection
element, and should return a value to be given to PREDICATE."
  )

|#

(defmethod ofind (item (cs compound-string)
		  &key from-end start end key test test-not)
  "Search for an element of the COLLECTION bounded by START and END that
satisfies the test TEST or TEST-NOT, as appropriate."
  (let (result)
    (if (or start end)
	(let ((offset 0) #| full-len |# piece-len)
	  (when (not end)
	    ;; (setf end (setf full-len (olength cs))))
	    (setf end (olength cs)))
	  (omapn (lambda (piece)
		   (when (and (<= start (+ offset
					   (setf piece-len (olength piece))))
			      (setf result
				    (ofind item piece
					   :start (- start offset)
					   :end (- end offset)
					   :key key :test test
					   :test-not test-not
					   :from-end from-end))
			      (return-from ofind result)))
		   (incf offset piece-len)
		   (when (>= offset end)
		     (return-from ofind nil)))
		 (if from-end
		     (reverse (compound-string-pieces cs))
		     (compound-string-pieces cs))))
	;; Simple case with no start or end where we don't have to keep track
	;; of the offset.
	(omapn (lambda (x)
		 (when (setf result
			     (ofind item x
				    :key key :test test :test-not test-not
				    :from-end from-end))
		   (return-from ofind result)))
	       (if from-end
		   (reverse (compound-string-pieces cs))
		   (compound-string-pieces cs)))))
  nil)

#|

(defmethod ofind-if (predicate (collection compound-string) &key from-end start end key)
  "Search for an element of the COLLECTION bounded by START and END that
satisfies the PREDICATE."
  )

|#

(defmethod oposition (item (cs compound-string)
		      &key from-end test test-not start end key)
  "Return the index of the element that satisfies the TEST in COLLECTION.
Return the leftmost if FROM-END is true, or of the rightmost if FROM-END is
false. If no element satifies the test, NIL is returned."
  (let ((offset 0) result full-len piece-len)
    (when (not end)
      (setf end (setf full-len (olength cs))))
    (when (not start)
      (setf start 0))
    (when from-end
      (setf offset (or full-len (setf full-len (olength cs)))))
    (flet ((backward (piece)
	     (setf piece-len (olength piece))
	     (when (and (>= end (- offset piece-len))
			(setf result
			      (oposition item piece
					 :start
					 (max 0 (- start offset piece-len))
					 :end
					 (min (- end (- offset piece-len))
					      piece-len)
					 :key key :test test
					 :test-not test-not
					 :from-end t)))
	       (return-from oposition (+ (- offset piece-len) result)))
	     (decf offset piece-len)
	     (when (zerop offset)
	       (return-from oposition nil)))
	   (forward (piece)
	     (when (and (<= start offset (+ offset
					    (setf piece-len (olength piece))))
			(>= end offset)
			(setf result
			      (oposition item piece
					 :start (max 0 (- start offset))
					 :end (min piece-len
						   (- end offset))
					 :key key :test test
					 :test-not test-not
					 :from-end nil)))
	       (return-from oposition (+ offset result)))
	     (incf offset piece-len)
	     (when (>= offset end)
	       (return-from oposition nil))))
      (if from-end
	  (omapn #'backward (reverse (compound-string-pieces cs)))
	  (omapn #'forward (compound-string-pieces cs)))))
  nil)

#|

(defmethod oposition-if (predicate (collection compound-string) &key from-end start end key)
  "Return the index of the element that satisfies the TEST in COLLECTION.
Return the leftmost if FROM-END is true, or of the rightmost if FROM-END is
false. If no element satifies the test, NIL is returned."
  )

(defmethod osearch ((collection-1 compound-string) (collection-2 compound-string) &rest args &key from-end test test-not key start1 start2 end1 end2)
  "Searches collection-2 for a subsequence that matches collection-1.
If from-end is true, the index of the leftmost element of the rightmost
matching subsequence is returned."
  )

(defmethod oconcatenate (first-collection &rest collections)
  "Return a collection containing all the individual elements of all the
collections. If they are ordered collections, the elements will be in the order
that they were supplied. The resulting collection is of the same type as the
first collection. For some"
  )

(defmethod oconcatenate-as (result-type &rest collections)
  "Return a collection containing all the individual elements of all the
collections. If they are ordered collections, the elements will be in the order
that they were supplied. The resulting collection is of type RESULT-TYPE."
  )

(defmethod opick (predicate (collection compound-string) &key from-end start end count key)
  "Return a new collection with only elemnets of COLLECTION that satisfy
PREDICATE."
  )

(defmethod oremove (item (collection compound-string) &key from-end test test-not start end count key)
  "Return a copy of the collection with ITEM removed."
  )

(defmethod oremove-if (predicate (collection compound-string) &key from-end start end count key)
  "Return a copy of the collection with elements removed for which PREDICATE
is true."
  )

(defmethod osplit (separator (ordered-collection compound-string) &key omit-empty start end test key)
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
"
  )

(defmethod osplit-if (predicate (ordered-collection compound-string) &key omit-empty start end key)
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
"
  )

(defmethod oreplace-subseq (target replacement sequence &key count)
  "Return a copy of SEQUECE but with sub-sequences of TARGET replaced
REPLACEMENT."
  )

(defmethod oaref (array &rest subscripts)
  "Return an element of an array."
  )

(defmethod opush-element ((collection compound-string) item)
  "Add ITEM to COLLECTION and return COLLECTION."
  )

(defmethod opushnew-element ((collection compound-string) item &rest key-args &key key test test-not)
  "Add ITEM to COLLECTION only if it isn't already the same as any
existing element, and return COLLECTION."
  )

(defmethod opop-element ((collection compound-string))
  "Remove the element from COLLECTION and return the element AND
the modified COLLECTION."
  )

(defmethod ointersection ((collection-1 compound-string) (collection-2 compound-string) &key key test test-not)
  "Return a collection that contains every element that occurs in both
COLLECTION-1 and COLLECTION-2."
  )

(defmethod onintersection ((collection-1 compound-string) (collection-2 compound-string) &key key test test-not)
  "Return COLLECTION-1 destructively modified to contain every element that
occurs in both COLLECTION-1 and COLLECTION-2."
  )

(defmethod oset-difference ((collection-1 compound-string) (collection-2 compound-string) &key key test test-not)
  "Returns a collection of elements of COLLECTION-1 that do not appear in
COLLECTION-2."
  )

(defmethod onset-difference ((collection-1 compound-string) (collection-2 compound-string) &key key test test-not)
  "Returns COLLECTION-1 possibly destructively modified to remove elements
that do not appear in COLLECTION-2."
  )

(defmethod oset-exclusive-or ((collection-1 compound-string) (collection-2 compound-string) &key key test test-not)
  "Return a collection of elements that appear in exactly one of COLLECTION-1
and COLLECTION-2"
  )

(defmethod onset-exclusive-or ((collection-1 compound-string) (collection-2 compound-string) &key key test test-not)
  "Return a COLLECTION-1 possibly destructively modified to contain only
elements that appear in exactly one of COLLECTION-1 and COLLECTION-2"
  )

(defmethod osubsetp ((collection-1 compound-string) (collection-2 compound-string) &key key test test-not)
  "Return true if every elemnt of COLLECTION-1 matches eome element of
COLLECTION-2."
  )

(defmethod ounion ((collection-1 compound-string) (collection-2 compound-string) &key key test test-not)
  "Return a collection that contains every element that occurs in either
COLLECTION-1 or COLLECTION-2."
  )

(defmethod onunion ((collection-1 compound-string) (collection-2 compound-string) &key key test test-not)
  "Return COLLECTION-1 possibly destructively modfied so that contains every
element that occurs in either COLLECTION-1 or COLLECTION-2."
  )
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ostring methods

(defun compound-string-p (object)
  "Return true if this is a compound string."
  (typep object 'compound-string))

;; @@@ Should this be like simplify or should it be allowed to return complex
;; strings?
;; (defgeneric ostring (thing)
;;   (:documentation "Make an ostring from the thing.")
;;   (:method ((thing t)) (string thing)))

(defmethod ochar:osimplify ((cs compound-string))
  (omap #'osimplify cs))

(defmethod ochar ((cs compound-string) index)
  (oelt cs index))

(defmethod (setf ochar) (value (cs compound-string) index)
  (multiple-value-bind (piece index #|offset|#) (piece-index cs index)
    (setf (ochar piece index) value)))

;; Is there some better way than this bullshit? Without messing up the lambda
;; list with a &rest and without using apply??
;; You can do this:
;;   (apply #'string-upcase `(,string ,@(when start-p `(:start ,start))
;; 	  			       ,@(when end-p `(:end end))))
;; but there's some overhead of consing and applying.

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

#|

(defgeneric ostring-upcase (string &key start end)
  (:documentation "")
  (:method ((string string) &key (start nil start-p) (end nil end-p))
    (call-with-start-and-end string-upcase (string))))

(defgeneric ostring-downcase (string &key start end)
  (:documentation "")
  (:method ((string string) &key (start nil start-p) (end nil end-p))
    (call-with-start-and-end string-downcase (string))))

(defgeneric ostring-capitalize (string &key start end)
  (:documentation "")
  (:method ((string string) &key (start nil start-p) (end nil end-p))
    (call-with-start-and-end string-capitalize (string))))

(defgeneric onstring-upcase (string &key start end)
  (:documentation "")
  (:method ((string string) &key (start nil start-p) (end nil end-p))
    (call-with-start-and-end nstring-upcase (string))))

(defgeneric onstring-downcase (string &key start end)
  (:documentation "")
  (:method ((string string) &key (start nil start-p) (end nil end-p))
    (call-with-start-and-end nstring-downcase (string))))

(defgeneric onstring-capitalize (string &key start end)
  (:documentation "")
  (:method ((string string) &key (start nil start-p) (end nil end-p))
    (call-with-start-and-end nstring-capitalize (string))))

(defgeneric ostring-trim (character-bag string)
  (:documentation "")
  (:method (character-bag (string string))
    (string-trim character-bag string)))

(defgeneric ostring-left-trim (character-bag string)
  (:documentation "")
  (:method (character-bag (string string))
    (string-left-trim character-bag string)))

(defgeneric ostring-right-trim (character-bag string)
  (:documentation "")
  (:method (character-bag (string string))
    (string-right-trim character-bag string)))

(defgeneric ostring= (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string= string-1 string-2)))

(defgeneric ostring/= (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string/= string-1 string-2)))

(defgeneric ostring< (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string< string-1 string-2)))

(defgeneric ostring> (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string> string-1 string-2)))

(defgeneric ostring<= (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string<= string-1 string-2)))

(defgeneric ostring>= (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string>= string-1 string-2)))

(defgeneric ostring-equal (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string-equal string-1 string-2)))

(defgeneric ostring-not-equal (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string-not-equal string-1 string-2)))

(defgeneric ostring-lessp (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string-lessp string-1 string-2)))

(defgeneric ostring-greaterp (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string-greaterp string-1 string-2)))

(defgeneric ostring-not-greaterp (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string-not-greaterp string-1 string-2)))

(defgeneric ostring-not-lessp (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string-not-lessp string-1 string-2)))

(defgeneric ostring-to-span (string)
  (:documentation "Convert STRING to a span.")
  (:method ((string string)) (list string)))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I/O

(defmethod print-object ((obj compound-string) stream)
  (macrolet ((with-quotes (() &body body)
	       `(progn
		  (when *print-escape*
		    (write-char #\" stream))
		  ,@body
		  (when *print-escape*
		    (write-char #\" stream)))))
    (cond
      (*print-readably*
       (if *read-eval*
	   (format stream "#.~s"
		   `(make-instance 'compound-string
				   :pieces ',(compound-string-pieces obj)))
	   (progn
	     (call-next-method))))
      (t
       (with-quotes ()
	 (omapn (lambda (x)
		  ;; @@@ or should it be print-object ?
		  (write x :stream stream :escape nil))
		(compound-string-pieces obj)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; char-util methods

;; @@@ There should really be a generic ostring method, but where to put it
;; since ostring can't depend on char-util or vice versa?
(defmethod graphemes ((string compound-string))
  ;; (dbugf :fatchar "compound grapheme ~s ~s~%" (type-of string) string)
  (let (result)
    (loop :for p :in (compound-string-pieces string)
       :do
       (when (and p (not (zerop (olength p))))
	 (do-graphemes (g p :result-type (type-of (oelt p 0)) :key osimplify)
	   (push g result))))
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; termminal methods

;; @@@ This seems like it should be somewhere else.
;; Currently only RL uses this, or compound string at all.
(defmethod terminal-write-string (tty (string compound-string) &key start end)
  (when (or start end)
    (error "I'm so very sorry but I don't support start or end yet!"))
  (omapn (_ (terminal-write-string tty _)) (compound-string-pieces string)))

;; End
