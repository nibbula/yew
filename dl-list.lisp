;;
;; dl-list - a doubly-linked list
;;

;; People tell me you should never use doubly-linked list.
;; I'm not quite sure I believe them, but after this there is a zipper-ish
;; structure that does just about the same thing while taking less space,
;; like about by half. I'm not sure about performance though.
;;
;; This actually demonstrates some palpable flaws in Common Lisp!
;; - sequences should be generic (and still be fast)
;;   i.e. objects all the way down (like dylan).
;; - Classes can't really be self referential

(defpackage :dl-list
  (:documentation "The nefarious doubly-linked list anti-pattern.")
  (:use :cl :collections)
  (:export
   #:dl-list
   #:dl-node
   #:dl-content
   #:dl-prev
   #:dl-next
   #:make-dl-list
   #:dl-push
   #:dl-pop
   #:dl-length
   #:dl-nth
   #:dl-last
   #:dl-list-do
   #:dl-list-do-element
   #:dl-list-do-backward
   #:dl-list-do-backward-element
   #:dl-list-to-list
   ))
(in-package :dl-list)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

; I want to be able to specifiy the types like this:
;    (prev :initarg :prev :accessor dl-prev :initform nil
; 	 :type (or null dl-list-type))
;    (next :initarg :next :accessor dl-next :initform nil
; 	 :type (or null dl-list-type))
; but it doesn't work. So what should I do? For now I just omit the types.

(defclass dl-list (ordered-collection)
  ((prev :initarg :prev :accessor %dl-prev :initform nil)
   (next :initarg :next :accessor %dl-next :initform nil)
   (content :initarg :content :accessor %dl-content :initform nil))
  (:documentation "A doubly linked list."))

;; I don't really know if specifing the type will make things faster or not.
;; @@@ Perhaps I could test it out.
(deftype dl-node () '(or null dl-list))

(defun dl-prev (lst)
  (cond
    ((typep lst 'dl-list)
     (%dl-prev lst))
    ((null lst)
      nil)
    (t
     (error "~w is not a dl-list." lst))))

(defun set-dl-prev (lst newval)
  (setf (%dl-prev lst) newval))

(defsetf dl-prev set-dl-prev)

(defun dl-next (lst)
  (cond
    ((typep lst 'dl-list)
     (%dl-next lst))
    ((null lst)
      nil)
    (t
     (error "~w is not a dl-list." lst))))

(defun set-dl-next (lst newval)
  (setf (%dl-next lst) newval))

(defsetf dl-prev set-dl-prev)

(defun dl-content (lst)
  (cond
    ((typep lst 'dl-list)
     (%dl-content lst))
    ((null lst)
      nil)
    (t
     (error "~w is not a dl-list." lst))))

(defun set-dl-content (lst newval)
  (setf (%dl-content lst) newval))

(defsetf dl-content set-dl-content)

(defmacro dl-push (lst obj)
  "Push an element onto the front of the list, modifying it."
;  (declare (type dl-list lst))
  `(let ((new (make-instance 'dl-list :next ,lst :content ,obj)))
     (when ,lst
       (setf (%dl-prev ,lst) new))
     (setf ,lst new)))

(defmacro dl-pop (lst)
  "Pop an element off from the front of the list, modifying it."
;  (declare (type dl-list lst))
  `(if ,lst
       (prog1 (%dl-content ,lst)
	 (setf ,lst (%dl-next ,lst))
	 (when ,lst
	   (setf (%dl-prev ,lst) nil)))
       nil))

(defun dl-length (lst)
  "Return the length of a dl-list."
  (let ((result 0))
    (loop :for i = lst :then (%dl-next i)
       :while i
       :do (incf result))
    result))

(defun dl-nth (n list)
  "Return the Nth element of a dl-list."
  (let ((i 0) (e list))
    (loop
       :while (and e (< i n))
       :do (incf i)
       (setf e (%dl-next e)))
    e))

(defun dl-last (list &optional (n 1))
  "Return the N th element from the end, where N defaults to 1, hence the last."
  (let ((i 0) (e list) (last list))
    (loop
       :while e
       :do
       (when (>= i n)
	 (setf last (%dl-next last)))
       (incf i)
       (setf e (%dl-next e)))
    last))

(defun make-dl-list (&optional from-sequence)
  "Construct a dl-list from a sequence."
  (if (> (length from-sequence) 0)
      (let ((l nil))
	(etypecase from-sequence
	  (list
	   (loop :for i :in (reverse from-sequence)
	      :do (dl-push l i)))
	  (vector
	   (loop :for i :across (reverse from-sequence)
	      :do (dl-push l i))))
	l)
      nil))

(defun dl-list-do (list func)
  "Call FUNC with the content each successive element of LIST."
  (declare (type dl-list list))
  (loop :for i = list :then (%dl-next i)
     :do (funcall func (%dl-content i))
     :while (dl-next i)))

(defun dl-list-do-element (list func)
  "Call FUNC with each successive element of LIST."
  (declare (type dl-list list))
  (loop :for i = list :then (%dl-next i)
     :do (funcall func i)
     :while (%dl-next i)))

(defun dl-list-do-backward (list func)
  "Call FUNC with the content of each element of LIST, going backwards."
  (declare (type dl-list list))
  (loop :for i = list :then (%dl-prev i)
	:do (funcall func (%dl-content i))
	:while (%dl-prev i)))

(defun dl-list-do-backward-element (list func)
  "Call FUNC with each element of LIST, going backwards."
  (declare (type dl-list list))
  (loop :for i = list :then (%dl-prev i)
	:do (funcall func i)
	:while (%dl-prev i)))

(defun dl-list-to-list (list)
  "Call FUNC with the content each successive element of LIST."
  (declare (type dl-list list))
  (loop :for i = list :then (%dl-next i)
     :collect (%dl-content i)
     :while (dl-next i)))

;; @@@ Actually, it could be able to be readable, right?
(defmethod print-object ((obj dl-list) stream)
  "Print a dl-list in an unreadable way."
  (print-unreadable-object (obj stream)
    (write-string "DL-LIST " stream)
    (let ((as-list (dl-list-to-list obj)))
      (pprint-logical-block (stream as-list :prefix "(" :suffix ")")
	(loop
	   :initially (when (and as-list (car as-list))
			(write (car as-list) :stream stream))
	   :for e :in (cdr as-list) :do
	   (pprint-newline :fill stream)
	   (write-char #\space stream)
	   (write e :stream stream))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collection methods

(defmethod emptyp ((thing dl-list))
  "Return true if there are no objects in the collection."
  (not (or (%dl-prev thing) (%dl-next thing)
	   (%dl-content thing))))

(defmethod oelt ((thing dl-list) n)
  "Return the element of COLLECTION specified by KEY."
  (dl-nth n thing))

(defmethod olength ((thing dl-list))
  "Return the length of a COLLECTION."
  (dl-length thing))

(defmethod omap (function (thing dl-list))
  "Return a sequence of the results of applying FUNCTION to successive elements
of COLLECTION. The sequence returned is usually a list unless COLLECTION is a
sequence."
  (loop :for i = thing :then (%dl-next i)
     :collect (funcall function (%dl-content i))
     :while (dl-next i)))

(defmethod omapn (function (thing dl-list))
  "Apply FUNCTION to successive elements of COLLECTION. Do not collect return
values."
  (dl-list-do thing function))

(defmethod mappable-p ((collection dl-list)) t)
(defmethod keyed-collection-p ((collection dl-list)) nil)

(defmacro map-collections (list action collections)
  "Not hygienic!!" 
  `(catch 'done
     (let ((i ,list))
       (loop :for c :in ,collections :do
	  (omapn (lambda (x)
		   ,action
		   (setf i (%dl-next i))
		   (when (not (dl-next i))
		     (throw 'done nil)))
		 c)))))

(defmethod omap-into ((mutable-collection dl-list) function &rest collections)
  "Apply FUNCTION to each object in the COLLECTIONs and store the results in
MUTABLE-COLLECTION. FUNCTION is a function of one argument that returns an
object appropriate to be element of a collection of MUTABLE-COLLECTION.
Returns MUTABLE-COLLECTION.

If COLLECTIONS are SEQUENCEs, the elements are applied in the sequence order."
  (map-collections mutable-collection
		   (setf (%dl-content i) (funcall function x))
		   collections)
  mutable-collection)

(defmethod oevery (function &rest collections)
  "Return true if FUNCTION returns true for every element of COLLECTIONS."
  (catch 'done
    (loop :for c :in collections :do
       (let ((i c))
	 (when (not (funcall function (%dl-content i)))
	   (throw 'done nil))
	 (setf i (%dl-next i))
	 (when (not (dl-next i))
	   (throw 'done nil))))
    t))

(defmethod oany (function &rest collections)
  "Return true if FUNCTION returns true for any element of COLLECTIONS."
  (catch 'done
    (loop :for c :in collections :do
       (let ((i c))
	 (when (funcall function (%dl-content i))
	   (throw 'done t))
	 (setf i (%dl-next i))
	 (when (not (dl-next i))
	   (throw 'done nil))))
    nil))

(defmethod ocopy ((collection dl-list))
  "Return new collection with the same elements as COLLECTION."
  (make-dl-list collection))

(defmethod ofill ((thing dl-list) item &key start end)
  "Replaces the elements of SEQUENCE bounded by START and END with ITEM."
  (block nil
    (let ((l thing)
	  (i 0))
      (when (>= i start)
	(setf (%dl-content l) item))
      (setf l (%dl-next l))
      (when (or (not (dl-next l)) (and end (>= i end)))
	(return thing))
      (incf i))))

(defmethod osubseq ((thing dl-list) start &optional end)
  "OSUBSEQ creates a sequence that is a copy of the subsequence of sequence
bounded by START and END."
  (block nil
    (let ((l thing)
	  (new nil)
	  (i 0))
      (when (>= i start)
	(dl-push new (%dl-content l)))
      (setf l (%dl-next l))
      (when (or (not (dl-next l)) (and end (>= i end)))
	(return new))
      (incf i))))

#|
(defmethod osort ((collection dl-list) predicate &key key)
  "Destructively sort a collection according to the order determined by the
predicate function. PREDICATE should return non-NIL if ARG1 is to precede ARG2.
As in other collection functions, KEY is a function that is given the collection
element, and should return a value to be given to PREDICATE."
  )

(defmethod ofind (item (collection dl-list) &key from-end start end key test test-not)
  "Search for an element of the COLLECTION bounded by START and END that
satisfies the test TEST or TEST-NOT, as appropriate."
  )

(defmethod oposition (item (collection dl-list) &key from-end test test-not start end key)
  "Return the index of the element that satisfies the TEST in COLLECTION.
Return the leftmost if FROM-END is true, or of the rightmost if FROM-END is
false. If no element satifies the test, NIL is returned."
  )

(defmethod osplit (separator (ordered-collection dl-list) &key omit-empty start end test key)
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

(defmethod oaref (array &rest subscripts)
  "Return an element of an array."
  )

(defmethod opush-element ((collection dl-list) item)
  "Add ITEM to COLLECTION and return COLLECTION."
  )

(defmethod opushnew-element ((collection dl-list) item &rest key-args &key key test test-not)
  "Add ITEM to COLLECTION only if it isn't already the same as any
existing element, and return COLLECTION."
  )

(defmethod opop-element ((collection dl-list))
  "Remove the element from COLLECTION and return the element AND
the modified COLLECTION."
  )

(defmethod ointersection ((collection-1 dl-list) (collection-2 dl-list) &key key test test-not)
  "Return a collection that contains every element that occurs in both
COLLECTION-1 and COLLECTION-2."
  )

(defmethod onintersection ((collection-1 dl-list) (collection-2 dl-list) &key key test test-not)
  "Return COLLECTION-1 destructively modified to contain every element that
occurs in both COLLECTION-1 and COLLECTION-2."
  )

(defmethod oset-difference ((collection-1 dl-list) (collection-2 dl-list) &key key test test-not)
  "Returns a collection of elements of COLLECTION-1 that do not appear in
COLLECTION-2."
  )

(defmethod onset-difference ((collection-1 dl-list) (collection-2 dl-list) &key key test test-not)
  "Returns COLLECTION-1 possibly destructively modified to remove elements
that do not appear in COLLECTION-2."
  )

(defmethod oset-exclusive-or ((collection-1 dl-list) (collection-2 dl-list) &key key test test-not)
  "Return a collection of elements that appear in exactly one of COLLECTION-1
and COLLECTION-2"
  )

(defmethod onset-exclusive-or ((collection-1 dl-list) (collection-2 dl-list) &key key test test-not)
  "Return a COLLECTION-1 possibly destructively modified to contain only
elements that appear in exactly one of COLLECTION-1 and COLLECTION-2"
  )

(defmethod osubsetp ((collection-1 dl-list) (collection-2 dl-list) &key key test test-not)
  "Return true if every elemnt of COLLECTION-1 matches eome element of
COLLECTION-2."
  )

(defmethod ounion ((collection-1 dl-list) (collection-2 dl-list) &key key test test-not)
  "Return a collection that contains every element that occurs in either
COLLECTION-1 or COLLECTION-2."
  )

(defmethod onunion ((collection-1 dl-list) (collection-2 dl-list) &key key test test-not)
  "Return COLLECTION-1 possibly destructively modfied so that contains every
element that occurs in either COLLECTION-1 or COLLECTION-2."
  )
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; @@@ how 'bout some tests?

#| 
Here's what I came up with after after reading some crap about the 
zipper data structure and not really understanding it.

(3 2 1) (4 5 6 7 8) = 1 2 3 4 5 6 7
^        ^                  ^

next
(4 3 2 1) (5 6 7 8) = 1 2 3 4 5 6 7
^          ^                  ^

prev
(2 1) (3 4 5 6 7 8) = 1 2 3 4 5 6 7
^      ^
|#

#| @@@ NOT DONE YET

(defclass zlist ()
  ((left  :initarg :left  :accessor zl-left  :initform nil :type list)
   (right :initarg :right :accessor zl-right :initform nil :type list)
   (head  :initarg :head  :accessor zl-head  :initform nil :type list)
   (tail  :initarg :tail  :accessor zl-tail  :initform nil :type list))
  (:documentation "A zipper list."))

(defmethod initialize-instance :after ((z zlist) &rest initargs
				       &key initial-contents
				       &allow-other-keys)
  (declare (type list initial-contents))
  (when initial-contents
    (setf (zl-left z) (reverse initial-contents))
    (setf (zl-head z) (last (zl-left z)))))

(defun zlist (&rest args)
  "Gratuitous abbreviation."
  (make-instance 'zlist :initial-contents args))

(defmacro zl-push (obj z)
  "Push an element onto the front of the list, modifying it."
  (declare (type zlist z))
  `(if (zl-head ,z)
       ;; Non-empty, so cons on to end of head, which is actually the beginning.
       (setf (cdr (zl-head ,z)) (cons ,obj nil))
       ;; Empty list, just set the head and left.
       (setf (zl-left ,z) (list ,obj)
	     (zl-head ,z) (zl-left ,z))))

(defmacro zl-pop (lst)
  "Pop an element off from the front of the list, modifying it."
  (declare (type zlist lst))
  `(prog1 (zl-head ,lst)
     (setf (car (zl-head ,lst)) nil
	   (zl-head ,lst) (last (zl-left ,lst)))))

(defmacro zl-next (lst)
  "Go to the next element. Modifies the list."
  )

(defmacro zl-prev (lst)
  "Go to the previous element. Modifies the list."
  )

(defun zl-list-do (lst func)
  "Call func with each successive element of the list."
  (declare (type dl-list lst))
  (loop for i = lst then (zl-next i)
	do (funcall func (zl-content i))
	while (dl-next i)))

(defun zl-list-do-backward (lst func)
  "Call func with each element of the list, going backwards."
  (declare (type dl-list lst))
  (loop for i = lst then (dl-prev i)
	do (funcall func (dl-content i))
	while (dl-prev i)))

;; @@@ Actually, it could be able to be readable, right???
(defmethod print-object ((obj zl-list) stream)
  "Print a dl-list in an unreadable way."
  (print-unreadable-object (obj stream :type t :identity t)
    (write-char #\( stream)
    (loop for i = obj then (zl-next i)
	  do (write (zl-content i) :stream stream)
	  while (and (zl-next i) (write-char #\space stream)))
    (write-char #\) stream)))

|#

;; EOF
