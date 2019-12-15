;;
;; dl-list - a doubly-linked list
;;

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
   #:dl-length-at-least-p
   #:dl-nth-element
   #:dl-nth
   #:dl-last
   #:dl-list-do
   #:dl-list-do-element
   #:dl-list-do-backward
   #:dl-list-do-backward-element
   #:dl-list-to-list
   #:dl-list-check
   ))
(in-package :dl-list)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

;; This seems to demonstrate some palpable issues in Common Lisp.
;; - We would like sequences to be generic (and still be fast).
;; - Classes can't be explicitly self referential.
;;
;; I think I might like to be able to specifiy the types like this:
;;    (prev :initarg :prev :accessor dl-prev :initform nil
;; 	 :type (or null dl-list-type))
;;    (next :initarg :next :accessor dl-next :initform nil
;; 	 :type (or null dl-list-type))
;; but it doesn't work. So what should I do? For now I just omit the types.

(defclass dl-list (ordered-collection)
  ((prev :initarg :prev :accessor %dl-prev :initform nil)
   (next :initarg :next :accessor %dl-next :initform nil)
   (content :initarg :content :accessor %dl-content :initform nil))
  (:documentation "A doubly linked list."))

;; I don't really know if specifing the type will make things faster or not.
;; @@@ Perhaps I could test it out.
(deftype dl-node () '(or null dl-list))

;; These wrappers around the slot accesors are just so we can, perhaps
;; pointlessly pretend NIL is the empty DL-LIST.

(defun dl-prev (list)
  "Return the previous item in LIST."
  (ctypecase list
    (dl-list (%dl-prev list))
    (null nil)))

(defun set-dl-prev (list new-value)
  "Set the previous item in LIST to NEW-VALUE."
  (setf (%dl-prev list) new-value))

(defsetf dl-prev set-dl-prev
  "Set the previous item.")

(defun dl-next (list)
  "Return the previous item in LIST."
  (ctypecase list
    (dl-list (%dl-next list))
    (null nil)))

(defun set-dl-next (list new-value)
  "Set the next item in LIST to NEW-VALUE."
  (setf (%dl-next list) new-value))

(defsetf dl-next set-dl-next
  "Set the previous item.")

(defun dl-content (list)
  "Return the content of the LIST item."
  (ctypecase list
    (dl-list (%dl-content list))
    (null nil)))

(defun set-dl-content (list new-value)
  "Set the content of the item LIST to NEW-VALUE."
  (setf (%dl-content list) new-value))

(defsetf dl-content set-dl-content "Set the content.")

(defmacro dl-push (list obj)
  "Push an element onto the front of the list, modifying it."
  (dlib:with-unique-names (new list-value)
    `(let* ((,list-value ,list)
	    (,new (make-instance 'dl-list :next ,list-value :content ,obj)))
       (dlib:with-muffled-notes
	 (when ,list
	   (setf (%dl-prev ,list-value) ,new)))
       (setf ,list ,new))))

(defmacro dl-pop (list)
  "Pop an element off from the front of the list, modifying it."
  (dlib:with-unique-names (list-value)
    `(let ((,list-value ,list))
       (if ,list-value
	   (prog1 (%dl-content ,list-value)
	     (when (setf ,list (%dl-next ,list-value))
	       (setf (%dl-prev ,list) nil)))
	   nil))))

(defun dl-length (list)
  "Return the length of a DL-LIST."
  (let ((result 0))
    (loop :for e = list :then (%dl-next e)
       :while e
       :do (incf result))
    result))

(defun dl-length-at-least-p (list n)
  "Return true if the length of a DL-LIST is at least N."
  (let ((result 0))
    (loop :for e = list :then (%dl-next e)
       :while (and e (< result n))
       :do (incf result))
    (= result n)))

(defun dl-nth-element (n list)
  "Return the Nth element of a DL-LIST."
  (let ((i 0) (e list))
    (loop
       :while (and e (< i n))
       :do (incf i)
       (setf e (%dl-next e)))
    e))

(defun dl-nth (n list)
  "Return the content of the Nth element of a DL-LIST."
  (let ((e (dl-nth-element n list)))
    (when e
      (%dl-content e))))

(defun dl-last (list &optional (n 1))
  "Return the Nth element from the end, where N defaults to 1, hence the last."
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
  (let (new)
    (ctypecase from-sequence
      (list
       (loop
	  :with node
	  :for i :in from-sequence
	  :do
	  (if node
	      (setf (%dl-next node)
		    (make-instance 'dl-list :prev node :content i)
		    node (%dl-next node))
	      (setf new (make-instance 'dl-list :content i)
		    node new))))
      (vector
       (loop :for i :from (length from-sequence) :downto 1
	  :do (dl-push new (aref from-sequence i)))))
    new))

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

(defun dl-list-do-backward (list-element func)
  "Call FUNC with the content of each element, going backwards from
LIST-ELEMENT. So to go backward over the whole list, you have to give it the
last element."
  (declare (type dl-list list-element))
  (loop :for i = list-element :then (%dl-prev i)
	:do (funcall func (%dl-content i))
	:while (%dl-prev i)))

(defun dl-list-do-backward-element (list-element func)
  "Call FUNC with each element of LIST, going backwards from LIST-ELEMENT."
  (declare (type dl-list list-element))
  (loop :for i = list-element :then (%dl-prev i)
	:do (funcall func i)
	:while (%dl-prev i)))

(defun dl-list-to-list (list &key start end)
  "Convert a DL-LIST to a list. Only include elements from START to END, which
default to the "
  (declare (type dl-list list)
	   (type (or integer null) start end))
  ;; (check-type start (or integer null))
  ;; (check-type end (or integer null))
  (when (not start)
    (setf start 0))
  (loop
     :for n = 0 :then (1+ n)
     :and i = list :then (%dl-next i)
     :if (>= n start)
       :collect (%dl-content i)
     :end ;; necessary??
     :while (and (dl-next i) (or (not end) (<= n end)))))

(defun dl-list-check (list)
  "Verify that internal structure of a DL-LIST is consistent."
  (ctypecase list
    (null t)
    (dl-list
     (when (not (null (%dl-prev list)))
       (error "Previous of the first node is not null."))
     (loop :with e = (%dl-next list)
	:and last
	:and count = 0
	:and already-seen
	:while e
	:do
	(when (and last (not (eq last (%dl-prev e))))
	  (error "Previous pointer of element ~d is messed up." count))
	(when (find e already-seen)
	  (error "Circularity detected at element ~d." count))
	(push e already-seen)
	(setf last e
	      count (1+ count)
	      e (%dl-next e)))
     t)))

(defun dl-list-ok-p (list)
  "Return true if the LIST passes the check."
  ;; @@@ of course error-occurred is too broad, but this is just for testing
  ;; right?
  (or (not (ignore-errors (dl-list-check list))) t))

;; @@@ Actually, it could be able to be readable, right?
(defmethod print-object ((obj dl-list) stream)
  "Print a dl-list in an unreadable way."
  (print-unreadable-object (obj stream)
    (write-string "DL-LIST " stream)
    (pprint-logical-block (stream nil :prefix "(" :suffix ")")
      (write (%dl-content obj) :stream stream)
      (let ((i (%dl-next obj)))
	(loop :with count = 1
	   :while (and i (or (not *print-length*) (< count *print-length*)))
	   :do
	   (pprint-newline :fill stream)
	   (write-char #\space stream)
	   (write (%dl-content i) :stream stream)
	   (setf i (%dl-next i))
	   (incf count))
	(when (and i (%dl-next i))
	  (pprint-newline :fill stream)
	  (write-char #\space stream)
	  (write-string "..." stream))))))

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
  (let ((l thing)
	(new nil)
	(i 0))
    (loop
       (when (>= i start)
	 (dl-push new (%dl-content l)))
       (setf l (%dl-next l))
       (when (or (not (dl-next l)) (and end (>= i end)))
	 (return new))
       (incf i))
    new))

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
