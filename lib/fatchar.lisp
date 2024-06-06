;;;
;;; fatchar.lisp - Characters with attributes.
;;;

(defpackage :fatchar
  (:documentation "Characters with attributes.
Defines a FATCHAR which is a character with color and font attributes.
Define a FATCHAR-STRING as a vector of FATCHARS.
Define a FAT-STRING as a standard-object with a FATCHAR-STRING so we can
specialize.
Define a TEXT-SPAN as a list representation of a FAT-STRING.
")
  (:use :cl :dlib :stretchy :char-util :collections :ochar :ostring :dcolor)
  (:export
   #:+default-char+
   #:fatchar
   #:fatchar-p
   #:make-fatchar
   #:fatchar-c #:fatchar-fg #:fatchar-bg #:fatchar-line #:fatchar-attrs
   #:fatchar-string
   #:fat-string #:fat-string-string
   #:fat-string-p
   #:fatchar-init
   #:copy-fatchar
   #:copy-fatchar-effects
   #:set-fatchar
   #:same-effects
   #:add-attr
   #:remove-attr
   #:fatchar=
   #:fatchar/=
   #:make-fat-string
   #:copy-fat-string
   #:make-fatchar-string
   #:make-fatchar-string-of-length
   #:copy-fatchar-string
   #:string-to-fat-string
   #:fatchar-string-to-string
   #:string-vector
   #:fat-string-to-string
   #:fat-string< #:fat-string> #:fat-string= #:fat-string/=
   #:fat-string<= #:fat-string>= #:fat-string-lessp #:fat-string-greaterp
   #:fat-string-equal #:fat-string-not-equal
   #:fat-string-not-lessp #:fat-string-not-greaterp
   #:span-length
   #:fat-string-to-span
   #:fatchar-string-to-span
   #:span-to-thing
   #:span-to-fat-string #:ß
   #:span-to-fatchar-string
   #:fatten
   #:get-nearest-xterm-color-index
   #:*xterm-256-color-table*
   #:process-ansi-colors
   #:remove-effects
   ))
(in-package :fatchar)

;;(declaim (optimize (speed 3) (safety 0) (debug 1) (space 0) (compilation-speed 0)))
;;(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(deftype grapheme ()
  "What a user thinks of as a character. One or more code points treated as the
smallest unit of written language."
  `(or character string))

(defgeneric copy-grapheme (g) #| sealed |#
  (:documentation "Return a new copy of a grapheme.")
  (:method ((g character)) g)
  (:method ((g string)) (copy-seq g)))

;; @@@ These methods could in theory be better than defering to equal, or
;; using typecase, but are they really in practice?

(defgeneric grapheme= (g1 g2) #| sealed |#
  (:documentation "Return true if the graphemes are equal based on char=.")
  (:method ((g1 character) (g2 character)) (char= g1 g2))
  (:method ((g1 string) (g2 string)) (string= g1 g2))
  (:method ((g1 string) (g2 character))
    (and (= (length g1) 1) (char= (char g1 0) g2)))
  (:method ((g1 character) (g2 string))
    (and (= (length g2) 1) (char= (char g2 0) g1))))

(defgeneric grapheme/= (g1 g2) #| sealed |#
  (:documentation "Return true if the graphemes are equal based on char=.")
  (:method ((g1 character) (g2 character)) (char/= g1 g2))
  (:method ((g1 string) (g2 string)) (string/= g1 g2))
  (:method ((g1 string) (g2 character))
    (or (/= (length g1) 1) (char/= (char g1 0) g2)))
  (:method ((g1 character) (g2 string))
    (and (/= (length g2) 1) (char/= (char g2 0) g1))))

(defgeneric grapheme-equal (g1 g2) #| sealed |#
  (:documentation "Return true if the graphemes are equal based on char-equal.")
  (:method ((g1 character) (g2 character)) (char-equal g1 g2))
  (:method ((g1 string) (g2 string)) (string-equal g1 g2))
  (:method ((g1 string) (g2 character))
    (and (= (length g1) 1) (char-equal (char g1 0) g2)))
  (:method ((g1 character) (g2 string))
    (and (= (length g2) 1) (char-equal (char g2 0) g1))))

(defconstant +default-char+ (code-char 0)
  "Default character value for fatchars.")

(defstruct (fatchar (:copier nil))
  "A character with attributes."
  ;;(c +default-char+ :type character)
  (c +default-char+ :type grapheme)
  (fg nil)
  (bg nil)
  (line 0 :type fixnum)
  (attrs nil :type list))

(defun fatchar-init (c)
  "Initialize a fatchar with the default vaules and return it."
  (setf (fatchar-c     c)	+default-char+
	(fatchar-fg    c)	:white
	(fatchar-bg    c)	:black
	(fatchar-line  c)	0
	(fatchar-attrs c)	nil)
  c)

(defun copy-fatchar-effects (from to)
  "Copy the effects from the fatchar FROM to the fatchar TO and return it."
  (assert (and from to (fatchar-p from) (fatchar-p to)))
  (setf (fatchar-fg    to) (copy-color (fatchar-fg from))
	(fatchar-bg    to) (copy-color (fatchar-bg from))
	(fatchar-line  to) (fatchar-line from)
	(fatchar-attrs to) (copy-list (fatchar-attrs from)))
  to)

;; Moved to color.lisp
;; (defun copy-color (color)
;;   (typecase color
;;     (symbol color)
;;     (list (copy-list color))
;;     (array (copy-seq color))
;;     (t color)))

;; I think we can just use the one made by defstruct?
(defun copy-fatchar (c)
  (declare (type fatchar c))
  (when c
    (make-fatchar
     :c	    (copy-grapheme (fatchar-c c))
     :fg    (copy-color (fatchar-fg c))
     :bg    (copy-color (fatchar-bg c))
     :line  (fatchar-line c)
     :attrs (copy-list (fatchar-attrs c)))))

(defun set-fatchar (from to)
  "Copy the effects from the fatchar FROM to the fatchar TO."
  (assert (and from to (fatchar-p from) (fatchar-p to)))
  (setf (fatchar-c     to) (copy-grapheme (fatchar-c from))
	(fatchar-fg    to) (copy-color (fatchar-fg from))
	(fatchar-bg    to) (copy-color (fatchar-bg from))
	(fatchar-line  to) (fatchar-line from)
	(fatchar-attrs to) (copy-list (fatchar-attrs from))))

(defun same-effects (a b)
  "Return true if the two fatchars have the same colors and attributes."
  (and (equal (fatchar-fg a) (fatchar-fg b))
       (equal (fatchar-bg a) (fatchar-bg b))
       (not (set-exclusive-or (fatchar-attrs a) (fatchar-attrs b)
			      :test #'eq))))

(defun add-attr (attr c)
  "Set ‘attr’ in fatchar ‘c’."
  (pushnew attr (fatchar-attrs c)))

(defun remove-attr (attr c)
  "Remove ‘attr’ from fatchar ‘c’."
  (setf (fatchar-attrs c) (delete attr (fatchar-attrs c))))

(defun same-char (a b)
  "Return true if the two fatchars have the same underlying character."
  (grapheme= (fatchar-c a) (fatchar-c b)))

(defun fatchar= (a b)
  "True if everything about a fatchar is the equivalent."
  (and (grapheme= (fatchar-c a) (fatchar-c b))
       (same-effects a b)
       (= (fatchar-line a) (fatchar-line b))))

(defun fatchar/= (a b)
  (not (fatchar= a b)))

(defmethod ochar= ((char-1 fatchar) (char-2 fatchar))
  (fatchar= char-1 char-2))

;; @@@ Should we really do these lossy comparisons?
(defmethod ochar= ((char-1 character) (char-2 fatchar))
  (grapheme= char-1 (fatchar-c char-2)))

(defmethod ochar= ((char-1 fatchar) (char-2 character))
  (grapheme= (fatchar-c char-1) char-2))

(defmethod ochar-equal ((char-1 character) (char-2 fatchar))
  (grapheme-equal char-1 (fatchar-c char-2)))

(defmethod ochar-equal ((char-1 fatchar) (char-2 character))
  (grapheme-equal (fatchar-c char-1) char-2))

(defmethod ochar-equal ((char-1 fatchar) (char-2 fatchar))
  (grapheme-equal (fatchar-c char-1) (fatchar-c char-2)))

(defmethod ochar/= ((char-1 fatchar) (char-2 fatchar))
  (fatchar/= char-1 char-2))

(defmethod ochar/= ((char-1 fatchar) (char-2 character))
  (grapheme/= (fatchar-c char-1) char-2))

(defmethod ochar/= ((char-1 character) (char-2 fatchar))
  (grapheme/= char-1 (fatchar-c char-2)))

(deftype fatchar-string (&optional size)
  "A string of FATCHARs."
  `(vector fatchar ,size))

;; This is quite wasteful, but how else can I specialize methods?
(defclass fat-string (ostring)
  ((string
    :initarg :string :accessor fat-string-string
    :documentation "A lot of crust around a string."))
  (:documentation "A vector of FATCHAR."))
;;(string (vector) :type fatchar-string)) ; Is this better or worse?

(defun fat-string-p (object)
  "Return true if ‘object’ is a fat-string."
  (typep object 'fat-string))

(defparameter *known-attrs*
  `(:normal :standout :underline :bold :inverse)
  "List of known attributes.")

;; Collection methods

(defmethod olength ((s fat-string))
  (length (fat-string-string s)))

(defun copy-fat-string (string)
  "Make a copy of the fat-string ‘string’."
  (make-instance 'fat-string
		 :string (copy-fatchar-string (fat-string-string string))))

(defun make-fat-string (&key string length initial-element)
  "Make a FAT-STRING. You can supply ‘string’ to make it from a FATCHAR-STRING,
which will be used as the contents, another FAT-STRING whose contents will be
copied, or a normal string. If you give ‘length’, one will be made for you.
If you supply ‘length’, you can also supply ‘initial-element’ that will
be copied to the initial elements of the string."
  (check-type string (or null fatchar-string string fat-string))
  (when (and string length)
    ;; @@@ I guess we could if length is >= the length of the string, and we
    ;; just use string to initialize it. But is that a good idea? or too DWIMish?
    (error "I can't both use your ‘string’, and make one of ‘length’."))
  (make-instance 'fat-string
		 :string
		 (cond
		   (string
		    (etypecase string
		      (fatchar-string string)
		      (fat-string
		       (copy-fatchar-string (fat-string-string string)))
		      (string (make-fatchar-string string))))
		   (length
		    (make-fatchar-string-of-length
		     length :initial-element initial-element)))))

(defmethod oelt ((s fat-string) key)
  (aref (fat-string-string s) key))

(defmethod (setf oelt) (value (s fat-string) key) ;; @@@ ??? test
  (setf (aref (fat-string-string s) key) value))

;; It's probably better to use oelt than oaref if you can.

(defmethod oaref ((s fat-string) &rest subscripts)
  (apply #'aref (fat-string-string s) subscripts))

(defmethod (setf oaref) (value (s fat-string) &rest subscripts) ;; @@@ ??? test
  ;; @@@ Perhaps we could make a compiler macro so we don't have to do this?
  (when (/= (length subscripts) 1)
    (error "A fat-string can only have one subscript."))
  (setf (aref (fat-string-string s) (car subscripts)) value))

(defmethod ochar ((s fat-string) index)
  (aref (fat-string-string s) index))

(defmethod (setf ochar) ((value character) (s fat-string) index)
  (setf (fatchar-c (aref (fat-string-string s) index)) value))

(defmethod (setf ochar) ((value fatchar) (s fat-string) index)
  (setf (aref (fat-string-string s) index) value))

(defmethod omap (function (collection fat-string))
  (make-fat-string :string
		   (map 'vector function (fat-string-string collection))))

(defmethod omapn (function (collection fat-string))
  (map nil function (fat-string-string collection)))

(defmethod mappable-p ((collection fat-string)) t)

(defmethod omap-into ((mutable-collection fat-string)
		      function &rest collections)
  (apply #'map-into (fat-string-string mutable-collection) function
	 collections)
  mutable-collection)

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

(defmethod osubseq ((string fat-string) start &optional end)
  "Sub-sequence of a fat-string."
  (make-fat-string
   :string
   (if end
       (subseq (fat-string-string string) start end)
       (subseq (fat-string-string string) start))))

(defmethod (setf osubseq) (value (string fat-string) start &optional end)
  "Set a sub-sequence of a fat-string."
  (make-fat-string
   :string
   (if end
       (setf (subseq (fat-string-string string) start end) value)
       (setf (subseq (fat-string-string string) start) value))))

(defmethod ofill ((string fat-string) (item character) &key (start 0) end)
  (when (not end)
    (setf end (olength string)))
  (loop :for i :from start :below end
    :do (setf (aref (fat-string-string string) i)
	      (make-fatchar :c item))))

(defmethod ofill ((string fat-string) (item fatchar) &key (start 0) end)
  (when (not end)
    (setf end (olength string)))
  (loop :for i :from start :below end
    :do (setf (aref (fat-string-string string) i)
	      (copy-fatchar item))))

(defmethod ocount ((item fatchar) (collection fat-string)
		   &key from-end key
		     (test nil test-p)
		     (test-not nil test-not-p)
		     (start nil start-p)
		     (end nil end-p))
  (if (or test-p test-not-p)
      (call-with-start-end-test
       count (item (fat-string-string collection) :from-end from-end
		   :key key))
      (call-with-start-and-end
       count (item (fat-string-string collection) :from-end from-end
		   :key key
		   :test (or test #'fatchar=)))))

(defmethod ocount ((item character) (collection fat-string)
		   &key from-end key
		     (test nil test-p)
		     (test-not nil test-not-p)
		     (start nil start-p)
		     (end nil end-p))
  (labels ((key-func (c)
	     (funcall key (fatchar-c c))))
    (call-with-start-end-test
     count (item (fat-string-string collection) :from-end from-end
		 :key (if key #'key-func #'fatchar-c)))))

(defmethod ocount-if (predicate (collection fat-string)
		      &key from-end key
			(start nil start-p)
			(end nil end-p))
  ;; We should probably let the caller decide if they want a char or a fatchar,
  ;; but it would be natural to have a fatchar in this case anyway.
  ;; (labels ((key-func (c)
  ;; 	     (funcall key (fatchar-c c))))
  ;;   (call-with-start-and-end
  ;;    count-if (predicate (fat-string-string collection) :from-end from-end
  ;; 			 :key (if key #'key-func #'fatchar-c))))
  (call-with-start-and-end
   count-if (predicate (fat-string-string collection) :from-end from-end
		       :key key)))

(defmethod oreverse ((string fat-string))
  (make-fat-string :string (reverse (fat-string-string string))))

(defmethod ofind ((item character) (string fat-string)
		  &key from-end key test test-not
		    (start nil start-p)
		    (end nil end-p))
  (declare (ignorable start start-p end end-p))
  (call-with-start-and-end
   find
   (item (fat-string-string string)
	 :from-end from-end
	 :test test :test-not test-not
	 ;; Make the key reach into the fatchar for the character.
	 :key (or (and key (_ (funcall key (fatchar-c _))))
		  #'fatchar-c))))

(defmethod oposition ((item fatchar) (string fat-string)
		      &key from-end
			(test #'equalp test-p)
			(test-not nil test-not-p)
			key
			(start nil start-p)
			(end nil end-p))
  "Position of a fatchar in a fat-string."
  (declare (ignorable start start-p end end-p))
  (when (not test-p)
    (setf test-p t
	  test #'equalp))
  (call-with-start-end-test
   position
   (item (fat-string-string string)
	 :from-end from-end
	 :key key)))

(defmethod oposition ((item character) (string fat-string)
		      &key from-end
			(test #'ochar=)
			(test-not #'ochar/= test-not-p)
			key
			(start nil start-p)
			(end nil end-p))
  "Position of a fatchar in a fat-string."
  (declare (ignorable start start-p end end-p))
  (cond
    (test-not-p
      (call-with-start-and-end
       position
       (item (fat-string-string string)
	     :from-end from-end
	     :test-not test-not
	     ;; Make the key reach into the fatchar for the character.
	     :key (or (and key (_ (funcall key (fatchar-c _))))
		      #'fatchar-c))))
    (t
      (call-with-start-and-end
       position
       (item (fat-string-string string)
	     :from-end from-end
	     :test test
	     ;; Make the key reach into the fatchar for the character.
	     :key (or (and key (_ (funcall key (fatchar-c _))))
		      #'fatchar-c))))))

(defmethod oposition ((item fatchar) (string vector)
		      &key from-end
			(test #'ochar=)
			(test-not #'ochar/= test-not-p)
			key
			(start nil start-p)
			(end nil end-p))
  "Position of a fatchar in a string."
  (declare (ignorable start start-p end end-p))
  (cond
    (test-not-p
     (call-with-start-and-end
      position
      (item string
	    :from-end from-end
	    :test-not test-not
	    :key key)))
    (t
     (call-with-start-and-end
      position
      (item string
	    :from-end from-end
	    :test test
	    :key key)))))

(defmethod oposition-if (predicate (string fat-string)
			 &key from-end key
			   (start nil start-p)
			   (end nil end-p))
  "Position of a fatchar in a fat-string."
  (declare (ignorable start start-p end end-p))
  (call-with-start-and-end
   position-if
   (predicate (fat-string-string string)
	      :from-end from-end
	      :key key)))

#| doesn't work on CCL because it still passes key as nil
(defmethod osubstitute ((new-item character)
			(old-item character) (collection fat-string)
			 &key from-end
			   (test nil test-p)
			   (test-not nil test-not-p)
			   (start nil start-p)
			   (end nil end-p)
			   count key)
  (call-with-start-end-test osubstitute
    ((make-fatchar :c new-item) (make-fatchar :c old-item)
     collection :from-end from-end :count count :key key)))

(defmethod onsubstitute ((new-item character)
			(old-item character) (collection fat-string)
			 &key from-end
			   (test nil test-p)
			   (test-not nil test-not-p)
			   (start nil start-p)
			   (end nil end-p)
			   count key)
  (call-with-start-end-test onsubstitute
    ((make-fatchar :c new-item) (make-fatchar :c old-item)
     collection :from-end from-end :count count :key key)))

(defmethod osubstitute ((new-item fatchar)
			(old-item fatchar) (collection fat-string)
			 &key from-end
			   (test nil test-p)
			   (test-not nil test-not-p)
			   (start nil start-p)
			   (end nil end-p)
			   count key)
  (call-with-start-end-test osubstitute
    (new-item old-item (fat-string-string collection)
     :from-end from-end :count count :key key)))

(defmethod onsubstitute ((new-item fatchar)
			(old-item fatchar) (collection fat-string)
			 &key from-end
			   (test nil test-p)
			   (test-not nil test-not-p)
			   (start nil start-p)
			   (end nil end-p)
			   count key)
  (call-with-start-end-test onsubstitute
    (new-item old-item (fat-string-string collection)
     :from-end from-end :count count :key key)))
|#

(defmethod osubstitute ((new-item character)
			(old-item character) (collection fat-string)
			&rest args
			&key from-end test test-not start end count key)
  (declare (ignorable from-end test test-not start end count key))
  (apply #'osubstitute
	 (make-fatchar :c new-item) (make-fatchar :c old-item) collection args))

(defmethod onsubstitute ((new-item character)
			(old-item character) (collection fat-string)
			 &rest args
			 &key from-end test test-not start end count key)
  (declare (ignorable from-end test test-not start end count key))
  (apply #'onsubstitute
	 (make-fatchar :c new-item) (make-fatchar :c old-item) collection args))

(defmethod osubstitute ((new-item fatchar)
			(old-item fatchar) (collection fat-string)
			&rest args
			&key from-end test test-not start end count key)
  (declare (ignorable from-end test test-not start end count key))
  (make-fat-string
   :string (apply #'osubstitute
		  new-item old-item (fat-string-string collection) args)))

(defmethod onsubstitute ((new-item fatchar)
			(old-item fatchar) (collection fat-string)
			 &rest args
			 &key from-end test test-not start end count key)
  (declare (ignorable from-end test test-not start end count key))
  (apply #'onsubstitute new-item old-item (fat-string-string collection) args))

(defmethod osearch ((collection-1 fat-string) (collection-2 fat-string)
		    &rest args
		    &key from-end test test-not key start1 start2 end1 end2)
  (declare (ignorable from-end test test-not key start1 start2 end1 end2))
  (let ((new-args args))
    (when (not (getf args :test))
      (setf new-args (copy-seq args)
	    (getf new-args :test) #'ochar-equal))
    (apply #'search
	   (fat-string-string collection-1)
	   (fat-string-string collection-2)
	   new-args)))

(defmethod osearch ((collection-1 string) (collection-2 fat-string)
		    &rest args
		    &key from-end test test-not key start1 start2 end1 end2)
  (declare (ignorable from-end test test-not key start1 start2 end1 end2))
  (let ((new-args args))
    (when (not (getf args :test))
      (setf new-args (copy-seq args)
	    (getf new-args :test) #'ochar-equal))
    (apply #'search
	   collection-1
	   (fat-string-string collection-2)
	   new-args)))

(defmethod osearch ((collection-1 fat-string) (collection-2 string)
		    &rest args
		    &key from-end test test-not key start1 start2 end1 end2)
  (declare (ignorable from-end test test-not key start1 start2 end1 end2))
  (let ((new-args args))
    (when (not (getf args :test))
      (setf new-args (copy-seq args)
	    (getf new-args :test) #'ochar-equal))
    (apply #'search
	   (fat-string-string collection-1)
	   collection-2
	   new-args)))

(defmethod oconcatenate ((first-collection fat-string) &rest collections)
  (when (not (every #'(lambda (_) (typep _ '(or string fat-string)))
		    collections))
    (error
     "I don't know how to concatenate things that aren't fat-strings or ~
      strings to a fat-string."))
  (make-fat-string :string
		   (apply #'concatenate 'vector
			  (mapcar (_ (if (stringp _)
					 (make-fatchar-string _)
					 (fat-string-string _)))
				  (append (list first-collection)
					  collections)))))

(defmethod oconcatenate-as ((result-type (eql 'fat-string)) &rest collections)
  (when (not (every #'(lambda (_) (typep _ '(or string fat-string)))
		    collections))
    (error
     "I don't know how to concatenate things that aren't fat-strings or ~
      strings to a fat-string."))
  (make-fat-string :string
		   (apply #'concatenate 'vector
			  (mapcar (_ (if (stringp _)
					 (make-fatchar-string _)
					 (fat-string-string _)))
				  collections))))

(defmethod oremove (item (collection fat-string)
		    &key from-end key
		      (test #'ochar= test-p)
		      (test-not #'ochar/= test-not-p)
		      (start nil start-p)
		      (end nil end-p)
		      count)
  (declare (ignorable start start-p end end-p test test-not))
  (when test-p
    (setf test-not nil))
  (when test-not-p
    (setf test nil))
  (when test
    (setf test-p t))
  (call-with-start-end-test remove
			    (item (fat-string-string collection)
				  :from-end from-end
				  :count count
				  :key key)))

(defmethod oremove-if (predicate (collection fat-string)
		       &key from-end key (start nil start-p) (end nil end-p)
			 count)
  (declare (ignorable start start-p end end-p))
  (call-with-start-and-end remove-if (predicate
				      (fat-string-string collection)
				      :from-end from-end
				      :count count
				      :key key)))

(defmethod osplit ((separator fatchar) (string fat-string)
		   &key omit-empty test key bag
		     (start nil start-p)
		     (end nil end-p))
  (declare (ignorable start start-p end end-p))
  (mapcar (_ (make-fat-string :string _))
	  (call-with-start-and-end
	   split-sequence
	   (separator (fat-string-string string)
		      :omit-empty omit-empty
		      ;; Default to a reasonable test for fatchars.
		      :test (or test #'equalp)
		      :key key
		      :bag bag))))

(defmethod osplit ((separator character) (string fat-string)
		   &key omit-empty test key bag
		     (start nil start-p)
		     (end nil end-p))
  (declare (ignorable start start-p end end-p))
  (mapcar (_ (make-fat-string :string _))
	  (call-with-start-and-end
	   split-sequence
	   (separator (fat-string-string string)
		      :omit-empty omit-empty
		      :test test
		      :bag bag
		      ;; Make the key reach into the fatchar for the character.
		      :key (or (and key (_ (funcall key (fatchar-c _))))
			       #'fatchar-c)))))

(defmethod osplit ((separator string) (string fat-string)
		   &key omit-empty test key bag
		     (start nil start-p)
		     (end nil end-p))
  (declare (ignorable start start-p end end-p))
  (mapcar (_ (make-fat-string :string _))
	  (call-with-start-and-end
	   split-sequence
	   (separator (fat-string-string string)
		      :omit-empty omit-empty
		      :test test
		      :bag bag
		      ;; Make the key reach into the fatchar for the character.
		      :key (or (and key (_ (funcall key (fatchar-c _))))
			       #'fatchar-c)))))

(defmethod osplit ((separator fat-string) (string fat-string)
		   &key omit-empty test key bag
		     (start nil start-p)
		     (end nil end-p))
  (declare (ignorable start start-p end end-p))
  (mapcar (_ (make-fat-string :string _))
	  (call-with-start-and-end
	   split-sequence
	   ((fat-string-string separator) (fat-string-string string)
	    :omit-empty omit-empty
	    ;; Default to a reasonable test for fatchars.
	    :test (or test #'equalp)
	    :key key :bag bag))))

(defmethod osplit-if (predicate (string fat-string)
		      &key omit-empty key
			(start nil start-p)
			(end nil end-p))
  (declare (ignorable start start-p end end-p))
  (mapcar (_ (make-fat-string :string _))
	  (call-with-start-and-end
	   split-sequence-if
	   (predicate (fat-string-string string)
		      :omit-empty omit-empty
		      :key key))))

(defmethod oreplace-subseq (target replacement (sequence fat-string) &key count)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (apply #'oreplace-subseq-as 'fat-string
	 target replacement sequence (list :count count)))

(defun make-fatchar-string-of-length (length &key initial-element fill-pointer)
  "Return a FATCHAR-STRING of LENGTH. If INITIAL-ELEMENT is supplied, copy that
for the initial contents. Note that this is different than if you supplied
initial-element to make-array, or if you don't supply initial-element."
  (let ((result (make-array (list length)
			    :element-type 'fatchar
			    :initial-element (make-fatchar)
			    :fill-pointer fill-pointer)))
    (when initial-element
      (loop :for i :from 0 :below length
	 :do (setf (aref result i) (copy-fatchar initial-element))))
    result))

(defun make-fatchar-string (thing &key fill-pointer)
  "Make a string of fatchars from THING, which can be a string or a character."
  (let (result)
    (flet ((from-string (string)
	     (let ((graphemes (char-util:graphemes string)))
	       (setf result (make-fatchar-string-of-length
			     (length graphemes)
			     :fill-pointer fill-pointer))
	       (loop :for g :in graphemes
		     :for i :from 0
		     :do (setf (aref result i)
			       (make-fatchar
				:c (if (= (length g) 1) (char g 0) g)))))))
      (etypecase thing
	(string
	 (from-string thing))
	(fatchar
	 (setf result (make-fatchar-string-of-length
		       1 :initial-element (copy-fatchar thing)
		       :fill-pointer fill-pointer)))
	(character
	 (setf result (make-fatchar-string-of-length
		       1 :initial-element (make-fatchar :c thing)
		       :fill-pointer fill-pointer))))
      ;; We could princ-to-string for other stuff, but it's probably better
      ;; if the caller does it explicitly.
      result)))

(defun copy-fatchar-string (fatchar-string)
  (let ((result (make-array (length fatchar-string)
			    :element-type 'fatchar
			    :initial-element (make-fatchar))))
    (loop :for i :from 0 :below (length fatchar-string) :do
      (setf (aref result i) (copy-fatchar (aref fatchar-string i))))
    result))

(defun string-to-fat-string (thing)
  "Make a fat-string out of a string or character."
  (make-fat-string :string (make-fatchar-string thing)))

(defun fat-string-to-string (fat-string)
  "Make a string from a fat string. This of course loses the attributes."
  (typecase fat-string
    (fat-string (fatchar-string-to-string (fat-string-string fat-string)))
    (fatchar-string (fatchar-string-to-string fat-string))
    (t fat-string)))

(defun fatchar-string-to-string (string)
  "Make a string from a fat string. This of course loses the attributes."
  ;; Arrays can't really distinguish their orginal element type if it's
  ;; upgraded, so we might not be able tell a string from a fatchar-string.
  (typecase string
    (fatchar-string
     ;; (let ((s (make-array (list (length fat-string))
     ;; 			  :element-type 'character)))
     ;;   (loop :for i :from 0 :below (length fat-string) :do
     ;; 	  (setf (aref s i) (fatchar-c (aref fat-string i))))
     ;;   s))
     ;; (map 'string (_ (if (fatchar-p _) (fatchar-c _) _)) string))
     (with-output-to-string (s)
       (loop :for c :across string
	     :do (princ (fatchar-c c) s))))
    (t string)))

;; @@@ Maybe this should be generic?
(defun string-vector (string)
  "Return the STRING as a vector. This converts FAT-STRINGs to FATCHAR-STRINGs,
and returns STRINGs or FATCHAR-STRINGs as-is. This is useful so you can iterate
over a string's characters, even with the 'normal' (non-collections) sequence
functions."
  (etypecase string
    (fat-string (fat-string-string string))
    ((or string fatchar-string) string)))

;; @@@ What about string equality considering the effects?

(defun fat-string-compare (f a b)
  (funcall f (fat-string-to-string a) (fat-string-to-string b)))

(eval-when (:compile-toplevel)
  (defmacro make-string-comparators ()
    (let ((forms
	   (loop :with func
	      :for f :in '(string< string> string= string/= string<= string>=
			   string-lessp string-greaterp string-equal
			   string-not-equal string-not-lessp
			   string-not-greaterp)
	      :do
	      (setf func (symbolify (s+ "FAT-" f)))
	      :collect `(defun ,func (a b)
			  (funcall #',f
				   (fat-string-to-string a)
				   (fat-string-to-string b))))))
      `(progn ,@forms))))
(make-string-comparators)

;; The char= methods are pre-done.
;; @@@ This is wrong for graphemes. How should we compare graphemes?
;; @@@ For now we just compare as strings.
(eval-when (:compile-toplevel)
  (defmacro make-char-comparators (prefix)
    (let ((forms
	   (loop :with func :and c-func :and s-func
	      :for f :in '(< > <= >=
			   -lessp -greaterp -equal
			   -not-equal -not-lessp
			   -not-greaterp)
	      :do
	      (setf func (symbolify (s+ prefix "char" f))
                    c-func (symbolify (s+ "char" f))
	            s-func (symbolify (s+ "string" f)))
	      :collect `(defun ,func (a b)
			  (let ((ca (fatchar-c a))
				(cb (fatchar-c b)))
			    (cond
			      ((and (characterp ca) (characterp cb))
			       (funcall #',c-func ca cb))
			      ((or (stringp ca) (stringp cb))
			       (funcall #',s-func (string ca) (string cb)))))))))
      `(progn ,@forms))))
(make-char-comparators "FAT")

;;;;;;;;;;;;;;;;;;
;; ochar methods

(eval-when (:compile-toplevel)
  (defmacro make-char-comparator-methods (prefix)
    (let ((forms
	   (loop :with func :and c-func :and s-func
	      :for f :in '(< > <= >=
			   -lessp -greaterp -equal
			   -not-equal -not-lessp
			   -not-greaterp)
	      :do
	      (setf func (symbolify (s+ prefix f))
		    c-func (symbolify (s+ "char" f))
		    s-func (symbolify (s+ "string" f)))
	      :collect `(defmethod ,func ((a fatchar) (b fatchar))
			  (let ((ca (fatchar-c a))
				(cb (fatchar-c b)))
			    (cond
			      ((and (characterp ca) (characterp cb))
			       (funcall #',c-func ca cb))
			      ((or (stringp ca) (stringp cb))
			       (funcall #',s-func (string ca) (string cb)))))))))
      `(progn ,@forms))))
(make-char-comparator-methods "O")

;; All the rest that are just wrappers for single chars.
;; @@@ How should we handle graphemes?
;; For now we say it's the quality if any pieces of the grapheme have the
;; quality. Except for with ‘graphic-char-p’ I think it should be "every"
;; instead of "any".
(eval-when (:compile-toplevel)
  (defmacro make-char-wrapper-methods (prefix names)
    (let ((forms
	   (loop :with func
	      :for f :in names
	      :do
	      (setf func (symbolify (s+ prefix f)))
	      :collect `(defmethod ,func ((character fatchar))
			  (let ((c (fatchar-c character)))
			    (etypecase c
			      (character (,f c))
			      (string (some #',f c))))))))
      `(progn ,@forms))))

(make-char-wrapper-methods
 "O" (alpha-char-p alphanumericp #| graphic-char-p |#
      upper-case-p lower-case-p both-case-p))

(defmethod ographic-char-p ((character fatchar))
  (let ((c (fatchar-c character)))
    (etypecase c
      (character (graphic-char-p c))
      (string (some #'graphic-char-p c)))))

;; (make-char-wrapper-methods "O" (char-code char-name))

(defmethod ochar-code ((character fatchar))
  (let ((c (fatchar-c character)))
    (etypecase c
      (character (char-code c))
      (string
       ;; (error "Can't get a code for a multi-character grapheme.")
       (values-list (map 'list #'char-code c))))))

(defmethod ochar-name ((character fatchar))
  (let ((c (fatchar-c character)))
    (etypecase c
      (character (char-name c))
      (string
       ;; (error "Can't get a name for a multi-character grapheme.")
       (values-list (map 'list #'char-name c))))))

(defmethod ochar-upcase ((c fatchar))
  (let* ((result (copy-fatchar c))
	 (c (fatchar-c result)))
    (etypecase c
      (character
       (setf (fatchar-c result) (char-upcase c)))
      (string
       (setf (fatchar-c result) (string-upcase c))))
    result))

(defmethod ochar-downcase ((c fatchar))
  (let* ((result (copy-fatchar c))
	 (c (fatchar-c result)))
    (etypecase c
      (character
       (setf (fatchar-c result) (char-downcase c)))
      (string
       (setf (fatchar-c result) (string-downcase c))))
    result))

(defmethod ocharacterp ((object fatchar)) T)
(defmethod odigit-char (weight (type (eql 'fatchar)) &optional radix)
  (make-fatchar :c (if radix (digit-char weight radix) (digit-char weight))))

;; For multi-char graphemes, we just use the first one.
(defmethod odigit-char-p ((character fatchar) &optional radix)
  (let ((c (fatchar-c character)))
    (etypecase c
      (character
       (if radix
	   (digit-char-p c radix)
	   (digit-char-p c)))
      (string
       (if radix
	   (digit-char-p (char c 0) radix)
	   (digit-char-p (char c 0)))))))

(defmethod ostandard-char-p ((character fatchar)) nil) ;; @@@ Right?

;; I think this was intended to encode the attributes too, but we've made a
;; character that's likely bigger than a fixnum. Should we really construct a
;; bizarrely formatted bignum? Also we have to define a limited fixed set of
;; attributes, and we don't support multiple code point graphemes, so this is
;; inherently lossy.
#|
 bottom <───┐
  right <──┐│
    top <─┐││
   left <┐│││
         ││││┌────────> [reserved]
no bg <─┐│││││┌───────> [reserved]
no fg <┐│││││││┌──────> [reserved]
       │││││││││┌─────> [reserved]
       ││││││││││┌────> :inverse
       │││││││││││┌───> :bold
       ││││││││││││┌──> :underline
       │││││││││││││┌─> :standout
       ││││││││││││││
     3210321076543210       foreground              background
     ┌──┬┬──┐┌──────┐┌──────┬┬──────┬┬──────┐ ┌──────┬┬──────┬┬──────┐
     │ 4││ 4││  8   ││  8   ││  8   ││  8   │ │  8   ││  8   ││   8  │
   flags line attrs  fg red  fg green fg blue  bg red bg green bg blue
     ┌──────────────────────────────┐
     │             32               │
             character code
|#

(defmethod ochar-int ((character fatchar))
  (typecase (fatchar-c character)
    (character
     (let ((int 0)
	   (flags 0)
	   (fg (and (fatchar-fg character)
		    (convert-color-to
		     (lookup-color (fatchar-fg character)) :rgb8)))
	   (bg (and (fatchar-bg character)
		    (convert-color-to
		     (lookup-color (fatchar-bg character)) :rgb8)))
	   (line (logand (fatchar-line character) #b1111)))
       (flet ((add (value width)
		(setf int (logior (ash int width) value)))
	      (attr-bits ()
		(loop :with result = 0
		      :for a :in (rest *known-attrs*)
		      :as i = 0 :then (1+ i)
		      :do
			 (when (find a (fatchar-attrs character))
			   (setf result (logior result (ash 1 i))))
		      :finally (return result))))
	 ;; With no effects set, the whole thing should be zero before the
	 ;; character code, so it'll just look like a character code.
	 (when (not (fatchar-fg character))
	   (setf flags (dpb 1 (byte 1 0) flags)))
	 (when (not (fatchar-bg character))
	   (setf flags (dpb 1 (byte 1 1) flags)))
	 (add flags 4)
	 (add line 4)
	 (add (attr-bits) 8)
	 (add (if fg (color-component fg :red)   0) 8)
	 (add (if fg (color-component fg :green) 0) 8)
	 (add (if fg (color-component fg :blue)  0) 8)
	 (add (if bg (color-component bg :red)   0) 8)
	 (add (if bg (color-component bg :green) 0) 8)
	 (add (if bg (color-component bg :blue)  0) 8)
	 (add (char-int (fatchar-c character)) 32))
       int))
    (string
     ;; Do you want a Zalgo text DDOS?
     (error "Heckin' no."))))

(defmethod oint-char (int (type (eql 'fatchar)))
  "Return a fatchar represented by the integer ‘int’."
  (let* ((bg-p (not (logbitp 92 int)))
	 (fg-p (not (logbitp 93 int)))
	 (line (ldb (byte 4 88) int))
	 (attrs nil)
	 (fg   (when fg-p (vector :rgba (ldb (byte 8 72) int)
				        (ldb (byte 8 64) int)
				        (ldb (byte 8 56) int))))
	 (bg   (when bg-p (vector :rgba (ldb (byte 8 48) int)
				        (ldb (byte 8 40) int)
				        (ldb (byte 8 32) int))))
	 (c    (logand int (1- (ash 1 32)))))
    (loop :for a :in (rest *known-attrs*)
	  :and i :from 80
	  :when (logbitp i int)
	  :do (push a attrs))
    (make-fatchar :c (code-char c) :fg fg :bg bg :line line :attrs attrs)))

(defmethod ocode-char (code (type (eql 'fatchar)))
  (make-fatchar :c (code-char code)))

(defmethod oname-char (name (type (eql 'fatchar)))
  (make-fatchar :c (name-char name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ostring methods

(eval-when (:compile-toplevel)
  (defmacro make-ostring-comparators ()
    (let ((forms
	   (loop :with ofunc :and ffunc
	      :for f :in '(string< string> string= string/= string<= string>=
			   string-lessp string-greaterp string-equal
			   string-not-equal string-not-lessp
			   string-not-greaterp)
	      :do
		(setf ofunc (symbolify (s+ "O" f))
		      ffunc (symbolify (s+ "FAT-" f)))
;;	      :collect `(defalias ',ofunc ',ffunc))))
	      :collect `(defmethod ,ofunc ((a fat-string) (b fat-string))
			  (,ffunc a b))
	      :collect `(defmethod ,ofunc ((a fat-string) (b string))
			  (,f (fat-string-to-string a) b))
	      :collect `(defmethod ,ofunc ((a string) (b fat-string))
			  (,f a (fat-string-to-string b))))))
      `(progn ,@forms))))
(make-ostring-comparators)

(defmethod ostring-upcase ((string fat-string) &key (start 0) end)
  (let* ((result (copy-fat-string string))
	 (s (fat-string-string string))
	 (ss (fat-string-string result)))
    (loop :for i :from start :below (or end (olength string))
	  :do (setf (aref ss i) (ochar-upcase (aref s i))))
    result))

(defmethod ostring-downcase ((string fat-string) &key (start 0) end)
  (let* ((result (copy-fat-string string))
	 (s (fat-string-string string))
	 (ss (fat-string-string result)))
    (loop :for i :from start :below (or end (olength string))
	  :do (setf (aref ss i) (ochar-downcase (aref s i))))
    result))

(defmethod ostring-capitalize ((string fat-string) &key (start 0) end)
  (let* ((result (copy-fat-string string))
	 (s (fat-string-string string))
	 (ss (fat-string-string result)))
    (loop :for i :from start :below (or end (olength string))
	  :do
	  (if (= i start)
	      (setf (aref ss i) (ochar-upcase (aref s i)))
	      (setf (aref ss i) (ochar-downcase (aref s i)))))
    result))

(defmethod ostring-left-trim (character-bag (string fat-string))
  (let ((pos (position-if #'(lambda (c)
			      (not (position c character-bag
					     ;; :test #'same-char)))
					     ;; :test #'fatchar=
					     :test #'ochar=
					     )))
			  (fat-string-string string))))
    (if pos
	(make-fat-string :string (subseq (fat-string-string string) pos))
	(make-fat-string :length 0))))

(defmethod ostring-right-trim (character-bag (string fat-string))
  (let ((pos (position-if #'(lambda (c)
			      (not (position c character-bag
					     ;;:test #'same-char)))
					     ;;:test #'fatchar=
					     :test #'ochar=
					     )))
			  (fat-string-string string)
			  :from-end t)))
    (if pos
	(make-fat-string :string (subseq (fat-string-string string) 0 (1+ pos)))
	(make-fat-string :length 0))))

(defmethod ostring-trim (character-bag (string fat-string))
  (ostring-left-trim character-bag (ostring-right-trim character-bag string)))

(defmethod ostring ((string fat-string))
  ;; @@@ but should we simplify?
  string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spans
;;
;; ([objects ...])
;; (:keyword [objects ...])
;; (:keyword [attribute ...] [objects ...])
;;
;; attribute => :keyword object
;;
;; (:fg :color #(:rgb 0 0 1) "hello")
;; (:fg-blue "hello")
;;
;; Some things may want to allow evaluation by letting an "object" be
;; a function call or a symbol that gets evaluated, instead of just literal
;; objects.

(defun span-length (span)
  "Calculate the length in characters of the span."
  (the fixnum (loop :for e :in span
		 :sum (typecase e
			(string (length e))
			(cons (span-length e))
			;; @@@ shouldn't we princ-to-string other some things?
			(t 0)))))

(defun listify-fake-span (fake-span)
  "Take a list of characters, strings, and keywords and make nested lists out
of them indicated by the parentheses. Actual parens should be given as
strings."
  (let ((str (make-string-output-stream))
	cur save tmp)
    (flet ((push-if-any ()
	     (when (> (length (setf tmp (get-output-stream-string str))) 0)
	       (push tmp cur))))
      (loop
	 :for x :in fake-span :do
	 (cond
	   ((eql x #\()
	    (push-if-any)
	    (push cur save)
	    (setf cur '()))
	   ((eql x #\))
	    (push-if-any)
	    (setf cur (nreverse cur))
	    (setf tmp (pop save))
	    (push cur tmp)
	    (setf cur tmp))
	   (t
	    (typecase x
	      ((or character string)
	       (princ x str))
	      (t
	       (push x cur)))))
;;;       (format t "~w~%" cur)
	 )
    (push-if-any)
    (nreverse cur))))

;; TODO:
;;  - Add END key

;; (fatchar:fat-string-to-span (pager::process-grotty-line (nth 4 (!_ "/bin/cat grotish.txt"))))

(defun fatchar-diffs (c1 c2)
  "The differences between c1 and c2, a list of :ATTR :FG :BG. NIL if the same."
  (let (diffs)
    (when (set-exclusive-or (and c1 (fatchar-attrs c1)) (fatchar-attrs c2)
			    ;; :test #'eq)
			    :test #'equal) ;; because :object
      (push :attr diffs))
    (when (not (equal (and c1 (fatchar-fg c1)) (fatchar-fg c2)))
      (push :fg diffs))
    (when (not (equal (and c1 (fatchar-bg c1)) (fatchar-bg c2)))
      (push :bg diffs))
    ;; (when (not diffs)
    ;;   (error "bug-a-roony: it twernt a difference ~s ~s" c1 c2))
    diffs))

(defun span-start (type value)
  "Return the reversed starting list for a span of TYPE and VALUE."
  (ecase type
    (:attr (fatchar-attrs value))
    (:fg (if (keywordp (fatchar-fg value))
	     (list (keywordify (s+ "FG-" (fatchar-fg value))))
	     (list (fatchar-fg value) :color :fg)))
    (:bg (if (keywordp (fatchar-bg value))
	     (list (keywordify (s+ "BG-" (fatchar-bg value))))
	     (list (fatchar-bg value) :color :bg)))
    ((nil) nil)))

#|

c    | foothebar0000
fg   |    rrrrrr
bg   |       bbbbbbb
attr | iiiiiiii

((:i "foo" (:fg-r "the" (:bg-b "ba"))) (:fg-r (:bg-b "r")) (:bg-b "0000"))
|#

;; This is the average line length of my code.
(defparameter *starting-string-size* 33
  "How many characters allocate initially for a span piece.")

;; @@@ remove all the debugging junk someday.
(defun fatchar-string-to-span (fatchar-string &key (start 0) #|pause|#)
  "Convert a FATCHAR line to tagged spans."
  (let ((i start)
	(len (length fatchar-string))
	(piece (make-stretchy-string *starting-string-size*))
	last
	c)
    (labels
	((add-chars ()
	   "Add characters to piece until a change or the end. Return true
            if we added one."
	   (let (did-one)
	     (loop
		:while (< i len)
		:do (setf c (aref fatchar-string i))
		:while (not (and did-one (fatchar-diffs last c)))
		:do
		(stretchy-append piece (fatchar-c c))
		;; (dbugf :fatchar "char ~s ~s ~%" i (fatchar-c c))
		(incf i)
		(setf last (copy-fatchar c)
		      did-one t))
	     did-one))
	 (sub-rendition-of-p (a b)
	   "True if fatchar A is sub-rendition of B, i.e. B can be nested in A."
	   (and (subsetp (fatchar-attrs a) (fatchar-attrs b))
		(or (not (fatchar-fg a))
		    (and (fatchar-fg a) (equal (fatchar-fg a) (fatchar-fg b))))
		(or (not (fatchar-bg a))
		    (and (fatchar-bg a) (equal (fatchar-bg a) (fatchar-bg b))))))
	 (build-span (type value rendition in)
	   "Build a span of TYPE and VALUE."
	   ;; (dbugf :fatchar "build-span ~s ~a ~s ~s~%" type value rendition in)
	   (let (sub-span new-type added)
	     ;; (when pause
	     ;;   (format *debug-io* "-> ") (finish-output *debug-io*)
	     ;;   (read-line *debug-io*))
	     (cond
	       ((> (length type) 1)
		;; (dbugf :fatchar "subtype ~s~%" type)
		(setf sub-span (span-start (pop type) value))
		(push (build-span type c rendition (cons (first type) in))
		      sub-span))
	       (t
		;; (dbugf :fatchar "actual type ~s~%" type)
		(setf sub-span (span-start (pop type) value))
		(when (add-chars)
		  (push (copy-seq piece) sub-span)
		  (stretchy-truncate piece))
		(when (< i len)
		  (setf new-type (fatchar-diffs last c))
		  ;; (dbugf :fatchar "differnce type ~s ~a ~a~%"
		  ;; 	 new-type last c)
		  ;; (when (and (not (member (first type) new-type))
		  ;; 	     (not (intersection new-type in)))
		  (when (and (sub-rendition-of-p rendition c)
			     (not (intersection new-type in)))
		    (if (eq (first new-type) :attr)
			(progn
			  (setf added (set-difference
				       (fatchar-attrs c)
				       (and last (fatchar-attrs last)))
				;; removed (set-difference
				;; 		(fatchar-attrs last)
				;; 		(fatchar-attrs c))
				)
			  (when added
			    ;; (dbugf :fatchar "add attr ~s~%" added)
			    (setf last nil)
			    (push (build-span new-type c c
					      (cons (first new-type) in))
				  sub-span)
			    (pop new-type)))
			(progn
			  ;; (dbugf :fatchar "add color ~s~%" c)
			  (setf last nil)
			  (push (build-span new-type c c
					    (cons (first new-type) in))
				sub-span)
			  (pop new-type)))))))
	     ;; Reverse it and return it.
	     (setf sub-span (nreverse sub-span))
	     sub-span)))
      ;; (dbugf :fatchar "~&############################~%")
      ;; (dbugf :fatchar "~&### string to span start ###~%")
      ;; (dbugf :fatchar "~&############################~%")
      (loop
	 :with rendition = (make-fatchar)
	 :while (< i len)
	 :do
	 ;; (dbugf :fatchar "Blurp ~s.~%" i)
	 (setf c (aref fatchar-string i))
	 :collect
	 (let ((cc (build-span (fatchar-diffs rendition c) c rendition nil)))
	   ;; (dbugf :fatchar "collected ~s~%" cc)
	   cc)))))

(defun fat-string-to-span (fat-string &key (start 0))
  (fatchar-string-to-span (fat-string-string fat-string) :start start))

(defmethod ostring-to-span ((string fat-string))
  (fat-string-to-span string))

(defun span-to-fat-string (span &key (start 0) end fatchar-string
				  unknown-func filter)
"Make a FAT-STRING from SPAN. See the documentation for SPAN-TO-FATCHAR-STRING."
  (make-fat-string
   :string
   (span-to-fatchar-string span :start start :end end
			   :fatchar-string fatchar-string
			   :unknown-func unknown-func
			   :filter filter)))

;; Wherein I inappropriately appropriate more of latin1.
(defalias 'ß 'span-to-fat-string)

#|
;; @@@ Consider dealing with the overlap between this and terminal:with-style.
(defun span-to-fatchar-string (span &key (start 0) end fatchar-string
				      unknown-func filter)
  "Make a FATCHAR-STRING from SPAN. A span is a list representation of a
fatchar string. The grammar is something like:

span ->
  string | fat-string |
  character | fatchar |
  span-list

span-list ->
  ([color-name] [span]*)
  ([attribute-name] [span]*)
  (:fg-[color-name] [span]*)
  (:bg-[color-name] [span]*)
  (:fg :color [color] [span]*)
  (:bg :color [color] [span]*)

Known colors are from dcolor:*simple-colors* and known attributes are in
fatchar:*known-attrs*.

  - START and END are character index limits.
  - FATCHAR-STRING can be provided as an already created adjustable string with a
    fill-pointer to put the result into.
  - UNKNOWN-FUNC is a fuction to call with un-recognized attributes, colors, or
    object types. The primary value is processed. If the second value is true,
    ignore the rest of the list.
  - FILTER is a function which is called with every string, which should return
    similar typed string to use as a replacement."
  (when (not fatchar-string)
    (setf fatchar-string (make-array 40
				     :element-type 'fatchar
				     :initial-element (make-fatchar)
				     :fill-pointer 0
				     :adjustable t)))
  (setf (fill-pointer fatchar-string) 0)
  (let (fg bg attrs (i 0))
    (declare (special fg bg attrs))
    (labels
	((spanky (s)
	   (when s
	     (typecase s
	       (string
		(loop :for c :across (if filter (funcall filter s) s)
		   :do
		   (when (and (>= i start)
			      (or (not end) (< i end)))
		     (vector-push-extend
		      (make-fatchar :c c :fg (car fg) :bg (car bg) :attrs attrs)
		      fatchar-string))
		   (incf i)))
	       (fat-string
		(loop :for c :across (fat-string-string
				      (if filter (funcall filter s) s))
		   :do
		   (when (and (>= i start)
			      (or (not end) (< i end)))
		     (vector-push-extend
		      (make-fatchar :c (fatchar-c c)
				    :fg (fatchar-fg c)
				    :bg (fatchar-bg c)
				    :line (fatchar-line c)
				    :attrs (union attrs (fatchar-attrs c))); <--
		      fatchar-string))
		   (incf i)))
	       (character
		(vector-push-extend
		 (make-fatchar :c s :fg (car fg) :bg (car bg) :attrs attrs)
		 fatchar-string)
		(incf i))
	       (list
		(let* ((f (first s))
		       (tag (and (or (keywordp f) (symbolp f)) f))
		       (rest (cdr s))
		       value ignore-the-rest)
		  (if tag
		      (let ((fg fg) (bg bg) (attrs attrs)
			    (tag-str (string tag)))
			(declare (special fg bg attrs))
			(cond
			  ((and (> (length tag-str) 3)
				(string= (subseq tag-str 0 3) "FG-"))
			   (push (keywordify (subseq (string tag) 3)) fg))
			  ((and (> (length tag-str) 3)
				(string= (subseq tag-str 0 3) "BG-"))
			   (push (keywordify (subseq (string tag) 3)) bg))
			  ((member tag *simple-colors*)
			   ;; An un-prefixed color is a foreground color.
			   (push tag fg))
			  ((member tag *known-attrs*)
			   (push tag attrs))
			  ((and (eq tag :fg) (eq (second s) :color))
			   (push (third s) fg)
			   (setf rest (cdddr s)))
			  ((and (eq tag :bg) (eq (second s) :color))
			   (push (third s) bg)
			   (setf rest (cdddr s)))
			  (t
			   (if unknown-func
			       (progn
				 (setf (values value ignore-the-rest)
				       (funcall unknown-func s))
				 (spanky value))
			       (push tag attrs))))
			;; (format t "tag ~s attrs ~s (cdr s) ~s~%"
			;; 	tag attrs (cdr s))
			(when (not ignore-the-rest)
			  (spanky rest))
			;;(setf fg nil bg nil)
			;;(pop attrs)
			)
		      (progn
			(spanky f)
			(spanky (cdr s))))))
	       (t
		(when unknown-func
		  (spanky (funcall unknown-func s))))))))
      (spanky span)))
  fatchar-string)
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *span-grammar* "
span ->
  string | fat-string |
  character | fatchar |
  span-list

span-list ->
  ([color-name] [span]*)
  ([attribute-name] [span]*)
  (:fg-[color-name] [span]*)
  (:bg-[color-name] [span]*)
  (:fg :color [color] [span]*)
  (:bg :color [color] [span]*)
  (:object <object> [span]*)
")

  (defparameter *span-args*
"
Known colors are from dcolor:*simple-colors* and known attributes are in
fatchar:*known-attrs*.

  - START and END are character index limits.
  - UNKNOWN-FUNC is a fuction to call with un-recognized attributes, colors, or
    object types. The primary value is processed. If the second value is true,
    ignore the rest of the list.
  - FILTER is a function which is called with every string, which should return
    similar typed string to use as a replacement."))

(defun span-to-thing (span out-func thing
		      &key (start 0) end unknown-func filter)
  #.(s+
     "Convert a SPAN to something else, by calling OUT-FUNC with succesive
FATCHARS and THING. A span is a list representation of a attributed string.
The grammar is something like:

"
     *span-grammar*
     *span-args*)
  (let (fg bg attrs (i 0))
    (declare (special fg bg attrs))
    (labels
	((spanky (s)
	   (when s
	     (typecase s
	       (string
		(loop :for c :across (if filter (funcall filter s) s)
		   :do
		   (when (and (>= i start)
			      (or (not end) (< i end)))
		     (funcall out-func
		      (make-fatchar :c c :fg (car fg) :bg (car bg) :attrs attrs)
		      thing))
		   (incf i)))
	       (fat-string
		(loop :for c :across (fat-string-string
				      (if filter (funcall filter s) s))
		   :do
		   (when (and (>= i start)
			      (or (not end) (< i end)))
		     ;; (format t "Kerchow ~s ~s ~s~%" fg bg c)
		     (funcall out-func
		      (make-fatchar :c (fatchar-c c)
				    ;; @@@ Shouldn't we take the containing
				    ;; color?
				    ;; :fg (fatchar-fg c)
				    ;; :bg (fatchar-bg c)
				    :fg (or (car fg) (fatchar-fg c))
				    :bg (or (car bg) (fatchar-bg c))
				    :line (fatchar-line c)
				    :attrs (union attrs (fatchar-attrs c))); <--
		      thing))
		   (incf i)))
	       (character
		(funcall out-func
		 (make-fatchar :c s :fg (car fg) :bg (car bg) :attrs attrs)
		 thing)
		(incf i))
	       (list
		(let* ((f (first s))
		       (tag (and (or (keywordp f) (symbolp f)) f))
		       (rest (cdr s))
		       value ignore-the-rest)
		  (if tag
		      (let ((fg fg) (bg bg) (attrs attrs)
			    (tag-str (string tag)))
			(declare (special fg bg attrs))
			(cond
			  ((and (> (length tag-str) 3)
				(string= (subseq tag-str 0 3) "FG-"))
			   (push (keywordify (subseq (string tag) 3)) fg))
			  ((and (> (length tag-str) 3)
				(string= (subseq tag-str 0 3) "BG-"))
			   (push (keywordify (subseq (string tag) 3)) bg))
			  ((known-color-name-p tag)
			   ;; An un-prefixed color is a foreground color.
			   (push tag fg))
			  ((member tag *known-attrs*)
			   (push tag attrs))
			  ((and (eq tag :fg) (eq (second s) :color))
			   (push (third s) fg)
			   (setf rest (cdddr s)))
			  ((and (eq tag :bg) (eq (second s) :color))
			   (push (third s) bg)
			   (setf rest (cdddr s)))
			  ((and (eq tag :object) (cdr rest))
			   (push (second s) attrs)
			   (setf rest (cddr s)))
			  (t
			   (if unknown-func
			       (progn
				 (setf (values value ignore-the-rest)
				       (funcall unknown-func s))
				 (spanky value))
			       (push tag attrs))))
			;; (format t "tag ~s attrs ~s (cdr s) ~s~%"
			;; 	tag attrs (cdr s))
			(when (not ignore-the-rest)
			  (spanky rest))
			;;(setf fg nil bg nil)
			;;(pop attrs)
			)
		      (progn
			(spanky f)
			(spanky (cdr s))))))
	       (t
		(when unknown-func
		  (spanky (funcall unknown-func s))))))))
      (spanky span)))
  thing)

(defun out-to-fatchar-string (fatchar string)
  (vector-push-extend fatchar string))

(defun span-to-fatchar-string (span &key (start 0) end
				      unknown-func filter fatchar-string)
  #.(s+ "Make a FATCHAR-STRING from SPAN. A span is a list representation of a
fatchar string. The grammar is something like: "
	*span-grammar*
	*span-args* "
  - FATCHAR-STRING can be provided as an already created adjustable string with a
    fill-pointer to put the result into.")

  (when (not fatchar-string)
    (setf fatchar-string (make-array 40
				     :element-type 'fatchar
				     :initial-element (make-fatchar)
				     :fill-pointer 0
				     :adjustable t)))
  (setf (fill-pointer fatchar-string) 0)
  (span-to-thing span #'out-to-fatchar-string fatchar-string
		 :start start :end end :unknown-func unknown-func
		 :filter filter))

(defun fatten (thing)
  "Turn almost anything into a fat-string."
  (typecase thing
    ((or string character)
     (string-to-fat-string thing))
    (list
     (span-to-fat-string thing))
    (t
     (string-to-fat-string (princ-to-string thing)))))

#|
(defun map-span-strings (span &key (start 0) end fat-string)
  "Make a fat string from a span."
  (labels ((spanky (s)
	     (when s
	       (typecase s
		 (string
		  )
		 (list
		  (let* ((f (first s))
			 (tag (and (or (keywordp f) (symbolp f)) f)))
		    (if tag
			(progn
			  (spanky (cdr s)))
			(progn
			  (spanky f)
			  (spanky (cdr s)))))))))))
      (spanky span)))
  fat-string)
|#

(defparameter *xterm-base-colors*
  #(#(:rgb8 #x00 #x00 #x00) ; black
    #(:rgb8 #xff #x00 #x00) ; red
    #(:rgb8 #x00 #xff #x00) ; green
    #(:rgb8 #xff #xff #x00) ; yellow
    #(:rgb8 #x00 #x00 #xff) ; blue
    #(:rgb8 #xff #x00 #xff) ; magenta
    #(:rgb8 #x00 #xff #xff) ; cyan
    #(:rgb8 #xdd #xdd #xdd) ; dim white
    #(:rgb8 #x44 #x44 #x44) ; bright black
    #(:rgb8 #xff #x22 #x22) ; bright red
    #(:rgb8 #x22 #xff #x22) ; bright green
    #(:rgb8 #xff #xff #x22) ; bright yellow
    #(:rgb8 #x25 #x25 #xff) ; bright blue
    #(:rgb8 #xff #x22 #xff) ; bright magenta
    #(:rgb8 #x22 #xff #xff) ; bright cyan
    #(:rgb8 #xff #xff #xff) ; white
    )
  "Normal standard xterm color values.")

;; see xterm/256colres.pl
(defun make-xterm-color-table ()
  ;; XXX This assumes the return type of make-color is a simple-vector, which
  ;; it is, but we probably shouldn't assume it.
  (let* ((placeholder-color (make-color :rgb8 :red 0 :green 0 :blue 0))
	 (result (make-array 256 :element-type (type-of placeholder-color))))
    ;; (declare (type (vector t 256) result))
    (declare (type (simple-vector 256) result))
    ;; colors 0-16 are the standard colors.
    ;; Unfortunately we don't really know what they are unless we query it
    ;; from the terminal, which is a big pain. So we just fake it, and use
    ;; the default colors from xterm, which is probably wrong for other
    ;; emulators.
    (loop :for i :from 0 :below 16 :do
       (setf (aref result i) (aref *xterm-base-colors* i)))

    ;; colors 16-231 are a 6x6x6 color cube
    (loop :for red :from 0 :below 6 :do
       (loop :for green :from 0 :below 6 :do
	  (loop :for blue :from 0 :below 6 :do
	     (setf (aref result
			 (+ 16 (* red 36) (* green 6) blue))
		   (make-color :rgb8
			       :red   (if (= red   0) 0 (+ (* red   40) 55))
			       :green (if (= green 0) 0 (+ (* green 40) 55))
			       :blue  (if (= blue  0) 0 (+ (* blue  40) 55)))))))
    ;; colors 232-255 are a grayscale ramp, without black & white
    (loop :with level
       :for gray :from 0 :below 24 :do
       (setf level (+ (* gray 10) 8)
	     (aref result (+ 232 gray))
	     (make-color :rgb8 :red level :green level :blue level)))
    result))

(defparameter *xterm-256-color-table* (make-xterm-color-table)
  "Table for old-timey xterm colors.")
(declaim (type (vector t 256) *xterm-256-color-table*))

(defvar *color-cache* (make-hash-table))

;; Could we calculate this better?
(defun get-nearest-xterm-color-index (color)
  "Return the index of the closest match for COLOR in the
*xterm-256-color-table*."
  ;; (declare (optimize (speed 3) (safety 0) (debug 0) (space 2)
  ;; 		     (compilation-speed 0)))
  ;; (when (not *xterm-256-color-table*)
  ;;   (make-xterm-color-table))
  (let* ((c (convert-color-to color :rgb8))
	 (r (color-component c :red))
	 (g (color-component c :green))
	 (b (color-component c :blue))
	 (pixel (logior (ash r 16) (ash g 8) b))
	 (hit (gethash pixel *color-cache*)))
    (declare (type fixnum pixel r g b))
    (or hit
	(let ((best -1) (result 0) (dist 0) (d-red 0) (d-green 0) (d-blue 0))
	  (declare (type fixnum best result dist d-red d-green d-blue))
	  (loop :for i :from 0 :below (length *xterm-256-color-table*)
	     :do
	     (setf d-red
		   (- r
		      (color-component (aref *xterm-256-color-table* i) :red))
		   d-green
		   (- g
		      (color-component (aref *xterm-256-color-table* i) :green))
		   d-blue
		   (- b
		      (color-component (aref *xterm-256-color-table* i) :blue))
		   dist
		   (+ (* d-red d-red) (* d-green d-green) (* d-blue d-blue)))
	     (when (or (minusp best)
		       (<= dist best)) ;; Take the highest index one
	       (setf best dist         ;; because the base colors may be wrong
		     result i)))
	  (setf (gethash pixel *color-cache*) result)))))

(defun ansi-params-to-fatchar (fatchar params)
  "Take a list of ANSI attribute parameter numbers, and a ‘fatchar’, and modify
the fatchar according to ‘params’. Return four values:
  the modified fatchar,
  a list of attributes added,
  a list of attributes removed,
  a new foreground or :unset if it wasn't changed,
  a new background or :unset.

Indexed pallet colors are looked up in *xterm-256-color-table*, which should
have a reasonable default, but of course can be bound for customized colors.

Note that this doesn't do any error checking on color values."
  (let ((attrs (copy-list (fatchar-attrs fatchar)))
	(fg :unset)
	(bg :unset)
	added removed hi-color hi-color-type r g b)
    (labels ((remove-attr (a)
	       ;; (when (find a (fatchar-attrs fatchar))
	       ;; 	 (pushnew a removed))
	       (pushnew a removed)
	       (setf attrs (delete a attrs)))
	     (remove-attrs (l)
	       (mapcar #'remove-attr l))
	     (add-attr (a)
	       (pushnew a added)
	       (pushnew a attrs))
	     (clear ()
	       ;; Remove all the attrs that were the original.
	       (remove-attrs (fatchar-attrs fatchar))
	       ;; And any that were added.
	       (remove-attrs attrs)
	       (setf added nil
		     (fatchar-attrs fatchar) '()
		     fg :default bg :default)))
      (loop :for p :in params :do
        (cond
	  ((and hi-color (not hi-color-type))
	   (case p
	     (2 (setf hi-color-type :3-color))
	     (5 (setf hi-color-type :1-color))))
	  ((eq hi-color-type :1-color)
	   (if (eq hi-color :fg)
	       (setf fg (aref *xterm-256-color-table* p))
	       (setf bg (aref *xterm-256-color-table* p)))
	   (setf hi-color nil hi-color-type nil))
	  ((eq hi-color-type :3-color)
	   (cond
	     ((not r) (setf r p))
	     ((not g) (setf g p))
	     ((not b) (setf b p)
	      (if (eq hi-color :fg)
		  (setf fg (make-color :rgb8 :red r :green g :blue b))
		  (setf bg (make-color :rgb8 :red r :green g :blue b)))
	      (setf hi-color nil hi-color-type nil r nil g nil b nil))))
	  (t
	   (case p
	     (0  (clear))
	     (1  (add-attr :bold))
	     (2  (add-attr :dim))
	     (3  (add-attr :italic))
	     (4  (add-attr :underline))
	     (5  (add-attr :blink))
	     (7  (add-attr :inverse))
	     (8  (add-attr :invisible))
	     (9  (add-attr :crossed-out))
	     (21 (add-attr :double-underline))
	     (22 (remove-attr :bold))
	     (23 (remove-attr :italic))
	     (24 (remove-attr :underline))
	     (25 (remove-attr :blink))
	     (27 (remove-attr :inverse))
	     (28 (remove-attr :invisible))
	     (29 (remove-attr :crossed-out))
	     (30 (setf fg :black))
	     (31 (setf fg :red))
	     (32 (setf fg :green))
	     (33 (setf fg :yellow))
	     (34 (setf fg :blue))
	     (35 (setf fg :magenta))
	     (36 (setf fg :cyan))
	     (37 (setf fg :white))
	     (38 (setf hi-color :fg))
	     (39 (setf fg :default))
	     (40 (setf bg :black))
	     (41 (setf bg :red))
	     (42 (setf bg :green))
	     (43 (setf bg :yellow))
	     (44 (setf bg :blue))
	     (45 (setf bg :magenta))
	     (46 (setf bg :cyan))
	     (47 (setf bg :white))
	     (48 (setf hi-color :bg))
	     (49 (setf bg :default))
	     (otherwise
	      #| just ignore unknown colors or attrs |#)))))
      (when (or added removed)
	(setf (fatchar-attrs fatchar) attrs))
      (when (not (eq fg :unset))
	(setf (fatchar-fg fatchar) fg))
      (when (not (eq bg :unset))
	(setf (fatchar-bg fatchar) bg))
      (when (not params)
	(clear))
      (values fatchar added removed fg bg))))

;;; ^[[00m	normal
;;; ^[[01;34m	bold, blue fg
;;; ^[[m	normal
;;; ^[[32m	green fg
;;; ^[[1m	bold

(defun grok-ansi-color (str &key (start 0))
  "Take the ostring ‘str’ with an ANSI terminal color escape sequence, starting
after the ^[[ and return NIL if there was no valid sequence, or the values of
an integer offset to after the sequence, the foreground, background and a list
of attributes. NIL stands for whatever the default is, and :UNSET means that
they were not set in this string."
  (let* ((i start)
	 (len (olength str))
	 (hi-color nil)
	 (fg :unset)
	 (bg :unset)
	 (attr '())
	 num offset attr-was-set hi-color-type r g b)
    (loop
       :while (< i len)
       :do
       (setf (values num offset) (oparse-integer str :start i :junk-allowed t))
       ;; (dbugf :fatchar "@~s num ~s offset ~s~%" i num offset)
       (if (or (not num) (not offset))
	   (progn
	     ;; Just an #\m without arguments means no attrs and unset color
	     ;; (dbugf :fatchar "@~s done ~a" i
	     ;;        (if (ochar= (ochar str i) #\m) "final m" "no numbers?"))
	     (when (ochar= (ochar str i) #\m)
	       (setf attr '() fg nil bg nil attr-was-set t i (1+ i)))
	     (return))
	   (progn
	     (setf i offset)
	     (when (and (< i len)
			(or (ochar= (ochar str i) #\;)
			    (ochar= (ochar str i) #\m)))
	       (incf i)
	       (cond
		 ((and hi-color (not hi-color-type))
		  ;; (dbugf :fatchar "@~s hi-color ~s~%" i num)
		  (case num
		    (2 (setf hi-color-type :3-color))
		    (5 (setf hi-color-type :1-color))))
		 ((eq hi-color-type :1-color)
		  ;; (dbugf :fatchar "@~s 1-color ~s ~s~%" i hi-color num)
		  ;; (when (not *xterm-256-color-table*)
		  ;;   (make-xterm-color-table))
		  (if (eq hi-color :fg)
		      (setf fg (aref *xterm-256-color-table* num))
		      (setf bg (aref *xterm-256-color-table* num)))
		  (setf hi-color nil hi-color-type nil))
		 ((eq hi-color-type :3-color)
		  ;; (dbugf :fatchar "@~s 3-color ~s ~s~%" i hi-color num)
		  (cond
		    ((not r) (setf r num))
		    ((not g) (setf g num))
		    ((not b) (setf b num)
		     ;; (dbugf :fatchar "@~s 3-color end ~s ~s~%" i hi-color num)
		     (if (eq hi-color :fg)
			 (setf fg (make-color :rgb8 :red r :green g :blue b))
			 (setf bg (make-color :rgb8 :red r :green g :blue b)))
		     (setf hi-color nil hi-color-type nil r nil g nil b nil))))
		 (t
		  ;; (dbugf :fatchar "@~s num ~s~%" i num)
		  (case num
		    (0  (setf attr '() fg nil bg nil attr-was-set t))
		    (1  (pushnew :bold attr)      (setf attr-was-set t))
		    (2  (pushnew :dim attr)       (setf attr-was-set t))
		    (3  (pushnew :italic attr)    (setf attr-was-set t))
		    (4  (pushnew :underline attr) (setf attr-was-set t))
		    (5  (pushnew :blink attr)     (setf attr-was-set t))
		    (7  (pushnew :inverse attr)   (setf attr-was-set t))
		    (8  (pushnew :invisible attr) (setf attr-was-set t))
		    (9  (pushnew :crossed-out attr) (setf attr-was-set t))
		    (21 (pushnew :double-underline attr) (setf attr-was-set t))
		    (22 (setf attr (delete :bold attr))
			(setf attr-was-set t))
		    (23 (setf attr (delete :italic attr))
			(setf attr-was-set t))
		    (24 (setf attr (delete :underline attr))
			(setf attr-was-set t))
		    (25 (setf attr (delete :blink attr))
			(setf attr-was-set t))
		    (27 (setf attr (delete :inverse attr))
			(setf attr-was-set t))
		    (28 (setf attr (delete :invisible attr))
			(setf attr-was-set t))
		    (29 (setf attr (delete :crossed-out attr))
			(setf attr-was-set t))
		    (30 (setf fg :black))
		    (31 (setf fg :red))
		    (32 (setf fg :green))
		    (33 (setf fg :yellow))
		    (34 (setf fg :blue))
		    (35 (setf fg :magenta))
		    (36 (setf fg :cyan))
		    (37 (setf fg :white))
		    (38 (setf hi-color :fg))
		    (39 (setf fg nil))
		    (40 (setf bg :black))
		    (41 (setf bg :red))
		    (42 (setf bg :green))
		    (43 (setf bg :yellow))
		    (44 (setf bg :blue))
		    (45 (setf bg :magenta))
		    (46 (setf bg :cyan))
		    (47 (setf bg :white))
		    (48 (setf hi-color :bg))
		    (49 (setf bg nil))
		    (otherwise #| just ignore unknown colors or attrs |#))))
	       (when (ochar= (ochar str (1- i)) #\m)
		 ;; (dbugf :fatchar "@~s done ~s~%" i num)
		 (return))))))
    (values
     ;; (if (and (eq fg :unset) (eq bg :unset) (not attr-was-set))
     ;; 	 1
     ;; 	 (- i start))
     i
     fg bg (if (not attr-was-set) :unset attr))))

(defun process-ansi-colors (fat-line)
  "Convert ANSI color escapes into colored fatchars. ‘fat-line’ can be an
ostring, but if it's a vector, it's assumed to be a vector of fatchar, aka a
fatchar-string. Returns a either fatchar-string if it was given one, or a
fat-string otherwise."
  (when (zerop (olength fat-line))
    (return-from process-ansi-colors fat-line))
  (let* ((input-line
	  (etypecase fat-line
	    ((or ostring string) fat-line)
	    (fatchar-string
	     (make-fat-string :string fat-line))))
	 (len (olength input-line))
	 (new-fat-line (make-stretchy-vector len :element-type 'fatchar))
	 (i 0)
	 fg bg attrs)
    (labels ((char-at (i)
	       (osimplify (oaref input-line i)))
	     (looking-at-attrs ()
	       "Return true if might be looking at some attrs."
	       (and (< i (1- len))
		    (char= (char-at i) #\escape)
		    (char= (char-at (1+ i)) #\[)))
	     (get-attrs ()
	       "Get the attrs we might be looking at."
	       (incf i 2)		; the ^[ and [
	       (multiple-value-bind (offset i-fg i-bg i-attrs)
		   (grok-ansi-color input-line :start i)
		 ;; (dbugf :fatchar "grok offset ~s fg ~s bg ~s attrs ~s~%"
		 ;;        offset i-fg i-bg i-attrs)
		 (when offset
		   (unless (eq i-fg    :unset) (setf fg i-fg))
		   (unless (eq i-bg    :unset) (setf bg i-bg))
		   (unless (eq i-attrs :unset) (setf attrs i-attrs))
		   ;;(incf i inc))))	; for the parameters read
		   (setf i offset))))	; for the parameters read
	     (copy-char ()
	       "Copy the current character to result."
	       ;; (dbugf :attrs "attrs = ~a~%" attrs)
	       ;; (dbugf :attrs "(aref fat-line i) = ~a~%" (aref fat-line i))
	       (let ((new-attrs
		      (if (fatchar-p (oaref input-line i))
			  (union attrs (fatchar-attrs (oaref input-line i)))
			  attrs)))
		 (stretchy:stretchy-append
		  new-fat-line (make-fatchar
				:c (char-at i) ;;(fatchar-c (aref fat-line i))
				:fg fg :bg bg
				:attrs (copy-seq new-attrs))))
	       (incf i)))
      (loop :while (< i len) :do
	 (if (looking-at-attrs)
	     (get-attrs)
	     (copy-char))))
    (if (ostringp fat-line)
	(make-fat-string :string new-fat-line)
	new-fat-line)))

(defun remove-effects (thing)
  "Remove any terminal colors or attributes from THING, which can be a string,
a fat-string, or a fatchar."
  (etypecase thing
    (string
     (fatchar-string-to-string
      (process-ansi-colors
       (make-fatchar-string thing))))
    (fat-string
     (map nil #'remove-effects (fat-string-string thing))
     thing)
    (fatchar
     (setf (fatchar-fg thing) nil
	   (fatchar-bg thing) nil
	   (fatchar-attrs thing) nil)
     thing)
    (character thing)))

;; Methods from char-util:

(defmethod display-length ((c fatchar))
  "Return the length of the fat character for display."
  (cond
    ((not (zerop (fatchar-line c)))
     1)				    ; assume line drawing can happen in 1 cell
    ;; ((char= #\nul (fatchar-c c))
    ;;  0)		; since an unset fatchar is #\nul
    (t (display-length (fatchar-c c)))))

(defmethod display-length ((s fat-string))
  "Return the length of the string for display."
  (display-length (fat-string-to-string s)))

(defmethod simplify-string ((s fat-string))
  "Return the fat-string as a string."
  (fat-string-to-string s))

(defmethod osimplify ((thing fat-string))
  (fat-string-to-string thing))

(defmethod osimplify ((thing fatchar))
  (fatchar-c thing))

(defmethod simplify-char ((c fatchar))
  "Return the FATCHAR as a character."
  (fatchar-c c))

(defmethod graphemes ((string fat-string))
  ;; It should already be graphemes.
  (map 'list #'identity (fat-string-string string)))

;; @@@ I'm not sure this is really the best place for this. But we need it for
;; grok-ansi-color. It's not fatchar specific but works on ochars, but since
;; there's a string method that wraps the normal implementaion function, this
;; one will probably only be used for fat-strings. And it's stolen from sbcl
;; of course.
(defmethod oparse-integer ((string ostring)
			  &key (start 0) end (radix 10) junk-allowed)
  (flet ((parse-error (format-control)
           ;;(declare (optimize #+sbcl sb-kernel:allow-non-returning-tail-call))
           (error 'simple-parse-error
                  :format-control format-control
                  :format-arguments (list string))))
    ;; @@@ I know with-array-data is way more optimized than this
    (let* ((start (or start 0))
	   (end (or end (olength string))))
      (let ((index
	     (do ((i start (1+ i)))
		 ((= i end)
		  (if junk-allowed
		      (return-from oparse-integer (values nil end))
		      (parse-error
		       "string is all whitespace characters ~S.")))
	       (declare (fixnum i))
	       (unless (whitespace-p (osimplify (ochar string i))) (return i))))
            (minusp nil)
            (found-digit nil)
            (result 0))
        (declare (fixnum index))
        (let ((char (ochar string index)))
          (cond ((ochar= char #\-)
                 (setf minusp t)
                 (incf index))
                ((ochar= char #\+)
                 (incf index))))
        (loop
         (when (= index end) (return nil))
         (let* ((char (ochar string index))
                (weight (odigit-char-p char radix)))
           (cond (weight
                  (setf result (+ weight (* result radix))
                        found-digit t))
                 (junk-allowed (return nil))
                 ((whitespace-p (osimplify char))
                  (loop
                   (incf index)
                   (when (= index end) (return))
                   (unless (whitespace-p (osimplify (char string index)))
                     (parse-error "junk in string ~S")))
                  (return nil))
                 (t
                  (parse-error "junk in string ~S"))))
         (incf index))
        (values
         (if found-digit
             (if minusp (- result) result)
             (if junk-allowed
                 nil
                 (parse-error "no digits in string ~S")))
         ;; (- index offset)
	 index)))))

;; EOF
