;;
;; dlib.lisp - Dan's utils of redundant Doom.
;;

;; These are mostly solving problems that have already been solved.
;; But it's mostly stuff that I need just to start up.
;; So:
;;  - Don't add any dependencies.
;;  - Try to keep it minimal.
;; More optional stuff can go in dlib-misc.

#+debug-rc (progn (format t "[ dlib ") (force-output *standard-output*))

(defpackage :dlib
  (:use :common-lisp
	;; We must have the MOP!!! Don't ever drop the MOP!
	#+mop :mop
	#+sbcl :sb-mop
	#+cmu :pcl
	#+ccl :ccl
	#+(or ecl clasp) :clos
	#+lispworks :hcl
	;; extensions
	#+sbcl :sb-ext
	)
  (:documentation
   "Dan's generally useful miscellaneous functions.
 I usually have this loaded automatically at startup.")
  #+lispworks (:shadow #:lambda-list #:with-unique-names)
  (:export
   ;; System-ish
   #:d-getenv
   #:shell-line
   #:shell-lines
   #:system-command-stream
   #:system-args
   #:exit-system
   #:save-image-and-exit
   #:overwhelming-permission
   #:exe-in-path-p
   #:try-things
   ;; io-ish
   #:resilient-read-line
   #:with-open-file-or-stream
   #:with-lines
   #:get-lines
   #:safe-read-from-string
   #:clean-read-from-string
   #:package-robust-read-from-string
   #:package-robust-read
   #:*buffer-size*
   #:copy-stream
   #:quote-format
   ;; sequences
   #:initial-span
   #:split-sequence
   #:split-sequence-if
   #:replace-subseq
   #:begins-with
   #:ends-with
   #:remove-prefix
   #:remove-suffix
   #:s+
   #:ltrim #:rtrim #:trim
   #:join
   #-clisp #:doseq
   ;; lists
   #:delete-nth
   #:alist-to-hash-table
   #:flatten
   #:range-list
   #:range-array
   #:range-lazy #:range-start #:range-end #:range-step #:make-range
   ;; objects
   #:shallow-copy-object
   #:*mop-package*
   #:d-add-feature
   #:d-remove-feature
   ;; ccl added it's own version
   #-ccl-1.6 #:add-feature
   #-ccl-1.6 #:remove-feature
   #:has-feature
;   #:with-struct-slots
   ;; Implementation-ish
   #:without-warning
   #+sbcl #:without-notes
   ;; language-ish
   #:define-constant
   #:define-alias #:defalias
   #-(or lispworks clasp) #:λ
   #:_
   #:symbolify
   #:keywordify
   #:likely-callable
   #-lispworks #:lambda-list
   #:lambda-list-vars
   #-lispworks #:with-unique-names
   #:with-package
   #:shortest-package-nick
   #:not-so-funcall #:symbol-call #-clasp #:※
   #:@
   #:ignore-conditions #:ignore-some-conditions
   ;; debugging
   #:*dbug* #:*dbug-package* #:*dbug-facility*
   #:dbug #:dbugf #:with-dbug #:with-dbug-package #:with-dbugf
   #:without-dbug
   #:dump-values
   ;; Environment features
   #:*host*
   #:*arch*
   #:*arch-nickname*
   #:*os*
   #:*lisp-implementation-nickname*
   #:*lisp-version*
   #:*lisp-version-number*
   #:*platform-nickname*
  )
)
(in-package :dlib)

(declaim (optimize (debug 3)))
;; (declaim (optimize (speed 3) (safety 0) (debug 0) (space 0)
;; 		   (compilation-speed 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; This should be "pure" Common Lisp, since it's compile time, e.g. we can't
  ;; use split-sequence or _ yet.
  (defun extract-version-number (version-string)
    "Make a single testable value out of a typical result of
(lisp-implementation-version)"
    (let (i spos (sum 0) (pos 0) (mag #(10000 100 1))
	    (str (substitute #\space #\. version-string)))
      (loop :with n = 0
	 :while (and (< n 3) (< pos (length str)) (digit-char-p (aref str 0)))
	 :do
	 (setf (values i spos) (parse-integer (subseq str pos) :junk-allowed t))
	 (incf sum (* i (aref mag n)))
	 (incf pos spos)
	 (incf n)
	 ;; (format t "~a ~a ~a ~a~%" n i pos sum)
	 )
      sum))

  ;; We need to have this early so we can make decisions based on it.
  (defparameter *lisp-version-number*
    #+sbcl (extract-version-number (lisp-implementation-version))
    #+ccl (extract-version-number
	   (let ((s (lisp-implementation-version)))
	     (setf s (subseq s (position-if (lambda (x) (digit-char-p x)) s))
		   s (subseq s 0 (position-if
				  (lambda (x)
				    (not (or (digit-char-p x) (eql x #\.))))
				  s)))
	     s))
    #-(or sbcl ccl) nil ;; You will have to put something here if it matters.
    "A version number for doing comparisons. Greater numbers should indicate
later versions.")

  ;; Feature Fiddling

  ;; As per CCL, it does make sense to aquire a mutex when adding features.
  ;; But how can we even do this portably at this point?

  (defun d-add-feature (f)
    "Add a feature named by the given string to *FEATURES*"
    (assert f)
    (if (stringp f)
	;;(nconc *features* (list (intern (string-upcase f) :keyword)))
	;;(nconc *features* (list f))))
	(pushnew (intern (string-upcase f) :keyword) *features*)
	(pushnew f *features*)))
  ;; or one could use:
  ;; (pushnew :feature *features*)

  #-ccl-1.6 ; which has it's own version
  (setf (symbol-function 'add-feature) #'d-add-feature)

  (defmacro has-feature (f)
    "For when #+feature isn't what you want."
    `(find ,f *features*))

  (defun d-remove-feature (f)
    "Remove a feature from *FEATURES*"
    (setq *features* (delete f *features*)))

  #-ccl-1.6 ; which has it's own version
  (setf (symbol-function 'remove-feature) #'d-remove-feature)
)

;; @@@ I should really do this with an error type
(defun missing-implementation (sym)
  "Complain that something is missing."
  (format t "You need to provide an implementation for ~a on ~a~%"
	  sym (lisp-implementation-type)))

(define-condition missing-implementation-error (error)
  ((symbol
    :accessor missing-implementation-error-symbol
    :initarg :symbol
    :type symbol
    :documentation "The symbol which is unimplemented.")
   (format
    :accessor missing-implementation-error-format
    :initarg :format
    :type string
    :documentation "Format control for error reporting.")
   (arguments
    :accessor missing-implementation-error-arguments
    :initarg :arguments
    :type list
    :documentation "Format arguments for error reporting."))
  (:report (lambda (c s)
	     (with-slots (symbol format arguments) c
	       (if format
		   (format s "~? ~a" format arguments symbol)
		   (format s "~a" symbol)))))
  (:documentation "A required function or symbol is missing or unimplemented."))

(defmacro without-warning (&body body)
  "Get rid of stupid warnings that you don't want to see.
 Just wrap your code with this."
  `(handler-bind
    ((warning #'(lambda (c)
		  (declare (ignore c))
		  (muffle-warning))))
     ,@body))

#+sbcl
(defmacro without-notes (&body body)
  "Get rid of compiler notes that you don't want to see.
 Just wrap your code with this."
  `(handler-bind
     ((sb-ext:compiler-note
       #'(lambda (c)
	   (declare (ignore c))
	   (muffle-warning))))
     ,@body))

;; Make sure we have getenv
(defun d-getenv (s)
  #+clisp (ext:getenv s)
  #+sbcl (sb-ext:posix-getenv s)
  #+openmcl (ccl::getenv s)
  #+cmu (let ((v (assoc (intern (string-upcase s) :keyword)
			ext:*environment-list*)))
	  (if v (cdr v)))
  #+ecl (si::getenv s)
  #+excl (sys::getenv s)
  #+lispworks (hcl:getenv s)
  #+gcl (system:getenv s)
  #+abcl (ext:getenv s)
  #+clasp (ext:getenv s)
  #-(or clisp sbcl openmcl cmu ecl excl lispworks gcl abcl clasp)
  (missing-implementation 'd-getenv))

;; Compare functions:
;;(defun foo1 (s l) (position l s :key #'list :test #'intersection))
;;(defun foo2 (s l) (position-if (lambda (c) (member c l)) s))
;;
;; on clisp foo2 is faster and less consing
;;
;; TODO: it might be nice if this could accecpt a string for the 2nd arg
;; but would it be better to make it a generic function specializing on
;; the 2nd arg?
;;
;; Apparently if I use member instead of find it's slightly faster for lists
;; Who cares?

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun initial-span (s l)
    "Return the initial portion of sequence S consiting of objects
 not in the list L"
    (subseq s 0 (position-if (lambda (c) (find c l)) s))))

;; Another square wheel.
;; @@@ I should probably either fix this to have the additional functionality
;; of the public version, or just copy the public version here.
;;
;; The public version:
;;
;; (defun split-sequence (delimiter seq &key (count nil)
;;                         (remove-empty-subseqs nil) (from-end nil) (start 0)
;;                         (end nil) (test nil test-supplied)
;;                         (test-not nil test-not-supplied)
;;                         (key nil key-supplied))
;;   "Return a list of subsequences in seq delimited by delimiter.
;;
;; If :remove-empty-subseqs is NIL, empty subsequences will be included
;; in the result; otherwise they will be discarded.  All other keywords
;; work analogously to those for CL:SUBSTITUTE.  In particular, the
;; behaviour of :from-end is possibly different from other versions of
;; this function; :from-end values of NIL and T are equivalent unless
;; :count is supplied. The second return value is an index suitable as an
;; argument to CL:SUBSEQ into the sequence indicating where processing
;; stopped."
;;
;; Lispworks version:
;;
;; Arguments: (SEPARATOR-BAG SEQUENCE &KEY (START 0) END (TEST (QUOTE EQL))
;;             KEY COALESCE-SEPARATORS)
;; Return a list of subsequences of SEQUENCE, split by the elements in the
;; sequence SEPARATOR-BAG.
;;
;; Everybody seems to agree that:
;;   - Separator first arg, sequence second arg
;;   - :start and :end keywords with :start defaulting to 0
;;   - :test keyword, presumably defaulting to something like #'eql
;;   - :key keyword
;; Disagreement on:
;;   - Should the separator be a sequence itself?
;;   - Should there be a :from-end keyword?
;;   - Should there be a :test-not keyword?
;;   - What the keyword I call :omit-empty should be called.
;;   - What the second value should be, if any.
;;
;; @@@ I need to add :end and :key
;;
;; I like the separator-bag idea, but I also like the separator sequence idea.
;; Of course the separator-bag  can just be achived with :test, something like:
;;   :test #'(lambda (a b) (declare (ignore a)) (position b "1289"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro flurp (function sep seq start end test)
    (let ((base
	   `(,function ,sep ,seq 
		       ,@(if (eq function 'position)
			     '(:start) '(:start2))
		       ,start
		       ,@(when end
			       `(,(if (eq function 'position)
				      :end :end2)
				  ,end)))))
      `(if ,test ,(append base (list :test test)) ,base)))

  (defun split-sequence (sep seq &key
			 omit-empty coalesce-separators remove-empty-subseqs
			 (start 0) end test #| key count |#)
    "Split the sequence SEQ into subsequences separated by SEP. Return a list of
the subsequences. SEP can be a sequence itself, which means the whole sequence
is the separator. If :omit-empty is true, then don't return empty subsequnces.
 :coalesce-separators :remove-empty-subseqs are compatability synonyms for
:omit-empty."
    (declare (type sequence seq))
    (declare (type boolean omit-empty coalesce-separators remove-empty-subseqs))
    (setf omit-empty (or omit-empty coalesce-separators remove-empty-subseqs))
    (let* ((sep-is-seq (typecase sep (vector t) (list t) (t nil)))
	   (sep-len (if sep-is-seq (length sep) 1))
	   (seq-len (if omit-empty (length seq) 0)))
      (declare (type boolean sep-is-seq))
      (declare (type fixnum sep-len seq-len))
      ;;(format t "sep-is-seq = ~w sep-len = ~w seq-len = ~w ~%"
      ;;        sep-is-seq sep-len seq-len count)
      ;;(setf test #'(lambda (x) 
      (macrolet
	  ((loopy (func t-omit-empty)
	      `(loop
		  :with t-start fixnum = start :and t-end
		  :do
		  ;;(format t "FLURP: ~w~%"
		  ;;    '(flurp ,func sep seq t-start end test))
		  (setq t-end
			(or (flurp ,func sep seq t-start end test)
			    -1))
		  :while (>= t-end 0)
		  ;;(format t "start = ~d end = ~d ~s~%" t-start t-end
		  ;;(subseq seq t-start t-end))
		  ,@(if t-omit-empty '(if (not (= t-start t-end))))
		  :collect (subseq seq t-start t-end) :into results
		  :do (setq t-start (+ t-end sep-len))
		  :finally (return-from nil
			     ,@(if t-omit-empty
				   '((if (= t-start seq-len)
					 results
					 (nconc results
						(list
						 (subseq seq t-start end)))))
				   '((nconc results
				      (list (subseq seq t-start end)))))))))
	(when (or (> sep-len 0) test)
	  (when (and test (< sep-len 1))
	    (setf sep-len 1 sep '(nil))) ; fake separator!
	  (if sep-is-seq
	      (if omit-empty
		  (if test
		      (loopy search t)
		      (loopy search t))
		  (if test
		      (loopy search nil)
		      (loopy search nil)))
	      (if omit-empty
		  (if test
		      (loopy position t)
		      (loopy position t))
		  (if test
		      (loopy position nil)
		      (loopy position nil))))))))

(defun split-sequence-if (predicate seq &key omit-empty coalesce-separators
					  remove-empty-subseqs
					  (start 0) end key
					  #| count |#)
  "Split the sequence SEQ into subsequences separated by SEP. Return a list of
the subsequences. SEP can be a sequence itself, which means the whole sequence
is the separator. If :omit-empty is true, then don't return empty subsequnces.
 :coalesce-separators :remove-empty-subseqs are compatability synonyms for
:omit-empty."
  (declare (type sequence seq))
  (declare (type boolean omit-empty coalesce-separators remove-empty-subseqs))
  (declare (ignore end key))
  (setf omit-empty (or omit-empty coalesce-separators remove-empty-subseqs))
  (let* ((seq-len (if omit-empty (length seq) 0)))
    (declare (type fixnum seq-len))
    (macrolet
	((loopy (t-omit-empty)
	    `(loop
		:with t-start fixnum = start :and t-end fixnum
		:do (setf t-end (or (position-if predicate seq :start t-start)
				    -1))
		:while (>= t-end 0)
		,@(if t-omit-empty '(if (not (= t-start t-end))))
		:collect (subseq seq t-start t-end) :into results
		:do (setf t-start (1+ t-end))
		:finally (return-from nil
			   ,@(if t-omit-empty
				 '((if (= t-start seq-len)
				       results
				       (nconc results
					      (list (subseq seq t-start)))))
				 '((nconc results
				    (list (subseq seq t-start)))))))))
      (when predicate
	(if omit-empty
	    (loopy t)
	    (loopy nil)))))))

;; cl-ppcre's version is better, so I recommend you just use that.
;;(setf (fdefinition 'split) #'split-sequence)

#|
(defun test-ss ( #| &key (n 1000) |# )
  (format t "~@{~w~%~}"
	  (split-sequence #\/ "/usr/local/bin/" :omit-empty t)
	  (split-sequence #\/ "/usr/local/bin/" :omit-empty nil)
	  (split-sequence "/" "/usr/local/bin/" :omit-empty t)
	  (split-sequence "/" "/usr/local/bin/" :omit-empty nil)
	  (split-sequence #\. "www.pooopoooho.com" :omit-empty t)
	  (split-sequence #\space " what  the fuck  " :omit-empty t))
;   (time (dotimes (x n)
; 	  (split-sequence #\/ "/usr/local/bin/"
; 			  :omit-empty nil)))
;   (terpri)
;   (time (dotimes (x n)
; 	  (fake-split-sequence #\/ "/usr/local/bin/"
; 			       :omit-empty nil)))
)
|#

(defun replace-subseq (target replacement in-seq &key count)
  "Return a copy of IN-SEQ but with sequences of FROM-SEQ replaced with TO-SEQ."
  (if (and (> (length target) 0) (or (not count) (> count 0)))
      (let ((pos 0)
	    (i 0)
	    (n 0)
	    new)
	(loop :while (setf pos (search target in-seq :start2 i))
	   :do
	   ;;(format t "i = ~a pos = ~a new = ~a~%" i pos new)
	   (setf new (nconc new (list (subseq in-seq i pos) replacement)))
	   (setf i (+ pos (length target)))
	   ;;(format t "i = ~a pos = ~a new = ~a~%" i pos new)
	   (incf n)
	   :until (and count (>= n count)))
	(setf new (nconc new (list (subseq in-seq i))))
	;;(apply #'concatenate (append '(string) new)))
	(apply #'concatenate
	       (etypecase in-seq
		 (string 'string)
		 (list 'list)
		 (vector 'vector)
		 (sequence (type-of in-seq)))
	       new))
      (copy-seq in-seq)))

;; @@@ compare vs. the ones in alexandria?
(defun begins-with (this that &key (test #'eql))
  "True if THAT begins with THIS."
  (let ((pos (search this that :test test)))
    (and pos (= 0 pos))))

(defun ends-with (this that &key (test #'eql))
  "True if THAT ends with THIS."
  (let ((pos (search this that :from-end t :test test)))
    (and pos (= pos (- (length that) (length this))))))

(defun remove-prefix (sequence prefix &key (test #'eql))
  "Remove PREFIX from SEQUENCE. SEQUENCE and PREFIX are both sequences.
If SEQUENCE is is not prefixed by PREFIX, just return SEQUENCE."
  (if (begins-with prefix sequence :test test)
      (subseq sequence (length prefix))
      (copy-seq sequence)))

(defun remove-suffix (sequence suffix &key (test #'eql))
  "Remove SUFFIX from the end of SEQUENCE. If SEQUENCE doesn't end in SUFFIX,
just return SEQUENCE. Elements are compared with TEST which defaults to EQL."
  (let ((pos (search suffix sequence :from-end t :test test)))
    (if (and pos (= pos (- (length sequence) (length suffix))))
	(subseq sequence 0 pos)
	(copy-seq sequence))))

(defun s+ (s &rest rest)
  "Return a string which is the arguments concatenated as if output by PRINC."
  ;; This is usually slower:
  ;; (labels ((as-string (s) (if (stringp s) s (princ-to-string s))))
  ;;   (apply #'concatenate 'string (as-string s) (mapcar #'as-string rest))))
  (if rest
    (with-output-to-string (result)
      (princ s result)
      (loop :for x :in rest :do (princ x result)))
    (princ-to-string s)))

(defparameter *ascii-whitespace*
  #(#\tab #\newline #-gcl #\vt #\page #\return #\space)
  "Characters considered whitespace in ASCII.")

#|

@@@ I wish unicode class information built in everywhere.
On SBCL we can generate the list by:

(loop :for c :from 0 :below char-code-limit
   :if (sb-unicode:whitespace-p (code-char c))
   :do (format t "~4,'0x ~a~%" c (char-name (code-char c))))

|#

(defparameter *unicode-whitespace-codes*
    #(#x0085 ; Next line
      #x00A0 ; no-break space
      #x1680 ; ogham space mark
      #x2000 ; en quad
      #x2001 ; em quad
      #x2002 ; en space
      #x2003 ; em space
      #x2004 ; three-per-em space
      #x2005 ; four-per-em space
      #x2006 ; six-per-em space
      #x2007 ; figure space
      #x2008 ; punctuation space
      #x2009 ; thin space
      #x200A ; hair space
      #x2028 ; line separator
      #x2029 ; paragraph separator
      #x202F ; narrow no-break space
      #x205F ; medium mathematical space
      #x3000 ; ideographic space
      )
  "Characters considered whitespace in Unicode")

(defparameter *whitespace* *ascii-whitespace*)

(defun ltrim (string &optional (character-bag *whitespace*))
  "Trim characters CHARACTER-BAG in from the left of STRING. STRING can be any
sequence of characters. CHARACTER-BAG defaults to *WHITESPACE*."
  (if (stringp string)
      (string-left-trim character-bag string)
      (let ((pos (position-if #'(lambda (c)
				  (not (position c character-bag))) string)))
	(subseq string (or pos 0)))))

(defun rtrim (string &optional (character-bag *whitespace*))
  "Trim characters in CHARACTER-BAG from the right of STRING. STRING can be any
sequence of characters. CHARACTER-BAG defaults to *WHITESPACE*."
  (if (stringp string)
      (string-right-trim character-bag string)
      (let ((pos (position-if #'(lambda (c)
				  (not (position c character-bag))) string
				  :from-end t)))
	(subseq string 0 (1+ (or pos (length string)))))))

(defun trim (string &optional (character-bag *whitespace*))
  "Trim characters in CHARACTER-BAG the beginning and end of STRING. STRING
can be any sequence of characters. CHARACTER-BAG defaults to *WHITESPACE*."
  (ltrim (rtrim string character-bag) character-bag))

;; I think I will regret this.
(defgeneric join (this that &key &allow-other-keys)
  (:documentation "Join this to that."))

(defmethod join ((this list) thing &key &allow-other-keys)
  "Return a string with a THING between every element of SEQUENCE. This is
basically the reverse of SPLIT-SEQUENCE."
  (with-output-to-string (str)
    (loop :with first = t
       :for e :in this
       :if first
	 :do (setf first nil)
       :else
	 :do (princ thing str)
       :end
       :do (princ e str))))

(defmethod join ((this vector) thing &key &allow-other-keys)
  "Return a string with a THING between every element of SEQUENCE. This is
basically the reverse of SPLIT-SEQUENCE."
  (with-output-to-string (str)
    (loop :with first = t
       :for e :across this
       :if first
	 :do (setf first nil)
       :else
	 :do (princ thing str)
       :end
       :do (princ e str))))

;; As you may know, improper use of this can cause troublesome bugs.
(defun delete-nth (n list)
  "Delete the Nth elemnt from LIST. This modifies the list.
Also, it can't really delete the first (zeroth) element."
  (if (zerop n)
      (cdr list)
      (let ((cons (nthcdr (1- n) list)))
	(prog1 list
	  (if cons
	      (setf (cdr cons) (cddr cons))
	      cons)))))

(defun alist-to-hash-table (alist table)
;  "Convert an association list into a hash table."
  "Load a hash table with data from an association list."
  (loop :for i :in alist
	:do (setf (gethash (car i) table) (cdr i)))
  table)

;; I cripped this from rosetta code. This seems to be the fastest of 8 methods
;; I tested. I converted it from using DO* to LOOP, and added comments, so I
;; could understand it better. I added PRESERVE-NILS in case you have a tree
;; where NILs are meaningful and not just empty lists.
;;
;; @@@ It might be nice to have a version of this that works on arbitray
;; sequences, but it would probably be slower than this pure list version, and
;; so we would probably still want this one. Flattening other type of sequence
;; trees seems far less common.
(defun flatten (tree &key preserve-nils)
  (declare (optimize (speed 3) (safety 3) (space 2) (compilation-speed 0)))
  ;; Make an outer list. This is the list we modify.
  (let ((result (list tree)))		      
    (loop
       :with node = result
       :until (null node)
       :do
       (if (consp (car node))		      ; If the left side is a cons
	   (progn
	     (when (cdar node)		      ; If it has more sub-lists
	       (push (cdar node) (cdr node))) ; move the sub-list up
	     ;; Move contents of the one element list, up one
	     (setf (car node) (caar node)))
	   ;; Move on to the next item at the top level
	   (setf node (cdr node)))
       ;;(format t "~s~%" node) 
       )
    ;; Get rid of any fake NILs in the results
    (if preserve-nils
	result
	(delete nil result))))

;; I have a feeling I'll regret this range crap.
;; See also: (alexandria:iota n &key (start 0) (step 1))

(defun range-list (n &key (start 0) (step 1))
  "Make a concrete range as a list."
  (loop :for i :from start :below (+ start (* n step)) :by step :collect i))

(defun range-array (n &key (start 0) (step 1))
  "Make a concrete range as an array."
  (let ((a (make-array n :element-type (type-of (+ start n)))))
    (loop :for i :from start :below (+ start (* n step)) :by step
       :for j = 0 :then (1+ j)
       :do (setf (aref a j) i))
    a))

(defstruct range
  "A lazy range."
  start
  end
  (step 1))

(defun range-lazy (start-or-end &optional end (step 1))
  "Make a lazy range."
  (if end
      (make-range :start start-or-end :end end :step step)
      (make-range :start 1 :end start-or-end :step step)))

;; Objects

(defvar *mop-package*
  #+mop :mop
  #+sbcl :sb-mop
  #+(or cmu gcl) :pcl
  #+ccl :ccl
  #+lispworks :hcl
  #+(or ecl clasp) :clos
  #-(or mop sbcl cmu ccl lispworks gcl ecl clasp) (error "GIVE ME MOP!!")
  "The package in which the traditional Meta Object Protocol resides.")

(defun slot-documentation (slot-def)
  "Return the documentation string for a slot as returned by something like
MOP:CLASS-SLOTS."
  (declare (ignorable slot-def))
  #+sbcl (sb-pcl::%slot-definition-documentation slot-def)
  #-sbcl (missing-implementation 'slot-documentation))

(defun shallow-copy-object (original)
  "Copy an object (a.k.a class) shallowly, i.e. just set the slots in the copy,
to the same value as the slots in the original."
  (let* ((class (class-of original))
         (copy (allocate-instance class)))
    (dolist (slot (mapcar #'slot-definition-name (class-slots class)))
      (when (slot-boundp original slot)
        (setf (slot-value copy slot)
              (slot-value original slot))))
    copy))

;; I know this is a ruse, but it makes me feel better.
;; Also, without good optimization, it could be code bloating.
#-clisp ;; Of course CLisp areeady had this idea and put it in by default.
(defmacro doseq ((var seq) &body body)
  "Iterate VAR on a ‘sequence’ SEQ, which can be a list, vector, hash-table
or package. For hash-tables, it iterates on the keys. For packages, it iterates
on all accessible symbols."
  (let ((seq-sym (gensym)))
    `(let ((,seq-sym ,seq))
       ;; Could be ctypecase, but a sequence seems an unusual thing for the
       ;; user to type in. Of course it could be programmatically correctable,
       ;; but let's just fix those bugs.
       (etypecase ,seq-sym
	 (list
	  (loop :for ,var :in ,seq-sym
	     :do ,@body))
	 (vector
	  (loop :for ,var :across ,seq-sym
	     :do ,@body))
	 (hash-table
	  ;; We pick the key not the value, since you can, of course, get
	  ;; value from the key.
	  (loop :for ,var :being :the :hash-keys :of ,seq-sym
	     :do ,@body))
	 (package
	  (loop :for ,var :being :the :symbols :of ,seq-sym
	     :do ,@body))
	 #| I might like to have:

	 (collection
	   (do-forward-iteration ,var ,@body))

	 See wip/collections.lisp.
	 |#
	 ))))

;; I know the args are probably getting stringified then un-stringified,
;; but this is mostly just for startup.
(defun system-command-stream (cmd args)
  "Return an output stream from the system command."
  #+clisp (ext:run-shell-command
	   (format nil "~a~{ ~a~}" cmd args) :output :stream)
  #+sbcl (sb-ext:process-output
	  (sb-ext:run-program cmd args :output :stream :search t))
  #+cmu (ext:process-output
	 (ext:run-program cmd args :output :stream))
  #+openmcl (ccl::external-process-output-stream
	     (ccl::run-program cmd args :output :stream))
  #+ecl (ext:run-program cmd args)
  #+excl (excl:run-shell-command (format nil "~a~{ ~a~}" cmd args)
				 :output :stream :wait t)
  ;; @@@ The LW manual says this only works on unix:
  #+lispworks (system:run-shell-command (format nil "~a~{ ~a~}" cmd args)
					:output :stream :wait nil)
  #+abcl (system:process-output (system:run-program cmd args))
  #-(or clisp sbcl cmu openmcl ecl excl lispworks abcl)
  (missing-implementation 'system-command-stream))

(defun shell-line (cmd &rest args)
  "Return the first line of output from the given shell command as a string.
 Return nil if there is no output."
  #-excl (let* ((s (system-command-stream cmd args))
		(r (read-line s nil nil)))
	   (close s)
	   r)
  #+excl (first (excl.osi:command-output (format nil "~a~{ ~a~}" cmd args)))
  )

(defun shell-lines (cmd &rest args)
  "Return a list of the lines of output from the given shell command.
 Return nil if there is no output."
  (let ((s nil))
    (unwind-protect
	 (progn
	   (setf s (system-command-stream cmd args))
	   (loop :with l = nil
		 :while (setf l (read-line s nil nil))
		 :collecting l))
      (if s (close s)))))

;; Don't clash with nos:lisp-args.
(defun system-args ()
  "Arguments given to when starting the lisp system."
  #+sbcl sb-ext:*posix-argv*
  #+clisp (ext:argv)
  #+cmu ext:*command-line-strings*
  #+(or ecl clasp) (loop :for i :from 0 :below (si:argc)
		      :collecting (si:argv i))
  #+openmcl (ccl::command-line-arguments)
  #+excl (sys:command-line-arguments)
  #+lispworks sys:*line-arguments-list*
  #+abcl ext:*command-line-argument-list*
  
  #-(or sbcl clisp cmu ecl openmcl excl lispworks abcl)
  (missing-implementation 'system-args))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (> *lisp-version-number* 10055)
    (d-add-feature :use-exit)))

(defun exit-system ()
  "Halt the entire Lisp system."
  #+openmcl (ccl::quit 0)
  #+cmu (ext:quit)
  #+(and sbcl use-exit) (sb-ext:exit)
  #+(and sbcl (not use-exit)) (sb-ext:quit)
  #+excl (excl:exit)
  #+clisp (ext:quit)
  #+ecl (ext:quit)
  #+abcl (ext:quit)
  #+clasp (core:quit)
  #-(or openmcl cmu sbcl excl clisp ecl abcl clasp)
  (missing-implementation 'exit-system))

(defun save-image-and-exit (image-name &optional initial-function)
  #+sbcl (sb-ext:save-lisp-and-die image-name :executable t
				   :toplevel initial-function)
  #+clisp (ext:saveinitmem image-name :executable t :quiet t :norc t
			   :init-function initial-function)
  #+ccl (save-application image-name :prepend-kernel t
			  :toplevel-function initial-function)
  #-(or sbcl clisp ccl) (declare (ignore image-name initial-function))
  #-(or sbcl clisp ccl) (missing-implementation 'save-image-and-exit))

;; a.k.a root
(defun overwhelming-permission ()
  #+ccl (= (ccl::getuid) 0)
  #+(and sbcl unix) (= (sb-unix:unix-getuid) 0)
  #+(and sbcl win32) nil
  ;; What about posix:getuid ? It seems to be missing on clisp 2.46.
  #+clisp (= (posix:user-info-uid (posix:user-info :default)) 0)
  #+cmu (= (unix:unix-getuid) 0)
  #+ecl (= (ext:getuid) 0)
  #+(or lispworks abcl) nil
  #-(or ccl sbcl clisp cmu ecl lispworks abcl)
  (missing-implementation 'overwhelming-permission))

;; Be forewarned! You will not get any more stupid defconstant warnings!
;; You will undoubtedly suffer the scourge of the silently workring!
;; Always test code that you are giving to someone else on a fresh lisp
;; unpolluted by your inrgratuous frobbery.

;; Stolen from alexandria:

#| I really hate this:
(defun %reevaluate-constant (name value test)
  (if (not (boundp name))
      value
      (let ((old (symbol-value name))
	    (new value))
	(if (not (constantp name))
	    (prog1 new
	      (cerror "Try to redefine the variable as a constant."
		      "~@<~S is an already bound non-constant variable ~
                       whose value is ~S.~:@>" name old))
	    (if (funcall test old new)
		old
		(restart-case
		    (error "~@<~S is an already defined constant whose value ~
                              ~S is not equal to the provided initial value ~S ~
                              under ~S.~:@>" name old new test)
		  (ignore ()
		    :report "Retain the current value."
		    old)
		  (continue ()
		    :report "Try to redefine the constant."
		    new)))))))

(defmacro define-constant (name initial-value &optional documentation
			   (test ''eql))
  "Ensures that the global variable named by NAME is a constant with a value
that is equal under TEST to the result of evaluating INITIAL-VALUE. TEST is a
/function designator/ that defaults to EQL. If DOCUMENTATION is given, it
becomes the documentation string of the constant.

Signals an error if NAME is already a bound non-constant variable.

Signals an error if NAME is already a constant variable whose value is not
equal under TEST to result of evaluating INITIAL-VALUE."
  (declare (ignore test))
  `(defconstant ,name (%reevaluate-constant ',name ,initial-value ,test)
     ,@(when documentation `(,documentation))))

|#

;; Back to the olde shite:

(defmacro define-constant (name value &optional doc (test 'equal))
  "Like defconstant but works with pendanticly anal SCBL."
  (declare (ignore test))
  #-ccl
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc)))
  #+ccl
  `(cl:defconstant ,name ,value ,@(when doc (list doc)))
  )

;; On other lisps just use the real one.
;;#-(or sbcl clisp ccl cmu lispworks ecl abcl)
;;(setf (macro-function 'define-constant) (macro-function 'cl:defconstant))

;; Ideally we would be able to make aliases for these:
;;   variable structure type package method-combination setf function
;;   compiler-macro slot-definition optimize
;; but for now, we'll stick to the easy ones. I'm still not convinced this
;; isn't just an overblown pile of crap.

(defgeneric define-alias (alias original alias-type)
  (:documentation "Define the symbol ALIAS as another name for ORIGINAL.")
  (:method (alias (original symbol) (alias-type t))
    (declare (ignore alias original))
    (error "We don't know how to make an alias for a ~S yet." alias-type))
  (:method (alias (original symbol) (alias-type (eql 'compiler-macro)))
    "Make an alias for a compiler macro."
    (setf (compiler-macro-function alias)
	  (compiler-macro-function original)
	  (documentation alias 'compiler-macro)
	  (documentation original 'compiler-macro)))
  (:method (alias (original symbol) (alias-type (eql 'macro)))
    "Make an alias for a macro."
    (setf (macro-function alias)
	  (macro-function original)
	  (documentation alias 'function)
	  (documentation original 'function)))
  (:method (alias (original symbol) (alias-type (eql 'variable)))
    "Make an alias for a variable."
    (define-symbol-macro alias original)
    (setf (documentation alias 'variable)
	  (documentation original 'variable)))
  (:method (alias (original symbol) (alias-type (eql 'function)))
    "Make an alias for a function."
    (setf (fdefinition alias)
	  (fdefinition original)
	  (documentation alias 'function)
	  (documentation original 'function))
    (define-alias alias original 'compiler-macro))
  (:method (alias (original package) alias-type)
    "Make a alias for a package by adding new nicknames for it."
    (declare (ignore alias-type))
    (let* ((pkg (find-package original))
	   (new-nicks (append (list alias) (package-nicknames pkg))))
      ;; Hopefully this trick works on most implementations.
      (rename-package original original new-nicks)))
  (:method (alias (original symbol) (alias-type (eql 'class)))
    (define-alias alias (find-class original) alias-type))
  (:method (alias (original standard-class) alias-type)
    "Make a alias for a standard class."
    (declare (ignore alias-type))
    (setf (find-class alias) original
	  (documentation alias 'type)
	  (documentation original 'type))))

;; I encouraged you make appropriate methods for your own types.

(defun defalias (alias original &optional alias-type)
  "Define ALIAS as another name for ORIGINAL. ALIAS should be a symbol.
ORIGINAL is something that a define-alias method is defined for."
  (when (not (symbolp alias))
    (error "ALIAS must be a symbol." ))
  (typecase original
    (symbol
     (cond
       ((fboundp original)
	;; Functions or macros (or methods lambda's ...)
	(cond
	  ((macro-function original)
	   (define-alias alias original 'macro))
	  ((symbol-function original)
	   (define-alias alias original 'function))))
       ((find-class original)
	;; symbols that denote class types or structure types
	(typecase (find-class original)
	  (structure-class (define-alias alias original 'structure))
	  (standard-class  (define-alias alias original 'class))
	  (t               (define-alias alias original alias-type))))
       (t
	;; Anything else is assumed to be a variable
	(define-alias alias original 'variable))))
    ;; If there was a way to get the name:
    ;;(function (define-alias alias original 'function))
    (package         (define-alias alias original 'package))
    (structure-class (define-alias alias original 'structure))
    (standard-class  (define-alias alias original 'class))
    (t               (define-alias alias original alias-type))))

;; This is just to pretend that we're trendy and modern.
;(setf (macro-function 'λ) (macro-function 'cl:lambda))
;;Umm actually I mean:
#-(or lispworks clasp)
(defmacro λ (&whole form &rest bvl-decls-and-body)
   (declare (ignore bvl-decls-and-body))
   `#'(lambda ,@(cdr form)))
;; Still doesn't work everywhere? WHY?
;; Also this doesn't work: (defalias 'λ 'lambda)

;; Is it really worth doing this? Is this gratuitous language mutation?
;; This is weird. Why do I love using it so much?
(defmacro _ (&rest exprs)
  "Shorthand for single argument lambda. The single argument is named '_'."
  `(lambda (_)
     (declare (ignorable _))
     ,@exprs))

(defun symbolify (string &key (package *package*) no-new)
  "Return a symbol, interned in PACKAGE, represented by STRING, after possibly
doing conventional case conversion. The main reason for this function is to
wrap the case conversion on implementations that need it. If NO-NEW is true,
never create a new symbol, and return NIL if the symbol doesn't already exist."
  (etypecase string
    (string
     (if no-new
	 (find-symbol (string-upcase string) package)
	 (intern (string-upcase string) package)))
    (symbol
     string)))

(defun keywordify (string)
  "Make a keyword from a string."
  (or (and (keywordp string) string)
      (intern (string-upcase string) :keyword)))

(defun likely-callable (f)
  "Return true if F is a function or an FBOUNDP symbol. This does not mean you
can actually FUNCALL it! Just that it's more likely."
  (or (functionp f) (and (symbolp f) (fboundp f))))

;; I really don't understand why introspection isn't better.

#+cmu
(progn
  (defun read-arglist (fn)
    "Parse the arglist-string of the function object FN."
    (let ((string (kernel:%function-arglist
		   (kernel:%function-self fn)))
	  (package (find-package
		    (c::compiled-debug-info-package
		     (kernel:%code-debug-info
		      (vm::find-code-object fn))))))
      (with-standard-io-syntax
	(let ((*package* (or package *package*)))
	  (read-from-string string)))))

  (defun debug-function-arglist (debug-function)
    "Derive the argument list of DEBUG-FUNCTION from debug info."
    (let ((args (di::debug-function-lambda-list debug-function))
	  (required '())
	  (optional '())
	  (rest '())
	  (key '()))
      ;; collect the names of debug-vars
      (dolist (arg args)
	(etypecase arg
	  (di::debug-variable
	   (push (di::debug-variable-symbol arg) required))
	  ((member :deleted)
	   (push ':deleted required))
	  (cons
	   (ecase (car arg)
	     (:keyword
	      (push (second arg) key))
	     (:optional
	      (push (debug-variable-symbol-or-deleted (second arg)) optional))
	     (:rest
	      (push (debug-variable-symbol-or-deleted (second arg)) rest))))))
      ;; intersperse lambda keywords as needed
      (append (nreverse required)
	      (if optional (cons '&optional (nreverse optional)))
	      (if rest (cons '&rest (nreverse rest)))
	      (if key (cons '&key (nreverse key))))))

  (defun debug-variable-symbol-or-deleted (var)
    (etypecase var
      (di:debug-variable
       (di::debug-variable-symbol var))
      ((member :deleted)
       '#:deleted)))

  (defun symbol-debug-function-arglist (fname)
    "Return FNAME's debug-function-arglist and %function-arglist.
A utility for debugging DEBUG-FUNCTION-ARGLIST."
    (let ((fn (fdefinition fname)))
      (values (debug-function-arglist (di::function-debug-function fn))
	      (kernel:%function-arglist (kernel:%function-self fn)))))

(defun byte-code-function-arglist (fn)
  ;; There doesn't seem to be much arglist information around for
  ;; byte-code functions.  Use the arg-count and return something like
  ;; (arg0 arg1 ...)
  (etypecase fn
    (c::simple-byte-function
     (loop for i from 0 below (c::simple-byte-function-num-args fn)
           collect (make-arg-symbol i)))
    (c::hairy-byte-function
     (hairy-byte-function-arglist fn))
    (c::byte-closure
     (byte-code-function-arglist (c::byte-closure-function fn)))))

(defun make-arg-symbol (i)
  (make-symbol (format nil "~A~D" (string 'arg) i)))

;;; A "hairy" byte-function is one that takes a variable number of
;;; arguments. `hairy-byte-function' is a type from the bytecode
;;; interpreter.
;;;
(defun hairy-byte-function-arglist (fn)
  (let ((counter -1))
    (flet ((next-arg () (make-arg-symbol (incf counter))))
      ;; (with-struct (c::hairy-byte-function- min-args max-args rest-arg-p
      ;;                                       keywords-p keywords) fn
      (with-slots (min-args max-args rest-arg-p keywords-p keywords) fn
        (let ((arglist '())
              (optional (- max-args min-args)))
          ;; XXX isn't there a better way to write this?
          ;; (Looks fine to me. -luke)
          (dotimes (i min-args)
            (push (next-arg) arglist))
          (when (plusp optional)
            (push '&optional arglist)
            (dotimes (i optional)
              (push (next-arg) arglist)))
          (when rest-arg-p
            (push '&rest arglist)
            (push (next-arg) arglist))
          (when keywords-p
            (push '&key arglist)
            (loop for (key _ __) in keywords
                  do (push key arglist))
            (when (eq keywords-p :allow-others)
              (push '&allow-other-keys arglist)))
          (nreverse arglist))))))

(defun function-arglist (fun)
    (if (and fun (symbolp fun))
	(function-arglist (or (macro-function fun)
			      (symbol-function fun)))
	(cond
	  ((eval:interpreted-function-p fun)
	   (eval:interpreted-function-arglist fun))
	  ((pcl::generic-function-p fun)
	   (pcl:generic-function-lambda-list fun))
	  ((c::byte-function-or-closure-p fun)
	   (byte-code-function-arglist fun)) ; ?
	  ((kernel:%function-arglist (kernel:%function-self fun))
	   (handler-case (read-arglist fun) ; ?
	     (error () :not-available)))
	  ;; this should work both for compiled-debug-function
	  ;; and for interpreted-debug-function
	  (t
	   (handler-case (debug-function-arglist ; ?
			  (di::function-debug-function fun))
	     (di:unhandled-condition () :not-available)))))))

#+lispworks
(defun strings-to-symbols (tree)
  (mapcar #'(lambda (x)
	      (typecase x
		(list (strings-to-symbols x))
		(symbol x)
		(string (intern x))
		(t (intern (write-to-string x)))))
	  tree))

;; @@@ This is mis-named.
(defun lambda-list (fun)
  "Return the function's arguments."
  #+sbcl 
  (sb-kernel:%fun-lambda-list (if (macro-function fun)
				  (macro-function fun)
				  (symbol-function fun)))
  #+ccl (ccl:arglist fun)
  #+cmu (function-arglist fun)
  #+lispworks (let ((args (lw:function-lambda-list fun)))
		(and (listp args) (strings-to-symbols args)))
  #+abcl (sys::arglist fun)
  #+ecl (ext:function-lambda-list fun)
  #-(or sbcl ccl cmu lispworks abcl ecl)
  (multiple-value-bind (exp closure-p name)
      (function-lambda-expression fun)
    (when exp
      (cadr exp))))

(defun lambda-list-vars (args &key all-p keywords-p)
  "Given a lambda list ARGS, return a list of variable names.
ALL-P       if true, includes supplied-p parameters and &AUX variables.
            This is useful for things like generating ignore lists for macros.
KEYWORDS-P  If true, include keywords and variable names."
  (flet ((keyword-var (k v)
	   "If KEYWORDS-P return a list of (:K V) otherwise just V."
	   (if keywords-p (list (keywordify k) v) v)))
    (loop :with state :and thing
       :for a :in args :do
       (setf thing nil)
       (case a
	 (&optional (setf state :optional))
	 (&rest     (setf state :rest))
	 (&key      (setf state :key))
	 (&allow-other-keys )
	 (&aux	  (setf state :aux))
	 (otherwise
	  (case state
	    (:optional
	     (setf thing
		   (if (listp a)
		       (if (and (> (length a) 2) all-p)
			   (list (first a) (third a))
			   (first a))
		       a)))
	    (:key
	     (setf thing
		   (if (listp a)
		       (let ((var (if (listp (first a))
				      (second (first a))
				      (first a)))
			     (key (if (listp (first a))
				      (first (first a))
				      (first a))))
			 (if (and (> (length a) 2) all-p)
			     ;;(list var (third a))
			     `(,@(keyword-var key var)
				 ,@(keyword-var (third a) (third a)))
			     (keyword-var key var)))
		       (keyword-var a a))))
	    (:aux (when all-p (setf thing a)))
	    (otherwise (setf thing a)))))
       :if (listp thing) :append thing :else :collect thing)))

(defmacro with-unique-names (names &body body)
  "Bind each symbol in NAMES to a unique symbol and evaluate the BODY.
Useful for making your macro “hygenic”."
  `(let ,(loop :for n :in names
	    :collect `(,n (gensym (symbol-name ',n))))
     ,@body))

(defmacro with-package (package &body body)
  "Evalute BODY with *package* set to the packaged designated by PACKAGE."
  `(let ((*package* (if (packagep ,package)
                        ,package
                        (find-package ,package))))
    ,@body))

(defun shortest-package-nick (&optional (package *package*))
  "Find the shortest nickname of PACKAGE."
  (if (package-nicknames package)
      (let ((p nil))
	(loop :with r = most-positive-fixnum
	   :for n :in (package-nicknames package)
	   :do (when (< (length n) r)
		 (setf p n)))
	p)
      (package-name package)))

#| How about not a macro?

(defmacro not-so-funcall (package symbol &rest args)
  "Call SYMBOL with ARGS if it's FBOUND in PACKAGE and PACKAGE exists."
  (with-unique-names (pkg sym)
    `(let* ((,pkg (find-package ,package))
	    (,sym (intern (string ,symbol))))
       (if (fboundp ,sym))
	 (funcall (intern ,sym (find-package ,package)) ,@args)
       (error "Not so funcall ~s ~s" ,package ,symbol))))

(defmacro ※ (package symbol &rest args)
  "Call SYMBOL with ARGS if it's FBOUND in PACKAGE and PACKAGE exists."
  `(not-so-funcall ,package ,symbol ,@args))
|#

(defun not-so-funcall (package symbol &rest args)
  "Call SYMBOL with ARGS if it's FBOUND in PACKAGE and PACKAGE exists."
  (let ((pkg (find-package package)) sym)
    (when (not pkg)
      (error "Package not found ~s" package))
    (setf sym (intern (string symbol) pkg))
    (if (fboundp sym)
	(apply sym args)
	(error "Symbol ~s not found in ~s" symbol package))))

(defalias '※ 'not-so-funcall)
(defalias 'symbol-call 'not-so-funcall)

;; Should I really?
(defmacro @ (object &rest slot-names)
  "Slot access."
  (let ((result object))
    (loop
       :for n :in slot-names
       :do (setf result `(slot-value ,result ',n)))
    result))

(defmacro ignore-conditions ((&rest conditions) &body body)
  "If body signals a condition type in CONDITIONS, return the values NIL and
the condition."
  `(handler-case
       (progn ,@body)
     ,@(loop :for cc :in conditions
	  :collect `(,cc (c) (values nil c)))))

(defalias 'ignore-some-conditions 'ignore-conditions)

;; Debugging messages
;;
;; This is so you can say: (dbug "message ~a~%" foo) in code, and then say
;; (with-dbug <exprs..>) or (without-dbug <exprs..>) to get debugging messages
;; or not. If you want to see only messages from a specific package, use
;; (with-dbug-package <package> <exprs..>).
;;
;; If you want keep them around but not always compile them, you could prefix
;; them with #+dbug or something. Of course, you should avoid unwanted side
;; effects in your messages. Be careful, because the "dbug" form might also
;; have syntactic effects.
;;
;; Output goes to *DEBUG-IO*, so you can modify that if you want the output to
;; go somewhere else, like:
;;  (let ((*debug-io* *earth*)) (with-dbug (land-on-mars)))

(defvar *dbug* nil
  "Dude, do you even debug?")

(defvar *dbug-package* nil
  "A package to debug. Output debugging message when we're in this package.")

(defvar *dbug-facility* nil
  "Facilities to debug. A sequence of keywords or something.")

(defmacro dbug (fmt &rest args)
  "Print a debugging message when debugging is turned on and maybe we're in
the right package."
  `(when (and dlib:*dbug*
	      (or (not *dbug-package*)
		  (equal *dbug-package* (package-name *package*))))
     (funcall #'format *debug-io* ,fmt ,@args) (finish-output)))

(defmacro dbugf (facility fmt &rest args)
  "Print a debugging message when debugging is turned on and maybe we're in
the right package, and the FACILITY is activated."
  `(when (and dlib:*dbug* dlib:*dbug-facility*
	      (or
	       (position ,facility *dbug-facility*)
	       (position :all *dbug-facility*)))
     (funcall #'format *debug-io* ,fmt ,@args) (finish-output)))

(defmacro with-dbug (&body body)
  "Evaluate the BODY with debugging message printing turned on."
  `(let ((*dbug* t)) ,@body))

(defmacro with-dbug-package (package &body body)
  "Evaluate the BODY with debugging message printing in PACKAGE turned on."
  `(let ((*dbug* t) (*dbug-package* (package-name ,package))) ,@body))

(defmacro with-dbugf (facility &body body)
  "Evaluate the BODY with debugging message printing tagged with FACILITY
turned on. FACILITY can be an atom or a list of facilities to turn on."
  (with-unique-names (fac)
    `(let* ((*dbug* t)
	    (,fac ,facility)		; so we only eval once
	    (*dbug-facility*
	     (append *dbug-facility*
		     (if (listp ,fac) ,fac (list ,fac)))))
     ,@body)))

(defmacro without-dbug (&body body)
  "Evaluate the BODY without printing any debugging messages."
  `(let ((*dbug* nil)) ,@body))

(defmacro dump-values (&rest args)
  "Print the names and values of the arguments, like NAME=value."
  (let ((za (loop :for z :in args :collect
	       `(format *debug-io* "~a=~a " ',z ,z))))
    `(progn ,@za (terpri *debug-io*))))

(defun fake-probe-file (f)
  "Fake probe-file because CLisp gets pointless errors."
  #+clisp (ignore-errors (probe-file f))
  #-clisp (probe-file f))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun exe-in-path-p (name)
    "True if file NAME exists in the current execute path, which is usually the
environment variable PATH. Note that this doesn't check if it's executable or
not or that you have permission to execute it."
    (loop :for d :in (split-sequence ":" (d-getenv "PATH"))
       :when (fake-probe-file (s+ d "/" name))
       :do (return t)
       :finally (return nil)))

  (defun try-things (things)
    "Return the first thing that works. THINGS is a list of lists. If a sublist
starts with a string, it should be a command in the path which is given the rest
of the arguments. If a sublist starts with a symbol, it is a function to call
with the rest of the arguments. Works means it doesn't return NIL. If nothing
works, return NIL."
    (loop :with result
       :for cmd :in things :do
       (setf result (etypecase (car cmd)
		      (string (when (dlib::exe-in-path-p (car cmd))
				(apply #'shell-line cmd)))
		      (symbol (apply (car cmd) (cdr cmd)))))
       (when result (return result)))))

;; This is of course not "safe" in that it may not preserve the actual
;; bytes of the stream.
#+nil
(defun resilient-read-line (&optional input-stream eof-error-p eof-value
			    recursive-p)
  "Like ANSI READ-LINE but try to ignore encoding errors. Lossy."
  #+sbcl
  (handler-bind
      ((sb-int:stream-decoding-error
	(lambda (e)
	  (let (r)
	    (cond
	      ((setf r (find-restart 'sb-int:attempt-resync))
	       (dbug "Decoding error, resyncing ~s.~%" r)
	       (invoke-restart r))
	      ((setf r (find-restart 'sb-int:force-end-of-file))
	       (dbug "Decoding error, force EOF ~s.~%" r)
	       (invoke-restart r))
	      ((setf r (find-restart 'abort e))
	       (dbug "Decoding error, Who fuking cares? ~s.~%" r)
	       (format *error-output* "Decoding error.~%")
	       ;; XXX This is bad. We should really signal an error which, the
	       ;; above code will catch, and print an appropriate error.
	       ;; For now, just return NIL as if we got an EOF.
	       (return-from resilient-read-line nil))
	      (t
	       #|
	       (format t "Decoding error, can't find a restart! So what!~%")
	       (format t "Cmd: ~s ~s~%" command args)
	       (format t "Line: ~s~%" line)
	       |#
	       (signal e)))))))
    (read-line input-stream eof-error-p eof-value recursive-p))
  #+ccl
  (handler-case
      (read-line input-stream eof-error-p eof-value recursive-p)
    (type-error ()
      (dbug "You probably got a character set decoding error.~%")
      #| ignore it |# )
    (error (e)
      (signal e)))
  #-(or sbcl ccl)
  (read-line input-stream eof-error-p eof-value recursive-p)
  )

;; This makes me feel like I'm sadly going to have to implement my own
;; UTF8 stuff. (see wip/read-utf8-char.lisp)

(defun resilient-read-line (&optional input-stream eof-error-p eof-value
			    recursive-p)
  #+sbcl
  (handler-bind
      ((sb-int:stream-decoding-error
	(lambda (e)
	  (declare (ignore e))
;	  (print e)
	  (let ((r (find-restart 'sb-int:attempt-resync)))
;	    (format t "resync: ~w~%" r)
	    (invoke-restart r)))))
    (read-line input-stream eof-error-p eof-value recursive-p))
  #+ccl
  (read-line input-stream eof-error-p eof-value recursive-p)
  #-(or sbcl ccl)
  (read-line input-stream eof-error-p eof-value recursive-p)
)

(defmacro with-open-file-or-stream ((var file-or-stream &rest args) &body body)
  "Evaluate BODY with VAR bound to FILE-OR-STREAM if it's already a stream, or
an open a stream named by FILE-OR-STREAM. ARGS are standard arguments to OPEN."
  (let ((thunk (gensym "thunk"))
	(abort-flag (gensym "abort-flag")))
    `(let ((,var nil) (,abort-flag t))
       (flet ((,thunk () ,@body))
	 (if (streamp ,file-or-stream)
	     (progn
	       (setf ,var ,file-or-stream)
	       (,thunk))
	     ;; We can't use with-open-file here because it creates it's own
	     ;; lexical stream variable, and we want to use ours.
	     (unwind-protect
		(progn
		  (setf ,var (open ,file-or-stream ,@args))
		  (,thunk)
		  (setf ,abort-flag nil))
	       (when ,var (close ,var :abort ,abort-flag))))))))

(defmacro with-lines ((line-var file-or-stream) &body body)
  "Evaluate BODY with LINE-VAR set to successive lines of FILE-OR-STREAM.
FILE-OR-STREAM can be a stream or a pathname or namestring."
  (let ((line-loop	  (gensym "WITH-LINES-LL"))
	(inner-stream-var (gensym "WITH-LINES-IS"))
	(outer-stream-var (gensym "WITH-LINES-OS"))
	(stream-var	  (gensym "WITH-LINES-SV")))
    `(labels ((,line-loop (,inner-stream-var)
		(loop :with ,line-var
		   :while (setf ,line-var (read-line ,inner-stream-var nil nil))
		   :do ,@body)))
       (let ((,stream-var ,file-or-stream)) ; so file-or-stream only eval'd once
	 (if (streamp ,stream-var)
	     (,line-loop ,stream-var)
	     (with-open-file (,outer-stream-var ,stream-var)
	       (,line-loop ,outer-stream-var)))))))

(defun get-lines (file-or-stream)
  "Return a list of the lines read from FILE-OR-STREAM."
  (flet ((stream-loop (stream)
	   (loop :with l
	      :while (setf l (read-line stream nil nil))
	      :collect l)))
    (if (streamp file-or-stream)
	(stream-loop file-or-stream)
	(with-open-file (stream file-or-stream)
	  (stream-loop stream)))))

;; Muffle the complaint about using &optional and &key.
#+sbcl (declaim (sb-ext:muffle-conditions style-warning))
(defun safe-read-from-string (string &optional (eof-error-p t) eof-value
			      &key (start 0) end preserve-whitespace)
  "Read from a string in a hopefully safe manner, such that the content
cannot cause evaluation."
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (read-from-string string eof-error-p eof-value
			:start start :end end
			:preserve-whitespace preserve-whitespace))))

(defun interninator (name package dirt-pile)
  "Return the symbol NAME from package if it exists, or from the DIRT-PILE
package if it doesn't. If DIRT-PILE is NIL, return a packageless symbol."
  (or (let ((pkg (find-package package)))
	(and pkg (find-symbol name pkg)))
      (if dirt-pile
	  (intern name dirt-pile)
	  (make-symbol name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (boundp '*read-intern*)
    (d-add-feature :has-read-intern)))

(defun clean-read-from-string (string package
			       &optional (eof-error-p t) eof-value
			       &key (start 0) end preserve-whitespace)
  "Read from a string without interning unknown symbols in *package*, instead
returning them as uninterned symbols."
  ;; This is the good way, which uses the *read-intern* extension.
  #+has-read-intern
  (let ((*read-intern* #'(lambda (str pkg)
			   (interninator str pkg package))))
    (read-from-string string eof-error-p eof-value
		      :start start :end end
		      :preserve-whitespace preserve-whitespace))
  ;; This is a very inefficient way which makes a new package every time.
  #-has-read-intern
  (declare (ignore package))
  #-has-read-intern
  (let (pkg)
    (unwind-protect
	 (progn
	   (setf pkg (make-package (package-name package) :use (list *package*)
				   :internal-symbols 0 :external-symbols 0))
	   (with-package pkg
	     (read-from-string string eof-error-p eof-value
			       :start start :end end
			       :preserve-whitespace preserve-whitespace)))
      (when pkg
	(delete-package pkg)))))

(defun package-robust-intern (s p)
  "Return S interned in package P, or S interned in *PACKAGE*, or S as an
un-interned symbol."
  (let ((p (find-package p)))
    (if p
	(multiple-value-bind (sym status) (find-symbol s p)
	  (if status
	      sym
	      (multiple-value-bind (sym status)
		  (find-symbol s *package*)
		(if status sym (make-symbol s)))))
	(multiple-value-bind (sym status) (find-symbol s *package*)
	  (if status sym (make-symbol s))))))

(defun package-robust-read-from-string (string
					&optional (eof-error-p t) eof-value
					&key (start 0) end preserve-whitespace)
  "Read from a string treating unknown symbols or packages as uninterned."
  (let ((*read-intern* #'package-robust-intern))
    (read-from-string string eof-error-p eof-value
		      :start start :end end
		      :preserve-whitespace preserve-whitespace)))

(defun package-robust-read (&optional (stream *standard-input*)
			      (eof-error-p t) (eof-value nil) (recursive-p nil))
  "Read treating unknown symbols or packages as uninterned."
  (let ((*read-intern* #'package-robust-intern))
    (read stream eof-error-p eof-value recursive-p)))

#+sbcl (declaim (sb-ext:unmuffle-conditions style-warning))

;; So it looks like we have at least a few implementations to choose from:
;;   UIOP/STREAM:COPY-STREAM-TO-STREAM (fbound)
;;   ALEXANDRIA.0.DEV:COPY-STREAM (fbound)
;;   LISH::COPY-STREAM (fbound)
;;   QL-UTIL:COPY-FILE
;; also for consideration:
;;   cl-fad-0.6.4/fad.lisp
;;  xcvb-0.596/driver.lisp
;;   metatilities-base-20120909-git/dev/copy-file.lisp
;;   flexi-streams-1.0.14/test/test.lisp

;; The size of this should really be taken from the system's page size or
;; some other known thing which is optimal for the system.
(defvar *buffer-size* (* 8 1014)
  "The default buffer size for efficient copying of streams and files.")

;; I suppose we could make this generic so that streams can do a special
;; things with it, but that might be sort of edging into the stream protocol.
(defun copy-stream (source destination &key (buffer-size *buffer-size*))
  "Copy data from reading from SOURCE and writing to DESTINATION, until we get
an EOF on SOURCE."
  ;; ^^^ We could try to make *buffer-size* be the minimum of the file size
  ;; (if it's a file) and the page size, but I'm pretty sure that the stat
  ;; call and possible file I/O is way more inefficient than wasting less than
  ;; 4k of memory to momentarily. Of course we could mmap it, but it should
  ;; end up doing approximately that anyway and the system should have a
  ;; better idea of how big is too big, window sizing and all that. Also,
  ;; that's way more complicated. Even this comment is too much. Let's just
  ;; imagine that a future IDE will collapse or footnotify comments tagged
  ;; with "^^^".
  (when (not (eql (stream-element-type source)
		  (stream-element-type destination)))
    ;; It would be nice if we could handle this, but I don't want to make
    ;; dlib dependant on flexi-streams or something. I think there might be
    ;; enough code in your average implementation to handle this, but it needs
    ;; more research.
    (error "Stream element types have to match."))
  (let ((buf (make-array buffer-size
			 :element-type (stream-element-type source)))
	pos)
    (loop :do
       (setf pos (read-sequence buf source))
       (when (> pos 0)
	 (write-sequence buf destination :end pos))
       :while (= pos *buffer-size*))))

(defun quote-format (s)
  "Quote a string to send to format, so that any possible format directives
are printed rather than interpreted as directives, which really just means:
repleace a single tilde with double tidles."
  (replace-subseq "~" "~~" s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System information features
;;;

;; Know where we are:
;;
;; The problem is that (machine-instance) gets the DNS name which can
;; vary depending on what network you're connected to, but what we want is a
;; pseudo-unique name of the machine independent of the network.

(defvar *host* (initial-span
		(try-things
		 '(("scutil" "--get" "ComputerName")
		   ("hostname") ("uname" "-n")
		   (machine-instance)))
		'(#\. #\space)))
(d-add-feature *host*)

#+(and darwin ecl) (d-add-feature :unix)

; (defun arch-from-uname ()
;   "Last ditch effort for constructing ARCH from UNIXish uname"
;   (let ((proc (shell-line "uname" "-p")
; 	(mach (shell-line "uname" "-m")
; 	(os   (shell-line "uname" "-s")
; 	(rel  (shell-line "uname" "-r")

;; Figure out environmental features
(defvar *arch* (or (d-getenv "ARCH") (d-getenv "MACHTYPE")))
(defparameter *arch-nickname*
  (cond
    ((search "sparc"   *arch*)	"sparc")
    ((and (search "x86_64"  *arch*)
	  (or (has-feature :X86-64) (has-feature :64-bit-target))
	  #+clisp (not (search "arch i386 " (software-type))))
	  "x86_64")
    ((search "x86"     *arch*)	"x86")
    ((search "386"     *arch*)	"x86")
    ((search "i686"    *arch*)	"x86")
    ((search "powerpc" *arch*)	"ppc")
    ((search "ppc"     *arch*)	"ppc")
    ((search "java"    *arch*)	"java")
    (t "unknown")
  )
  "A short nickname for the current architecture."
)
; Petulant feature adding
(d-add-feature *arch-nickname*)

(defvar *os* #+unix (shell-line "uname" "-s") #+(or win32 windows) "windows")
(defparameter *os-nickname*
  (cond
    ((search "Linux"  *os*)	"linux")
    ((search "SunOS"  *os*)	"solaris") ; come on now
    ((search "Darwin" *os*)	"darwin")
    ((search "windows" *os*)	"windows")
    (t "unknown")
  )
  "A short nickname for the current operating system."
)
(d-add-feature *os*)			; Gratuitous feature adding

(defparameter *lisp-implementation-nickname*
  #+clisp	"CLisp"
  #+sbcl	"SBCL"
  #+cmu		"CMU"
;  #+openmcl	"OpenMCL"
  #+ccl		"CCL"
  #+excl	"Allegro"
  #+lispworks	"LW"
  #+ecl		"ECL"
  #+abcl	"ABCL"
  #+clasp	"Clasp"
  #-(or clisp sbcl cmu excl openmcl lispworks ecl abcl clasp) "Unknown"
  "A short nickname for the current implementation.")

(defparameter *lisp-version*
  #+clisp	(format nil "~a"
			(let ((v (lisp-implementation-version)))
			  (substitute #\- #\.
				      (subseq v 0 (position #\space v)))))
  #+sbcl	(format nil "~a" (lisp-implementation-version))
  #+cmu		(format nil "~a"
			(let ((v (lisp-implementation-version)))
			  (subseq v 0 (position #\space v))))
;  #+openmcl	"openmcl"
  #+ccl		(format nil "~a"
			(let* ((v (lisp-implementation-version))
			       (vv (subseq v (1+ (position #\space v)))))
			  (subseq vv 0 (position #\space v))))
  #+excl	"allegro"
  #+lispworks	"lw"
  #+ecl		"ecl"
  #+abcl	(format nil "~a"
			(let ((v (lisp-implementation-version)))
			  (substitute #\- #\.
				      (subseq v 0 (position #\space v)))))
  #+clasp	(format nil "~a"
			(let ((v (lisp-implementation-version)))
			  (substitute
			   #\- #\.
			   (subseq v (1- (position #\- v :from-end t))))))

  #-(or clisp sbcl cmu excl openmcl lispworks ecl abcl) "unknown"
  "A version suitable for putting in a logical pathname.")

(defparameter *platform-nickname* (format nil "~(~a~)~a-~a-~a"
					  *lisp-implementation-nickname*
					  *lisp-version*
					  *os-nickname*
					  *arch-nickname*)
 "Nickname for the platform. Hopefully determines compiled code compatibility.")
(d-add-feature *platform-nickname*)	; Incorrigible feature adding

;; Before you add stuff here, consider whether to put it in dlib-misc instead.
;; For example stuff that requires opsys should go in there.

#+debug-rc (progn (format t "] ") (force-output *standard-output*))

;; End
