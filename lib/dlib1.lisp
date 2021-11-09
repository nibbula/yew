;;;							-*- encoding: utf-8 -*-
;;; dlib1.lisp - Utilities of redundant doom, first file.
;;;

;; Util libraries are a problem. I strongly dislike all of them, including
;; this one.
;;
;;  - Don't add any dependencies.
;;  - Try to be "minimal".

;;#+debug-rc (progn (format *terminal-io* "dlib") (force-output *terminal-io*))

(when (find-package :the-real-fake-dlib)
  (cerror "Go ahead and load the real DLIB anyway."
	  "Detected \"The IMPOSTER\"!!~%~
           If you're going to use the real DLIB, load it before you load~%~
           OPSYS and thus the FAKE-DLIB."))

(defpackage :dlib
  (:documentation
   "Utilities of redundant doom.
The existance of this is probbaly a societal logistic problem of Lisp
programmers, that will likely eventually have to be dealt with. I apologize for
my addition to the problem, and hope I can some day contribute to the solution
of it.")
  (:use :common-lisp
	;; We must have the MOP!!! Don't ever drop the MOP!
	#+(or (and clisp mop) abcl excl) :mop
	#+sbcl :sb-mop
	#+cmu :pcl
	#+ccl :ccl
	#+(or ecl clasp) :clos
	#+lispworks :hcl
	;; extensions
	#+sbcl :sb-ext
	)
   #+lispworks (:shadow #:lambda-list #:with-unique-names)
  #-the-real-fake-dlib (:nicknames :fake-dlib) ; Crazy Sauce!
  (:export
   ;; bootstrapping System-ish
   #:d-getenv
   #:shell-line
   #:shell-lines
   #:run-system-command
   #:system-command-stream
   #:system-args
   #:exit-system
   #:save-image-and-exit
   #:overwhelming-permission
   #:exe-in-path
   #:try-things
   ;; io-ish
   #:resilient-read-line
   #:with-open-file-or-stream
   #:with-lines
   #:get-lines
   #:file-lines
   #:copy-package
   #:fancy-read-from-string
   #:safe-read-from-string
   #:safe-read
   #:clean-read-from-string
   #:package-robust-read-from-string
   #:package-robust-read
   #:*buffer-size*
   #:copy-stream
   #:stream-string
   #:quote-format
   #:name-to-title
   #:print-properties
   #:interactive-interrupt
   ;; sequences
   ;;#:call-with-start-and-end
   #:initial-span
   #:split-sequence
   #:split-sequence-if
   #:split-sequence-by-range
   #:replace-subseq
   #:displaced-subseq
   #:copy-array-into
   #:copy-array
   #:begins-with
   #:ends-with
   #:remove-prefix
   #:remove-suffix
   #:s+
   #:*ascii-whitespace* #:*unicode-whitespace-codes* #:*whitespace*
   #:ltrim #:rtrim #:trim
   #:join-by-string
   #:join-by
   #:fill-by
   ;; lists
   #:delete-nth
   #:insert-at
   #:alist-to-hash-table
   #:plist-to-hash-table
   #:hash-table-to-alist
   #:hash-table-to-plist
   #:get-plist-values
   #:do-plist
   #:do-alist
   #:do-kv-list
   #:flatten
   #:range-list
   #:range-array
   #:range-lazy #:range-start #:range-end #:range-step #:make-range
   #:clamp
   #:clampf
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
   #:missing-implementation-error
   #:missing-implementation-error-symbol
   #:missing-implementation-error-foramt
   #:missing-implementation-error-arguments
   #:missing-implementation
   #:without-warning
   #:without-notes
   #:with-muffled-notes
   #:sort-muffled
   #:stable-sort-muffled
   ;; language-ish
   #:define-constant
   #:defconstant-to-list
   #:define-alias #:defalias
   ;; #-(or lispworks clasp) #:#.(code-char #x039B) #\greek_capital_letter_lamda
   #:_ #:__
   #:and-<> #:<> #:-> #:->> #:-<>>
   #:symbolify
   #:keywordify
   #:likely-callable
   ;; #-lispworks #:lambda-list
   #:argument-list
   #:lambda-list-vars
   #-lispworks #:with-unique-names
   #:with-names
   #:with-package
   #:ensure-exported
   #:without-package-variance
   #:shortest-package-nick
   #:not-so-funcall #:symbol-call
   #:refer-to
   ;; #-(or lispworks clasp) #:#.(code-char #x203B) #\reference_mark
   #:maybe-refer-to
   ;; #-(or lispworks clasp) #:#.(code-char #x203B)?
   ;; #:@
   #:with-internal-slots
   #:ignore-conditions #:ignore-some-conditions
   #:find-slot-name
   #:defmethod-quiet
   #:with-decls-and-body #:doc-and-decls #:fixed-body
   #:function-designator
   #:+simple-condition-format-control-slot+
   #:+simple-condition-format-arguments-slot+
   ;; debugging
   #:*dbug* #:*dbug-output* #:*dbug-package* #:*dbug-facility*
   #:dbug #:dbugf #:if-dbugf #:with-dbug #:with-dbug-package
   #:with-dbugf
   #:with-dbugf-to
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
  (defun compute-version-integer (version-string)
    "Make a single testable value out of a typical result of
LISP-IMPLEMENTATION-VERSION."
    (let ((i 0) (spos 0) (sum 0) (pos 0) (mag #(10000 100 1))
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

  (defun extract-version-number (version-string)
    "Pull a semi-numerical version number out of the version string."
     (let ((s version-string))
       (setf s (subseq s (position-if (lambda (x) (digit-char-p x)) s))
             s (subseq s 0 (position-if
                             (lambda (x)
			       (not (or (digit-char-p x) (eql x #\.))))
			    s)))
	s))

  ;; We need to have this early so we can make decisions based on it.
  (defparameter *lisp-version-number*
    #+(or sbcl ecl) (compute-version-integer (lisp-implementation-version))
    #+(or ccl clisp)
    (compute-version-integer
      (extract-version-number (lisp-implementation-version)))
    ;; You will have to put something here if it matters.
    #-(or sbcl ecl ccl clisp) nil
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

  (defmacro with-unique-names ((&rest names) &body body)
    "Bind each symbol in NAMES to a unique symbol and evaluate the BODY.
Useful for making your macro 'hygenic'."
    `(let ,(loop :for n :in names
	      :collect `(,n (gensym (symbol-name ',n))))
       ,@body)))

;; Because we don't have alias up here yet.
(setf (macro-function 'with-names)
      (macro-function 'with-unique-names)
      (documentation 'with-names 'function)
      (documentation 'with-unique-names 'function))

(define-condition missing-implementation-error (warning)
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

(defun missing-implementation (symbol)
  "Complain that something is missing."
  (warn 'missing-implementation-error
	:format "The implementation for ~a on ~a is missing.~%"
	:symbol symbol
	:arguments (list symbol (lisp-implementation-type))))

(defmacro without-warning (&body body)
  "Get rid of stupid warnings that you don't want to see.
 Just wrap your code with this."
  `(handler-bind
    ((warning #'(lambda (c)
		  (declare (ignore c))
		  #-ccl (muffle-warning)
		  )))
     ,@body))

(defmacro without-notes (&body body)
  "Get rid of compiler notes that you don't want to see.
 Just wrap your code with this. Too bad it won't always work."
  #+sbcl
  `(handler-bind
     ((sb-ext:compiler-note
       #'(lambda (c)
	   (declare (ignore c))
	   (muffle-warning))))
     ,@body)
  #-sbcl
  `(progn ,@body))

;; @@@ What are the advantages or diffreneces, if any, of using
;; with-muffled-notes vs without-notes?
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-muffled-notes (&body body)
    "Evaluate BODY with compiler notes muffled."
    `(locally #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	      ,@body)))

;; This is just for a particularly complaintive implementation.
;; (declaim (inline sort-muffled))
;; (defun sort-muffled (seq pred &rest args &key key)
;;   (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
;; 	   (ignorable key))
;;   (apply #'sort seq pred args))

(defmacro sort-muffled (&whole whole seq pred &rest args &key key)
  "Just like ‘cl:sort’ but with compiler notes muffled."
  (declare (ignorable seq pred args key))
  `(with-muffled-notes
     (sort ,@(cdr whole))))

(defmacro stable-sort-muffled (&whole whole seq pred &rest args &key key)
  "Just like ‘cl:sort’ but with compiler notes muffled."
  (declare (ignorable seq pred args key))
  `(with-muffled-notes
     (stable-sort ,@(cdr whole))))

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
  #+cormanlisp
  ;; @@@ maybe we could get this added to cormanlisp?
  (let (name null-pointer value size result blurp)
    (unwind-protect
	 (progn
	   (setf name (ct:lisp-string-to-c-string s)
		 null-pointer (ct:malloc 1)
		 size (win:getenvironmentvariable name null-pointer 0))
	   (if (and (zerop size) (= (win:getlasterror) 203))
	       (setf result nil)
	       (progn
		 (setf value (ct:malloc (1+ size))
		       blurp (win:getenvironmentvariable name value size))
		 (when (/= (1+ blurp) size)
		   (error "corman bootstrap getenv failed?."))
		 (setf result (ct:c-string-to-lisp-string value)))))
      (when null-pointer
	(ct:free null-pointer))
      (when value
	(ct:free value)))
    result)
  #+mezzano (declare (ignore s))
  #+mezzano nil ; no such thing, not missing
  #-(or clisp sbcl openmcl cmu ecl excl lispworks gcl abcl clasp cormanlisp
	mezzano)
  (missing-implementation 'd-getenv))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun initial-span (sequence not-in)
    "Return the initial portion of SEQUENCE consiting of objects not in
the sequence NOT-IN."
    (subseq sequence 0 (position-if #'(lambda (c) (find c not-in)) sequence))))

;; Another square wheel.
;; @@@ I should probably either fix this to have the additional functionality
;; of the public version, or just copy the public version here. Just actually
;; use the public version?
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
  ;; This is useful implementing wrappers or methods for various standard
  ;; functions that take START and END keywords.
  ;; @@@ I would like to not have to pass: start start-p end end-p
  ;; but then I would have to export them from here. :(
  (defmacro %call-with-start-and-end (func args)
    "Call func with args and START and END keywords, assume that an environemnt
that has START and START-P and END and END-P."
    `(if start-p
	 (if end-p
	     (,func ,@args :start start :end end)
	     (,func ,@args :start start))
       (if end-p
	   (,func ,@args ::end end)
	   (,func ,@args))))

  (defun bag-position (bag seq &key
				 (start nil start-p)
				 (end nil end-p)
				 test test-not key)
    (%call-with-start-and-end
     position-if
     (#'(lambda (c)
	  (position c bag :test test :test-not test-not))
	seq :key key)))

  (defmacro call-looker (function sep seq start end test key)
    "Call FUNCTION, which is likely to be POSITION or SEARCH. Make START, END,
and TEST be the appropriate keywords."
    (let ((base
	   `(,function ,sep ,seq
		       ,@(when key `(:key ,key))
		       ,@(if (or (eq function 'position)
				 (eq function 'bag-position))
			     '(:start) '(:start2))
		       ,start
		       ,@(when end
			       `(,(if (or (eq function 'position)
					  (eq function 'bag-position))
				      :end :end2)
				  ,end)))))
      `(if ,test ,(append base (list :test test)) ,base)))

  (defun split-sequence (sep seq &key
			 omit-empty coalesce-separators remove-empty-subseqs
			 bag
			 (start 0) end test key #| count |#)
    "Split the sequence SEQ into subsequences separated by SEP. Return a list of
the subsequences. SEP can be a sequence itself, which means the whole sequence
is the separator. If :omit-empty is true, then don't return empty subsequnces.
 :coalesce-separators :remove-empty-subseqs are compatability synonyms for
:omit-empty."
    (declare (type sequence seq))
    (declare (type boolean omit-empty coalesce-separators remove-empty-subseqs))
    (setf omit-empty (or omit-empty coalesce-separators remove-empty-subseqs))
    (let* ((sep-is-seq (typecase sep (vector t) (list t) (t nil)))
	   (sep-len (if (and sep-is-seq (not bag)) (length sep) 1))
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
		  ;;    '(call-looker ,func sep seq t-start end test))
		  (setq t-end
			(or (call-looker ,func sep seq t-start end test key)
			    -1))
		  :while (>= t-end 0)
		  ;;(format t "start = ~d end = ~d ~s~%" t-start t-end
		  ;;(subseq seq t-start t-end))
		  ,@(if t-omit-empty `(if (not (= t-start t-end))))
		  :collect (subseq seq t-start t-end) :into results
		  :do (setq t-start (+ t-end sep-len))
		  :finally (return-from nil
			     ,@(if t-omit-empty
				   `((if (= t-start seq-len)
					 results
					 (nconc results
						(list
						 (subseq seq t-start end)))))
				   `((nconc results
					    (list (subseq seq t-start end)))))))))
	(when (or (> sep-len 0) test)
	  (when (and test (< sep-len 1))
	    (setf sep-len 1 sep '(nil))) ; fake separator!
	  (if sep-is-seq
              (if omit-empty
                  (if test
                      (if bag
                          (loopy bag-position t)
                          (loopy search t))
                      (if bag
                          (loopy bag-position t)
                          (loopy search t)))
                  (if test
                      (if bag
                          (loopy bag-position nil)
                          (loopy search nil))
                      (if bag
                          (loopy bag-position nil)
                          (loopy search nil))))
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
  (let ((seq-len (if omit-empty (length seq) 0))) ;; @@@ don't length for lists?
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

(defun split-sequence-by-range (ranges sequence)
  "Split SEQUENCE into pieces specified by RANGES which is a list of pairs of
start and end indices."
  (loop :with range-start :and range-end :and seq-end = (1- (length sequence))
     :for range :in ranges :do
     (setf range-start (or (car range) 0)
	   range-end (or (if (consp (cdr range)) (cadr range) (cdr range))
			 seq-end))
     :collect
     (subseq sequence range-start (1+ range-end))))

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

(defun replace-subseq (target replacement sequence &key count)
  "Return a copy of SEQUECE but with sub-sequences of TARGET replaced
REPLACEMENT."
  (if (and (> (length target) 0) (or (not count) (> count 0)))
      (let ((pos 0)
	    (i 0)
	    (n 0)
	    (new '()))
	(loop :while (setf pos (search target sequence :start2 i))
	   :do
	   ;;(format t "i = ~a pos = ~a new = ~a~%" i pos new)
	   (setf new (nconc new (list (subseq sequence i pos) replacement)))
	   (setf i (+ pos (length target)))
	   ;;(format t "i = ~a pos = ~a new = ~a~%" i pos new)
	   (incf n)
	   :until (and count (>= n count)))
	(setf new (nconc new (list (subseq sequence i))))
	;;(apply #'concatenate (append '(string) new)))
	(apply #'concatenate
	       (etypecase sequence
		 (string 'string)
		 (list 'list)
		 (vector 'vector)
		 (sequence (type-of sequence)))
	       new))
      (copy-seq sequence)))

(defun displaced-subseq (array start &optional end)
  "Like subseq, but returns a displaced array. Try not to use this."
  (make-array (- (or end (length array)) start)
	      :element-type (array-element-type array)
	      :displaced-to array
	      :displaced-index-offset start))

(defun copy-array-into (to from)
  "Copy the contents of the array FROM to TO. They must be the same size."
  (when (not (equal (array-dimensions from) (array-dimensions to)))
    (error "This doesn't know how to copy arrays of different dimensions."))
  (cond
   ((= (array-rank to) 1)
    (replace to from))
   (t
    #+sbcl
    (replace (sb-ext:array-storage-vector to)
	     (sb-ext:array-storage-vector from))
    #-sbcl
    (dotimes (i (array-total-size to))
      (setf (row-major-aref to i) (row-major-aref from i))))))

(defun copy-array (array &key (element-type (array-element-type array))
			   (adjustable (adjustable-array-p array))
			   (fill-pointer (and (array-has-fill-pointer-p array)
					      (fill-pointer array)))
			   keep-displacement)
  "Return a copy of ARRAY with the same properties, except when given as the
keyword arguments. If KEEP-DISPLACEMENT is true, make a new array with the same
displacement."
  (if keep-displacement
      (multiple-value-bind (to offset) (array-displacement array)
	(if fill-pointer
	    (make-array (array-dimensions array)
			:element-type element-type
			:adjustable adjustable
			:displaced-to to :displaced-index-offset offset
			:fill-pointer fill-pointer)
	    (make-array (array-dimensions array) :adjustable adjustable
			:element-type element-type
			:displaced-to to :displaced-index-offset offset)))
      (let ((result
	     (if fill-pointer
		 (make-array (array-dimensions array)
			     :element-type element-type
			     :adjustable adjustable
			     :fill-pointer fill-pointer)
		 (make-array (array-dimensions array)
			     :element-type element-type
			     :adjustable adjustable))))
	(copy-array-into result array)
	result)))

;; @@@ compare vs. the ones in alexandria?
;; The difference between using using search or mismatch seems quite negligible.
(defun begins-with (prefix thing &key (test #'eql))
  "True if THAT begins with THIS."
  (let ((pos (search prefix thing :test test)))
    (and pos (zerop pos))))

(defun ends-with (suffix thing &key (test #'eql))
  "True if THAT ends with THIS."
  ;; (let ((pos (search this that :from-end t :test test)))
  ;;   (and pos (= pos (- (length that) (length this))))))
  (let ((pos (mismatch suffix thing :test test :from-end t)))
    (and pos (zerop pos))))

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

(defun s+ (&rest rest)
  "Return a string which is the arguments concatenated as if output by PRINC."
  ;; This is usually slower:
  ;; (labels ((as-string (s) (if (stringp s) s (princ-to-string s))))
  ;;   (apply #'concatenate 'string (as-string s) (mapcar #'as-string rest))))
  (macrolet ((to-string (x)
	       `(if (stringp ,x) ,x (princ-to-string ,x))))
    (cond
      ((null rest) (make-string 0))
      (t
       ;; Here's 3 different possible versions.
       ;; This should pick the one that's fastest for the implementation.
       ;; Currently version 1 is fastest for ECL, and version 3 seems fastest
       ;; for others I tested: CCL, CLISP, ECL.
       ;;
       ;; Version 1: string streams
       #+ecl
       (with-output-to-string (result)
	 (loop :for x :in rest :do (princ x result)))
       ;;
       ;; Version 2: adjustable strings
       ;;
       ;; (let ((result (make-array 0 :element-type 'character
       ;; 			       :fill-pointer 0 :adjustable t
       ;; 			       :initial-element (code-char 0)))
       ;; 	   (new-size 0)
       ;; 	   (old-len 0)
       ;; 	   (str ""))
       ;;   (declare (type fixnum new-size)
       ;; 		(type string str))
       ;;   (loop :for s :in rest :do
       ;; 	  (setf str (if (stringp s) s (princ-to-string s))
       ;; 		old-len (fill-pointer result)
       ;; 		new-size (+ old-len (length str)))
       ;; 	  (when (>= new-size (array-total-size result))
       ;; 	    (setf result (adjust-array result new-size)))
       ;; 	  (incf (fill-pointer result) (length str))
       ;; 	  (setf (subseq result old-len new-size) str))
       ;;   result)
       ;;
       ;; Version 3: reduce by concatenate
       #-ecl
       (if (not (cdr rest))
	   ;; reduce only works for at least 2 elements
	   (to-string (car rest))
	   (reduce (lambda (a b)
		     (concatenate 'string
				  (if (stringp a) a (princ-to-string a))
				  (if (stringp b) b (princ-to-string b))))
		   rest))))))

;; Version 0: apply concatenate
;; This version probably shouldn't be used due to apply limits.
#+(or)
(defun s0+ (&rest rest)
  "Return a string which is the arguments concatenated as if output by PRINC."
  (labels ((as-string (s) (if (stringp s) s (princ-to-string s))))
    (apply #'concatenate 'string (mapcar #'as-string rest))))

;; Version 1: string streams
#+(or)
(defun s1+ (&rest rest)
  "Return a string which is the arguments concatenated as if output by PRINC."
  (macrolet ((to-string (x)
	       `(if (stringp ,x) ,x (princ-to-string ,x))))
    (cond
      ((null rest) (make-string 0))
      (t
       (with-output-to-string (result)
	 (loop :for x :in rest :do (princ x result)))))))

;; Version 2: adjustable strings
#+(or)
(defun s2+ (&rest rest)
  "Return a string which is the arguments concatenated as if output by PRINC."
  (macrolet ((to-string (x)
	       `(if (stringp ,x) ,x (princ-to-string ,x))))
    (cond
      ((null rest) (make-string 0))
      (t
       (let ((result (make-array 0 :element-type 'character
				 :fill-pointer 0 :adjustable t
				 :initial-element (code-char 0)))
	     (new-size 0)
	     (old-len 0)
	     (str ""))
         (declare (type fixnum new-size)
		  (type string str))
         (loop :for s :in rest :do
	    (setf str (if (stringp s) s (princ-to-string s))
		  old-len (fill-pointer result)
		  new-size (+ old-len (length str)))
	    (when (>= new-size (array-total-size result))
	      (setf result (adjust-array result new-size)))
	    (incf (fill-pointer result) (length str))
	    (setf (subseq result old-len new-size) str))
         result)))))

;; Version 3: reduce by concatenate
#+(or)
(defun s3+ (&rest rest)
  "Return a string which is the arguments concatenated as if output by PRINC."
  (macrolet ((to-string (x)
	       `(if (stringp ,x) ,x (princ-to-string ,x))))
    (cond
      ((null rest) (make-string 0))
      (t
       (if (not (cdr rest))
	   ;; reduce only works for at least 2 elements
	   (to-string (car rest))
	   (reduce (lambda (a b)
		     (concatenate 'string
				  (if (stringp a) a (princ-to-string a))
				  (if (stringp b) b (princ-to-string b))))
		   rest))))))

;; Sum lengths and replace
#+(or)
(defun s4+ (&rest rest)
  (macrolet ((to-string (x)
	       `(if (stringp ,x) ,x (princ-to-string ,x))))
    (let* ((str-list (mapcar (_ (to-string _)) rest))
	   (len (loop :for i :in rest :sum (length str-list)))
	   (new-str (make-string len)))
      (loop :with i = 0
	 :for s :in str-list
	 :do
	 (replace new-str s :start1 i)
	 (incf i (length s)))
      new-str)))

;; format with pre-allocated fill pointer string
#+(or)
(defun s5+ (&rest rest)
  (macrolet ((to-string (x)
	       `(if (stringp ,x) ,x (princ-to-string ,x))))
    (let* ((str-list (mapcar (_ (to-string _)) rest))
	   (len (loop :for i :in rest :sum (length str-list)))
	   (new-str (make-array len :fill-pointer 0 :element-type 'character)))
      (format new-str "~{~a~}" str-list)
      new-str)))

;; format to a string
#+(or)
(defun s6+ (&rest rest)
  (format nil "~{~a~}" rest))

;; This seems likely to make things faster on any implementation.
(define-compiler-macro s+ (&whole whole &rest rest)
  "Special case for all string arguments."
  (cond
    ((null rest) (make-string 0))
    ((every #'stringp rest)
     `(concatenate 'string ,@rest))
    (t whole)))

(defparameter *ascii-whitespace*
  ;;#(#\tab #\newline #-gcl #\vt #\page #\return #\space)
  #(#\tab #\newline #.(code-char 11) #\page #\return #\space)
  "Characters considered whitespace in ASCII.")

#|

I wish unicode class information was built-in everywhere.
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

(defgeneric join-by-string (sequence connector &key &allow-other-keys)
  (:documentation
   "Return a string with CONNECTOR inbetween every element of SEQUENCE."))

(defmethod join-by-string ((sequence list) connector &key &allow-other-keys)
  "Return a string with a CONNECTOR between every element of SEQUENCE. This is
basically the reverse of SPLIT-SEQUENCE."
  (with-output-to-string (str)
    (loop :with first = t
       :for e :in sequence
       :if first
	 :do (setf first nil)
       :else
	 :do (princ connector str)
       :end
       :do (princ e str))))

(defmethod join-by-string ((sequence vector) connector &key &allow-other-keys)
  "Return a string with a CONNECTOR between every element of SEQUENCE. This is
basically the reverse of SPLIT-SEQUENCE."
  (with-output-to-string (str)
    (loop :with first = t
       :for e :across sequence
       :if first
	 :do (setf first nil)
       :else
	 :do (princ connector str)
       :end
       :do (princ e str))))

;; I already regret this.
(defgeneric join-by (type sequence connector &key &allow-other-keys)
  (:documentation
   "Join elements of SEQUENCE by CONNECTOR, resulting in a TYPE."))

(defmethod join-by ((type (eql 'string)) (sequence list) connector
		    &key &allow-other-keys)
  "Return a string with a CONNECTOR between every element of SEQUENCE. This is
basically the reverse of SPLIT-SEQUENCE."
  (join-by-string sequence connector))

(defmethod join-by ((type (eql 'string)) (sequence vector) connector
		    &key &allow-other-keys)
  "Return a string with a CONNECTOR between every element of SEQUENCE. This is
basically the reverse of SPLIT-SEQUENCE."
  (join-by-string sequence connector))

(defmethod join-by ((type (eql 'list)) (sequence list) connector
		    &key &allow-other-keys)
  "Return a string with a CONNECTOR between every element of SEQUENCE. This is
basically the reverse of SPLIT-SEQUENCE."
  (loop :with first = t
     :for e :in sequence
       :if first
	 :do (setf first nil)
       :else
	 :collect connector
       :end
       :collect e))

;; (defmethod join ((type (eql 'list)) (sequence list) connector
;;  		 &key &allow-other-keys)
;;   (cons sequence (if (atom connector) (list connector) connector)))

;; Not exactly fill or map-into
(defun fill-by (sequence function &key (start 0) end)
  "Replace elements of SEQUENCE from START to END with the results of calling
FUNCTION. START defaults to 0. END defaults to the end of the sequence."
  (etypecase sequence
    (list
     ;; This seems bad somehow.
     (loop
       :for e :on (nthcdr start sequence)
       :for i = start :then (1+ i)
       :while (and e (if end (< i end) t))
         :do (rplaca e (funcall function))))
    (vector
     ;; I think it's more useful for vectors.
     (loop :for i :from start :below (or end (length sequence))
	:do (setf (aref sequence i) (funcall function)))))
  sequence)

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

(defun insert-at (n item list)
  "Insert ‘item’ at position ‘n’ in ‘list’. Of course you have to use the return
value, if you want to delete the first item."
  (if (zerop n)
      (cons item list)
      (let ((x (nthcdr (1- n) list)))
	(prog1 list
	  (if x
	      (rplacd x (cons item (cdr x)))
	      x)))))

(defun alist-to-hash-table (alist &optional (table (make-hash-table)))
  "Load a hash table with data from an association list."
  (loop :for i :in alist
	:do (setf (gethash (car i) table) (cdr i)))
  table)

(defun plist-to-hash-table (plist &optional (table (make-hash-table)))
  "Load a hash table with data from a property list."
  (loop :with l = plist
	:while (not (endp l))
	:do (if (cdr l)
		(setf (gethash (pop l) table) (pop l))
		(cerror "Just keep going."
			"We weren't given a proper property list.")))
  table)

(defun hash-table-to-alist (hash-table)
  "Return an association list constructed from HASH-TABLE."
  (loop :for key :being :the :hash-keys :of hash-table
     :collect (list key (gethash key hash-table))))

(defun hash-table-to-plist (hash-table)
  "Return a property list constructed from HASH-TABLE."
  (loop :for key :being :the :hash-keys :of hash-table
	:collect key
	:collect (gethash key hash-table)))

(defun get-plist-values (properties from-plist)
  "Return a list of the values of the PROPERTIES from the plist FROM-PLIST."
  (mapcar (lambda (x) (getf from-plist x)) properties))

(defmacro do-plist ((key value list) &body body)
  "Evalute ‘body’ with each successive ‘key’ and ‘value’ from the plist ‘list’."
  (with-unique-names (l thunk)
    `(let ((,l ,list) ,key ,value)
       (declare (ignorable ,value))
       (flet ((,thunk () ,@body))
	 (when ,l
	   (loop
	      (setf ,key (car ,l)
		    ,value (if (cdr ,l)
			       (cadr ,l)
			       (error "Malformed property list")))
	      (,thunk)
	      (if (cddr ,l)
		  (setf ,l (cddr ,l))
		  (return))))))))

(defmacro do-alist ((key value list) &body body)
  "Evalute ‘body’ with each successive ‘key’ and ‘value’ from the alist ‘list’."
  (with-unique-names (l thunk)
    `(let ((,l ,list) ,key ,value)
       (declare (ignorable ,value))
       (flet ((,thunk () ,@body))
	 (when ,l
	   (loop
	      (when (atom (car ,l))
		(error "Malformed association list"))
	      (setf ,key (caar ,l)
		    ,value (cdar ,l))
	      (,thunk)
	      (if (cdr ,l)
		  (setf ,l (cdr ,l))
		  (return))))))))

(defmacro do-kv-list ((key value list) &body body)
  "Iterate over the keys and values of a plist or an alist. If the first element
of LIST is an atom, assume it's a plist, otherwise it's an alist."
  (with-unique-names (l)
    `(let ((,l ,list))
       (if (and ,l (atom (car ,l)))
	   (do-plist (,key ,value ,l) ,@body)
	   (do-alist (,key ,value ,l) ,@body)))))

;; I took this from rosetta code. This seems to be the fastest of 8 methods
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
       (cond
         ((consp (car node))		      ; If the left side is a cons
	   (when (cdar node)		      ; If it has more sub-lists
	     (push (cdar node) (cdr node)))   ; move the sub-list up
	   ;; Move contents of the one element list, up one
	   (setf (car node) (caar node)))
	 (t ;; Move on to the next item at the top level
	  (setf node (cdr node)))))
    ;; Get rid of any fake NILs in the results
    (if preserve-nils
	result
	(delete nil result))))

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

(declaim (inline clamp))
(defun clamp (n start end)
  "If N isn't in the range START - END, return the one it's closest to.
Otherwise, return N."
  (cond
    ((< n start) start)
    ((> n end) end)
    (t n)))

(defmacro clampf (n start end)
  "Set N to be in the range START - END, if it's not already."
  (let ((value (gensym "VALUE")))
    `(let ((,value ,n))
       (cond
	((< ,value ,start) (setf ,n ,start))
	((> ,value ,end)   (setf ,n ,end))
	(t ,value)))))

;; Objects

(defparameter *mop-package*
  #+(or (and clisp mop) abcl excl) :mop
  #+sbcl :sb-mop
  #+(or cmu gcl) :pcl
  #+ccl :ccl
  #+lispworks :hcl
  #+(or ecl clasp) :clos
  #+cormanlisp :cl
  #+mezzano :mezzano.clos
  #-(or mop sbcl cmu ccl lispworks gcl ecl clasp cormanlisp abcl mezzano excl
        clasp)
  (error "GIVE ME MOP!!")
  "The package in which the traditional Meta Object Protocol resides.")

#+(or (and clisp mop) sbcl cmu gcl ccl ecl mezzano excl) (d-add-feature :has-mop)

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

#+abcl (defvar *abcl-bug* nil "True if we've already hit the bug.")

;; I know the args are probably getting stringified then un-stringified,
;; but this is mostly just for startup.
(defun run-system-command (cmd args &key output-stream)
  "Return an output stream from the system command."
  #+clisp
  (if output-stream
      (ext:run-shell-command
       (format nil "~a~{ ~a~}" cmd args) :output :stream)
      (ext:run-shell-command
       (format nil "~a~{ ~a~}" cmd args)))
  #+sbcl
  (if output-stream
      (sb-ext:process-output
       (sb-ext:run-program cmd args :output :stream :search t))
      (sb-ext:run-program cmd args :search t))
  #+cmu
  (if output-stream
      (ext:process-output
       (ext:run-program (exe-in-path cmd) args :output :stream))
      (ext:run-program (exe-in-path cmd) args))
  #+openmcl
  (if output-stream
      (ccl::external-process-output-stream
       (ccl::run-program cmd args :output :stream))
      (ccl::run-program cmd args))
  #+ecl
  (ext:run-program cmd args) ;; @@@ is this right?
  #+excl
  (if output-stream
      (excl:run-shell-command (format nil "~a~{ ~a~}" cmd args)
			      :output :stream :wait t)
      (excl:run-shell-command (format nil "~a~{ ~a~}" cmd args) :wait t))
  ;; @@@ The LW manual says this only works on unix:
  #+lispworks
  (if output-stream
      (system:run-shell-command (format nil "~a~{ ~a~}" cmd args)
				:output :stream :wait nil)
      (system:run-shell-command (format nil "~a~{ ~a~}" cmd args) :wait t))
  #+abcl
  (progn
    (when (not *abcl-bug*)
      ;; It doesn't work the first time.
      ;; @@@ How can we suppress the giant stack trace???
      (ignore-errors
       (system:process-output (system:run-program "/bin/true" nil))))
    (if output-stream
	(system:process-output (system:run-program cmd args))
	(system:run-program cmd args)))
  #+clasp
  (if output-stream
      (multiple-value-bind (result pid stream)
	  (ext:fork-execvp (cons cmd args) t)
	(declare (ignore result pid))
	stream)
      (ext:fork-execvp (cons cmd args) #| what was t for? |# ))
  #+mezzano (declare (ignore cmd args output-stream))
  #+mezzano (make-string-input-stream "")
  #-(or clisp sbcl cmu openmcl ecl excl lispworks abcl clasp mezzano)
  (declare (ignore cmd args output-stream))
  #-(or clisp sbcl cmu openmcl ecl excl lispworks abcl clasp mezzano)
  (missing-implementation 'run-system-command))

(defun system-command-stream (cmd args)
  (run-system-command cmd args :output-stream t))

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
      (when s (close s)))))

;; Don't clash with nos:lisp-args.
(defun system-args ()
  "Return a sequence of arguments given when starting the Lisp system.
May be list or vector of strings."
  #+sbcl sb-ext:*posix-argv*
  #+clisp (ext:argv)
  #+cmu ext:*command-line-strings*
  #+(or ecl clasp) (loop :for i :from 0 :below (si:argc)
		      :collecting (si:argv i))
  #+openmcl (ccl::command-line-arguments)
  #+excl (sys:command-line-arguments)
  #+lispworks sys:*line-arguments-list*
  #+abcl ext:*command-line-argument-list*
  #+mezzano nil
  #-(or sbcl clisp cmu ecl openmcl excl lispworks abcl mezzano)
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
  #+mezzano nil ;; or we could (mezzano.supervisor:reboot) ?
  #-(or openmcl cmu sbcl excl clisp ecl abcl clasp)
  (missing-implementation 'exit-system))

(defun save-image-and-exit (image-name &rest keys
			    &key initial-function &allow-other-keys)
  "Save an executable image in IMAGE-NAME. If provideed, call INITIAL-FUNCTION
on starting the new image. Other unspecified implementation specific keywords
can be provided, which are usually passed to the underlying function."
  (declare (ignorable keys))
  #+sbcl
  (if keys
      (let ((extra-args (copy-list keys)))
	(remf extra-args :initial-function)
	(when initial-function
	  (setf (getf extra-args :toplevel) initial-function))
	(apply #'sb-ext:save-lisp-and-die image-name :executable t
	       extra-args))
      (sb-ext:save-lisp-and-die image-name :executable t
				:toplevel initial-function))
  #+clisp (ext:saveinitmem image-name :executable t :quiet t :norc t
			   :init-function initial-function)
  #+ccl (save-application image-name :prepend-kernel t
			  :toplevel-function initial-function)
  ;; ?? or something. but should we really shutdown?
  ;; Should we even consider this to be the same?
  ;; or maybe (snapshot-and-exit)
  #+mezzano (declare (ignore image-name initial-function))
  #+mezzano (mezzano.supervisor:snapshot)
  #-(or sbcl clisp ccl mezzano) (declare (ignore image-name initial-function))
  #-(or sbcl clisp ccl mezzano) (missing-implementation 'save-image-and-exit))

;; a.k.a root
(defun overwhelming-permission ()
  #+ccl (= (ccl::getuid) 0)
  #+(and sbcl unix) (= (sb-unix:unix-getuid) 0)
  #+(and sbcl win32) nil
  ;; What about posix:getuid ? It seems to be missing on clisp 2.46.
  #+clisp (= (posix:user-info-uid (posix:user-info :default)) 0)
  #+cmu (= (unix:unix-getuid) 0)
  #+ecl (= (ext:getuid) 0)
  #+(or lispworks abcl clasp) nil
  #+mezzano t				; sort of?
  #-(or ccl sbcl clisp cmu ecl lispworks abcl clasp mezzano)
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
  "Like defconstant but don't warn on re-definitions."
  (declare (ignore test))
  #-ccl
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc)))
  #+ccl
  (eval-when (:compile-toplevel :load-toplevel :execute)
  `(cl:defconstant ,name ,value ,@(when doc (list doc))))
  )

;; On other lisps just use the real one.
;;#-(or sbcl clisp ccl cmu lispworks ecl abcl)
;;(setf (macro-function 'define-constant) (macro-function 'cl:defconstant))

(defmacro defconstant-to-list (list-var constant-array)
  "Define constants and put the names in LIST-VAR."
  `(progn
     ,@(loop :with name :and value :and doc
	  :for c :across constant-array :do
	  (setf name  (aref c 0)
		value (aref c 1)
		doc   (aref c 2))
	  :collect
	  `(defconstant ,name ,value ,doc)
	  :collect
	  `(push ',name ,list-var))))

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
	   (new-nicks (cons alias (package-nicknames pkg))))
      ;; Hopefully this trick works on most implementations.
      (rename-package original (package-name original) new-nicks)))
  (:method (alias (original symbol) (alias-type (eql 'class)))
    (define-alias alias (find-class original) alias-type))
  (:method (alias (original standard-class) alias-type)
    "Make a alias for a standard class."
    (declare (ignore alias-type))
    (setf (find-class alias) original
	  (documentation alias 'type)
	  (documentation original 'type))))

;; You can make appropriate methods for your own types.

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
	   (define-alias alias original 'function))
	  ;; (t (error "~s is fbound but isn't a function or macro?" original))
	  ))
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

;; So we can just say mop: on any implementation?
(#+sbcl without-package-locks
 #+clisp ext:without-package-lock #+clisp ()
 #-(or sbcl clisp) progn
 #-(or cmu excl) (defalias :mop (find-package *mop-package*)))

(defalias '⇚ 'multiple-value-list)

;; This is just to pretend that we're trendy and modern.
;(setf (macro-function 'λ) (macro-function 'cl:lambda))
;;Umm actually I mean:
#-(or lispworks clasp)
(defmacro λ (&whole form &rest decls-and-body)
   (declare (ignore decls-and-body))
   ;; `#'(lambda r form)))
   ;; `(function (lambda ,(cdr form))))
   `(lambda ,@(cdr form)))
;; Still doesn't work everywhere? WHY?
;; Also this doesn't work: (defalias 'λ 'lambda)
;; I think there's some set of places in standard CL where something does a
;; (eq 'lambda x) in a context that doesn't macro expand, but what is that set?

;; Is it really worth doing this? Is this gratuitous language mutation?
;; This is weird. Why do I love using it so much? I wonder if there is a better
;; symbol to use?
(defmacro _ (&rest exprs)
  "Shorthand for single argument lambda. The single argument is named '_'."
  `(lambda (_)
     (declare (ignorable _))
     ,@exprs))

(defmacro __ ((&rest args) &body body)
  "Extra bogus shorthand for lambda, but with auto ignored args. Arguments whose
names start with #\_ are ignored."
  (let ((ignore-list
	 (loop :for a :in args
	    :when (char= #\_ (char (symbol-name a) 0))
	    :collect a)))
    `(lambda (,@args)
       ,@(when ignore-list `((declare (ignore ,@ignore-list))))
       ,@body)))

;; We can pronounce "<>" as lozenge.
(defmacro and-<> (&rest expressions)
  "Evalute EXPRESSIONS until one is false, binding <> to the result of the
previous expression. Return the last expression or NIL."
  (cond
    ((not expressions) nil)
    ((= 1 (length expressions))
     (car expressions))
    (t
     `(let ((<> ,(first expressions)))
	(when <>
	  ,(reduce (lambda (&rest x)
		     (when x
		       `(when (setf <> ,(cadr x))
			  ,(car x))))
		   (reverse (rest expressions))))))))

(defmacro -<>> (&rest expressions)
  "Evalute EXPRESSIONS until one is false, binding <> to the result of the
previous expression. Return the last expression or NIL."
  (cond
    ((not expressions) nil)
    ((= 1 (length expressions))
     (car expressions))
    (t
     `(let ((<> ,(first expressions)))
        ,(let (r)
           (loop :for e :in (rest expressions)
	      :do (setf r
			(if r (if (listp e) `(,@r <> ,e) `(,@r <> (,e <>)))
			    `(setf <> ,(if (listp e) `(,@e) `(,e <>))))))
           r)))))

;; @@@ Despite it's popularity, this seems like it might be too seldomly
;; used to take the very iconic "->" syntax.
(defmacro -> (&rest expressions)
  "Transform the expressions so each expression is the second argument to the
previous expression. Expressions except the first that aren't lists are made
into lists so they look like function applications."
  (cond
    ((null expressions) nil)
    ((= 1 (length expressions))
     (car expressions))
    (t
     (reduce (lambda (a b)
	       `(,(first b) ,(second b) ,a ,@(cddr b)))
	     (mapcar (_ (if (not (listp _)) (list _) _))
		     (rest expressions))
	     :initial-value (first expressions)))))

(defmacro ->> (&rest expressions)
  "Transform the expressions so each expression is the last argument to the
previous expression. Expressions except the first that aren't lists are made
into lists so they look like function applications."
  (cond
    ((null expressions) nil)
    ((= 1 (length expressions))
     (car expressions))
    (t
     (reduce (lambda (a b)
	       (append (if (not (listp b)) (list b) b) (list a)))
	     (rest expressions) :initial-value (first expressions)))))

(defun symbolify (string &key (package *package*) no-new)
  "Return a symbol, interned in PACKAGE, represented by STRING, after possibly
doing conventional case conversion. The main reason for this function is to
wrap the case conversion on implementations that need it. If NO-NEW is true,
never create a new symbol, and return NIL if the symbol doesn't already exist.
Package can be NIL in which case it returns an un-interned symbol."
  (etypecase string
    (string
     (if no-new
	 (find-symbol (string-upcase string) package)
	 (if package
	     (intern (string-upcase string) package)
	     (make-symbol string))))
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

(defun argument-list (fun)
  "Return the function's arguments."
  #+sbcl
  (let ((f (or (macro-function fun)
	       (symbol-function fun))))
    (cond
      ((typep f 'generic-function)
       (sb-mop:generic-function-lambda-list f))
      ((null f) nil)
      (t (sb-kernel:%fun-lambda-list f))))
  #+ccl (ccl:arglist fun)
  #+clisp (when (not (special-operator-p fun))
	    (ext:arglist fun))
  #+cmu (function-arglist fun)
  #+lispworks (let ((args (lw:function-lambda-list fun)))
		(and (listp args) (strings-to-symbols args)))
  #+abcl (sys::arglist fun)
  #+ecl (ext:function-lambda-list fun)
  #-(or sbcl ccl clisp cmu lispworks abcl ecl)
  (multiple-value-bind (exp closure-p name)
      (function-lambda-expression fun)
    (declare (ignore closure-p name))
    (when exp
      (cadr exp))))

(defun lambda-list-vars (args &key all-p keywords-p)
  "Given a lambda list ARGS, return a list of variable names.
ALL-P       if true, includes supplied-p parameters and &AUX variables.
            This is useful for things like generating ignore lists for macros.
KEYWORDS-P  If true, include keywords and variable names."
  (flet ((keyword-var (k v)
	   "If KEYWORDS-P return a list of (:K V) otherwise just V."
	   (if keywords-p (list (keywordify k) v) v))
	 (more-than-2 (l)
	   "True if there are more than two items left in L."
	   (and (cddr l) t)))
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
		       (if (and (more-than-2 a) all-p)
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
			 (if (and (more-than-2 a) all-p)
			     (list (keyword-var key var)
				   (third a))
			     (keyword-var key var)))
		       (keyword-var a a))))
	    (:aux (when all-p (setf thing a)))
	    (otherwise (setf thing a)))))
       :when thing
         :if (listp thing) :append thing :else :collect thing)))

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

(defun ensure-exported (symbols &optional package)
  "Export SYMBOLS from PACKAGE only if they aren't already exported.
SYMBOLS is a designator for a symbol or list of symbols like for EXPORT."
  (loop :for s :in (if (atom symbols) (list symbols) symbols) :do
     (multiple-value-bind (symbol status) (find-symbol s)
       (when (and symbol (not (eq status :external)))
	 (export symbol package)))))

(defmacro without-package-variance (&body body)
  ;; `(let (#+sbcl (sb-ext:*on-package-variance* '(:ignore t)))
  ;;    ,@body))
  #+sbcl
  ;; `(let ((sb-ext:*on-package-variance*
  ;; 	  (if (>= dlib:*lisp-version-number* 20001)
  ;; 	      ;; '(:ignore t)
  ;; 	      ;; '(:suppress t)
  ;; 	      '(:warn t))))
  `(restart-bind
       ((package-at-variance-error
	 (lambda () (invoke-restart 'sb-impl::keep-them)))
	(package-at-variance
	 (lambda () (invoke-restart 'sb-impl::keep-them))))
     (without-warning ,@body))
  #-sbcl
  `(progn ,@body))

#| How about not a macro?

(defmacro not-so-funcall (package symbol &rest args)
  "Call SYMBOL with ARGS if it's FBOUND in PACKAGE and PACKAGE exists."
  (with-unique-names (pkg sym)
    `(let* ((,pkg (find-package ,package))
	    (,sym (intern (string ,symbol))))
       (if (fboundp ,sym))
	 (funcall (intern ,sym (find-package ,package)) ,@args)
       (error "Not so funcall ~s ~s" ,package ,symbol))))

(defmacro #.(code-char #x203b) (package symbol &rest args)
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

(defalias 'symbol-call 'not-so-funcall)
;; (defalias 'ﬀ 'not-so-funcall)

(defun refer-to (package symbol &rest args)
  "Call or get the value of SYMBOL in PACKAGE."
  (let ((pkg (find-package package)) sym)
    (when (not pkg)
      (error "Package not found ~s" package))
    (setf sym (intern (string symbol) pkg))
    (cond
      ((fboundp sym)
       (apply sym args))
      ((boundp sym)
       (when args
	 (warn "Useless refer-to args were provided."))
       (symbol-value sym))
      (t
       (error "Symbol ~s not bound in ~s" symbol package)))))

;; (defalias '#.(code-char #x203b) 'refer-to)

(defun maybe-refer-to (package symbol &rest args)
  "Call or get the value of SYMBOL in PACKAGE, or return NIL if the symbol or
package doesn't exist or isn't bound."
  (let ((pkg (find-package package)) sym)
    (when pkg
      (setf sym (intern (string symbol) pkg))
      (cond
	((fboundp sym)
	 (apply sym args))
	((boundp sym)
	 (when args
	   (warn "Useless refer-to args were provided."))
	 (symbol-value sym))))))

;; (defalias '#.(code-char #x203b)? 'maybe-refer-to)

;; Is this even useful??
;; (defmacro @ (object &rest slot-names)
;;   "Slot access."
;;   (let ((result object))
;;     (loop
;;        :for n :in slot-names
;;        :do (setf result `(slot-value ,result ',n)))
;;     result))

;; This probably violates what people consider good design.
;; @@@ This could probably use more error checking?
(defmacro with-internal-slots ((&rest slot-names) object package &body body)
  "Like WITH-SLOTS but getting using the slot names internal to PACKAGE."
  `(with-slots (,@(mapcar (lambda (x) (list x (intern (symbol-name x) package)))
			  slot-names)) ,object
     ,@body))

(defmacro ignore-conditions ((&rest conditions) &body body)
  "If body signals a condition type in CONDITIONS, return the values NIL and
the condition."
  `(handler-case
       (progn ,@body)
     ,@(loop :for cc :in conditions
	  :collect `(,cc (c) (values nil c)))))

(defalias 'ignore-some-conditions 'ignore-conditions)

(defmacro defmethod-quiet (name &rest args)
  "Same as defmethod, but don't complain."
  ;; Why doesn't this work????
  ;; #+sbcl 
  ;; `(locally (declare (sb-ext:muffle-conditions warning))
  ;;    (defmethod ,name ,@args))
  ;; #-sbcl
  `(without-warning
       (defmethod ,name ,@args)))

;; @@@ Perhaps we should also have a version that separates doc and decls?

(defmacro with-decls-and-body ((your-body) &body my-body)
  "Pull the documentation and declarations out of YOUR-BODY, and bind them to
DOC-AND-DECLS. The body without DOC-AND-DECLS is bound to FIXED-BODY.
Evaluate MY-BODY. This is useful for making a macro that wraps a standard
definition form, like defun. For example:

  (defmacro defoo (name args &body body)
     (with-decls-and-body (body)
       `(defun ,name ,args
           ,@doc-and-decls
           (format t \"Hi.~%\")
           ,@fixed-body)))"
  `(let* ((fixed-body ,your-body)
	  (doc-and-decls
	   (loop :with doc :and first = t
	      :for form = ,your-body :then (cdr form)
	      :while form
	        :if (and (stringp (car form))
		       (not (and first (not (cdr form)))) ; a single string
		       (not doc))
	          :collect (setf doc (car form))
	        :else :if (and (consp (car form))
			       (eq (caar form) 'declare))
	          :collect (car form)
                :else
	          :do (loop-finish)
	        :end
	      :do (setf fixed-body (cdr form)
			first nil))))
     ,@my-body))

(deftype function-designator ()
  "Something that denotes a function."
  `(or function symbol null))

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
;; Output goes to *DBUG-OUTPUT*, so you can modify that if you want the output
;; to go somewhere else, like:
;;  (let ((*dbug-output* *earth*)) (with-dbug (land-on-mars)))

(defparameter *dbug* nil
  "True to output debugging messages.")

(defparameter *dbug-output* *trace-output*
  "Where debugging message go. Defaults to *TRACE-OUTPUT*.")

(defparameter *dbug-package* nil
  "A package to debug. Output debugging message when we're in this package.")

(declaim (type (or cons null vector) *dbug-facility*))
(defparameter *dbug-facility* nil
  "Facilities to debug. A sequence of symbols or something.")

(defmacro dbug (fmt &rest args)
  "Print a debugging message when debugging is turned on and maybe we're in
the right package."
  `(when (and dlib:*dbug*
	      (or (not *dbug-package*)
		  (equal *dbug-package* (package-name *package*))))
     (funcall #'format *dbug-output* ,fmt ,@args)
     (finish-output *dbug-output*)))

(defmacro dbugf (facility fmt &rest args)
  "Print a debugging message when debugging is turned on and maybe we're in
the right package, and the FACILITY is activated."
  (when (not (typep facility '(or symbol list)))
    (error "You probably forgot the facility."))
  `(when (and #| dlib:*dbug* |# dlib:*dbug-facility*
	      (or
	       (member ,facility *dbug-facility*)
	       (member :all *dbug-facility*)))
     (funcall #'format *dbug-output* ,fmt ,@args)
     (finish-output *dbug-output*)))

(defmacro if-dbugf ((facility) &body body)
  "Evaluate BODY when debugging is turned on and maybe we're in
the right package, and the FACILITY is activated."
  `(when (and #| dlib:*dbug* |# dlib:*dbug-facility*
	      (or
	       (member ,facility *dbug-facility*)
	       (member :all *dbug-facility*)))
     (multiple-value-prog1 (progn ,@body)
       (finish-output *dbug-output*))))

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
    `(let* (;(*dbug* t)
	    (,fac ,facility)		; so we only eval once
	    (*dbug-facility*
	     (append *dbug-facility*
		     (if (listp ,fac) ,fac (list ,fac)))))
     ,@body)))

(defmacro with-dbugf-to (facility file &body body)
  "Evaluate the BODY with debugging message printing tagged with FACILITY
turned on. FACILITY can be an atom or a list of facilities to turn on.
FILE is a file to redirect *dbug-output* to, and therefore also DBUGF messages."
  `(with-open-file (*dbug-output* ,file :direction :io :if-exists :append)
     (with-dbugf ,facility
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
  (defun exe-in-path (name)
    "Return the full path of file NAME exists in the current execute path,
which is usually the environment variable PATH. Note that this doesn't check
if it's executable or not or that you have permission to execute it.
Return NIL if it's not found."
    (loop :with full-name
	  ;; @@@ This is probably wrong for Windows, but maybe not in cygwin.
	  :for d :in (split-sequence ":" (d-getenv "PATH"))
	  :when (fake-probe-file (setf full-name (s+ d "/" name)))
	  :do (return full-name)
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
		      (string (when (dlib::exe-in-path (car cmd))
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

;; @@@ This is totally wrong.
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

;; In theory the spec says (in 21.1.3 fig 21-7) that open and with-open-file
;; can take a stream as the file argument. In practice, it seems this can only
;; be some kinds of stream, like a file-stream. So I guess we have to do this
;; anyway.
(defmacro with-open-file-or-stream ((var file-or-stream &rest args) &body body)
  "Evaluate BODY with VAR bound to FILE-OR-STREAM if it's already a stream, or
an open a stream named by FILE-OR-STREAM. ARGS are standard arguments to OPEN."
  (let ((thunk (gensym "thunk"))
	(abort-flag (gensym "abort-flag"))
	(results (gensym "results")))
    `(let ((,var nil) (,abort-flag t) ,results)
       (flet ((,thunk () ,@body))
	 (if (streamp ,file-or-stream)
	     (progn
	       (setf ,var ,file-or-stream)
	       (,thunk))
	     ;; We can't use with-open-file here because it creates it's own
	     ;; lexical stream variable, and we want to use ours.
	     (progn
	       (unwind-protect
		  (progn
		    (setf ,var (open ,file-or-stream ,@args))
		    (setf ,results (multiple-value-list (,thunk))
			  ,abort-flag nil))
		 (when ,var (close ,var :abort ,abort-flag)))
	       (values-list ,results)))))))

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

(defalias 'file-lines 'get-lines)

(defun package-copy-name (package &optional (prefix "COPY-OF-"))
  "Pick a stupid name for a copied package."
  (let (name)
    (loop :with i = 0
       :while (find-package
	       (setf name (format nil "~a~a~a" prefix (package-name package)
				  (if (> i 0) i ""))))
       :do (incf i)
       ;;:if (> i 1000000) (error "Something probably went wrong.")
       )
    name))

;;; @@@ Test? or eliminate!
(defun copy-package (package)
  "Return a copy of PACKAGE. The new package has all the symbols imported,
shadowed symbols shadowed, and used packages used, from the old package, and
is named \"COPY-OF-<Package><n>\"."
  (assert package)
  (let ((new-package (make-package (package-copy-name package) :use '())))
    (loop :for s :being :each :present-symbol :of package
       :do (import s new-package))
    (loop :for s :in (package-shadowing-symbols package)
       :do (shadow (list (symbol-name s)) new-package))
    (loop :for pkg in (package-use-list package)
       :do (use-package pkg new-package))
    new-package))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (boundp '*read-intern*)
    (d-add-feature :has-read-intern)))

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
(defparameter *buffer-size* (* 8 1014)
  "The default buffer size for efficient copying of streams and files.")

;; I suppose we could make this generic so that streams can do a special
;; things with it, but that might be sort of edging into the stream protocol.
(defun copy-stream (source destination &key (buffer-size *buffer-size*)
					 (errorp t)
					 element-type)
  "Copy data from reading from SOURCE and writing to DESTINATION, until we get
an EOF on SOURCE. If ERRORP is true, signal an error if the element types are
not the same. Use ELEMENT-TYPE as the specific element type for copying, which
could be useful in case the streams support multiple element types."
  ;; ^^^ We could try to make *buffer-size* be the minimum of the file size
  ;; (if it's a file) and the page size, but I'm pretty sure that the stat
  ;; call and possible file I/O is way more inefficient than wasting less than
  ;; 4k of memory to momentarily. Of course we could mmap it, but it should
  ;; end up doing approximately that anyway and the system should have a
  ;; better idea of how big is too big, window sizing and all that. Also,
  ;; that's way more complicated. Even this comment is too much. Let's just
  ;; imagine that a future IDE will collapse or footnotify comments tagged
  ;; with "^^^".
  (when (and errorp
	     (not (eql (stream-element-type source)
		       (stream-element-type destination))))
    ;; It would be nice if we could handle this, but there are all kinds of
    ;; potential issues. Also, only subtypes of integer and character are in
    ;; the standard.
    (error "Stream element types have to match."))
  (let ((buf (make-array buffer-size
			 :element-type (or element-type
					   (stream-element-type source))))
	pos)
    (loop :do
       (setf pos (read-sequence buf source))
       (when (> pos 0)
	 (write-sequence buf destination :end pos))
       :while (= pos *buffer-size*))))

;; See also dlib-misc: slurp
(defun stream-string (&optional (stream *standard-input*))
  "Return the conents of ‘stream’ as a string. ‘stream’ defaults to
‘*standard-input*’."
  (with-output-to-string (string-stream)
    (copy-stream stream string-stream)))

(defun quote-format (s)
  "Quote a string to send to format, so that any possible format directives
are printed rather than interpreted as directives, which really just means:
repleace a single tilde with double tidles."
  (replace-subseq "~" "~~" s))

;; I would say titlify or titlize, but the spelling is weird.
(defun name-to-title (s)
  "Turn a typical Lisp identifier into a title, by capitalizing and changing
deashes and underscores to spaces. This should be called with something that
can be passed to STRING."
  (string-capitalize
   (substitute #\space #\_ (substitute #\space #\- (string s)))))

;; I wish this could be in dlib-misc, but opsys needs it.
(defun print-properties (prop-list &key (right-justify nil) (de-lispify t)
				     (stream t) (format-char #\a))
  "Print a set of names and values nicely in two vertical columns. If the first
element of PROP-LIST is a cons, it guesses that it's an alist, otherwise it
assumes it's a plist."
  (let ((label-length 0)
	(format-string
	 (s+ "~v" (if right-justify "@" "") "a: ~" format-char "~%")))
    (labels ((output-line (str name value)
	       (format str format-string
		       label-length
		       (if de-lispify
			   (name-to-title (princ-to-string name))
			   ;; (string-downcase (princ-to-string name)))
			   (princ-to-string name))
		       value)))
      (do-kv-list (key value prop-list)
	(setf label-length
	      (max label-length (length (princ-to-string key)))))
      (if (null stream)
	  (with-output-to-string (str)
	    (do-kv-list (name value prop-list)
	      ;;value (if (and (cdr p) (listp (cdr p))) (cadr p) (cdr p)))
	      (output-line str name value)))
	  (do-kv-list (name value prop-list)
	    (output-line stream name value))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System information features
;;;

;; Know where we are:
;;
;; The problem is that (machine-instance) gets the DNS name which can
;; vary depending on what network you're connected to, but what we want is a
;; pseudo-unique name of the machine independent of the network.

#+cormanlisp
(defun get-computer-name ()
  (let (name len result)
    (unwind-protect
	 (progn
	   (setf name (ct:malloc (* 2 15))
		 len (ct:malloc 4))
	   (setf (ct:cref (:unsigned-long *) len) 15)
	   (when (zerop (win:getcomputername name len))
	     (error "get-computer-name: "))
	   (setf result (ct:c-string-to-lisp-string name)))
      (when name (ct:free name))
      (when len (ct:free len)))
    result))

(defparameter *host* (or (and (not (null (machine-instance)))
			      (length (machine-instance))
			      (machine-instance))
			 #+mezzano "bogozzano"
			 (initial-span
			  (try-things
			   '(("scutil" "--get" "ComputerName")
			     ("hostname") ("uname" "-n")
			     (machine-instance)
			     #+cormanlisp (get-computer-name)
			     ;; (machine-instance) return NIL on mezzano?
			     ))
			  '(#\. #\space)))
  "The name of the machine. Usually the same as the network host name.")
(d-add-feature *host*)

#+(and darwin ecl) (d-add-feature :unix)

; (defun arch-from-uname ()
;   "Last ditch effort for constructing ARCH from UNIXish uname"
;   (let ((proc (shell-line "uname" "-p")
; 	(mach (shell-line "uname" "-m")
; 	(os   (shell-line "uname" "-s")
; 	(rel  (shell-line "uname" "-r")

;; Figure out environmental features
(defparameter *arch* (or (d-getenv "ARCH") (d-getenv "MACHTYPE")
			 (machine-type))
  "String uniquely identifying this platform. A combination of processor
architecture, computer vendor, operating system, and operating system version.")

(defparameter *arch-nickname*
  (cond
    ((and (or (search "x86_64"  *arch*)
	      (search "x86-64"  *arch*))
	  (or (has-feature :X86-64) (has-feature :64-bit-target))
	  #+clisp (not (search "arch i386 " (software-type))))
	  "x86_64")
    ((search "aarch64" *arch*) "arm64")
    ((search "arm64"   *arch*) "arm64")
    ((search "x86"     *arch*)	"x86")
    ((search "i686"    *arch*)	"x86")
    ((search "386"     *arch*)	"x86")
    ((search "sparc"   *arch*)	"sparc")
    ((search "powerpc" *arch*)	"ppc")
    ((search "ppc"     *arch*)	"ppc")
    ((search "java"    *arch*)	"java")
    (t "unknown")
  )
  "A short nickname for the current architecture.")
(d-add-feature *arch-nickname*)

(defparameter *os*
  #+unix (shell-line "uname" "-s")
  #+mezzano "Mezzano"
  #+(or win32 windows) "windows"
  #-(or unix (or win32 windows) mezzano) "unknown"
  "A string identifying the operating system.")

(defparameter *os-nickname*
  (cond
    ((search "Linux"  *os*)	"linux")
    ((search "SunOS"  *os*)	"solaris") ; come on now
    ((search "Darwin" *os*)	"darwin")
    ((search "windows" *os*)	"windows")
    (t
     #+mezzano "mezzano"
     #-mezzano "unknown"
     ))
  "A short nickname for the current operating system.")
(d-add-feature *os*)

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
  #+mezzano	"Mezzano"
  #-(or clisp sbcl cmu excl openmcl lispworks ecl abcl clasp mezzano) "Unknown"
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
			   (subseq v (1- (position #\. v))))))
  #+mezzano
  (let* ((v (lisp-implementation-version))
	 (s (position #\space v)))
    (or (and s (>= (length v) (+ s 9))
	     (format nil "~a-~a" (subseq v (1+ s) (+ s 9))
		     (subseq v 0 s)))
	(lisp-implementation-version)
	"unknown"))
  #-(or clisp sbcl cmu excl openmcl lispworks ecl abcl clasp mezzano) "unknown"
  "A version suitable for putting in a logical pathname.")

(defparameter *platform-nickname* (format nil "~(~a~)~a-~a-~a"
					  *lisp-implementation-nickname*
					  *lisp-version*
					  *os-nickname*
					  *arch-nickname*)
 "Nickname for the platform. Hopefully determines compiled code compatibility.")
(d-add-feature *platform-nickname*)	; Incorrigible feature adding

(d-add-feature :dlib)			; @@@ unecessary? only for fake-dlib?

;; Before you add stuff here, consider whether to put it in dlib-misc instead.
;; For example stuff that requires opsys should go in there.

#+debug-rc (progn (format t " 1") (force-output *standard-output*))

;; End
