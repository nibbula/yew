;;;
;;; fake-dlib.lisp - Stuff from dlib to eliminate the dependency.
;;;

;; This is doomed code. An irksome workaround to detach this pod from the
;; mothership.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (getf opsys-config:*config* :use-fake-dlib))
    (error "Configured not use use fake-dlib but loading it anyway?")))

(defpackage :fake-dlib
  (:documentation
   "Stuff from dlib to eliminate the dependency. I apologize for every stupid
thing in here. At least it's a little shorter than dlib. At least someday it
will disappear, when we resolve the issues with dlib.")
  (:use :cl
	;; We must have the MOP!!! Don't ever drop the MOP!
	#+(or (and clisp mop) abcl) :mop
	#+sbcl :sb-mop
	#+cmu :pcl
	#+ccl :ccl
	#+(or ecl clasp) :clos
	#+lispworks :hcl
	;; extensions
	#+sbcl :sb-ext
	) ;; Don't add any more imports.
  #+lispworks (:shadow #:with-unique-names)
  (:nicknames :the-real-fake-dlib)
  (:export
   #:define-constant
   #:defconstant-to-list
   #-lispworks #:with-unique-names
   #:with-names
   #:without-warning
   #:define-alias
   #:defalias
   #:symbolify
   #:keywordify
   #:symbol-call
   #:clamp
   #:*mop-package*
   #:copy-stream
   #:s+
   #:_
   #:initial-span
   #:split-sequence
   #:begins-with
   #:flatten
   #:+simple-condition-format-control-slot+
   #:+simple-condition-format-arguments-slot+
   #:function-designator
   #:dbugf
   ))
(in-package :fake-dlib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun d-add-feature (f)
    "Add a feature named by the given string to *FEATURES*"
    (assert f)
    (if (stringp f)
	;;(nconc *features* (list (intern (string-upcase f) :keyword)))
	;;(nconc *features* (list f))))
	(pushnew (intern (string-upcase f) :keyword) *features*)
	(pushnew f *features*)))

  (defmacro has-feature (f)
    "For when #+feature isn't what you want."
    `(find ,f *features*))

  (defun d-remove-feature (f)
    "Remove a feature from *FEATURES*"
    (setq *features* (delete f *features*))))

(defun same-with-warning (name new test)
  "If ‘name’ is bound, return it's value, but but warn if ‘new’ is different
from it according to ‘test’. Return ‘new’ if name isn't bound."
  (if (boundp name)
      (let ((old (symbol-value name)))
	(unless (funcall test old new)
	  (warn "Not redefining ~s from ~s to ~s." name old new))
	old)
      new))

(defmacro define-constant (name value &optional doc (test ''equal))
  "Like ‘defconstant’ but don't actually redefine the constant. If the ‘value’
is equal according to ‘test’, which defaults to ‘equal’, then don't even
complain. Otherwise just warn, and don't redefine it."
  `(cl:defconstant ,name (same-with-warning ',name ,value ,test)
     ,@(when doc (list doc))))

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

(defmacro with-unique-names ((&rest names) &body body)
  "Bind each symbol in NAMES to a unique symbol and evaluate the BODY.
Useful for making your macro 'hygenic'."
  `(let ,(loop :for n :in names
	    :collect `(,n (gensym (symbol-name ',n))))
     ,@body))

(setf (macro-function 'with-names)
      (macro-function 'with-unique-names)
      (documentation 'with-names 'function)
      (documentation 'with-unique-names 'function))

(defmacro without-warning (&body body)
  "Get rid of stupid warnings that you don't want to see.
 Just wrap your code with this. Too bad it won't always work."
  `(handler-bind
    ((warning #'(lambda (c)
		  (declare (ignore c))
		  #-ccl (muffle-warning)
		  )))
     ,@body))

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

(declaim (inline clamp))
(defun clamp (n start end)
  "If N isn't in the range START - END, return the one it's closest to.
Otherwise, return N."
  (cond
    ((< n start) start)
    ((> n end) end)
    (t n)))

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

;; So we can just say mop: on any implementation?
(#+sbcl sb-ext:without-package-locks
 #+clisp ext:without-package-lock #+clisp ()
 #-(or sbcl clisp) progn
 #-(or cmu excl) (defalias :mop (find-package *mop-package*)))

;; The size of this should really be taken from the system's page size or
;; some other known thing which is optimal for the system.
(defvar *buffer-size* (* 8 1014)
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

(defmacro _ (&rest exprs)
  "Shorthand for single argument lambda. The single argument is named '_'."
  `(lambda (_)
     (declare (ignorable _))
     ,@exprs))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun initial-span (sequence not-in)
    "Return the initial portion of SEQUENCE consiting of objects not in
the sequence NOT-IN."
    (subseq sequence 0 (position-if #'(lambda (c) (find c not-in)) sequence))))

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
                      (loopy position nil)))))))))

(defun begins-with (prefix thing &key (test #'eql))
  "True if THAT begins with THIS."
  (let ((pos (search prefix thing :test test)))
    (and pos (zerop pos))))

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

;; Since defalias for the :mop package won't work until after dlib1,
;; I put these in here rather than be potentially slow with symbol-call.
;; In general, anything that uses the MOP might have to be in here.

(defun find-slot-name (class symbol)
  "Return the symbol which is the name of the slot in CLASS whose symbol-name
matches SYMBOL."
  (slot-definition-name
   (find symbol (class-slots (find-class class))
	 :key (_ (slot-definition-name _))
	 :test (lambda (a b)
		 (search (symbol-name a) (symbol-name b) :test #'equalp)))))

#+excl (mop:finalize-inheritance (find-class 'simple-condition))

(defparameter +simple-condition-format-control-slot+
  (find-slot-name 'simple-condition
		  #-lispworks 'format-control
		  #+lispworks 'format-string
		  )
  "Name of the slot that simple-condition-format-control accesses.")

(defparameter +simple-condition-format-arguments-slot+
  (find-slot-name 'simple-condition 'format-arguments)
  "Name of the slot that simple-condition-format-arguments accesses.")

(deftype function-designator ()
  "Something that denotes a function."
  `(or function symbol null))

;; This is even a fake dbugf!! 
(defmacro dbugf (facility fmt &rest args)
  "Eliminate the stupid debugging."
  (declare (ignore facility fmt args))
  '(values))

(de

;; EOF
