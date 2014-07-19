;;
;; dlib.lisp - Dan's redundant miscellany.
;;

;; $Revision: 1.34 $

;; These are mostly solving problems that have already been solved.
;; But it's mostly stuff that I need just to start up.
;; Try to keep this minimal. Don't add any dependencies.
;; More optional stuff can go in dlib-misc.

#+debug-rc (progn (format t "[ dlib ") (force-output *standard-output*))

(defpackage :dlib
  (:use :common-lisp #+mop :mop #+ccl :ccl)
  (:documentation
   "Dan's generally useful miscellaneous functions.
 I usually have this loaded automatically at startup.")
  (:export
   ;; System-ish
   #:d-getenv
   #:shell-line
   #:shell-lines
   #:system-command-stream
   #:system-args
   #:exit-system
   #:overwhelming-permission
   #:exe-in-path-p
   #:try-things
   ;; sequences
   #:initial-span
   #:split-sequence
   #:replace-subseq
   #:begins-with
   #:ends-with
   #:s+
   ;; lists
   #:delete-nth
   #:alist-to-hash-table
   #:with-package
   #:d-add-feature
   #:d-remove-feature
   ;; ccl added it's own version
   #-ccl-1.6 #:add-feature
   #-ccl-1.6 #:remove-feature
   #:has-feature
;   #:with-struct-slots
   ;; Implementation-ish
   #:without-warning
   #:define-constant
   #:*dbug* #:dbug #:with-dbug #:without-dbug
   ;; Environment features
   #:*host*
   #:*arch*
   #:*arch-nickname*
   #:*os*
   #:*lisp-implementation-nickname*
   #:*lisp-version*
   #:*platform-nickname*
  )
)
(in-package :dlib)

;(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))

;; @@@ I should really do this with an error type
(defun missing-implementation (sym)
  "Complain that something is missing."
  (format t "You need to provide an implementation for ~a on ~a~%"
	  sym (lisp-implementation-type)))

(defmacro without-warning (&body body)
  "Get rid of stupid warnings that you don't want to see.
 Just wrap your code with this."
  `(handler-bind
    ((warning #'(lambda (c)
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
  #-(or clisp sbcl openmcl cmu ecl excl lispworks)
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

(defun initial-span (s l)
"Return the initial portion of sequence S consiting of objects
 not in the list L"
  (subseq s 0 (position-if (lambda (c) (find c l)) s)))

;; Another square wheel.
;; @@@ I should probably either fix this to have the additional functionality
;; of the public version, or just copy the public version here.
;;
;; The public version:
;;
;; (defun split-sequence (delimiter seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (test nil test-supplied) (test-not nil test-not-supplied) (key nil key-supplied))
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
;; Arguments: (SEPARATOR-BAG SEQUENCE &KEY (START 0) END (TEST (QUOTE EQL)) KEY COALESCE-SEPARATORS)
;; Return a list of subsequences of SEQUENCE, split by the elements in the sequence SEPARATOR-BAG.
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

(defun split-sequence (sep seq &key
			   omit-empty coalesce-separators remove-empty-subseqs
			   (start 0) end test key
			   #+nil count)
  "Split the sequence SEQ into subsequences separated by SEP. Return a list of the subsequences. SEP can be a sequence itself, which means the whole sequence is the separator. If :omit-empty is true, then don't return empty subsequnces.
 :coalesce-separators :remove-empty-subseqs are compatability synonyms for :omit-empty."
  (declare (type sequence seq))
  (declare (type boolean omit-empty coalesce-separators remove-empty-subseqs))
  (declare (ignore end key))
  (setf omit-empty (or omit-empty coalesce-separators remove-empty-subseqs))
  (let* ((sep-is-seq (typecase sep (vector t) (list t) (t nil)))
	 (sep-len (if sep-is-seq (length sep) 1))
	 (seq-len (if omit-empty (length seq) 0)))
    (declare (type boolean sep-is-seq))
    (declare (type fixnum sep-len seq-len))
;     (format t "sep-is-seq = ~w sep-len = ~w seq-len = ~w ~%"
;  	    sep-is-seq sep-len seq-len count)
;    (setf test #'(lambda (x) 
    (macrolet
	((loopy (func t-omit-empty t-test)
	   `(loop
	     :with t-start fixnum = start :and t-end fixnum
	     :do (setq t-end
		 (or (,func sep seq
			    ,@(if (eq func 'position) '(:start) '(:start2))
			    t-start ,@(if t-test '(:test test)))
		  -1))
	     :while (>= t-end 0)
;	     (format t "start = ~d end = ~d ~s~%" t-start t-end
;  		 (subseq seq t-start t-end))
	     ,@(if t-omit-empty '(if (not (= t-start t-end))))
	     :collect (subseq seq t-start t-end) :into results
	     :do (setq t-start (+ t-end sep-len))
	     :finally (return-from nil
		       ,@(if t-omit-empty
			     '((if (= t-start seq-len)
				   results
				   (nconc results
					  (list (subseq seq t-start)))))
			     '((nconc results
				(list (subseq seq t-start)))))))))
      (when (or (> sep-len 0) test)
	(when (and test (< sep-len 1))
	  (setf sep-len 1 sep '(nil)))	; fake separator!
	(if sep-is-seq
	    (if omit-empty
		(if test
		    (loopy search t t)
		    (loopy search t nil))
		(if test
		    (loopy search nil t)
		    (loopy search nil nil)))
	    (if omit-empty
		(if test
		    (loopy position t t)
		    (loopy position t nil))
		(if test
		    (loopy position nil t)
		    (loopy position nil nil))))))))

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

(defun replace-subseq (from-seq to-seq in-seq &key count)
  "Return a copy of in-seq but with sequences of from-seq replaced with to-seq."
  (if (and (> (length from-seq) 0) (or (not count) (> count 0)))
      (let ((pos 0)
	    (i 0)
	    (n 0)
	    new)
	(loop while (setf pos (search from-seq in-seq :start2 i))
	  do
;	(format t "i = ~a pos = ~a new = ~a~%" i pos new)
	  (setf new (nconc new (list (subseq in-seq i pos) to-seq)))
	  (setf i (+ pos (length from-seq)))
;	(format t "i = ~a pos = ~a new = ~a~%" i pos new)
	  (incf n)
	  until (and count (>= n count))
	  )
	(setf new (nconc new (list (subseq in-seq i))))
	(apply #'concatenate (append '(string) new)))
      in-seq))

(defun begins-with (this that)
  "True if THAT begins with THIS."
  (let ((pos (search this that)))
    (and pos (= 0 pos))))

(defun ends-with (this that)
  "True if THAT ends with THIS."
  (let ((pos (search this that :from-end t)))
    (and pos (= pos (- (length that) (length this))))))

(defun s+ (s &rest rest)
  "Abbreviation for (concatenate 'string ...)"
  (apply #'concatenate 'string s rest))

;; As you may know, improper use of this can cause troublesome bugs.
(defun delete-nth (n list)
  "Delete the Nth elemnt from LIST."
  (if (zerop n)
    (cdr list)
    (let ((cons (nthcdr (1- n) list)))
      (if cons
        (setf (cdr cons) (cddr cons))
        cons))))

(defun alist-to-hash-table (alist table)
  "Convert an association list into a hash table."
  (loop :for i :in alist
	:do (setf (gethash (car i) table) (cdr i)))
  table)

;;; Feature Fiddling

;; As per CCL, it does make sense to aquire a mutex when adding features.
;; But how can we even do this portably at this point?

(defun d-add-feature (f)
  "Add a feature named by the given string to *FEATURES*"
  (assert f)
  (if (stringp f)
;      (nconc *features* (list (intern (string-upcase f) :keyword)))
;      (nconc *features* (list f))))
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
  #-(or clisp sbcl cmu openmcl ecl excl lispworks)
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
  #+ecl (loop :for i :from 0 :below (si:argc) :collecting (si:argv i))
  #+openmcl (ccl::command-line-arguments)
  #+excl (sys:command-line-arguments) 
  #-(or sbcl clisp cmu ecl openmcl excl)
  (missing-implementation 'lisp-args))

#+sbcl
(defparameter *sbcl-version*
  (destructuring-bind (major minor rev)
      (subseq (split-sequence #\. (lisp-implementation-version)) 0 3)
    (+ (* 10000 (parse-integer major))
       (* 100 (parse-integer minor)) 
       (parse-integer rev))))

#+sbcl (when (> *sbcl-version* 10055)
	 (add-feature :use-exit))

(defun exit-system ()
  "Halt the entire Lisp system."
  #+openmcl (ccl::quit 0)
  #+cmu (ext:quit)
  #+(and sbcl use-exit) (sb-ext:exit)
  #+(and sbcl (not use-exit)) (sb-ext:quit)
  #+excl (excl:exit)
  #+clisp (ext:quit)
  #+ecl (ext:quit)
  #-(or openmcl cmu sbcl excl clisp ecl) (missing-implementation 'exit-system))

;; a.k.a root
(defun overwhelming-permission ()
  #+ccl (= (ccl::getuid) 0)
  #+sbcl (= (sb-unix:unix-getuid) 0)
  ;; What about posix:getuid ? It seems to be missing on clisp 2.46.
  #+clisp (= (posix:user-info-uid (posix:user-info :default)) 0)
  #+cmu (= (unix:unix-getuid) 0)
  #+ecl (= (ext:getuid) 0)
;  #+lispworks ()
  #-(or ccl sbcl clisp cmu ecl)
  (missing-implementation 'overwhelming-permission))

#| THIS IS TOTALLY FUXORD
;; Most lisps' with-slots can handle structs too.
#+(or sbcl clisp ccl)
  (setf (macro-function 'with-struct-slots) (macro-function 'with-slots))
;; For other lisps we roll our own. Mostly stolen from slime.
#-(or sbcl clisp ccl)
;; @@@ NOT REALLY WORKING YET
(defmacro with-struct-slots ((&rest names) obj &body body)
  "Like with-slots but works only for structs and only if the struct type
name matches the accessor names, as is the default with defstruct."
  (let ((struct-name (gensym "ws-"))
	(reader-func (gensym "ws-")))
    `(flet ((,reader-func (slot struct-name)
	     (intern (concatenate 'string
				  (symbol-name struct-name) "-"
				  (symbol-name slot))
		     (symbol-package struct-name))))
      (let ((,struct-name (type-of ,obj)))
; 	  (format t "reader for ~a is ~a~%"
; 	   (quote ,(first names)) (quote ,(reader (first names))))
	(symbol-macrolet
	    ,(loop for name in names collect
	       (typecase name
		 (symbol `(,name
			   ((,reader-func ,name ,struct-name) ,obj)))
		 (cons   `(,(first name)
			   ((,reader-func ,(second name) ,struct-name) ,obj)))
		 (t (error
		     "A NAME is not symbol or a cons in WITH-STRUCT: ~A"
		     name))))
	    ,@body)))))
|#

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

(defmacro define-constant (name initial-value &optional documentation (test ''eql))
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

#+(or sbcl clisp ccl cmu lispworks)
(defmacro define-constant (name value &optional doc (test 'equal))
  "Like defconstant but works with pendanticly anal SCBL."
  (declare (ignore test))
  #+(or sbcl clisp cmu lispworks)
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc)))
  #+ccl
  `(cl:defconstant ,name ,value ,@(when doc (list doc)))
  )

;; On other lisps just use the real one.
#-(or sbcl clisp ccl cmu lispworks)
(setf (macro-function 'define-constant) (macro-function 'cl:defconstant))

(defmacro with-package (package &body body)
  "Evalute BODY with *package* set to the packaged designated by PACKAGE."
  `(let ((*package* (if (packagep ,package)
                        ,package
                        (find-package ,package))))
    ,@body))

;; Debugging messages
;;
;; This is so you can say: (dbug "message ~a~%" foo) in code, and then say
;; (with-dbug (func x)) or (without-dbug (func x)) to get debugging messages
;; or not.
;;
;; If you want keep them around but not always compile them, you could prefix
;; them with #+dbug or something.  Of course, you should avoid side effects
;; in your messages. Also the form can have syntactic effects.

(defvar *dbug* nil)
(defmacro dbug (fmt &rest args)
  `(when dlib:*dbug* (funcall #'format *debug-io* ,fmt ,@args) (finish-output)))

(defmacro with-dbug    (&body body) `(let ((*dbug* t))   ,@body))
(defmacro without-dbug (&body body) `(let ((*dbug* nil)) ,@body))

(defun exe-in-path-p (name)
  "True if file NAME exists in the current execute path, which is usually the environment variable PATH. Note that this doesn't check if it's executable or not or that you have permission to execute it."
  (loop :for d :in (split-sequence ":" (d-getenv "PATH"))
     :when (probe-file (s+ d "/" name))
     :do (return t)
     :finally (return nil)))

(defun try-things (things)
  "Return the first thing that works. THINGS is a list of lists. If a sublist starts with a string, it should be a command in the path which is given the rest of the arguments. If a sublist starts with a symbol, it is a function to call with the rest of the arguments. Works means it doesn't return NIL. If nothing works, return NIL."
  (loop :with result
     :for cmd :in things :do
     (setf result (etypecase (car cmd)
		    (string (when (dlib::exe-in-path-p (car cmd))
			      (apply #'shell-line cmd)))
		    (symbol (apply (car cmd) (cdr cmd)))))
     (when result (return result))))

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
	  (or (has-feature :X86-64) (has-feature :64-bit-target)))
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
  #+clisp	"clisp"
  #+sbcl	"sbcl"
  #+cmu		"cmu"
;  #+openmcl	"openmcl"
  #+ccl		"ccl"
  #+excl	"allegro"
  #+lispworks	"lw"
  #+ecl		"ecl"
  #-(or clisp sbcl cmu excl openmcl lispworks ecl) "unknown"
  "A short nickname for the current implementation."
)

(defparameter *lisp-version*
  #+clisp	(format nil "~a"
			(let ((v (lisp-implementation-version)))
			  (substitute #\- #\. (subseq v 0 (position #\space v)))))
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
  #-(or clisp sbcl cmu excl openmcl lispworks ecl) "unknown"
  "A version suitable for putting in a logical pathname."
)

(defparameter *platform-nickname* (format nil "~a~a-~a-~a"
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
