;;
;; dlib-interactive.lisp - Functions for interactive use.
;;

(defpackage :dlib-interactive
  (:documentation
   "Functions for interactive use. These are things that would typically
be used at a REPL, but not as likely to be called by other programs.")
  (:use :cl :mop :dlib :dlib-misc :dtime :table :table-print :terminal :grout
	:rl)
  (:nicknames :dlib-i)
  (:export
   #:show-expansion
   #:printenv
   #:char-apropos #:character-apropos
   #:dir
   #:pick-spin
   #:do-at
   #:show-features
   #:describe-environment
   #:describe-implementation
   #:describe-packages
   #:describe-package
   #:describe-printing
   #:describe-readtable
   #:describe-reader
   #:describe-system
   #:describe-class
   #:describe-float

   #:safe-set-bracketed-paste
   #:bracketed-paste-on
   #:bracketed-paste-off
   #:setup-bracketed-paste

   #:trace-package
   #:trace-out
   #:dbug-out

   #:file-matches
   #:not-file-matches			; this is inconsistent
   #:file-filter
   #:file-filter-not			; with this, but ...
   ))					; file-does-not-match is also crap
(in-package :dlib-interactive)		; and not-file-filter seems wrong too

(declaim (optimize (debug 2)))

(defun show-expansion (form &optional full)
  "Show a pretty printed macro expansion of the form. If full is true,
expand all macros recursively."
  (let ((*print-case* :downcase))
    (format t "~%~:w~%~%"
	    (if full (macroexpand form) (macroexpand-1 form)))))

(defun printenv (&optional original-order)
  "Like the unix command."
  (let ((mv (reduce #'max (nos:environment)
		    :key #'(lambda (x) (length (symbol-name (car x))))))
	(sorted-list (if original-order
			 (nos:environment)
			 (sort-muffled
			  (nos:environment) #'string-lessp
			  :key #'(lambda (x) (symbol-name (car x)))))))
    (loop :for v :in sorted-list
       :do (format t "~v@a ~30a~%" mv (car v) (cdr v)))))

;; Perhaps it would be more efficient if we could use the implementation's own
;; list, instead of having to go thru non-existent code points here, but it's
;; not like the speed is a big problem now, and this seems portable.
(defun char-apropos (name &key (regexp-p t))
  "List characters with names matching NAME."
  (let ((match-name
	 (if (symbolp name)
	     (symbol-name name)
	     name))
	(scanner (when regexp-p
		   (ppcre:create-scanner name :case-insensitive-mode t))))
    (loop :for c :from 0 :below char-code-limit
       :do (let* ((code  (code-char c))
		  (name  (when code (char-name code)))
		  (match (when name
			   (if regexp-p
			       (ppcre:scan scanner name)
			       (search match-name name :test #'equalp)))))
	     (when match
	       (format t "#x~8,'0x ~c ~a~%" c code name))))))

;; alias for
(setf (symbol-function 'character-apropos) #'char-apropos)

#+clisp (ext:without-package-lock ("EXT")
	  (unintern (find-symbol "DIR" :ext) :ext)
	  (unintern (find-symbol "DIR" :cl-user) :cl-user)
	  ) ; mine is better :-P

(defun dir (&optional (pattern "*.*"))
  "Simple portable CL only directory listing."
;;;  (declare (optimize (debug 3) (safety 0) (speed 0)))
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (setf pattern (pathname pattern))
  (when (not (pathname-name pattern))
    (setf pattern (merge-pathnames pattern (pathname "*.*"))))
  (let ((table
	 (loop
	    :for p :in #-clisp (directory pattern)
	    	       #+clisp (append (directory "*/") (directory "*"))
	    :when (probe-file p)
	    :collect
	    (if (or (pathname-name p) (pathname-type p))
		;; maybe a normal file
		(list
		 (format nil "~@[~a~]~@[.~a~]" (pathname-name p)
			 (pathname-type p))
		 (file-author p)
		 ;; (print-size
		 ;;  (or (ignore-errors
		 ;; 	(with-open-file (s p) (file-length s)))
		 ;;      0)
		 ;;  :stream nil)
		 (or (ignore-errors
		       (with-open-file (s p) (file-length s)))
		     0)
		 ;; (date-string :time (file-write-date p))
		 (file-write-date p)
		 )
		;; possibly a directory
		(list
		 (format nil "~a/"
			 #-ccl (car (last (pathname-directory p)))
			 #+ccl (car (last (pathname-directory p)))
			 )
		 (file-author p)
		 0
		 ;; (date-string :time (file-write-date p))
		 (file-write-date p)
		 )))))
    (setf table (sort table #'string< :key #'first)
	  table (make-table-from table :columns
				 `((:name "Name")
				   (:name "Author")
				   (:name "Size" :align :right :type number
				    :format
				    ,(lambda (n width)
				       (format nil "~v@a" width
					       (print-size n :stream nil))))
				   (:name "Date" :type number
				    :format
				    ,(lambda (n width)
				       (format nil "~va" width
					       (date-string :time n)))))))
    (print-table table)
    table))

(defun pick-spin ()
  (let (result)
    (with-terminal ()
      (tt-clear)
      (loop :with quit-flag :and k
	 :for i = 0 :then (1+ i)
	 :do
	 (tt-home)
	 (loop :for (name string) :in *spin-strings*
	    :for n = 0 :then (1+ n)
	    :do
	    (tt-format "~2d - ~c  ~(~s~)~%"
		       n (char string (mod i (length string))) name))
	 (tt-format "Pick a number (or 'q' to quit) ->  ")
	 (tt-finish-output)
	 (when (tt-listen-for .1)
	   (setf k (tt-get-key))
	   (cond
	     ((digit-char-p k)
	      (setf *default-spin-string* (second
					   (elt *spin-strings*
						(digit-char-p k)))
		    quit-flag t
		    result (digit-char-p k)))
	     ((or (eql k #\escape) (eql k #\q) (eql k #\Q))
	      (setf quit-flag t))))
	 (when (= i most-positive-fixnum)
	   (setf i 0))
	 :while (not quit-flag))
      (when result
	(format t "~s~%" (first (elt *spin-strings* result)))))))

(defmacro do-at ((time &key (interval 0.2)) form)
  "Call evaluate FORM with args at TIME. TIME is a universal time or a string,
parsed by simple-parse-time. INTERVAL is how many seconds between checks, which
defaults to 0.2."
  (with-names (u-time)
    `(progn
       (let (,u-time)
	 (etypecase ,time
	   (string
	    (setf ,u-time (simple-parse-time ,time)))
	   (integer
	    (setf ,u-time ,time)))
	 (loop :until (>= (get-universal-time) ,u-time)
	    :do (sleep ,interval))
	 ,form))))

(defun show-features ()
  "Print the features list nicely."
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (print-columns (sort (copy-seq *features*) #'string<) :format-char #\s))

(defun describe-environment (&optional (stream *standard-output*))
  "Print the Lisp environmental data."
  (print-values  '(lisp-implementation-type
		   lisp-implementation-version
		   short-site-name
		   long-site-name
		   machine-instance
		   machine-type
		   machine-version
		   software-type
		   software-version
		   ;; @@@ other things
		   user-homedir-pathname
		   internal-time-units-per-second
		   get-internal-run-time)
		 stream)
  (write-string (with-output-to-string (*standard-output*) (room)) stream)
  (values))

(defun describe-implementation (&optional (stream *standard-output*))
  "Print the Lisp implementation data."
  (print-values  '(lisp-implementation-type
		   lisp-implementation-version
		   call-arguments-limit
		   lambda-list-keywords
		   lambda-parameters-limit
		   multiple-values-limit
		   most-positive-fixnum
		   most-negative-fixnum
		   most-positive-short-float
		   least-positive-short-float
		   least-positive-normalized-short-float
		   most-positive-double-float
		   least-positive-double-float
		   least-positive-normalized-double-float
		   most-positive-long-float
		   least-positive-long-float
		   least-positive-normalized-long-float
		   most-positive-single-float
		   least-positive-single-float
		   least-positive-normalized-single-float
		   most-negative-short-float
		   least-negative-short-float
		   least-negative-normalized-short-float
		   most-negative-single-float
		   least-negative-single-float
		   least-negative-normalized-single-float
		   most-negative-double-float
		   least-negative-double-float
		   least-negative-normalized-double-float
		   most-negative-long-float
		   least-negative-long-float
		   least-negative-normalized-long-float
		   short-float-epsilon
		   short-float-negative-epsilon
		   single-float-epsilon
		   single-float-negative-epsilon
		   double-float-epsilon
		   double-float-negative-epsilon
		   long-float-epsilon
		   long-float-negative-epsilon
		   char-code-limit
		   array-dimension-limit
		   array-rank-limit
		   array-total-size-limit
		   *break-on-signals*
		   *gensym-counter*
		   *macroexpand-hook*
		   *debugger-hook*
		   *compile-print*
		   *compile-verbose*
		   *load-print*
		   *load-verbose*
		   *modules*
		   )
		 stream)
  ;; Possible other things:
  ;;  - It might be interesting to make a list of things which
  ;;    special-operator-p returns true for.
  ;;  - How about (type-of (expt 2 32)) vs say (type-of (expt 2 30))
  ;;    and similarly around 64, to determine how many GC bits in an integer.
  ;;  - Test EQ and EQL
  ;;  - (stream-external-format *standard-input* -output* *terminal-io* etc
  ;;  - number formats as in:
  ;;       (let ((*read-default-float-format* 'double-float))
  ;;         (read-from-string "(1.0 1.0e0 1.0s0 1.0f0 1.0d0 1.0L0)"))
  (values))

(defun package-symbol-count (pack &key (external nil))
  (declare (type package pack))
  (let ((sc 0))
    (if external
	(do-external-symbols (s pack)
	  #-(or ccl) (declare (ignore s)) (incf sc))
	(do-symbols          (s pack)
	  #-(or ccl) (declare (ignore s)) (incf sc)))
    sc))

(defun describe-packages (&key include-systems)
  "List packages in a hopefully consise format. If INCLUDE-SYSTEMS is true,
it will also list packages it thinks are ASDF system packages."
  (let* ((paks (copy-seq (list-all-packages)))
	 (spaks
	  (locally
	      #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	      (sort paks #'(lambda (p1 p2)
			     (string< (package-name p1)
				      (package-name p2))))))
	 nicks nice-used-by
	 (table
	  (make-table-from
	   (loop :for p :in spaks
	      :do
	      (setf nicks (package-nicknames p)
		    nice-used-by
		    (mapcar
		     #'(lambda (p)		 ; shortest package id
			 (reduce #'(lambda (a b) ; shortest sequence
				     (if (< (length a) (length b)) a b))
				 `(,(package-name p)
				    ,@(package-nicknames p))))
		     (package-use-list p)))
	      :when (or include-systems
			(not (and (equal nice-used-by '("CL" "ASDF"))
				  (ends-with "-SYSTEM" (package-name p)))))
	      :collect
	      (list
	       (format nil "~a ~:[~;~:*~a~]" (package-name p) nicks)
	       (package-symbol-count p :external t)
	       (let ((*print-pretty* nil)
		     (*print-escape* nil))
		 (format nil "~(~w~)" nice-used-by))
	       ))
	   :columns
	   '((:name "Package Name" :type string :format "~(~va~)")
	     (:name "Count" :type number)
	     ;; (:name "Dependencies" :type 'string :format "~(~va~)")
	     (:name "Dependencies")
	     ))))
    (with-grout ()
      (grout-print-table table))
    table))

(defun describe-package (p &key symbols)
  "Describe a package. SYMBOLS is :ext (or just non-nil) to show external
symbols, :all to show internal symbols too."
  (setf p (find-package p))
  (format t "Package ~a~%" (package-name p))
  (let* ((doc     (documentation p t))
	 (nicks   (package-nicknames p))
	 (uses    (mapcar #'package-name (package-use-list p)))
	 (used-by (mapcar #'package-name (package-used-by-list p)))
	 (shadow  (package-shadowing-symbols p))
	 (ext-count (package-symbol-count p :external t)))
    (if nicks
	(format t "~17a ~{~a ~}~%"
		(format nil "Nickname~p:" (length nicks)) nicks))
    (format t "~17a ~d~%" "External Symbols:" ext-count)
    (format t "~17a ~d~%" "Internal Symbols:"
	    (package-symbol-count p))
    (if shadow (format t "~17a ~a~%" "Shadowing Symbols:" shadow))
    (if uses (format t "~17a ~{~a ~}~%" "Uses:" uses))
    (if used-by (format t "~17a ~{~a ~}~%" "Used by:" used-by))
    (if doc (format t "Doc:~%~a~%" doc))
    (when (and symbols (> ext-count 0))
      (progn
	(format t "External Symbols:~%")
	(let* ((syms (sort-muffled
		      (loop :for v :being :the external-symbols :in p
			 :collect v)
		      #'string-lessp))
	       (max (if symbols
			(apply #'max
			       (mapcar
				#'(lambda (s) (length (string s)))
				syms)))))
	  (when syms
	    (loop :for s :in syms
		  :do
		  (format t "~(~va ~a~)~%" max s
			  (cond
			    ((fboundp s) (fdefinition s))
			    ((boundp s) (type-of (symbol-value s)))
			    (t "<unbound>"))))))))
    (when (eql symbols :all)
      (progn
	(format t "Internal Symbols:~%")
	(let* ((syms (sort-muffled
		      (loop :for v :being :the :present-symbols :in p
			 :collect v) #'string-lessp))
	       (max (if syms
			(apply #'max
			       (mapcar
				#'(lambda (s) (length (string s)))
				syms)))))
	  (when syms
	    (loop :for s :in syms
		  :do
		  (format t "~(~va ~a~)~%" max s
			  (cond
			    ((fboundp s) (fdefinition s))
			    ((boundp s) (type-of (symbol-value s)))
			    (t "<unbound>"))))))))))

(defun describe-printing ()
  "Describe the current Lisp printing parameters."
  (print-values '(*print-array*
		  *print-base*
		  *print-radix*
		  *print-case*
		  *print-circle*
		  *print-escape*
		  *print-gensym*
		  *print-level*
		  *print-length*
		  *print-lines*
		  *print-pretty*
		  *print-readably*
		  *print-right-margin*
		  *print-miser-width*
		  ;;*print-pprint-dispatch*
		  )))

(defun describe-readtable (&optional (readtable *readtable*))
  "Describe a readtable. If not given an argument, describe the current one."
  (format t "Readtable case: ~a~%" (readtable-case readtable))
  (table-print:nice-print-table
   (loop :with func :and non-term :and cc :and sub-chars
      :for c :from 0 :below 128
      :do
      (setf cc (code-char c))
      (multiple-value-setq (func non-term)
	(get-macro-character cc readtable))
      (setf sub-chars
	    (when func
	      (loop :with sub-func
		 :for sub-c :from 0 :below 128
		 :do (setf sub-func
			   (ignore-errors
			     (get-dispatch-macro-character cc (code-char sub-c)
							   readtable)))
		 :when sub-func
		 :collect (list "" ""
				(format nil "~11s ~a"
					(code-char sub-c) sub-func)
				""))))
      :if func
      :collect (list c (format nil "~s" cc)
		     ;; (string-downcase (char-name cc))
		     ;; (let ((s (format nil "~a" func))) 
		     ;;   (subseq s 0 (1- (min 60 (length s)))))
		     func
		     non-term)
      :if sub-chars
      :append sub-chars)
   (mapcar #'string '(code char function non-t))
   :max-width (1- (tt-width)))
  (values))

(defun describe-reader (&key (full t))
  "Describe the current Lisp reader parameters."
  (let ((vals '(*read-base*
		*read-default-float-format*
		*read-eval*
		*read-suppress*)))
    (when full
      (setf vals (append vals (list '*readtable*))))
    (print-values vals)
    (when full
      (describe-readtable)))
  (values))

(defun describe-system (system)
  "Print various properties of an ASDF system."
  (let ((sys (asdf:find-system system))
	(symbol-list
	 ;; There there; they're there.
	 (loop :with s
	    :for name :in '("SYSTEM-DESCRIPTION"
			    "SYSTEM-LONG-DESCRIPTION"
			    "SYSTEM-LONG-NAME"
			    "COMPONENT-VERSION"
			    "SYSTEM-LICENSE"
			    "SYSTEM-SOURCE-DIRECTORY"
			    "SYSTEM-AUTHOR"
			    "SYSTEM-MAINTAINER"
			    "SYSTEM-MAILTO"
			    "SYSTEM-HOMEPAGE"
			    "SYSTEM-SOURCE-CONTROL"
			    "SYSTEM-DEPENDS-ON")
	    :if (setf s (find-symbol name :asdf))
	    :collect s)))
    (when sys
      (print-values-of symbol-list sys :prefix 'system- :error-p nil))))

;; (defmacro stfu (&body body)
;;   "I mean it."
;;   (let ((nilly (gensym)))
;;     ;; @@@ /dev/null is not portable. maybe add something like this to opsys??
;;     `(with-open-file (,nilly "/dev/null" :direction :io :if-exists :append)
;;        (let ((*standard-output*	,nilly)
;; 	     (*error-output*	,nilly)
;; 	     (*trace-output*	,nilly)
;; 	     (*load-print*	nil)
;; 	     (*load-verbose*	nil)
;; 	     )
;; 	 ,@body))))

(defun describe-class (class &optional (stream *standard-output*))
  "Describe a class or structure. Send output to STREAM which defaults, to
*STANDARD-OUTPUT*. Requires a working MOP."
  #+has-mop
  (with-grout (*grout* stream)
    (let ((symb nil) class-doc type)
      (ctypecase class
	((or string keyword)
	 (setf symb (make-symbol (string-upcase class)))
	 (setf class (find-class symb)))
	(class (setf symb (class-name class)))
	(symbol (setf symb class
		      class (find-class class))))
      (cond
	((eq (class-of class) (find-class 'structure-class))
	 (setf class-doc (documentation symb 'structure)
	       type :struct))
	((typep class (type-of (find-class 'condition)))
	 (setf class-doc
	       (documentation symb 'type)
	       type :condition))
	((typep (class-of class) 'class)
	 (setf class-doc
	       (documentation symb 'type)
	       type :class)))
      (grout-format "~a : ~a~%" symb class-doc)
      (when (not (class-finalized-p class))
	;; For the most part this shouldn't be a problem, but it could be weird
	;; if we call describe-class from inside a compilation which is building
	;; that class. I wonder how we could detect if that could be so.
	(finalize-inheritance class))
      (grout-print-table
       (case type
	 (:struct
	   (make-table-from
	    (loop :for s :in (class-slots class)
	       :collect (list (slot-definition-name s)
			      (slot-definition-type s)
			      (slot-definition-initform s)))
	    :columns '((:anem "Name") (:name "Type")
		       (:name "Default" :align :left))))
	 (:condition
	   (make-table-from
	    (loop :for s :in (class-slots class)
	       :collect (list (slot-definition-name s)
			      (slot-definition-type s)))
	    :column-names '("Name" "Type")))
	 (:class
	   (make-table-from
	    (loop :for s :in (class-slots class)
	       :collect (list (slot-definition-name s)
			      (slot-definition-type s)
			      (aref (string (slot-definition-allocation s)) 0)
			      (documentation s t)))
	    :columns '((:name "Name") (:name "Type")
		       (:name "A") (:name "Description" :align :left)))))))
    (values))
    #-has-mop    
    (format stream "No MOP, so I don't know how to describe the class ~s~%"
	    class))

(defun describe-float (n)
  (check-type n float)
  (multiple-value-bind (significand exponent sign) (decode-float n)
    (multiple-value-bind (i-significand i-exponent i-sign)
	(integer-decode-float n)
      (let ((radix (float-radix n))
	    (digits (float-digits n))
	    (precision (float-precision n)))
      (print-values* (significand exponent sign
		      i-significand i-exponent i-sign
		      radix digits precision))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bracketed paste

(defun safe-set-bracketed-paste (state)
  "Set bracketed paste to STATE, if we're using an ANSI terminal or a wrapper
that wraps an ANSI terminal."
  ;; (dbugf :bracketed-paste "bracketed paste ~s~%" state)
  (when (find-package :terminal-ansi)
    (let ((ta (symbolify "terminal-ansi" :package :terminal-ansi
			 :no-new t)))
      (if (typep *terminal* ta)
	  (progn
	    (symbol-call :terminal-ansi :set-bracketed-paste-mode
			 *terminal* state)
	    ;;(terminal-finish-output *terminal*)
	    )
	  (when (and (typep *terminal* 'terminal-wrapper)
		     (typep (terminal-wrapped-terminal *terminal*) ta))
	    (symbol-call :terminal-ansi :set-bracketed-paste-mode
			 (terminal-wrapped-terminal *terminal*)
			 state)
	    ;; (terminal-finish-output (terminal-wrapped-terminal *terminal*))
	    )))))

;; (defun bracketed-paste-on (c ct)
;;   (declare (ignore c ct))
;;   (safe-set-bracketed-paste t))

;; (defun bracketed-paste-off (c ct)
;;   (declare (ignore c ct))
;;   (safe-set-bracketed-paste nil))

(defun bracketed-paste-on  () (safe-set-bracketed-paste t))
(defun bracketed-paste-off () (safe-set-bracketed-paste nil))

(defun setup-bracketed-paste ()
  (add-hook rl:*entry-hook* 'bracketed-paste-on)
  (add-hook rl:*exit-hook* 'bracketed-paste-off)
  (safe-set-bracketed-paste t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tracing

;; This is totally unnecessary since you can just say (untrace). Duh.
#|
(defun untrace-all ()
  "Untrace everything that's currently being traced."
  (let ((things (trace)))
    (cond
      (things
       (loop :for tt :in things
	  :do (eval `(untrace ,tt)))
       (format t "Untraced ~d: ~s.~%" (length things) things))
      (t
       (format t "Nothing to untrace."))))
  (values))
|#

(defun trace-package (package &key internal-p)
  "Trace everything in a package. If INTERNAL-P is true, trace internal
functions too."
  (let ((count 0))
    (macrolet
	((do-it (x)
	   `(,(if x 'do-symbols 'do-external-symbols) (s (find-package package))
	      (when (and (fboundp s)
			 (let ((type (second
				      (multiple-value-list
				       (find-symbol (symbol-name s) package)))))
			   (or (eq type :external)
			       (and ,x (eq type :internal)))))
		;; grrr
		(with-simple-restart (skip-it "Skip it then, okay?")
		  (eval `(trace ,s))
		  (incf count))))))
      (if internal-p
	  (do-it t)
	  (do-it nil)))
    (format t "~d symbols traced.~%" count)
    count))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-redirector (name stream-variable)
    "Define a stream redirector function and variable."
    (let ((save-name (symbolify (s+ "*SAVED-" stream-variable))))
      `(progn
	 (defvar ,save-name nil)

	 (defun ,name (&optional file)
	   ,(s+ "Toggle sending " stream-variable
		" to a FILE. If FILE is omitted, restore the previous value, "
		"and close the file.")
	   (cond
	     (,save-name
	      (format t ,(s+ stream-variable " redirection off.~%"))
	      (let (old)
		(shiftf old ,stream-variable ,save-name nil)
		(close old)))
	     ((and (not ,save-name) file)
	      (format t ,(s+ stream-variable " redirection to ~s.~%") file)
	      (shiftf ,save-name ,stream-variable
		      (open file :direction :output :if-exists :append
			    :if-does-not-exist :create)))
	     (t
	      (format t ,(s+ stream-variable
			     " output isn't already redirected. Give me a file ~
                            name to redirect to.~%"))))
	   (values))))))

(define-redirector trace-out *trace-output*)
(define-redirector dbug-out *dbug-output*)

#|
(defvar *saved-trace-output* nil)

(defun trace-out (&optional file)
  "Toggle tracing to a FILE. If FILE is omitted, restore the previous
*TRACE-OUTPUT*, and close the file."
  (cond
    (*saved-trace-output*
     (format t "Trace redirection off.~%")
     (let (old)
       (shiftf old *trace-output* *saved-trace-output* nil)
       (close old)))
    ((and (not *saved-trace-output*) file)
     (format t "Trace redirection to ~s.~%" file)
     (shiftf *saved-trace-output* *trace-output*
	     (open file :direction :output :if-exists :append)))
    (t
     (format t "Trace output isn't already redirected. Give me a file name ~
                to redirect to.~%")))
  (values))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file-filter

;; @@@ Is this really the best place for this?

#+unix
(progn
  (defun is-device (info)
    "Return true if INFO is a device."
    (or (uos:is-block-device (uos:file-status-mode info))
	(uos:is-character-device (uos:file-status-mode info))))

  (defun is-owner (info)
    "Return is owned by the current user."
    (= (uos:file-status-uid info) (uos:getuid)))

  (defun is-group (info)
    "Return is owned by the current group."
    (= (uos:file-status-gid info) (uos:getgid)))

  (defun our-is-executable (info)
    (uos:is-executable info))

  (defparameter *file-filters*
    #((#\/ :directory        uos:is-directory          "Directories")
      ;; (#\F :full             is-not-"Full (not empty) directories")
      (#\. :regular          uos:is-regular-file       "Regular files")
      (#\@ :symlink          uos:is-symbolic-link      "Symlinks")
      (#\= :socket           uos:is-socket             "Sockets")
      (#\s :socket           uos:is-socket             "Sockets")
      (#\p :named-pipe       uos:is-fifo               "Named pipes")
      (#\% :device           is-device		       "Device files")
      (#\* :executable       our-is-executable         "Executable files")
      (#\U :user             is-owner                  "Owned by current UID")
      (#\G :group            is-group                  "Owned by current GID")
      ;; (#\u :user             "Owned by the given user account or UID")
      ;; (#\g :group            "Owned by the given group account or GID")
      (#\r :owner-readable   uos:is-user-readable      "Readable by owner")
      (#\A :group-readable   uos:is-group-readable     "readable by group")
      (#\R :world-readable   uos:is-other-readable     "Readable by World")
      (#\w :owner-writable   uos:is-user-writable      "Writable by owner")
      (#\I :group-writable   uos:is-group-writable     "writable by group")
      (#\W :world-writable   uos:is-other-writable     "Writable by World")
      (#\x :owner-executable uos:is-user-executable    "Executable by owner")
      (#\E :group-executable uos:is-group-executable   "executable by group")
      (#\X :world-executable uos:is-other-executable   "Executable by world")
      (#\s :owner-setuid     uos:is-set-uid            "setuid (for user)")
      (#\S :group-setuid     uos:is-set-gid            "setgid (for group)")
      (#\t :sticky           uos:is-sticky             "Sticky bit"))
    "Filters for files."))

#-unix
(progn
  (defun is-directory (info file)
    (declare (ignore file))
    (eq (nos:file-info-type info) :directory))

  (defun is-symbolic-link (info file)
    (declare (ignore file))
    (eq (nos:file-info-type info) :link))

  (defun is-device (info file)
    (declare (ignore file))
    (eq (nos:file-info-type info) :device))

  (defun is-regular-file (info file)
    (declare (ignore file))
    (eq (nos:file-info-type info) :regular))

  ;; (defun is-device (info file)
  ;;   (declare (ignore file))
  ;;   (eq (file-info-type info) :other))

  (defun is-executable (info file)
    (declare (ignore info))
    (nos:is-executable file))

  (defparameter *file-filters*
    #((#\/ :directory        is-directory     "directories")
      (#\. :regular          is-regular-file  "regular files")
      (#\@ :symlink          is-symbolic-link "symlinks")
      (#\% :device           is-device        "device files")
      (#\* :executable       is-executable    "executable files")
      (#\U :user             is-owner         "owned by current UID")
      (#\G :group            is-group         "owned by current GID")
      (#\r :owner-readable   :missing         "readable by owner")
      (#\A :group-readable   :missing         "readable by group")
      (#\R :world-readable   :missing         "readable by World")
      (#\w :owner-writable   :missing         "writable by owner")
      (#\I :group-writable   :missing         "writable by group")
      (#\W :world-writable   :missing         "writable by World")
      (#\x :owner-executable :missing         "executable by owner")
      (#\E :group-executable :missing         "executable by group")
      (#\X :world-executable :missing         "executable by world")
      (#\s :owner-setuid     :missing         "setuid (for user)")
      (#\S :group-setuid     :missing         "setgid (for group)")
      (#\t :sticky           :missing         "the sticky bit"))
    "Filters for files."))

(defparameter *file-filter-table*
  (let ((tab (make-hash-table)))
    (loop :for f :across *file-filters* :do
       (setf (gethash (first f) tab) (third f))
       (setf (gethash (second f) tab) (third f)))
    tab))

(eval-when (:load-toplevel :execute)
  (defparameter *file-tag-doc*
    (with-output-to-string (stream)
      (loop :for f :across *file-filters*
	 :do
	 (format stream "  |~a  ~(~20s~) Matches ~a~%"
		 (first f) (second f) (fourth f))))))

(defun %file-info-matches-tag (tag info file #| &optional param |#)
  #+unix (declare (ignore file))
  (let ((func (gethash tag *file-filter-table*)))
    (cond
      ((not func)
       (error "Unknown file filter tag ~s" tag))
      ((eq func :missing)
       (warn "file filter tag ~s is not defined for this system."
	     tag)
       nil)
      #+unix ((eq (symbol-package func) (find-package :uos))
	      (funcall func (uos:file-status-mode info)))
      (t
       #+unix (funcall func info)
       #-unix (funcall func info file)))))

(defun %file-info (file info)
  (cond
    ;; If we already have the info, just return it.
    ((and info
	  #+unix (uos::file-status-p info)
	  #-unix (nos::file-info-p info))
     info)
    ;; Otherwise, gather it.
    (t
     #+unix (uos:lstat file)
     #-unix (nos:get-file-info file))))

;; Since these things are for interactive use, it seems fairly useful to have
;; the unabridged tag doc in each one.

(defun file-matches (tag-sequence file &optional info)
  (let ((info (%file-info file info)))
    (every (_ (%file-info-matches-tag _ info file)) tag-sequence)))

(setf (documentation #'file-matches 'function)
      (format nil
	      "Return true if the FILE matches every tag in TAG-SEQUENCE.~%
Tags are:~%~a" *file-tag-doc*))

(defun not-file-matches (tag-sequence file &optional info)
  (let ((info (%file-info file info)))
    (some (_ (not (%file-info-matches-tag _ info file))) tag-sequence)))

(setf (documentation #'not-file-matches 'function)
      (format nil
      "Return true if the FILE does not matches any tag in TAG-SEQUENCE.~%
Tags are:~%~a" *file-tag-doc*))

(defun file-filter (tag-sequence file-list)
  (declare (optimize (debug 3)))
  (remove-if (_ (not-file-matches tag-sequence _)) file-list))

(setf (documentation #'file-filter 'function)
      (format nil
  "Given a list of file names in FILE-LIST, return a sequence with only the
files that match the tags in TAG-SEQUENCE. ~%
Tags are:~%~a" *file-tag-doc*))

(defun file-filter-not (tag-sequence file-list)
  (declare (optimize (debug 3)))
  (remove-if (_ (file-matches tag-sequence _)) file-list))

(setf (documentation #'file-filter-not 'function)
      (format nil
  "Given a list of file names in FILE-LIST, return a sequence with only the
files that do NOT match the tags in TAG-SEQUENCE.~%
Tags are:~%~a" *file-tag-doc*))

;; EOF
