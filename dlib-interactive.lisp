;;
;; dlib-interactive.lisp - Dan's functions for interactive use.
;;

(defpackage :dlib-interactive
  (:documentation
   "Dan's functions for interactive use. These are things that would typically
be used at a REPL, but not as likely to be called by other programs.")
  (:use :cl :dlib :dlib-misc :table-print :mop :terminal)
  (:export
   #:show-expansion
   #:printenv
   #:char-apropos #:character-apropos
   #:dir
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
   ))
(in-package :dlib-interactive)

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
			 (sort (nos:environment) #'string-lessp
			       :key #'(lambda (x) (symbol-name (car x)))))))
    (loop :for v :in sorted-list
       :do (format t "~v@a ~30a~%" mv (car v) (cdr v)))))

;; Perhaps it would be more efficient if we could use the implementation's own
;; list, instead of having to go thru non-existent code points here, but it's
;; not like the speed is a big problem now, and this seems portable.
(defun char-apropos (name)
  "List characters with names matching NAME."
  (let ((match-name
	 (if (symbolp name)
	     (symbol-name name)
	     name)))
    (loop :for c :from 0 :below char-code-limit
       :do (let* ((code  (code-char c))
		  (name  (when code (char-name code)))
		  (match (when name (search match-name name :test #'equalp))))
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
		 (print-size
		  (or (ignore-errors
			(with-open-file (s p) (file-length s)))
		      0)
		  :stream nil)
		 (date-string :time (file-write-date p)))
		;; possibly a directory
		(list
		 (format nil "~a/"
			 #-ccl (car (last (pathname-directory p)))
			 #+ccl (car (last (pathname-directory p)))
			 )
		 (file-author p)
		 "-"
		 (date-string :time (file-write-date p)))))))
    (setf table (sort table #'string< :key #'first))
    (nice-print-table table '("Name" "Author" ("Size" :right) "Date"))))

(defmacro do-at (time form)
  "Call func with args at time. Time is a universal time or a string."
  (let ((tm (gensym)))
  `(progn
    (let (,tm)
      (etypecase ,time
	(string
	 (setf ,tm (simple-parse-time ,time)))
	(integer
	 (setf ,tm ,time)))
      (loop :until (>= (get-universal-time) ,tm)
	:do (sleep .2))
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
  (format t "~30a ~5a ~a~%" "Package Name" "Count" "Package Deps")
  (format t "~30,,,'-a ~5,,,'-a ~43,,,'-a~%" "-" "-" "-")
  (let* ((paks (copy-seq (list-all-packages)))
	 (spaks
	  (locally
	      #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	      (sort paks #'(lambda (p1 p2)
			     (string< (package-name p1)
				      (package-name p2))))))
	 name nicks nice-used-by)
    (loop :for p :in spaks
       :do
       (setf nicks (package-nicknames p)
	     nice-used-by (mapcar
			   #'(lambda (p)      ; shortest package id
			       (reduce #'(lambda (a b) ; shortest sequence
					   (if (< (length a) (length b)) a b))
				       `(,(package-name p)
					  ,@(package-nicknames p))))
			   (package-use-list p)))
       (when (or include-systems
		 (not (and (equal nice-used-by '("CL" "ASDF"))
			   (ends-with "-SYSTEM" (package-name p)))))
	 (setf name (format nil "~a ~:[~;~:*~a~]" (package-name p) nicks))
	 (format t "~(~30a ~5d ~s~)~%" name
		 (package-symbol-count p :external t)
		 nice-used-by)))))

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
	(let* ((syms (sort (loop :for v :being :the external-symbols :in p
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
	(let* ((syms (sort (loop :for v :being :the :present-symbols :in p
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
    (print-values-of symbol-list sys :prefix 'system-)))

;; This only works with a MOP
(defun describe-class (class &optional (stream *standard-output*))
  #+has-mop
  (progn
    (let ((symb nil))
      (ctypecase class
	((or string keyword)
	 (setf symb (make-symbol (string-upcase class)))
	 (setf class (find-class symb)))
	(class (setf symb (class-name class)))
	(symbol (setf symb class
		      class (find-class class))))
      (format stream "~a : ~a~%" symb
	      (documentation symb 'type))
      ;; (let ((max-width (loop :for s :in (class-slots class)
      ;; 			  :maximize
      ;; 			  (length (string (slot-definition-name s))))))
      (when (not (class-finalized-p class))
	(finalize-inheritance class))
      (nice-print-table
       (loop :for s :in (class-slots class)
	  :collect (list (slot-definition-name s)
			 (slot-definition-type s)
			 (aref (string (slot-definition-allocation s)) 0)
			 (documentation s t)))
       '("Name" "Type" "A" ("Description" :left))))
    (values))
    #-has-mop    
    (format stream "No MOP, so I don't know how to describe the class ~s~%"
	    class))

;; EOF
