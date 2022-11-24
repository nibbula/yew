;;;
;;; view-lisp.lisp - View things in the Lisp system.
;;;

(defpackage :view-lisp
  (:documentation "View things in the Lisp system.")
  (:use :cl :dlib :tree-viewer :dlib-interactive :syntax :syntax-lisp
	:terminal :collections :fatchar :fatchar-io :completion :ochar :ostring
	:view-generic :terminal-utils)
  (:export
   #:view-lisp
   #:!view-lisp
   #:view-packages
   ))
(in-package :view-lisp)

;; This is like the mother-of-all demos for the tree-viewer. But it actually
;; turns out be useful too.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package browsing

(defun package-mostly-use-list (pkg)
  "All packages except the superfluous :COMMON-LISP package."
  (remove (find-package :cl) (package-use-list pkg)))

(defun all-package-dependencies-tree ()
  "Return a tree viewer tree of all package dependencies."
  (make-object-node
   :object "All Packages"
   :open t
   :branches
   (loop :for p :in (list-all-packages)
      :collect (make-cached-dynamic-node
		:object (package-name p)
		:func #'package-mostly-use-list
		:open nil))))

(defun view-package-dependencies ()
  "View the entire package dependency hierarchy with the tree viewer."
  (view-tree (all-package-dependencies-tree)))

(defvar *cl-ext-sym* 
  (let ((lst ()))
    (do-external-symbols (s :cl lst) (push s lst)))
  "External symbols in CL package.")

;; @@@ This very foolishly duplicates code from doc.lisp
(defparameter *doc-types* '(compiler-macro function method-combination setf
			    structure type variable)
  "Types of documentation to try for the DOC function.")

(defun print-doc (symbol stream)
  (let ((did-one nil) (did-structure nil)
	(pkg (symbol-package symbol)))
    (labels ((maybe-doc (obj type)
	       (without-warning (documentation obj type)))
	     (output-text (s)
	       (format-comment-text
		(make-instance 'syntax-lisp:lisp-token :object s) stream))
	     (print-it (sym pkg doc-type &optional fake-type)
	       (when (maybe-doc sym doc-type)
		 (when did-one
		   (princ #\newline stream))
		 (if (and (eq doc-type 'function) (maybe-doc sym doc-type))
		     (progn
		       (when (and pkg (not (eq pkg (find-package :cl))))
			 (format stream "~a " (package-name pkg))
			 (format stream "~:(~a~):~%"
				 (or fake-type doc-type)))
		       (format stream "~a~%" (function-help sym nil)))
		     (progn
		       (format stream "~:(~a~): " (or fake-type doc-type))
		       (when pkg
			 (format stream "~a:" (package-name pkg)))
		       (format stream "~a~%" sym)))
		 (if (eq doc-type :command)
		     (format stream "~a" (maybe-doc sym doc-type))
		     (progn
		       (output-text (maybe-doc sym doc-type))))
		 (setf did-one t))))
      (loop :for d :in *doc-types*
	 :do
	 ;; Don't print duplicate type documentation for structures,
	 (when (and (eq d 'structure) (maybe-doc symbol d))
	   (setf did-structure t))
	 (when (not (and (eq d 'type) did-structure))
	   (print-it symbol pkg d))))
    ;;(princ #\newline stream)
    ))

(defstruct decorated-symbol
  symbol)

(defun symbol-node-contents (symbol)
  (list
   (make-text-func-node "Description"
			(lambda () (describe
				    (decorated-symbol-symbol symbol))))
   (make-text-func-node "Documentation"
			(lambda ()
			  (print-doc (decorated-symbol-symbol symbol)
				     *standard-output*)))
   ;; @@@ other things?
   ))

(defmethod print-object ((object decorated-symbol) stream)
  "Print a class to ‘stream’."
  (cond
    ((or *print-readably* *print-escape*)
     (call-next-method))
    (t
     (format stream
	     "~a" (stylize-token (make-instance
				  'lisp-symbol-token
				  :object (decorated-symbol-symbol object))))
     ;;(princ (decorated-symbol-symbol object) stream)
     (when (fboundp (decorated-symbol-symbol object))
       (princ " (F)" stream)))))

(defclass symbol-node (cached-dynamic-node)
  ()
  (:default-initargs
   :func #'symbol-node-contents)
  (:documentation "A node that describes a symbol."))

(defun display-fat-object (node object level)
  (let ((lines (split-sequence #\newline (fat-string-string (fs+ object))
			       :key #'fatchar-c)))
    (when (eq node (tb::current *viewer*))
      (tt-bold t))
    (display-node-line node (span-to-fat-string
			     (if (eq node (tb::current *viewer*))
				 `(,(display-prefix node level)
				    (:bold
				     ,(make-fat-string :string (first lines)))
				    #\newline)
				 `(,(display-prefix node level)
				    ,(make-fat-string :string (first lines))
				    #\newline))))
    (when (eq node (tb::current *viewer*))
      (tt-bold nil))
    (loop :for l :in (cdr lines) :do
       (display-node-line node (fs+ (display-indent node level)
				    (make-fat-string :string l)
				    #\newline)))))

(defmethod display-object ((node symbol-node) object level)
  (display-fat-object node object level))

(defclass package-node (cached-dynamic-node)
  ()
  (:documentation "A node that is a package."))

(defmethod display-node ((node package-node) level)
  "Make packages show up in lower case."
  (display-object node
		  (format nil "~(~a~)"
			  (let ((obj (node-object node)))
			    (typecase obj
			      (package (package-name obj))
			      (t obj))))
		  level))

(defun view-package (package)
  (view-tree (make-instance 'package-node
			    :object package
			    :func #'package-contents
			    :open t)))

(defmethod view-node ((node package-node))
  (view-package (node-object node)))

(defmethod view ((thing package))
  (view-package thing))

(defun package-contents (package-in)
  (when (or (and (typep package-in 'object-node)
		 (not (find-package (node-object package-in))))
	    (and (stringp package-in)
		 (not (find-package package-in))))
    (return-from package-contents nil))
  (flet ((nn (o &optional b)
	   (make-object-node :object o :branches b :open nil)))
    (let* ((package
	     (typecase package-in
	       ((or string package) package-in)
	       (object-node (node-object package-in))))
	   (pkg (find-package package))
	   (doc (documentation (find-package package) t))
	   (nicks (package-nicknames package))
	   (all (set-difference
		 (let ((lst ()))
		   (do-symbols (s package lst) (push s lst)))
		 *cl-ext-sym*))
	   (external (sort
		      (let ((lst ()))
			(do-external-symbols (s package lst) (push s lst)))
		      #'(lambda (a b) (string< (string a) (string b)))))
	   (internal (sort
		      (set-difference
		       (remove-if
			(_ (not (eq (symbol-package _) pkg))) all)
			external)
			   #'(lambda (a b) (string< (string a) (string b)))))
	   (shadow (package-shadowing-symbols package))
	   contents)
            (push (nn "Documentation" (list (nn doc))) contents)
      (when nicks
	(push (nn "Nicknames"
		  (loop :for n :in nicks :collect (nn n))) contents))
      (push (nn "Uses"
		(loop :for p :in (package-use-list package)
		      :collect
		      (make-instance 'package-node
				     :object p
				     :func #'package-mostly-use-list
				     :open nil)))
	    contents)
      (push (nn "Used By"
		(loop :for p :in (package-used-by-list package)
		   :collect
		   (make-instance 'package-node
				  :object p
				  :func #'package-used-by-list
				  :open nil)))
	    contents)
      (push (nn (format nil "External Symbols (~d)" (length external))
		(loop :for e :in external
		   :collect (make-instance
			     'symbol-node :object
			     (make-decorated-symbol :symbol e))))
	    contents)
      (push (nn (format nil "Internal Symbols (~d)" (length internal))
		(loop :for e :in internal
		   :collect (make-instance
			     'symbol-node
			     :object
			     (make-decorated-symbol :symbol e))))
	    contents)
      (when shadow
	(push (nn (format nil "Shadowing Symbols (~d)" (length shadow))
		  (loop :for e :in shadow :collect (nn e))) contents))
      (nreverse contents))))

(defun package-contents-tree ()
  "Return a tree viewer tree of all packages."
  (make-object-node
   :object "All Packages"
   :open t
   :branches
   (loop :for p :in (list-all-packages)
      :collect
      (make-instance 'package-node
		     :object p
		     :func #'package-contents
		     :open nil))))

(defun view-packages ()
  (view-tree (package-contents-tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic text nodes

(defun trim-trailing-newline (str)
  (when (and str (not (zerop (olength str)))
	     (ochar= (oelt str (1- (olength str))) #\newline))
    (setf (ochar str (1- (olength str))) #\space)))

(defclass text-func-node (object-node)
  ((text-func
    :initarg :text-func :accessor text-func :initform nil 
    :documentation "A function that prints some text to a stream.")))

(defmethod display-node ((node text-func-node) level)
  "Display an Lisp image environment node."
  (let ((str (with-output-to-fat-string (*standard-output*)
	       (funcall (text-func node)))))
    (when (not (zerop (olength str)))
      (trim-trailing-newline str)
      (display-fat-object node str level))))

(defun make-text-func-node (name func &optional object)
  "Make an openable node named NAME that has a text func node inside."
  (make-object-node
   :object name :open nil
   :branches (list (make-instance 'text-func-node
				  :object (or object name)
				  :text-func func
				  :open nil :branches nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ASDF system nodes

(defclass parent-component-node (cached-dynamic-node)
  ((name
    :initarg :name :accessor parent-component-node-name :initform nil
    :documentation "Displayed name."))
  (:default-initargs
   :func #'component-contents))

(defmethod display-node ((node parent-component-node) level)
  (display-object node
		  (or (parent-component-node-name node)
		      (asdf:component-name (node-object node)))
		  level))

(defclass child-component-node (object-node) ())
(defmethod display-node ((node child-component-node) level)
  (display-object node
		  (format nil "~a ~s"
			  (string-downcase
			   (symbol-name (type-of (node-object node))))
			  (asdf:component-name (node-object node)))
		  level))

(defun component-contents (component)
  (loop :for c :in (asdf:component-children component)
     :if (typep c 'asdf:parent-component)
     :collect (make-instance 'parent-component-node :object c :open nil)
     :else
     :collect (make-instance 'child-component-node :object c :open nil)))

(defun system-use-list (name)
  (typecase name
    (null nil)
    (list
     ;; Could be:?
     ;; | ( :feature FEATURE-EXPRESSION dependency-def )
     ;; | ( :version simple-component-name version-specifier )
     ;; | ( :require module-name )
     (let ((s (asdf/find-component::resolve-dependency-spec nil name)))
       (when s
	 (list
	  (make-instance 'system-use-node
			 :object
			 (or (ignore-errors (asdf:find-system s)) s))))))
    ((or string symbol asdf/system:system)
     (let ((sys (asdf:find-system name nil)))
       (if sys
	   (mapcar (_ (make-instance 'system-use-node
				     :object
				     (or (ignore-errors (asdf:find-system _))
					 _)))
		   (asdf:system-depends-on sys))
	   name)))))

(defun system-contents (system)
  (list
   (make-object-node :object "Description"
		     :branches
		     (list
		      (make-instance 'system-description-node :object system))
		     :open nil)
   (make-instance 'parent-component-node :name "Components"
		  :object system :open nil)
   (make-object-node :object "Depends on"
		     :branches (system-use-list system)
		     :open nil)))

(defclass system-node (cached-dynamic-node)
  ()
  (:default-initargs
   :func #'system-contents))

(defmethod display-node ((node system-node) level)
  (display-object node (asdf:component-name (node-object node)) level))

(defclass system-description-node (object-node) ())

(defmethod print-object ((object system-description-node) stream)
  "Print a system-description-node to STREAM."
  (if (or *print-readably* *print-escape*)
      (print-unreadable-object (object stream)
	(format stream "system-node ~w" (node-object object)))
      (let ((*standard-output* stream))
	(describe-system (asdf:find-system (node-object object))))))

(defmethod display-node ((node system-description-node) level)
  (let* (;;(sys (asdf:find-system (node-object node)))
	 ;; (str (with-output-to-string (*standard-output*)
	 ;; 	(describe-system sys))))
	 (str (princ-to-string node)))
    (when (eql (char str (1- (length str))) #\newline)
      (setf (char str (1- (length str))) #\space))
    (display-object node str level)))

(defclass system-use-node (system-node)
  ()
  (:default-initargs
   :func #'system-use-list))

(defmethod display-node ((node system-use-node) level)
  (display-object node
		  (typecase (node-object node)
		    (asdf/system:system
		     (asdf:component-name (node-object node)))
		    (t
		     (node-object node)))
		  level))

;; List of systems nodes
  
(defclass systems-node (cached-dynamic-node) ())
(defun systems-contents (node)
  (declare (ignore node))
  (loop :for s :in (sort (asdf:registered-systems) #'string<)
     :collect
     (make-instance
      'system-node
      :object (asdf::registered-system s)
      :open nil)))

(defun view-system (system)
  (view-tree (make-instance 'system-node
			    :object (asdf:find-system system)
			    :open t)))

;; (defmethod view ((thing asdf:cl-source-file))
(defmethod view ((thing asdf:component))
  (view (asdf:component-pathname thing)))

(defmethod view ((thing asdf/system:system))
  (view-system thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class nodes

;; Simple class viewer:
;; (tb:view-tree
;;  (tb:make-tree (find-class 'standard-object)
;; 	       (_ (sb-mop:class-direct-subclasses _)))

(defun get-class (c)
  (typecase c
    (class
     c)
    (symbol
     (find-class c))
    (otherwise
     nil)))

(defmacro mop-call (func &rest args)
  `(funcall (intern (string-upcase ,func) *mop-package*) ,@args))

(defun subclasses (class)
  (mop-call "class-direct-subclasses" (get-class class)))

(defun node-subclasses (obj)
  (loop :for c :in (subclasses (node-object obj))
     :collect
     (make-instance 'class-node :object c)))

(defclass class-slots-node (object-node)
  ()
  (:documentation "Node for slot descriptions."))

(defmethod display-object ((node class-slots-node) object level)
  (display-fat-object node object level))

(defun class-node-contents (class)
  (when (not (mop-call "class-finalized-p" class))
    (mop-call "finalize-inheritance" class))
  (list
   (make-instance
    'object-node
    :object "Slots"
    :branches
    (list
     (make-instance
      ;; 'object-node
      'class-slots-node
      :object
      (let ((str (with-terminal-output-to-fat-string (:trim t)
		   (describe-class class *terminal*))))
	str)
      :open nil)))
   (make-instance
    'object-node
    :object "Subclasses"
    :branches
    (loop :for c :in (subclasses class)
       :collect
       (make-instance 'class-node :object c)))))

(defclass class-node (cached-dynamic-node)
  ()
  (:default-initargs
   :func #'class-node-contents)
  (:documentation "A node that shows a class."))

(defmethod display-node ((node class-node) level)
  ;; (format t "node-object = ~w of type ~a~%" (node-object node)
  ;; 	  (type-of (node-object node)))
  ;; (format t "find-class = ~w~%" (get-class (node-object node)))
  (let* ((klass (get-class (node-object node)))
	 ;; (str (with-output-to-string (*standard-output*)
	 ;; 	(describe-class klass)))
	 )
    ;; (when (eql (char str (1- (length str))) #\newline)
    ;;   (setf (char str (1- (length str))) #\space))
    (display-object node
		    (if (symbol-package (class-name klass))
			(format nil "~(~a:~a~)"
				(package-name
				 (symbol-package (class-name klass)))
				(class-name klass))
			(format nil "~(#:~a~)" (class-name klass)))
		    level)))
    ;; (display-object node str level)))

(defclass classes-node (cached-dynamic-node) ())
(defun classes-contents (node)
  (declare (ignore node))
  (loop :for sc :in (sort (copy-list (subclasses (find-class 'standard-object)))
			  #'string< :key (_ (string-downcase
					     (prin1-to-string (class-name _)))))
     :collect (make-instance 'class-node :object sc :open nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions (which just use class nodes)

(defclass conditions-node (cached-dynamic-node) ())
(defun conditions-contents (node)
  (declare (ignore node))
  (loop :for sc :in (subclasses (find-class 'condition))
     :collect (make-instance 'class-node :object sc :open nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands

;; (defun command-node-contents (cmd)
;;   (list
;;    (make-instance
;;     'object-node
;;     :object "Documentation"
;;     :
;;    (make-instance
;;     'object-node
;;     :object "Arguments"
;;     :branches
;;     s (symbol-call :lish :command-name (node-object node))
;; 	 (split-sequence
;; 	  #\newline
;; 	  (with-output-to-string (str)
;; 	    (symbol-call :lish :print-command-help (node-object node) str))
;; 	  :omit-empty t))

(defun command-node-contents (cmd)
  (list
   (make-instance
    'object-node
    :object "Documentation"
    :branches
    (list
     (make-instance
      'object-node
      :object
      (let ((str (with-output-to-string (str)
		   (symbol-call :lish :print-command-help cmd :stream str))))
	(trim-trailing-newline str)
	str)
      :open nil)))))

(defclass command-node (cached-dynamic-node)
  ()
  (:default-initargs
   :func #'command-node-contents)
  (:documentation "A node that show a command."))

(defmethod display-node ((node command-node) level)
  (display-object node
     (symbol-call :lish :command-name (node-object node)) level))

(defclass commands-node (cached-dynamic-node) ())
(defun commands-contents (node)
  (declare (ignore node))
  (when (find-package :lish)
    (loop :for p :in (symbol-call :lish :command-list t)
       :collect (make-instance 'command-node :object p :open nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Your whole Lisp image browsing.

(defun lisp-image-contents (node)
  (declare (ignore node))
  (list
   (make-text-func-node "Environment" #'describe-environment)
   (make-text-func-node "Implementation" #'describe-implementation)
   (make-text-func-node "Printing" #'describe-printing)
   (make-text-func-node "Reader" #'describe-reader)
   (make-instance
    'systems-node
    :object "Systems"
    :func #'systems-contents
    :open nil)
   (make-object-node
    :object "Packages"
    :open nil
    :branches
    (loop :for p :in (sort (mapcar #'package-name (list-all-packages)) #'string<)
       :collect
       (make-instance 'package-node
		      :object (or (ignore-errors (find-package p)) p)
		      :func #'package-contents
		      :open nil)
	  ))
   (make-instance
    'cached-dynamic-node
    :object "Classes"
    :func #'classes-contents
    :open nil)
   (make-instance
    'cached-dynamic-node
    :object "Conditions"
    :func #'conditions-contents
    :open nil)
   (make-instance
    'commands-node
    :object "Commands"
    :func #'commands-contents
    :open nil)))

(defclass lisp-node (cached-dynamic-node)
  ()
  (:default-initargs
   :object "Lisp Image"
   :func #'lisp-image-contents
   :open t))

(defun view-lisp ()
  (view-tree (make-instance 'lisp-node)))

#+lish
(lish:defcommand view-lisp ()
  "View things in the Lisp system."
  (view-lisp))

;; EOF
