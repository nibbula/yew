;;
;; view-lisp.lisp - View things in the Lisp system.
;;

(defpackage :view-lisp
  (:documentation "View things in the Lisp system.")
  (:use :cl :dlib :tree-viewer :dlib-interactive)
  (:export
   #:view-lisp
   ))
(in-package :view-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package browsing

(defun package-mostly-use-list (pkg)
  "All packages except the superfluous :COMMON-LISP package."
  (loop :with name
     :for p :in (package-use-list pkg)
     :do (setf name (package-name p))
     :if (not (equal name "COMMON-LISP"))
     :collect name))

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

(defun package-used-by-name-list (package)
  "Like PACKAGE-USED-BY-LIST but returns package names instead of objects."
  (mapcar (_ (package-name _))
	  (package-used-by-list package)))

(defun package-contents (package-in)
  (when (or (and (typep package-in 'object-node)
		 (not (find-package (node-object package-in))))
	    (and (stringp package-in)
		 (not (find-package package-in))))
    (return-from package-contents nil))
  (flet ((nn (o &optional b)
	   (make-object-node :object o :branches b :open nil)))
    (let* ((package (or (and (stringp package-in) package-in)
			(and (typep package-in 'object-node)
			     (node-object package-in))))
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
		   :collect (make-cached-dynamic-node
			     :object (package-name p)
			     :func #'package-mostly-use-list
			     :open nil)))
	    contents)
      (push (nn "Used By"
		(loop :for p :in (package-used-by-list package)
		   :collect (make-cached-dynamic-node
			     :object (package-name p)
			     :func #'package-used-by-name-list
			     :open nil)))
	    contents)
      (push (nn (format nil "External Symbols (~d)" (length external))
		(loop :for e :in external
		   :collect (nn (if (fboundp e) (s+ e " (F)") e))))
	    contents)
      (push (nn (format nil "Internal Symbols (~d)" (length internal))
		(loop :for e :in internal
		   :collect (nn (if (fboundp e) (s+ e " (F)") e))))
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
      (make-cached-dynamic-node
       :object (package-name p)
       :func #'package-contents
       :open nil))))

(defun view-packages ()
  (view-tree (package-contents-tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic text nodes

(defun trim-trailing-newline (str)
  (when (eql (char str (1- (length str))) #\newline)
    (setf (char str (1- (length str))) #\space)))

(defclass text-func-node (object-node)
  ((text-func
    :initarg :text-func :accessor text-func :initform nil 
    :documentation "A function that prints some text to a stream.")))

(defmethod display-node ((node text-func-node) level)
  "Display an Lisp image environment node."
  (let ((str (with-output-to-string (*standard-output*)
	       (funcall (text-func node)))))
    (trim-trailing-newline str)
    (display-object node str level)))

(defun make-text-func-node (name func)
  "Make an openable node named NAME that has a text func node inside."
  (make-object-node
   :object name :open nil
   :branches (list (make-instance 'text-func-node
				  :object name
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
  (asdf:system-depends-on (asdf:find-system name)))

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
		     :branches
		     (loop :for dep :in (asdf:system-depends-on system)
			:collect (make-cached-dynamic-node
				  :object dep
				  :func #'system-use-list
				  :open nil))
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

;; List of systems nodes
  
(defclass systems-node (cached-dynamic-node) ())
(defun systems-contents (node)
  (declare (ignore node))
  (loop :for s :in (asdf:registered-systems)
     :collect
     (make-instance
      'system-node
      :object (asdf:find-system s)
      :open nil)))

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
      'object-node
      :object
      (let ((str (with-output-to-string (*standard-output*)
		   (describe-class class))))
	(when (eql (char str (1- (length str))) #\newline)
	  (setf (char str (1- (length str))) #\space))
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
  (loop :for sc :in (subclasses (find-class 'standard-object))
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
		   (symbol-call :lish :print-command-help cmd
				str))))
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
    (loop :for p :in (list-all-packages)
       :collect
       (make-cached-dynamic-node
	:object (package-name p)
	:func #'package-contents
	:open nil)))
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

;; This is really just for the autoload.
#+lish
(lish:defcommand view-lisp ()
  "View things in the Lisp system."
  (view-lisp))

;; EOF
