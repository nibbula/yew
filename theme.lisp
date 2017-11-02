;;
;; theme.lisp - Customize the style.
;;

(defpackage :theme
  (:documentation "Customize the style.")
  (:use :cl :dlib)
  (:export
   #:theme
   #:theme-activate
   #:theme-deactivate
   #:theme-value
   #:make-theme
   ))
(in-package :theme)

;; This is really just a fairly lame word based hierarchical data store.

(deftype collection ()
  `(or sequence hash-table))

(defclass theme-node ()
  ((name
    :initarg :name :accessor theme-name :type symbol
    :documentation "Name of the theme node.")
   (description
    :initarg :description :accessor theme-description
    :type (or string null) :initform nil
    :documentation "Tell me about it.")
   (children
    :initarg :children :accessor theme-children :initform nil
    :type (or collection null)
    :documentation "A sequence of sub-nodes, or NIL."))
  (:documentation "A node in the theme tree."))

(defclass theme-value-node (theme-node)
  ((value
    :initarg :value :accessor theme-node-value
    :documentation "The value of this node.")
   ;; (default-value
   ;;  :initarg :default-value :accessor theme-node-default-value  
   ;;  :documentation "The default value for this node.")
   )
  (:documentation "A theme value."))

(defclass theme (theme-node)
  ((title
    :initarg :title :accessor theme-title :type string
    :documentation "The title of the theme."))
  (:documentation "Set of customizations."))

(defmethod initialize-instance
    :after ((o theme) &rest initargs &key &allow-other-keys)
  "Initialize a theme."
  (declare (ignore initargs))
  (when (not (slot-boundp o 'name))
    (error "The theme should have a name."))
  ;; Default the title from the name.
  (when (not (slot-boundp o 'title))
    (setf (theme-title o) (format nil "~:(~a~)" (theme-name o)))))

(defgeneric theme-activate (theme &key &allow-other-keys)
  (:documentation "Make the theme active."))

(defgeneric theme-deactivate (theme &key &allow-other-keys)
  (:documentation "Make the theme inactive."))

(defgeneric theme-value (theme item)
  (:documentation "Get the value of a theme item."))

(defgeneric (setf theme-value) (value theme item)
  (:documentation "Set the value of a theme item."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((object theme-node) stream)
  "Print a theme-node to STREAM."
  (with-slots (name description children) object
    (print-unreadable-object (object stream :type t)
      (format stream "~a ~d" name (length children)))))

(defmethod print-object ((object theme-value-node) stream)
  "Print a theme-value-node to STREAM."
  (with-slots (name description children value #| default-value |#) object
    (print-unreadable-object (object stream :type t)
      ;; (format stream "~a ~d ~s [~s]" name (length children)
      ;; 	      (and (slot-boundp object 'value) value)
      ;; 	      (and (slot-boundp object 'default-value) default-value)))))
      (format stream "~a ~d ~s" name (length children)
      	      (and (slot-boundp object 'value) value)))))

(defgeneric get-item-in-node (node name)
  (:documentation "Return an item named NAME from NODE."))

(defgeneric put-item-in-node (node name type)
  (:documentation "Put a item named NAME in NODE."))

(defgeneric get-node (theme item)
  (:documentation "Get the node for ITEM from THEME."))

(defgeneric make-node (theme item)
  (:documentation "Make ITEM exist in THEME if it doesn't, and return it."))

;---------------------------------------------------------------------------

(defmethod get-item-in-node ((node theme-node) name)
  "Return an item named NAME from NODE."
  (find name (theme-children node) :key #'theme-name))

(defmethod put-item-in-node ((node theme-node) name type)
  "Put a item named NAME in NODE."
  (let ((new (make-instance type :name name)))
    (push new (theme-children node))
    new))

(defmethod get-node ((theme theme) item)
  "Get a node in THEME described by the node path ITEM."
  (let ((node theme))
    (loop :for n :in item
       :do (setf node (find n (theme-children node) :key #'theme-name))
       :while node)
    node))

(defun set-node (theme item value)
  "Set the node in THEME described by ITEM to VALUE. ITEM is a theme node path,
which is a list of symbols which are the names of nodes in a path in the tree.
If the path doesn't exist, it is created."
  (let ((cur (car item))
	existing)
    (if cur
	(progn
	  ;;(format t "~a " cur)
	  (setf existing (find cur (theme-children theme) :key #'theme-name))
	  (if existing
	      (progn
		;;(write-string ". ")
		(set-node existing (rest item) value))
	      (progn
		(if (= 1 (length item))
		    (progn
		      ;;(format t " := ~s~%" value)
		      (push (make-instance 'theme-value-node
					   :name cur
					   :value value)
			    (theme-children theme)))
		    (let ((new-node
			   (make-instance 'theme-node :name cur)))
		      ;;(write-string "+ ")
		      (push new-node (theme-children theme))
		      (set-node new-node (rest item) value))))))
	(progn
	  ;;(format t " = ~s~%" value)
	  (if (typep theme 'theme-value-node)
	      (setf (theme-value theme) value)
	      (error
	       "Attempt to set the value of an existing non-value node."))))))

(defmethod theme-value ((theme theme) item)
  "Return the value in THEME given by the node path ITEM,"
  (let ((node (get-node theme item)))
    (when node
      (cond
	((typep node 'theme-value-node)
	 (theme-node-value node))
	((and (eq (type-of node) 'theme-node)
	      (theme-children node))
	 ;; @@@ could return the whole sub-tree
	 )))))

(defmethod (setf theme-value) (value (theme theme) item)
  "Setter for theme node values."
  (set-node theme item value))

;; This is of dubious value.
(defun print-theme-node (theme-node &key prefix repeat-line stream)
  (let* ((name-string (format nil "~(~a~)" (theme-name theme-node)))
	 (new-prefix (+ (or prefix 0) (length name-string) 1)))
    (when (and repeat-line prefix (> prefix 0))
      (format t "~v,,,va" prefix #\space #\space))
    (princ name-string)
    (if (typep theme-node 'theme-value-node)
	(progn
	  (if (slot-boundp theme-node 'value)
	      (progn
		(format t ": ~w" (theme-node-value theme-node))
		;; (when (slot-boundp theme-node 'default-value)
		;; 	(format t " [~w]" (theme-node-default-value theme-node)))
		)
	      (princ ": unbound"))
	  (terpri))
	(when (theme-children theme-node)
	  (princ #\.)
	  (print-theme-node (first (theme-children theme-node))
			    :prefix new-prefix :stream stream)
	  (loop :for n :in (rest (theme-children theme-node)) :do
	     (print-theme-node n :prefix new-prefix :repeat-line t
			       :stream stream))))))

(defun theme-as-tree (theme)
  (let (;(result `(,(theme-name theme) ,@(or (theme-description theme)))))
	(result `(,(theme-name theme))))
    (if (theme-children theme)
	(append result (mapcar #'theme-as-tree (theme-children theme)))
	(if (typep theme 'theme-value-node)
	    (cons (theme-name theme) (theme-node-value theme))
	    result))))

(defconstant +theme-version+ 1)

(defgeneric serialize (theme version)
  (:documentation "Keep your eyes on the prize, or some other platitude."))

(defmethod serialize (theme (version (eql 1)))
  (typecase theme
    (theme
     `(:theme ,version
	      :name ,(theme-name theme)
	      :title ,(theme-title theme)
	      :description ,(theme-description theme)
	      ,(mapcar (_ (serialize _ version)) (theme-children theme))))
    (theme-value-node
     `(:value ,(theme-node-value theme)
	      ,(theme-name theme) ,(theme-description theme)))
    (theme-node
     `(,(theme-name theme) ,(theme-description theme)
	,(mapcar (_ (serialize _ version)) (theme-children theme))))))

(defgeneric unserialize (theme version)
  (:documentation "Keep your eyes on the prize, or some other platitude."))

(defmethod unserialize (obj (version (eql 1)))
  (let ((o (pop obj)))
    (cond
      ((eq o :theme)
       (let (name title description children)
	 (when (/= (pop obj) version)
	   (error "Incorrect version number."))
	 (when (< (length obj) 6)
	   (error "Theme list too short."))
	 (when (not (eql (pop obj) :name))
	   (error "Theme missing name tag."))
	 (setf name (pop obj))
	 (when (not (eql (pop obj) :title))
	   (error "Theme missing title tag."))
	 (setf title (pop obj))
	 (when (not (eql (pop obj) :description))
	   (error "Theme missing description tag."))
	 (setf description (pop obj))
	 (when (not (listp (car obj)))
	   (error "Theme missing children list."))
	 (setf children
	       (mapcar (_ (unserialize _ version)) (pop obj)))
	 ;;(format t "children = ~s~%" children)
	 (make-instance 'theme :name name :title title :description description
			:children children)))
      ((eq o :value) ; leaf
       (when (< (length obj) 3)
	 (error "Theme value node too short."))
       (let (value name description)
	 (setf value (pop obj)
	       name (pop obj)
	       description (pop obj))
	 (make-instance 'theme-value-node
			:name name :description description :value value)))
      ((symbolp o) ; branch
       (when (< (length obj) 2)
	 (error "Theme branch node too short."))
       (let (name description children)
	 ;;(format t "branch ~s~%" o)
	 (setf name o
	       description (pop obj)
	       children (mapcar (_ (unserialize _ version)) (pop obj)))
	 (make-instance 'theme-node
			:name name :description description
			:children children))))))

(defun print-theme (theme &key as-tree readably (stream *standard-output*))
  (cond
    (readably
     (print (serialize theme +theme-version+) stream))
    (as-tree
     (print (theme-as-tree theme) stream))
    (t
     (format stream "Theme:  ~s ~a~%" (theme-name theme) (theme-title theme))
     (loop :for n :in (theme-children theme) :do
	(print-theme-node n :stream stream))))
  (values))

(defun make-theme (name &rest keys &key title description)
  "Make a new theme named NAME. If TITLE isn't given it's set from name."
  (declare (ignorable title description))
  (apply #'make-instance 'theme :name name keys))

(defun write-theme (theme stream)
  (write (serialize theme) :stream stream))

(defun read-theme (stream)
  (let ((s (read stream)))
    (when (not (consp s))
      (error "Theme is not a CONS."))
    (when (not (eql (car s) :theme))
      (error "Theme doesn't have a :theme tag."))
    (when (let ((n (second s)))
	    (or (not (numberp n)) (> n +theme-version+)))
      (error "Theme version is not recognized."))
    (unserialize s)))

(defun save-theme (theme file)
  "Save THEME to FILE."
  (with-open-file (out file :direction :output :if-exists :supersede)
    (write (serialize theme +theme-version+) :stream out)))

(defun load-theme (file)
  "Load a theme from FILE and return it."
  (with-open-file (in file :direction :input)
    (unserialize (safe-read in) +theme-version+)))

(defun test-theme ()
  (let ((tt (make-theme 'cool
			:title "( ͡° ͜ʖ ͡°)"
			:description "Yubba dooba foo.")))
    (setf (theme:theme-value tt '(file dir fg)) :blue)
    (setf (theme:theme-value tt '(file dir bg)) :black)
    (setf (theme:theme-value tt '(file regular fg)) :white)
    (setf (theme:theme-value tt '(file regular bg)) :black)
    tt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun view-theme (theme)
  "Look at a vague representation of THEME in the tree viewer."
  (tree-viewer:view-tree (theme-as-tree theme)))

;; EOF
