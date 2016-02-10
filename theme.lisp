;;
;; theme.lisp - Customize the style.
;;

(defpackage :theme
  (:documentation "Customize the style.")
  (:use :cl :dlib)
  (:export
   #:theme
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
    :initarg :description :accessor theme-description :type string
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
   (default-value
    :initarg :default-value :accessor theme-node-default-value  
    :documentation "The default value for this node."))
  (:documentation "A theme value."))

(defclass theme (theme-node)
  ((author
    :initarg :author :accessor theme-author  
    :documentation "Who made this theme?"))
  (:documentation "Set of customizations."))

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
  (with-slots (name description children value default-value) object
    (print-unreadable-object (object stream :type t)
      (format stream "~a ~d ~s [~s]" name (length children)
	      (and (slot-boundp object 'value) value)
	      (and (slot-boundp object 'default-value) default-value)))))

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
  "Get a node"
  (let ((node theme))
    (loop :for n :in item
       :do (setf node (find n (theme-children node)))
       :while node)
    node))

(defmethod make-node ((theme theme) item)
  (let ((node theme)
	(remainder item)
	(last-existing theme))
    ;; Find the last existing node, where we should start adding.
    (loop :for i :on item
       :do (setf node (get-item-in-node node (car i)))
       (format t "found ~a~%" node)
       (when node
	 (setf last-existing node
	       remainder i))
       :while node)
    (format t "node = ~w last-existing = ~w remainder = ~w~%"
	    node last-existing remainder)
    (when (not node)
      (format t "node not pre-existing~%")
      (let ((n last-existing)
	    (i 0)
	    (node-names remainder))
	(loop
	   :with end = (1- (length remainder))
	   :while (< i end)
	   :do
	   (setf n (put-item-in-node n (car node-names) 'theme-node))
	   (format t "made ~s~%" n)
	   (incf i)
	   (setf node-names (cdr node-names)))
	(setf node (put-item-in-node n (car node-names) 'theme-value-node))))
    node))

(defmethod theme-value ((theme theme) item)
  (let ((node (get-node theme item)))
    (when node
      (theme-node-value node))))

(defmethod (setf theme-value) (value (theme theme) item)
  (format t "hi~%")
  (let ((node (get-node theme item)))
    (when (not node)
      (setf node (make-node theme item)))
    (setf (theme-node-value node) value)))

(defun print-theme-node (theme-node &optional prefix)
  (when (and prefix (> prefix 0))
    (format t "~v,,,va" prefix #\space #\space))
  (format t "~a" (theme-name theme-node))
  (if (typep theme-node 'theme-value-node)
      (progn
	(if (slot-boundp theme-node 'value)
	    (progn
	      (format t ": ~w" (theme-node-value theme-node))
	      (when (theme-node-default-value theme-node)
		(format t " [~w]" (theme-node-default-value theme-node))))
	    (princ ": unbound"))
	(terpri))
      (when (theme-children theme-node)
	(princ #\.)
	(print-theme-node (first (theme-children theme-node)))
	(loop :for n :in (rest (theme-children theme-node)) :do
	   (print-theme-node
	    n (+ prefix (length (theme-name theme-node)) 1))))))

(defun print-theme (theme)
  (format t "Theme:  ~a~%Author: ~a~%"
	  (theme-name theme) (theme-author theme))
  (loop :for n :in (theme-children theme) :do
     (print-theme-node n)))

;; EOF
