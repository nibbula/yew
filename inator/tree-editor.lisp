;;;
;;; tree-editor.lisp - Edit trees.
;;;

(defpackage :tree-editor
  (:documentation "Edit trees.")
  (:use :cl :dlib :terminal :keymap :char-util :ostring :inator :tree-viewer
	:rl-widget)
  (:export
   #:tree-editor
   #:edit-tree
   ))
(in-package :tree-editor)

(defkeymap *tree-edit-keymap* ()
  `((#\"		. insert-string)
    (#\'		. insert-symbol)
    (#\newline		. insert-thing)
    (#\return		. insert-thing-after)
    (#\a		. add-thing-node)
    (#\s		. add-thing-node)
    (,(ctrl #\d)	. delete-node)
    (#\e		. edit-node)
    ))

(defclass tree-editor (tree-viewer)
  ((widget
    :initarg :widget :accessor widget :initform nil
    :documentation "The input widget.")
   (node-type
    :initarg :node-type :accessor node-type
    :initform 'object-node :type (or symbol null)
    :documentation
    "The default type of nodes created, which should probably be a subclass of
object-node."))
  (:default-initargs
   :keymap (list *tree-edit-keymap*
		 tree-viewer::*tree-keymap*
		 inator:*default-inator-keymap*))
  (:documentation "Editor for trees."))

(defun ensure-widget (e)
  (with-slots (widget) e
    (when (not widget)
      (setf widget
	    (make-instance 'widget
			   :bbox (make-instance 'rl-widget::bbox
						:x 0 :y 0
						:width 33 :height 1)
			   :completion-func #'completion:complete-symbol)))))

(defun set-box (e &key (x 0) (y 0) (width 33) (height 1))
  (setf (widget-bbox (widget e))
	(make-instance 'rl-widget::bbox
		       :x x :y y :width width :height height)))

(defun value-string (value)
  (when value
    (if (not (ostringp value))
	(princ-to-string value)
	value)))

(defgeneric input (e type &key value)
  (:documentation "Allow TYPE to be input."))

(defmethod input ((e tree-editor) (type (eql 'string)) &key value)
  (with-slots (widget) e
    (ensure-widget e)
    (let ((x (tb::current-left e))
	  (y (tb::current-position e)))
      (set-box e :x x :y y
	       :width (- (tt-width) x 1) :height 1)
      (rl:rl :editor widget :string (value-string value)))))

(defmethod input ((e tree-editor) (type (eql 'symbol)) &key value)
  (with-slots (widget) e
    (ensure-widget e)
    (let ((x (tb::current-left e))
	  (y (tb::current-position e)))
      (set-box e :x x :y y
	       :width (- (tt-width) x 1) :height 1)
      (symbolify (rl:rl :editor widget :string (value-string value))))))

(defmethod input ((e tree-editor) type &key value)
  (with-slots (widget) e
    (ensure-widget e)
    (let ((x (tb::current-left e))
	  (y (tb::current-position e)))
      (set-box e :x x :y y
	       :width (- (tt-width) x 1) :height 1)
      (let ((result (rl:rl :editor widget :string (value-string value))))
	(if (not (stringp value))
	    (handler-case
		(read-from-string result)
	      (error (c)
		(fui:show-text
		 (format nil "Your thing fucked up.~%~s~s" result c))
		(return-from input result)))
	    result)))))

(defun insert-before (e thing)
  "Insert THING before the current in editor E."
  (with-slots ((current tb::current)
	       (root tb::root)) e
    (let ((parent (get-parent current)))
      (when (not parent)
	(assert (eq current root))
	(setf parent root))
      (loop :for prev = (node-branches parent)
	 :for n :on (node-branches parent) :do
	 (when (eq (car n) current)
	   (rplacd n (cons (car n) (cdr n)))
	   (rplaca n thing)
	   (return))
	 (setf prev n)))))

(defgeneric insert-object (e type)
  (:documentation "Insert an object of TYPE after the current."))

(defmethod insert-object ((e tree-editor) type)
  (let ((node (make-instance (node-type e) :object :_PLACE_HOLDER_)))
    (insert-before e node)
    (tb::previous-node e)
    (update-display e)
    (tt-finish-output)
    (setf (node-object node) (input e type))))

(defun insert-string (e)
  "Insert a string at the current node."
  (insert-object e 'string))

(defun insert-symbol (e)
  "Insert a symbol at the current node."
  (insert-object e 'symbol))

(defun insert-thing (e)
  "Insert an object at the current node."
  (insert-object e t))

(defun insert-after (e thing)
  "Insert THING after the current in editor E."
  (with-slots ((current tb::current)
	       (root tb::root)) e
    (let ((parent (get-parent current)))
      (when (not parent)
	(assert (eq current root))
	(setf parent root))
      (loop :for n :on (node-branches parent)
	 :when (eq (car n) current)
	 :do (rplacd n (push thing (cdr n)))
	 (return)))))

(defgeneric insert-object-after (e type)
  (:documentation "Insert an object of TYPE after the current."))

(defmethod insert-object-after ((e tree-editor) type)
  (let ((node (make-instance (node-type e) :object :_place_holder_)))
    (insert-after e node)
    (tb::next-node e)
    (update-display e)
    (tt-finish-output)
    (setf (node-object node) (input e type))))

(defun insert-thing-after (e)
  "Insert an object after the currrent node."
  (insert-object-after e t))

(defun delete-node (e)
  "Delete the current node."
  (with-internal-slots (current) e :tree-viewer
    (setf (node-open current) nil)
    (let ((next (tb::find-next-node current))
	  (parent (get-parent current)))
      (when next
	(setf (node-branches parent) (delete current (node-branches parent)))
	(setf current next)
	))))

(defun add-sub-node (e type)
  "Add a sub-node."
  (with-internal-slots (current) e :tree-viewer
    (let ((node (make-instance (node-type e) :object :_place_holder_)))
      (setf (node-open current) t)
      (push node (node-branches current))
      (tb::next-node e)
      (update-display e)
      (tt-finish-output)
      (setf (node-object node) (input e type)))))

(defun add-thing-node (e)
  "Add a sub-node to the current node."
  (add-sub-node e t))

(defgeneric edit-node-type (editor type)
  (:documentation "Edit the current node of the given type."))

(defmethod edit-node-type ((e tree-editor) type)
  (with-internal-slots (current) e :tree-viewer
    (setf (node-object current) (input e type :value (node-object current)))))

(defun edit-thing-node (e)
  (edit-node-type e t))

(defgeneric edit-node (editor)
  (:documentation "Edit the current node."))

(defmethod edit-node ((e tree-editor))
  "Edit the current node."
  (edit-thing-node e))

(defun edit-tree (tree &key editor)
  "Edit a tree."
  (with-terminal ()
    (let ((*viewer* editor)
	  result)
      (labels ((ensure-tree () (if (listp tree) (convert-tree tree) tree))
	       (ensure-root ()
		 ;; (when (not (tb::root *viewer*))
		 (when (not (slot-boundp *viewer* 'tb::root))
		   (setf (tb::root *viewer*) (ensure-tree)))
		 (when (or (not (slot-boundp *viewer* 'tb::current))
			   (not (slot-value *viewer* 'tb::current)))
		   (setf (tb::current *viewer*) (tb::root *viewer*)))))
	(if editor
	    (progn
	      (ensure-root)
	      (event-loop *viewer*))
	    (with-inator (*viewer* 'tree-editor
				   :keymap (list *tree-edit-keymap*
						 tb::*tree-keymap*
						 inator:*default-inator-keymap*)
				   :bottom (- (tt-height) 2)
				   :root (if (listp tree)
					     (convert-tree tree) tree))
	      (ensure-root)
	      (setf (node-open (tb::root *viewer*)) t)
	      (event-loop *viewer*)))
	(tt-move-to (tt-height) 0)
	result))))

;; End
