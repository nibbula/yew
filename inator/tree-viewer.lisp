;;;
;;; tree-viewer.lisp - View trees.
;;;

(defpackage :tree-viewer
  (:documentation "View trees.

This module provides a few things.
  - A set of classes to construct and describe viewable trees.
  - An ‘inator’ user interface piece, to view the trees.

The basic unit of the trees is the class NODE, which has NODE-BRANCHES and an
NODE-OPEN indicator. A plain node can be made with MAKE-NODE. There are a
number of sub-classes of NODE which provide for different ways of describing
the tree. The class hierarchy of nodes is like:

  node
    object-node
      dynamic-ndoe
        cached-dynamic-node

The OBJECT-NODE is a simple static node which provide a NODE-OBJECT. A static
tree can be just made out of OBJECT-NODEs. You can make one with
MAKE-OBJECT-NODE.

A DYNAMIC-NDOE is node that creates it's sub-nodes by calling a function, in the
slot NODE-FUNC. There is a MAKE-DYNAMIC-NODE.

A CACHED-DYNAMIC-NDOE is a DYNAMIC-NDOE that will save the results of generating
the branches and subsequently behave like a static OBJECT-NODE.

There are methods which one can specialize to change the behavior of nodes:

   node-branches ndoe => list of branches
     Specializing this, you can make a tree out of anything. This is how the
     dynamic and cached-dynamic trees work, so for those you would have to
     preserve their semantics, if you want them to live up to their name.

   node-has-branches node => boolean
     This is mostly provided to show the indicator on a closed node in a way
     which hints that it has branches. For dynamic trees that don't know if
     it does, this can be optimistic and return true when it's not really
     known.

There are functions for dealing with nodes and trees:

   close-all-subnodes node
   open-all-subnodes node
   node-open-p node => boolean
   all-subnodes-open-p node => boolean

   map-tree func tree &key (max-count 10000)
     Applies FUNC to each node in TREE, up to max-count nodes. You can set
     max-count to nil to try to blow out your stack.

   print-tree node &key max-depth (indent 2) key
     is mostly useful for debugging. If you don't provide a max-depth for a
     dynamic tree, you can guess the results.

There are convenience functions for making trees:

  make-tree object func &key type max-depth (test #'equal)
    Makes a tree given an object and a function that returns a list of the
    branches of the node given an object. The objects in the branches are then
    used build sub-trees as the tree is viewed. You can provide a TYPE so one
    can build a static or dynamic tree, or some combination thereof determined
    by the results of calling FUNC. You can limit the tree size by providing
    MAX-DEPTH. The TEST function can be used to detect cycles.

The module also provides methods to customize the display of the tree, in
general, or for different node types. These are the display methods.

   display-indent
   display-prefix
   display-node-line
   display-object
   display-node
   display-tree

Not all of them will be useful to a customizing given node type.
These methods may need to access the current viewer INATOR in *VIEWER*.

Once you have a tree all set up, you can call VIEW-TREE:

  view-tree (tree &key viewer default-action)

which fires up an INATOR, of which you can roll your own with the VIEWER, and
which can be customized by the usual set of INATOR-isms, keymaps, display
methods, and the like.
")
  (:nicknames :tb)
  (:use :cl :dlib :opsys :dlib-misc :dtime :char-util :keymap :pick-list
	:glob :collections :inator :terminal :terminal-inator :fui :fatchar-io
	:view-generic)
  (:export
   #:view-tree
   #:node #:node-branches #:node-open #:make-node
   #:object-node #:node-object #:make-object-node
   #:node-has-branches
   #:map-tree
   #:close-all-subnodes
   #:open-all-subnodes
   #:node-open-p
   #:all-subnodes-open-p
   #:dynamic-node #:node-func #:make-dynamic-node #:make-dynamic-tree
   #:cached-dynamic-node #:node-cached #:make-cached-dynamic-node
   #:make-cached-dynamic-tree
   #:convert-tree #:print-tree #:make-tree #:subdirs
   #:tree-viewer
   #:*viewer*
   #:get-parent
   #:display-indent
   #:display-prefix
   #:display-node-line
   #:display-object
   #:display-node
   #:display-tree
   ))
(in-package :tree-viewer)

;;;(declaim (optimize (speed 3) (safety 0) (debug 0) (space 3) (compilation-speed 0)))
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 1) (compilation-speed 0)))

;; @@@ The tree-viewr is quite flexible and fairly featureful, but its seems
;; somewhat difficult to use. I'd like to try to imagine easier to use
;; interface, while keeping the flexibility and features.
;;
;; Some real documentation and simple examples might go a long way towards making
;; it easier to use.

(defclass node ()
  ((branches
    :initarg :branches :accessor node-branches :initform nil
    :documentation "Sequence of nodes which are the branches.")
   (open
    :initarg :open :accessor node-open :initform nil :type boolean
    :documentation "True if this node is open."))
  (:documentation
   "A generic node in a viewable tree."))

(defun make-node (&rest args &key &allow-other-keys)
  (apply #'make-instance 'node args))

(defclass object-node (node)
  ((object
    :initarg :object :accessor node-object :initform nil
    :documentation "The object in the node."))
  (:documentation "A tree node with an object."))

(defun make-object-node (&rest args &key &allow-other-keys)
  (apply #'make-instance 'object-node args))

(defgeneric node-has-branches (node)
  (:documentation "Return true if the node supposedly has branches."))

(defmethod node-has-branches ((node node))
  "Return true if the node has branches."
  ;; Static nodes just check the branches slot.
  (node-branches node))

(defvar *map-tree-count* 0
  "How many nodes we've processed with map-tree.")
(declaim (type integer *map-tree-count*))

(defvar *map-tree-max-count* 0
  "When to stop processing with map-tree.")
(declaim (type integer *map-tree-count*))

(defun %map-tree (func node)
  (incf *map-tree-count*)
  (when (or (not *map-tree-max-count*)
	    (< *map-tree-count* *map-tree-max-count*))
    (funcall func node)
#|    (when dlib:*dbug*
      (mvaddstr 5 50 (format nil "~a ~a" *map-tree-count* *map-tree-max-count*))
      (refresh)) |#
    (mapc #'(lambda (x) (%map-tree func x))
	  (node-branches node))))

(defun map-tree (func node &key (max-count 10000))
  (let ((*map-tree-count* 0)
	(*map-tree-max-count* max-count))
    (%map-tree func node)))

(defun close-all-subnodes (node)
  (map-tree #'(lambda (x) (setf (node-open x) nil)) node))

(defun open-all-subnodes (node)
  (map-tree #'(lambda (x) (setf (node-open x) t)) node))

(defun node-open-p (node)
  "If it has no branches it's both open and closed."
  (or (not (node-has-branches node)) (node-open node)))

(defun all-subnodes-open-p (node)
  (if (not (node-open-p node))
      nil
      (loop :for n :in (node-branches node) :do
	 (when (or (not (node-open-p n))
		   (not (all-subnodes-open-p n)))
	   (return-from all-subnodes-open-p nil))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic nodes

(defclass dynamic-node (object-node)
  ((func
    :initarg :func :accessor node-func
    :documentation
    "A function that given an OBJECT generates a list of branch objects
or nodes."))
  (:default-initargs
   :func nil
   )
  (:documentation "A dynamic node in viewable tree. A dynamic node has a
function that generates the branches."))

(defun make-dynamic-node (&rest args &key &allow-other-keys)
  (apply #'make-instance 'dynamic-node args))

(defmethod node-branches ((node dynamic-node))
  (if (node-func node)
      (mapcar #'(lambda (x)
		  (if (typep x 'node)
		      x
		      (make-instance (type-of node)
				     :object x
				     ;; :open t
				     :open nil
				     :func (node-func node))))
	      (funcall (node-func node) (node-object node)))
      nil))

(defmethod node-has-branches ((node dynamic-node))
  "Return true if the node has branches."
  ;; Dynamic nodes are optimistic and return true if there's a
  ;; generating function.
  (not (null (node-func node))))

(defun make-dynamic-tree (thing func)
  "Return a dynamic tree for THING, where FUNC is a function (FUNC THING)
which returns a list of the branches of THING."
  ;; (make-dynamic-node :object thing :func func :open t)
  (make-dynamic-node :object thing :func func :open nil)
  )

;; Dynamic cached node

(defclass cached-dynamic-node (dynamic-node)
  ((cached
    :initarg :cached :accessor node-cached
    :documentation "True if the results of FUNC were already retrieved."))
  (:default-initargs
   :cached nil
   )
  (:documentation "A dynamic node in a viewable tree. A dynamic node has a
function that generates the branches. It caches the results of the branch
generating function, so it will be called only the first time."))

(defun make-cached-dynamic-node (&rest args &key &allow-other-keys)
  (apply #'make-instance 'cached-dynamic-node args))

(defmethod node-branches ((node cached-dynamic-node))
  (with-slots (branches cached func object) node
    (if cached
	branches
	(if func
	    (setf cached t
		  branches
		  (mapcar #'(lambda (x)
			      (if (typep x 'node)
				  x
				  (make-instance (type-of node)
						 :object x :open nil
						 :func func
						 :cached nil)))
			  (funcall func object)))
	    nil))))

(defmethod node-has-branches ((node cached-dynamic-node))
  "Return true if the node has branches."
  ;; Cached dynamic nodes are optimistic and return true if there's a
  ;; generating function and it's not cached, but if it's cached return
  ;; based on if we have any cached branches.
  (if (node-cached node)
      (not (null (node-branches node)))
      (not (null (node-func node)))))

(defun make-cached-dynamic-tree (thing func)
  "Return a cached dynmaic tree for THING, where FUNC is a function
(FUNC THING) which returns a list of the branches of THING."
  (make-cached-dynamic-node :object thing :func func :open nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List based trees

(defun convert-tree (tree &key (level 0) (type 'object-node))
  "Convert a list based ‘tree’ to a node based tree."
  (flet ((make-branch (n)
	   (convert-tree n :level (1+ level) :type type))
	 (make-leaf (n)
	   (make-instance type :object n :open nil :branches nil)))
    (cond
      ((atom tree)
       (make-leaf tree))
      (t
       (let ((node (make-instance type
				  :object (car tree)
				  :open (zerop level))))
	 (setf (node-branches node)
	       (cond
		 ((null (rest tree)) nil)
		 ((atom (rest tree))
		  (list (make-leaf (rest tree))))
		 (t
		  ;; Handle dotted lists
		  (loop :with n = (rest tree)
		     :and result
		     :do (if (consp n)
			     ;; (push (car n) result)
			     (push (make-branch (car n)) result)
			     (progn
			       (when n
				 (push (make-branch n) result))
			       (return (nreverse result))))
		     :do (setf n (cdr n)))
		  ;; (loop :for n :in (rest tree)
		  ;;    :collect (make-branch n))
		  )))
	 node)))))

(defgeneric %print-tree (tree &key max-depth indent level key)
  (:documentation "The inner part of print-tree which recurses and does all
the work. This is probably the only part which needs to be overridden for
other tree types."))
(defmethod %print-tree ((tree list) &key max-depth (indent 2) (level 0) key)
  (when (or (not max-depth) (< level max-depth))
    (loop :for n :in tree :do
       (if (atom n)
	   (format t "~v,,,va~a~%" (* level indent) #\space ""
		   (if key (funcall key n) n))
	   (%print-tree n :level (1+ level)
			:indent indent :max-depth max-depth :key key)))))

(defun print-tree (tree &key max-depth (indent 2) key)
  "Print a tree up to a depth of MAX-DEPTH. Indent by INDENT spaces for every level. INDENT defaults to 2. Apply KEY function to each element before printing."
  (%print-tree tree :max-depth max-depth :indent indent :key key))

(defun %make-tree (thing func &key max-depth (test #'equal)
				(depth 0) (flat '()))
  "Generate a list based tree for THING, where FUNC is a function (FUNC THING)
which returns a list of the branches of THING. Makes a tree of up to a depth
of MAX-DEPTH. TEST is used to compare THINGS. TEST defaults to EQUAL. DEPTH is
the current depth in the tree, and FLAT is a flat list of things encountered
to prevent following infinite cycles."
  (declare (ignore test))
  (when (and max-depth (> depth max-depth))
    (return-from %make-tree nil))
  (let ((tree '())
	(branches (funcall func thing)))
    (push thing tree)
    (pushnew thing flat)
    (when branches
      (loop :for b :in branches :do
	 (when (not (find b flat)) ; don't follow cycles
	   (let ((sub-tree (%make-tree
			    b func :depth (1+ depth) :flat flat
			    :max-depth max-depth)))
	     (when sub-tree
	       (push sub-tree tree))))))
    (nreverse tree)))

;; I'm not sure this works or is useful?
(defun %make-tree-with-type (thing func type &key max-depth (test #'equal)
					  (depth 0) (flat '()))
  "Generate a tree for THING, where FUNC is a function (FUNC THING) which
returns a list of the branches of THING. Makes a tree of up to a depth of
MAX-DEPTH. TEST is used to compare THINGS. TEST defaults to EQUAL. DEPTH is
the current depth in the tree, and FLAT is a flat list of things encountered
to prevent following infinite cycles. TYPE must be a node or a subclass
of node."
  (when (and max-depth (> depth max-depth))
    (return-from %make-tree-with-type nil))
  (let ((tree (make-instance type))
	(branches (funcall func thing)))
    (pushnew thing flat :test test)
    (when branches
      (loop :for b :in branches :do
	 (when (not (find b flat :test test)) ; don't follow cycles
	   (let ((sub-tree (%make-tree-with-type
			    b func type
			    :depth (1+ depth) :flat flat
			    :max-depth max-depth)))
	     (when sub-tree
	       (push sub-tree (node-branches tree)))))))
    (setf (node-branches tree) (nreverse (node-branches tree)))
    tree))

(defun make-tree (thing func &key max-depth (test #'equal) type)
  "Generate a tree for THING, where FUNC is a function (FUNC THING) which ~
returns a list of the branches of THING. Makes a tree of up to a depth of ~
MAX-DEPTH. TEST is used to compare THINGS. TEST defaults to EQUAL."
  (if type
      (%make-tree-with-type thing func type :max-depth max-depth :test test)
      (%make-tree thing func :max-depth max-depth :test test)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Viewer

(defmethod next-file (o)
  "Go to the next file."
  (declare (ignore o))
  (when (find-restart 'next-file)
    (invoke-restart 'next-file)))

(defmethod previous-file (o)
  "Go to the previous file."
  (declare (ignore o))
  (when (find-restart 'previous-file)
    (invoke-restart 'previous-file)))

(defkeymap *tree-keymap* ()
  `((#\q		. quit)
    (#\Q		. quit)
    (#.(ctrl #\C)	. quit)
    ;;(#\return		. pick-object)
    (#\newline	. pick-object)
    ;; Node state control
    (#\space		. toggle)
    (#\tab		. cycle)
    (:btab		. close-all-subnodes-command)
    (:back-tab		. close-all-subnodes-command)
    (#\+		. open-node)
    (#\-		. close-node)
    ;; Movement
    (#\n		. next-node)
    (,(ctrl #\N)	. next-node)
    (:down		. next-node)
    (#\N		. next-hierarchical-node)
    (#\p		. previous-node)
    (,(ctrl #\P)	. previous-node)
    (:up		. previous-node)
    (#\P		. previous-hierarchical-node)
    (,(ctrl #\F)	. forward-some)
    (,(ctrl #\B)	. backward-some)
    (,(ctrl #\V)	. next-page)
    (:npage		. next-page)
    (:ppage		. previous-page)
    (,(meta-char #\v)	. previous-page)
    (#\<		. goto-first-node)
    (,(meta-char #\<)	. goto-first-node)
    (:home		. goto-first-node)
    (#\>		. goto-bottom-node)
    (,(meta-char #\>)	. goto-bottom-node)
    (:end		. goto-bottom-node)
    (:left		. shift-left)
    (:right	     	. shift-right)
    (,(ctrl #\A)	. shift-beginning)
    (,(ctrl #\E)	. shift-end)
    (,(ctrl #\S)	. search-forward-command)
    (#\/		. search-forward-command)
    (,(ctrl #\R)	. search-backward-command)
    ;; Miscellaneous
    (#\m		. toggle-modeline)
    (,(ctrl #\L)	. redraw)
    (#\i		. node-info)
    (#\v		. view-node)
    (#\V		. view-node-raw)
    (#\?		. help)
    (,(meta-char #\n)	. next-file)
    (,(meta-char #\p)	. previous-file)
    (,(meta-char #\=)	. describe-key-briefly)
    (#\escape		. *tree-escape-keymap*)))

(defparameter *tree-escape-keymap* (build-escape-map *tree-keymap*))

(defclass tree-viewer (terminal-inator)
  ((picked-object
     :initarg :picked-object :accessor picked-object :initform nil
     :documentation "The object that was picked.")
   (current
    :initarg :current :accessor current :initform nil 
    :documentation "The current node.")
   (current-position
    :initarg :current-position :accessor current-position :initform nil 
    :documentation "The position of the current element.")
   (current-left
    :initarg :current-left :accessor current-left :initform nil
    :documentation "Horizontal position of the current item.")
   (parents
    :initarg :parent :accessor parents :initform nil 
    :documentation "A table of the node parents we have encountered.")
   (root
    :initarg :root :accessor root
    :documentation "The root node of the tree.")
   (top
    :initarg :top :accessor top :initform nil
    :documentation "First node to display.")
   (bottom
    :initarg :bottom :accessor bottom :initform nil :type (or null integer)
    :documentation "Last position on the screen.")
   (bottom-node
    :initarg :bottom-node :accessor bottom-node :initform nil
    :documentation "Last node on the screen.")
   (indent
    :initarg :indent :accessor indent :initform 2 :type integer
    :documentation "How much to indent each level.")
   (open-indicator
    :initarg :open-indicator :accessor open-indicator :initform #\+
    :documentation "Indicator that a node is open.")
   (closed-indicator
    :initarg :closed-indicator :accessor closed-indicator :initform #\-
    :documentation "Indicator that a node is closed.")
   (left
    :initarg :left :accessor left :initform 0 :type integer
    :documentation "Horizontal offset of the view.")
   (current-max-right
    :initarg :current-max-right :accessor current-max-right :initform nil
    :documentation "Maximum potential column for the current node.")
   (show-modeline
    :initarg :show-modeline :accessor show-modeline :initform nil :type boolean
    :documentation "True to show the modeline.")
   (message-string
    :initarg :message-string :accessor message-string :initform nil 
    :documentation "Message to show at the bottom of the screen.")
   (scroll-hint
    :initarg :scroll-hint :accessor scroll-hint
    :initform nil :type (or null symbol)
    :documentation "Hint about what direction to scroll.")
   (default-action
    :initarg :default-action :accessor tree-viewer-default-action :initform nil
    :documentation "The default action to perform when the user accepts."))
  (:default-initargs
   :keymap (list *tree-keymap*
		 inator:*default-inator-keymap*)
   :open-indicator (theme:value '(:program :tree :open-indicator))
   :closed-indicator (theme:value '(:program :tree :closed-indicator)))
  (:documentation "A tree viewer."))

(defvar *viewer* nil
  "The current tree viewer.")

(defun get-parent (node)
  "Return the parent of the given NODE in the current tree-viewer, if it's
been encountered."
  (gethash node (parents *viewer*)))

(defun set-parent (node parent)
  "Set the PARENT of the given NODE in the current tree-viewer."
  (setf (gethash node (parents *viewer*)) parent))

;; (defun quit ()
;;   "Quit the tree viewer."
;;   (setf (inator-quit-flag *viewer*) t))

(defun pick-object (o)
  "Pick the current object and return it."
  (with-slots (picked-object current) o
    (setf (inator-quit-flag o) t
	  picked-object current)))

(defmethod accept ((o tree-viewer))
  (with-slots (default-action) o
    (if default-action
	(funcall default-action o)
	(pick-object o))))

(defun toggle (o)
  "Toggle the node between open and closed."
  (with-slots (current) o
    (setf (node-open current) (not (node-open current)))))

(defun cycle (o)
  "Cycle through open, all subnodes open, and closed."
  (with-slots (current) o
    (if (node-open current)
	(if (all-subnodes-open-p current)
	    (close-all-subnodes current)
	    (open-all-subnodes current))
	(setf (node-open current) t))))

(defun open-node (o)
  "Open the current node."
  (with-slots (current) o
    (setf (node-open current) t)))

(defun close-node (o)
  "Close the current node."
  (with-slots (current) o
    (setf (node-open current) nil)))

(defun close-all-subnodes-command (o)
  "Close all the sub-nodes of the current node."
  (close-all-subnodes (current o)))

(defun node-after-child (parent child)
  "Retun the node after CHILD in the PARENT, or NIL is there is none."
  (loop :with is-next
     :for n :in (node-branches parent) :do
     (cond
       (is-next
	(return-from node-after-child n))
       ((eq n child)
	(setf is-next t))))
  nil)

(defun find-next-node (node)
  "Return the next node after NODE in the linearized tree."
  (if (and (node-open node) (node-branches node))
      (first (node-branches node))
      (let ((parent (get-parent node))
	    (child node))
	(loop :while parent :do
	   (let ((next (node-after-child parent child)))
	     (when next
	       (return-from find-next-node next))
	     (setf child parent
		   parent (get-parent parent)))))))

(defun next-node (o)
  "Set current to the next node in the linearized tree."
  (with-slots (current bottom-node top scroll-hint) o
    (setf scroll-hint :down)
    (let ((next (find-next-node current)))
      (when next
	(when (and (eq current bottom-node) #| (not (eq current top)) |#)
	  (let ((next-top (find-next-node top)))
	    (when next-top
	      (setf top next-top))))
	(setf current next)))))

(defun last-open-subnode (branches)
  (let ((last-node (car (last branches))))
    (if (and (node-open last-node) (node-branches last-node))
	(progn
	  (last-open-subnode (node-branches last-node)))
	last-node)))

(defun find-previous-node (node)
  (let ((parent (get-parent node)))
;    (message "~a" (and parent (node-object parent)))
    (when parent
      (loop :with prev = parent
	 :for n :in (node-branches parent) :do
	 (when (eq n node)
	   (return-from find-previous-node
	     (cond
	       ((eq prev parent)
		parent)
	       ((and (node-open prev) (node-branches prev))
		(when (node-branches prev)
		  (last-open-subnode (node-branches prev))))
	       (t prev))))
	 (setf prev n)))))

(defun previous-node (o)
  "Set current to the previous node in the linearized tree."
  (with-slots (current top scroll-hint) o
    (setf scroll-hint :up)
    (when (eq current top)
      (setf top (or (find-previous-node top) top)))
    (let ((prev (find-previous-node current)))
      (when prev
	(setf current prev)))))

(defun forward-some (o &optional (n 15))
  "Go forward by a few nodes. Defaults to 15."
  (with-slots (current top bottom-node scroll-hint) o
    (setf scroll-hint :down)
    (loop :with after-bottom
       :for i :from 1 :to n
       :while (next-node o)
       :do
       (when (eq current bottom-node)
	 (setf after-bottom t))
       (when after-bottom
	 (setf top (find-next-node top))))))

(defun backward-some (o &optional (n 15))
  "Go backward by a few nodes. Defaults to 15."
  (setf (scroll-hint o) :up)
  (loop :for i :from 1 :to n
     :while (previous-node o)))

(defun find-next-hierarchical-node (node)
  "Return the next node after NODE in the linearized tree."
  (let ((parent (get-parent node))
	(child node))
    (loop :while parent :do
       (let ((next (node-after-child parent child)))
	 (when next
	   (return-from find-next-hierarchical-node next))
	 (setf child parent
	       parent (get-parent parent))))))

(defun next-hierarchical-node (o)
  "Go to the next node at the same level."
  (with-slots (current bottom-node top scroll-hint) o
    (setf scroll-hint :down)
    (let ((next (find-next-hierarchical-node current)))
      (when next
	(when (and (eq current bottom-node))
	  (let ((next-top (find-next-node top)))
	    (when next-top
	      (setf top next-top))))
	(setf current next)))))

(defun find-previous-hierarchical-node (node)
  (let ((parent (get-parent node)))
    (when parent
      (loop :with prev = parent
	 :for n :in (node-branches parent) :do
	 (when (eq n node)
	   (return-from find-previous-hierarchical-node
	     (cond
	       ((eq prev parent)
		parent)
	       (t prev))))
	 (setf prev n)))))

(defun previous-hierarchical-node (o)
  "Go to the previous node at the same level."
  (with-slots (current top scroll-hint) o
    (setf scroll-hint :up)
    (when (eq current top)
      (setf top (or (find-previous-node top) top)))
    (let ((prev (find-previous-hierarchical-node current)))
      (when prev
	(setf current prev)))))

;; Page movement is approximate.

(defmethod next-page ((o tree-viewer))
  "Go to the next page."
  (with-slots (scroll-hint) o
    (setf scroll-hint :down)
    (loop :for i :from 1 :to (tt-height)
       :while (next-node o))))

(defmethod previous-page ((o tree-viewer))
  "Go to the previous page."
  (backward-some o (tt-height)))

(defun shift-left (o)
  "Shift the edge left."
  (with-slots (left) o
    (decf (left o) 10)
    (when (< left 0)
      (setf left 0))))

(defun shift-right (o)
  "Shift the edge right."
  (incf (left o) 10))

(defun shift-beginning (o)
  "Shift the view all the way left."
  (setf (left o) 0))

(defun shift-end (o)
  "Shift to the end of the rightmost content."
  (with-slots (left current-max-right) o
    (when current-max-right
      (setf left (max 0 (- current-max-right (tt-width)))))))

(defun goto-first-node (o)
  "Go to the first node."
  (with-slots (current root top) o
    (setf current root
	  top root)))

(defun goto-bottom-node (o)
  "Go to the last node."
  (with-slots (current root top scroll-hint) o
    (let ((last (last-open-subnode (node-branches root)))) 
      (setf current last
	    ;; We could jus set scroll-hint :down here, but that can be very
	    ;; inefficient, so let's go to the end and then go backwards some.
	    top current)
      (backward-some o (tt-height))
      (setf current last
	    scroll-hint :down))))

(defun open-path-to-node (o)
  (declare (ignore o))
  )

(defun search-tree-forward (o string tree)
  (with-slots (current bottom-node top scroll-hint) o
    ;; (let ((obj-str (princ-to-string (node-object tree)))
    ;; 	  (node-str (princ-to-string tree)))
    ;;   (when (or (search string obj-str :test #'equalp)
    ;; 		(search string node-str :test #'equalp))
    ;; 	(setf current tree)
    ;; 	(open-path-to-node tree)
    ;; 	(throw 'search-done :found))
    ;;   (loop :for n :in (node-branches tree) :do
    ;; 	 (search-tree o string n)))))
    (loop :with after-bottom :and obj-str :and node-str :and node = tree
       ;;:for i :from 1 :to n
       :while (setf node (find-next-node node))
       :do
       (setf obj-str (princ-to-string (node-object node))
	     node-str (princ-to-string node))
       (when (or (search string obj-str :test #'equalp)
		 (search string node-str :test #'equalp))
	 (setf current node)
	 (open-path-to-node node)
	 (setf scroll-hint :down)
	 (throw 'search-done :found))
       (when (eq current bottom-node)
	 (setf after-bottom t))
       (when after-bottom
	 (setf top (find-next-node top))))))

(defun search-tree-backward (o string tree)
  (with-slots (current top scroll-hint) o
    (loop :with obj-str :and node-str :and node = tree
       :while (setf node (find-previous-node node))
       :do
       (setf obj-str (princ-to-string (node-object node))
	     node-str (princ-to-string node))
       (when (or (search string obj-str :test #'equalp)
		 (search string node-str :test #'equalp))
	 (setf current node
	       top node)
	 (open-path-to-node node)
	 (setf scroll-hint :up)
	 (throw 'search-done :found)))))

(defun search-tree-command (o &optional (direction :forward))
  "Search for a node."
  (with-slots (current) o
    (tt-move-to (1- (tt-height)) 0) (tt-erase-to-eol)
    (tt-finish-output)
    (let ((result (rl:rl :prompt "Search for: ")))
      (tt-finish-output)
      (when (and result (not (zerop (length result))))
	(when (not (eq :found
		       (catch 'search-done
			 (ecase direction
			   (:forward
			    (search-tree-forward o result current))
			   (:backward
			    (search-tree-backward o result current))))))
	  (message o "~s not found" result))))))

(defun search-forward-command (o)
  "Search open nodes forward."
  (search-tree-command o :forward))

(defun search-backward-command (o)
  "Search open nodes backward."
  (search-tree-command o :backward))

(defun toggle-modeline (o)
  (setf (show-modeline o) (not (show-modeline o))))

(defun node-info (o)
  "Display node information."
  (with-slots (current) o
    (fui:show-text
     (with-output-to-string (str)
       (describe current str))
     :title "Node Info"
     ;; (list (format nil "Node: ~a" current)
     ;; 	   (format nil " open     : ~a" (node-open current))
     ;; 	   (format nil " branches : ~a" (node-branches current))
     ;; 	   (format nil " parent   : ~a" (get-parent current)))
     )))

(defun %view-node (o &key raw)
  "View the current node with a generic viewer. Use the raw viewer if ‘raw’
is true."
  (let ((viewer (if raw #'view-raw #'view)))
    (with-slots (current) o
      (with-simple-restart (abort "Go back to the tree viewer.")
	(handler-case
	    (typecase current
	      (object-node
	       (funcall viewer (node-object current)))
	      (t
	       ;; This is probably useless.
	       (funcall viewer current)))
	  (error (c)
	    (if (fui:popup-y-or-n-p
		 (fatchar:span-to-fat-string
		  `((:red "Error: ") ,(apply #'format nil "~a" (list c))
		    #\newline #\newline "Enter the debugger?"))
		 :default #\N)
		(invoke-debugger c)
		;; (continue)
		(invoke-restart (find-restart 'abort)))))))))

(defun view-node (o)
  "View the current node with a generic viewer."
  (%view-node o))

(defun view-node-raw (o)
  "View the current node with a generic viewer."
  (%view-node o :raw t))

(defmethod sort-command ((o tree-viewer))
  "Sort the branches of the current node."
  (with-slots (current) o
    (setf (node-branches current)
	  ;; @@@ Perhaps could make a node specific sort method?
	  (sort (node-branches current) #'string<
		:key (_ (princ-to-string (node-object _)))))))

(defmethod initialize-instance
    :after ((o tree-viewer) &rest initargs &key &allow-other-keys)
  "Initialize a tree-viewer."
  (declare (ignore initargs))
  (with-slots (parents current root top keymap bottom) o
    (when (not parents)
      (setf parents (make-hash-table :test #'equal)))
    (when (slot-boundp o 'root)
      (setf current root
	    top root))
    (when (not (slot-boundp o 'keymap))
      (setf keymap (list *tree-keymap*
			 inator:*default-inator-keymap*)))
    (when (or (not (slot-boundp o 'bottom))
	      (not bottom))
      (setf bottom (- (tt-height) 2)))))

(defun show-message (format-string &rest format-args)
  "Display a formatted message."
  ;; (tt-move-to (- (tt-height) 1) 2)
  (tt-move-to (- (tt-height) 1) 0)
  (tt-erase-to-eol)
  ;;(tt-write-string (apply #'format nil format-string format-args))
  (apply #'terminal-format *terminal* format-string format-args))

(defmethod message ((o tree-viewer) format-string &rest format-args)
  (setf (message-string o)
	(apply #'format nil format-string format-args)))

(defmethod prompt ((o tree-viewer) format-string &rest format-args)
  (apply #'show-message format-string format-args)
  (tt-finish-output))

(defun message-pause (format-string &rest format-args)
  "Display a formatted message and wait for a key."
  (apply #'show-message format-string format-args)
  (tt-finish-output)
  (tt-get-char))

(defgeneric display-indent (node level)
  (:documentation
   "Display the normal indentation for a node."))

(defmethod display-indent ((node node) level)
  "Return the normal indentation for a node."
  (format nil "~v,,,va  " (* level (indent *viewer*)) #\space ""))

(defgeneric display-prefix (node level)
  (:documentation
   "Display the normal indentation and open / close indicator."))

(defmethod display-prefix ((node node) level)
  "Return the normal indentation and open / close indicator."
  (with-slots (open-indicator closed-indicator indent) *viewer*
    (with-output-to-fat-string (str)
      (format str "~v,,,va~/fatchar-io:print-string/ "
	      (* level indent) #\space ""
	      (if (node-branches node)
		  (if (node-open node) open-indicator closed-indicator)
		  #\space)))))

(defgeneric display-node-line (node line)
  (:documentation
   "Display a line of node output.
    If you don't use display-node-line to display your content, you should
    handle the LEFT offset yourself, if you want left/right scrolling."))

;; @@@ probably olength and osubseq in here should be in terms of grid cells
;; not cheracter codes, so somehow using display-length.
(defmethod display-node-line ((node node) line)
  "Display a line of node output."
  (with-slots (left current current-max-right) *viewer*
    (let ((len (olength line)))
      (when (< (terminal-get-cursor-position *terminal*)
	       (1- (tt-height)))
	(if (>= left len)
	    (tt-write-char #\newline)
	    (tt-write-string
	     (osubseq line left (min len (+ left (tt-width)))))))
      (when (eq node current)
	(setf current-max-right (olength line))))))

(defgeneric display-object (node object level)
  (:documentation "Display an object in the manner of display-node."))

(defmethod display-object ((node node) object level)
  "Display an object for a node. The object is printed to a string as
with PRINC, and indented properly for multi-line objects."
  (with-slots (current current-left left) *viewer*
    (let ((lines (osplit #\newline (princ-to-string object)))
	  (prefix (display-prefix node level)))
      (when (eq node current)
	(setf current-left (- (display-length prefix) left))
	(tt-bold t))
      (display-node-line node (fs+ prefix (first lines) #\newline))
      (when (eq node current)
	(tt-bold nil))
      (loop :for l :in (cdr lines) :do
	 (display-node-line node (fs+ (display-indent node level)
				      l #\newline))))))

(defgeneric display-node (node level)
  (:documentation "Display a node.
If you make your own display-node method, you can use display-indent, and
display-prefix to generate line strings, and then use display-node-line, to
output them."))

(defmethod display-node :before ((node node) level)
  (with-slots (current current-position bottom-node) *viewer*
    (when (eq node current)
      (setf current-position (terminal-get-cursor-position *terminal*)))
    (setf bottom-node node)))

(defmethod display-node ((node object-node) level)
  "Display an object node. The object is printed to a string as with PRINC,
and indented properly for multi-line objects."
  (display-object node (node-object node) level))

(defgeneric display-tree (viewer tree level)
  (:documentation "Display a tree brower tree."))

(defvar *display-start* nil
  "True if we hit the top node.")

(defmethod display-tree ((o tree-viewer) tree level)
  "Display a tree."
  (with-slots (top bottom) o
    ;; (let ((y (getcury *stdscr*)))
    (let ((y (terminal-get-cursor-position *terminal*)))
      (when (< y bottom)
	(when (eq tree top)
	  (setf *display-start* t))
	(when *display-start*
	  (display-node tree level))
	(when (and (node-open tree) (node-branches tree))
	  (loop :for n :in (node-branches tree) :do
	     (set-parent n tree)
	     (display-tree o n (1+ level))))))))

(defmethod redraw ((o tree-viewer))
  (with-slots (top current root current-max-right) o
    (tt-clear)
    (tt-move-to 0 0)
    (setf current-max-right nil)
    (let ((*display-start* nil))
      (display-tree o root 0))
    (setf top current)))

;; @@@ just for debugging
(defun node-abbrev (node)
  (when node
    (let ((str (format nil "~w" (node-object node))))
      (subseq str 0 (min 15 (length str))))))

(defmethod update-display ((o tree-viewer))
  (let ((*viewer* o))
    (with-slots (root quit-flag picked-object current left top bottom
		 bottom-node current-position current-max-right current-left
		 message-string scroll-hint) o
      (tagbody
       again
	 (tt-home)
	 (tt-erase-below)
	 (setf current-position nil current-left nil current-max-right nil)
	 (let ((*display-start* nil))
	   (display-tree o root 0))
	 ;; Reposition display if the current node is not visible.
	 ;; This can be highly inefficient if it has to reposition far,
	 ;; since it redraws the whole thing to curses, so movement code
	 ;; should try to get close and then set the scroll-hint.
	 (when (not current-position)
	   (case scroll-hint
	     (:down
	      (let ((next-top (find-next-node top)))
		(when next-top
		  (setf top next-top)
		  (go again))))
	     (:up
	      (let ((prev-top (find-previous-node top)))
		(when prev-top
		  (setf top prev-top)
		  (go again))))
	     (otherwise
	      ;; Punt and start from the top.
	      (setf top root
		    scroll-hint :down)
	      (go again)))))
    
    (when message-string
      (show-message (quote-format message-string))
      (setf message-string nil))

    (when (show-modeline o)
      (tt-move-to (- (tt-height) 2) 0)
      (tt-erase-to-eol)
      (tt-format "~a ~a (left=~a top=~a bot=~a)"
		 current-position
		 (node-abbrev current)
		 left
		 (node-abbrev top)
		 (node-abbrev bottom-node)))

    (when current-position
      (tt-move-to current-position 0)))))

(defmethod finish-inator ((o tree-viewer))
  (tt-move-to (1- (tt-height)) 0)
  (tt-scroll-down 2)
  (tt-finish-output))

;; Just so our *viewer* is set for clients that call the ‘event-loop’ directly
;; instead of ‘view-tree’.
(defmethod event-loop :around ((o tree-viewer))
  (let ((*viewer* o))
    (call-next-method)))

(defun view-tree (tree &key viewer default-action)
  "Look at a tree, with expandable and collapsible branches."
  (let (result)
    (with-terminal (#| :crunch |#)
      (let ((*viewer* viewer))
	(if viewer
	    (progn
	      (event-loop *viewer*)
	      (setf result (picked-object *viewer*)))
	    (with-inator (*viewer* 'tree-viewer
				   ;; :keymap (list *tree-keymap*
				   ;; 	       inator:*default-inator-keymap*)
				   :bottom (- (tt-height) 2)
				   :root (if (listp tree)
					     (convert-tree tree) tree)
				   :default-action default-action)
	      (setf (node-open (root *viewer*)) t)
	      (event-loop *viewer*)
	      (setf result (picked-object *viewer*)))))
      (tt-move-to (tt-height) 0))
    result))

(defmethod view ((thing node))
  (view-tree thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some examples

;; @@@ I should probably get this out of here since there's plenty of examples
;; elsewhere, and it's not really a very good introductory example.

(defclass foo-node (object-node)
  ())

(defmethod display-node ((node foo-node) level)
  "Display a foo node."
  (display-node-line
   node 
   (format nil "~v,,,va~c ~a~%" (* level (indent *viewer*)) #\space ""
	   (if (node-branches node)
	       (if (node-open node) #\- #\+)
	       #\space)
	   (node-object node)))
  (when (node-open node)
    (let* ((info (nos:get-file-info (node-object node)))
	   (prefix (format nil "~v,,,va  "
			   (* level (indent *viewer*)) #\space ""))
	  (string
	   (with-accessors ((size file-info-size)
			    (access-time file-info-access-time)
			    (modify-time file-info-modification-time)
			    (change-time file-info-creation-time)
			    #| (birth-time nos::birth-time) |#) info
	     (with-output-to-string (str)
	       (format str "~a Size:        ~d~%" prefix size)
	       (format str "~a Access time: ~a~%" prefix
		       (date-string
			:time (nos:os-time-seconds access-time)))
	       (format str "~a Mod time:    ~a~%" prefix
		       (date-string
			:time (nos:os-time-seconds modify-time)))
	       (format str "~a Change time: ~a~%" prefix
		       (date-string
			:time (nos:os-time-seconds change-time)))
	       #|
	       (format str "~a Birth time:  ~a~%" prefix
		       (date-string
			:time (nos:unix-to-universal-time
			       (nos:timespec-seconds birth-time))))
	       |#
	       (finish-output str)))))
      (display-node-line node string))))

(defun subdirs (dir)
  "Generating function for filesystem tree starting at DIR."
    (loop :for d :in (ignore-errors
		       (nos:read-directory :dir dir :full t :omit-hidden t))
       :if (eql :directory (nos:dir-entry-type d))
       :collect (concatenate 'string dir "/" (nos:dir-entry-name d))))

(defun view-foo ()
  (view-tree
   (convert-tree (make-tree "." #'subdirs)
		 :type 'foo-node)))

;; EOF
