;;
;; tree-viewer.lisp - View trees.
;;

;; TODO:
;;  - It would be best if the generic viewer didn't rely on any output
;;    such as curses, but I would need ‘that thing’, so I'll have to convert
;;    it later.

(defpackage :tree-viewer
  (:documentation "View trees.")
  (:nicknames :tb)
  (:use :cl :dlib :curses :char-util :keymap :opsys :dlib-interactive :terminal
	:fui :inator)
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
   #:display-indent
   #:display-prefix
   #:display-node-line
   #:display-object
   #:display-node
   #:display-tree
   #:fake-code-view
   #:fake-view-project
   #:view-package-dependencies
   #:view-packages
   #:view-lisp
   ))
(in-package :tree-viewer)

;;;(declaim (optimize (speed 3) (safety 0) (debug 0) (space 3) (compilation-speed 0)))
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 1) (compilation-speed 0)))

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
		       :object x :open t :func (node-func node))))
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
  (make-dynamic-node :object thing :func func :open t))

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
  "Convert a list based TREE to a node based tree. "
  (if (atom tree)
      (make-instance type
		     :object tree
		     :open nil
		     :branches nil)
      (let ((node (make-instance type
				:object (car tree)
				:open (zerop level))))
	(setf (node-branches node)
	      (let ((list-of-rest-of-obj
		     (if (listp (cdr tree)) (cdr tree) (list (cdr tree)))))
		(loop :for n :in list-of-rest-of-obj
		   :collect (convert-tree n :level (1+ level) :type type))))
	node)))

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
returns a list of the brances of THING. Makes a tree of up to a depth of ~
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

(defkeymap *tree-keymap*
    `((#\q		. quit)
      (#\Q		. quit)
      (#.(ctrl #\C)	. quit)
      (#\return		. pick-object)
      (#\newline	. pick-object)
      ;; Node state control
      (#\space		. toggle)
      (#\tab		. cycle)
      (:btab		. close-all-subnodes-command)
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
      (:ppage           . previous-page)
      (,(meta-char #\v)	. previous-page)
      (#\<		. goto-first-node)
      (,(meta-char #\<) . goto-first-node)
      (:home		. goto-first-node)
      (#\>		. goto-bottom-node)
      (,(meta-char #\>) . goto-bottom-node)
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
      (#\?		. help)
      (,(meta-char #\n) . next-file)
      (,(meta-char #\p) . previous-file)
      (#\escape		. *tree-escape-keymap*)))

(defparameter *tree-escape-keymap* (build-escape-map *tree-keymap*))

(defclass tree-viewer (fui-inator)
  ((picked-object
     :initarg :picked-object :accessor picked-object :initform nil
     :documentation "The object that was picked.")
   (current
    :initarg :current :accessor current :initform nil 
    :documentation "The current node.")
   (current-position
    :initarg :current-position :accessor current-position :initform nil 
    :documentation "The position of the current element.")
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
    :documentation "Hint about what direction to scroll."))
  (:default-initargs
   :keymap (list *tree-keymap*
		 inator:*default-inator-keymap*))
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
    (loop :for i :from 1 :to curses:*lines*
       :while (next-node o))))

(defmethod previous-page ((o tree-viewer))
  "Go to the previous page."
  (backward-some o curses:*lines*))

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
      (setf left (max 0 (- current-max-right *cols*))))))

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
      (backward-some o *lines*)
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
    (move (1- *lines*) 0) (clrtoeol)
    (let ((result (with-terminal (:curses)
		    (rl:rl :prompt "Search for: "))))
      (refresh)
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
    (fui:display-text
     "Node Info"
     (list (format nil "Node: ~a" current)
	   (format nil " open     : ~a" (node-open current))
	   (format nil " branches : ~a" (node-branches current))
	   (format nil " parent   : ~a" (get-parent current))))))

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
      (setf bottom (- *lines* 2)))))

(defun show-message (format-string &rest format-args)
  "Display a formatted message."
  (move (- *lines* 1) 2)
  (clrtoeol)
  (addstr (apply #'format nil format-string format-args)))

(defmethod message ((o tree-viewer) format-string &rest format-args)
  (setf (message-string *viewer*)
	(apply #'format nil format-string format-args)))

(defun message-pause (format-string &rest format-args)
  "Display a formatted message and wait for a key."
  (apply #'show-message format-string format-args)
  (refresh)
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
  (format nil "~v,,,va~c " (* level (indent *viewer*)) #\space ""
	  (if (node-branches node)
	      (if (node-open node) #\- #\+)
	      #\space)))

(defgeneric display-node-line (node line)
  (:documentation
   "Display a line of node output.
    If you don't use display-node-line to display your content, you should
    handle the LEFT offset yourself, if you want left/right scrolling."))

(defmethod display-node-line ((node node) line)
  "Display a line of node output."
  (with-slots (left current current-max-right) *viewer*
    (let ((len (length line)))
      (when (< (getcury curses:*stdscr*) (1- curses:*lines*))
	(if (>= left len)
	    (addch (char-code #\newline))
	    (addstr (subseq line left (min len (+ left *cols*))))))
      (when (eq node current)
	(setf current-max-right (length line))))))

(defgeneric display-object (node object level)
  (:documentation "Display an object in the manner of display-node."))

(defmethod display-object ((node node) object level)
  "Display an object for a node. The object is printed to a string as
with PRINC, and indented properly for multi-line objects."
  (let ((lines (split-sequence #\newline (princ-to-string object))))
    (when (eq node (current *viewer*))
      (attron +a-bold+))
    (display-node-line node (s+ (display-prefix node level)
				(format nil "~a~%" (first lines))))
    (when (eq node (current *viewer*))
      (attroff +a-bold+))
    (loop :for l :in (cdr lines) :do
       (display-node-line node (s+ (display-indent node level)
				   (format nil "~a~%" l))))))

(defgeneric display-node (node level)
  (:documentation "Display a node.
If you make your own display-node method, you can use display-indent, and
display-prefix to generate line strings, and then use display-node-line, to
output them."))

(defmethod display-node :before ((node node) level)
  (declare (ignore level))
  (when (eq node (current *viewer*))
    (setf (current-position *viewer*) (getcury *stdscr*)))
  (setf (bottom-node *viewer*) node))

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
    (let ((y (getcury *stdscr*)))
      (when (< y bottom)
	(when (eq tree top)
	  (setf *display-start* t))
	(when *display-start*
	  (display-node tree level))
	(if (and (node-open tree) (node-branches tree))
	    (loop :for n :in (node-branches tree) :do
	       (set-parent n tree)
	       (display-tree o n (1+ level))))))))

(defmethod redraw ((o tree-viewer))
  (with-slots (top current root current-max-right) o
    (clear)
    (move 0 0)
    (setf current-max-right nil)
    (let ((*display-start* nil))
      (display-tree o root 0))
    (setf top current)))

;; @@@ just for debugging
(defun node-abbrev (node)
  (when node
    (let ((str (format nil "~w" (node-object node))))
      (subseq str 0 (min 15 (length str))))))

#|
(defun view-tree (tree &key viewer)
  "Look at a tree, with expandable and collapsible branches."
  (fui:with-curses
    (when (listp tree)
      (setf tree (convert-tree tree)))
    (let ((*viewer* (or viewer
			 (make-instance 'tree-viewer
					:root tree
					:bottom (- *lines* 2)))))
      (with-slots (root quit-flag picked-object current left top bottom
		   bottom-node current-position current-max-right
		   message-string scroll-hint)
	  *viewer*
	(loop :do
	   (tagbody
	    again
	      (move 0 0)
	      (erase)
	      (setf current-position nil current-max-right nil)
	      (let ((*display-start* nil))
		(display-tree *viewer* root 0))
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
		   (go again))))
	      (when message-string
		(show-message (quote-format message-string))
		(setf message-string nil))
	      (when (show-modeline *viewer*)
		(move (- *lines* 2) 0)
		(clrtoeol)
		(addstr (format nil "~a ~a (left=~a top=~a bot=~a)"
				current-position
				(node-abbrev current)
				left
				(node-abbrev top)
				(node-abbrev bottom-node))))
	      (when current-position
		(move current-position 0))
	      (perform-key (tt-get-char))
	      ;; bounds checking
	      (when (< left 0)
		(setf left 0)))
	   :while (not quit-flag))
	picked-object))))
|#

(defmethod update-display ((o tree-viewer))
  (with-slots (root quit-flag picked-object current left top bottom
	       bottom-node current-position current-max-right
	       message-string scroll-hint) o
    (tagbody
     again
       (move 0 0)
       (erase)
       (setf current-position nil current-max-right nil)
       (let ((*display-start* nil))
	 (display-tree *viewer* root 0))
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

    (when (show-modeline *viewer*)
      (move (- *lines* 2) 0)
      (clrtoeol)
      (addstr (format nil "~a ~a (left=~a top=~a bot=~a)"
		      current-position
		      (node-abbrev current)
		      left
		      (node-abbrev top)
		      (node-abbrev bottom-node))))

    (when current-position
      (move current-position 0))))

(defun view-tree (tree &key viewer)
  "Look at a tree, with expandable and collapsible branches."
  (with-terminal (:curses)
    (let ((*viewer* viewer))
      (if viewer
	  (progn
	    (event-loop *viewer*)
	    (picked-object *viewer*))
	  (with-inator (*viewer* 'tree-viewer
				 ;; :keymap (list *tree-keymap*
				 ;; 	       inator:*default-inator-keymap*)
				 :bottom (- *lines* 2)
				 :root (if (listp tree)
					   (convert-tree tree) tree))
	    (setf (node-open (root *viewer*)) t)
	    (event-loop *viewer*)
	    (picked-object *viewer*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some examples

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
		       (dlib-misc:date-string
			:time (nos:derp-time-seconds access-time)))
	       (format str "~a Mod time:    ~a~%" prefix
		       (dlib-misc:date-string
			:time (nos:derp-time-seconds modify-time)))
	       (format str "~a Change time: ~a~%" prefix
		       (dlib-misc:date-string
			:time (nos:derp-time-seconds change-time)))
	       #|
	       (format str "~a Birth time:  ~a~%" prefix
		       (dlib-misc:date-string
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code browsing

(defclass code-node (object-node)
  ())

(defmethod display-node ((node code-node) level)
  (addstr
   (format nil "~v,,,va~c "
	   (* level (indent *viewer*)) #\space ""
	   (if (node-branches node)
	       (if (node-open node) #\- #\+)
	       #\space)))
  (flet ((get-name (n) (node-object (first (node-branches n))))
	 (get-args (n)
	   (let ((arg-node (second (node-branches n))))
	     (append (list (node-object arg-node))
		     (loop :for s :in (node-branches arg-node)
			:collect (node-object s))))))
    (cond
      ((symbolp (node-object node))
       (case (node-object node)
	 ((defun defmacro defgeneric defmethod)
	     (let* ((name (get-name node))
		    (args (get-args node)))
	       (fui:with-fg (+color-magenta+)
		 (addstr (format nil "~(~s~) " (node-object node))))
	       (fui:with-fg (+color-green+)
		 (addstr (format nil "~(~a~)" name)))
	       (addstr (format nil " (~{~(~w~)~^ ~})~%" args))))
	 ((defstruct defclass deftype defvar defparameter defconstant)
	  (fui:with-fg (+color-magenta+)
	    (addstr (format nil "~(~s~)" (node-object node))))
	  (fui:with-fg (+color-green+)
	    (addstr (format nil " ~(~a~)~%" (get-name node)))))
      	 (otherwise
	  (let* ((pkg (symbol-package (node-object node)))
		 (pkg-name (and pkg (package-name pkg))))
	    (cond
	      ((equal pkg-name "COMMON-LISP")
	       (fui:with-fg (+color-magenta+)
		 (addstr (format nil "~(~s~)~%" (node-object node)))))
	      ((equal pkg-name "KEYWORD")
	       (fui:with-fg (+color-blue+)
		 (addstr (format nil "~(~s~)~%" (node-object node)))))
	      (t
	       (addstr (format nil "~(~a~)~%" (node-object node)))))))))
      ((stringp (node-object node))
       (fui:with-fg (+color-cyan+)
	 (addstr (format nil "~s~%" (node-object node)))))
      (t
       (let ((*print-case* :downcase))
	 (addstr (format nil "~s~%" (node-object node)))))))
  (values))

(defun fake-reader (stream subchar arg)
  "A reader macro which should have no effect on the following form."
  (declare (ignore subchar arg))
  (read stream t nil t))

(defvar *safer-readtable* nil)

(defun safer-read (stream)
  (when (not *safer-readtable*)
    (setf *safer-readtable* (copy-readtable))
    ;; This should make it so that #. neither evals or errors but just
    ;;; reads the thing, unlike setting *read-eval* false.
    (set-dispatch-macro-character #\# #\. #'fake-reader *safer-readtable*))
  (let ((*readtable* *safer-readtable*))
    (package-robust-read stream nil nil)))

(defun fake-code-view (&optional (file (pick-list:pick-file)))
  "This shows why s-exps are cool."
  (with-open-file (stm file)
    (view-tree
     (convert-tree
      (append (list file)
	      (loop :with exp
		 :while (setf exp (safer-read stm))
		 :collect exp))
      :type 'code-node))))

;; (defun fake-code-view-dir (&optional (directory "."))
;;   "This shows why s-exps are cool."
;;   (view-tree
;;    (convert-tree
;;     (loop :for file :in (glob:glob (s+ directory "/*.lisp"))
;;        :collect
;;        (with-open-file (stm file)
;; 	 (append (list file)
;; 		 (loop :with exp
;; 		    :while
;; 		    (setf exp (safer-read stm))
;; 		    :collect exp))))
;;     :type 'code-node)))

(defun fake-code-view-files (&optional files)
  (view-tree
   (convert-tree
    (append (list "Files")
	    (loop :for file :in (or files (glob:glob "*.lisp"))
	       :collect
	       ;; @@@ (handler-case
	       (with-open-file (stm file)
		 (append (list file)
			 (loop :with exp
			    :while
			    (setf exp (safer-read stm))
			    :collect exp)))))
    :type 'code-node)))

;; This doesn't really work, because we need a reader that can handle
;; unknown package prefixes and other errors.
(defun fake-view-project (&rest files)
  (let* ((ff (or files '("*.lisp" "*.asd")))
	 (pp (or (loop :for f :in ff :appending (glob:glob f))
		 (glob:glob "*"))))
    (fake-code-view-files (pick-list:pick-list pp :multiple t))))

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
;; Your whole Lisp image browsing.

(defclass environment-node (object-node) ())
(defmethod display-node ((node environment-node) level)
  "Display an Lisp image environment node."
  (let ((str (with-output-to-string (stream)
	       (describe-environment stream))))
    (when (eql (char str (1- (length str))) #\newline)
      (setf (char str (1- (length str))) #\space))
    (display-object node str level)))

(defclass system-node (object-node) ())

(defmethod print-object ((object system-node) stream)
  "Print a system-node to STREAM."
  (if (or *print-readably* *print-escape*)
      (print-unreadable-object (object stream)
	(format stream "system-node ~w" (node-object object)))
      (let ((*standard-output* stream))
	(describe-system (asdf:find-system (node-object object))))))

(defmethod display-node ((node system-node) level)
  (let* (;;(sys (asdf:find-system (node-object node)))
	 ;; (str (with-output-to-string (*standard-output*)
	 ;; 	(describe-system sys))))
	 (str (princ-to-string node)))
    (when (eql (char str (1- (length str))) #\newline)
      (setf (char str (1- (length str))) #\space))
    (display-object node str level)))

(defclass systems-node (cached-dynamic-node) ())
(defun systems-contents (node)
  (declare (ignore node))
  (loop :for s :in (asdf:registered-systems)
     :collect
     (make-instance
      'object-node
      :object s :open nil
      :branches (list
		 (make-instance 'system-node
				:object s :open nil)))))

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

(defclass commands-node (cached-dynamic-node) ())

(defun commands-contents (node)
  (declare (ignore node))
  )

(defun lisp-image-contents (node)
  (declare (ignore node))
  (list
   (make-instance
    'object-node :object "Environment" :open nil
    :branches
    (list
     (make-instance
      'environment-node :object "Environment" :open nil :branches nil)))
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

;; EOF
