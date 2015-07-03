;;
;; tree-browser.lisp - Browse trees.
;;

;; TODO:
;;  - It would be best if the generic browser didn't rely on any output
;;    such as curses, but I would need that thing, so I'll have to convert
;;    it later.

(defpackage :tree-browser
  (:documentation "Browse trees.")
  (:nicknames :tb)
  (:use :cl :dlib :curses :char-util :keymap #| :fui |#)
  (:export
   #:browse-tree
   ))
(in-package :tree-browser)

(declaim (optimize (speed 3) (safety 0) (debug 0) (space 3) (compilation-speed 0)))

(defclass node ()
  ((branches
    :initarg :branches :accessor node-branches
    :documentation "Sequence of nodes which are the branches.")
   (open
    :initarg :open :accessor node-open :initform  :type 
    :documentation "True if this node is open."))
  (:documentation
   "A generic node in a browseable tree."))

(defclass object-node (node)
  ((object
    :initarg :object :accessor node-object :initform nil
    :documentation "The object in the node."))
  (:documentation "A tree node with an object."))

(defgeneric node-has-branches (node)
  (:documentation "Return true if the node supposedly has branches."))

(defmethod node-has-branches ((node node))
  "Return true if the node has branches."
  ;; Static nodes just check the branches slot.
  (node-branches node))

(defvar *map-tree-count* nil
  "How many nodes we've processed with map-tree.")

(defvar *map-tree-max-count* nil
  "When to stop processing with map-tree.")

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

(defun map-tree (func node &key (max-count 1000))
  (let ((*map-tree-count* 0)
	(*map-tree-max-count* max-count))
    (%map-tree func node)))

(defun close-all-subnodes (node)
  (map-tree #'(lambda (x) (setf (node-open x) nil)) node))

(defun open-all-subnodes (node)
  (map-tree #'(lambda (x) (setf (node-open x) t)) node))

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

(defclass tree-browser ()
  ((quit-flag
    :initarg :quit-flag :accessor quit-flag :initform nil :type boolean
    :documentation "True to quit.")
   (picked-object
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
    :initarg :top :accessor top :initform nil :type (or null integer)
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
  (:documentation "A tree browser."))

(defmethod initialize-instance
    :after ((o tree-browser) &rest initargs &key &allow-other-keys)
  "Initialize a tree-browser."
  (declare (ignore initargs))
  (with-slots (parents current root top) o
    (when (not parents)
      (setf parents (make-hash-table :test #'equal)))
    (when (slot-boundp o 'root)
      (setf current root
	    top root))))

(defvar *browser* nil
  "The current tree browser.")

(defun get-parent (node)
  "Return the parent of the given NODE in the current tree-browser, if it's
been encountered."
  (gethash node (parents *browser*)))

(defun set-parent (node parent)
  "Set the PARENT of the given NODE in the current tree-browser."
  (setf (gethash node (parents *browser*)) parent))

(defun quit ()
  "Quit the tree browser."
  (setf (quit-flag *browser*) t))

(defun pick-object ()
  "Pick the current object and return it."
  (with-slots (quit-flag picked-object current) *browser*
    (setf quit-flag t
	  picked-object current)))

(defun toggle ()
  (with-slots (current) *browser*
    (setf (node-open current) (not (node-open current)))))

(defun cycle ()
  (with-slots (current) *browser*
    (if (node-open current)
	(if (all-subnodes-open-p current)
	    (close-all-subnodes current)
	    (open-all-subnodes current))
	(setf (node-open current) t))))

(defun open-node ()
  (with-slots (current) *browser*
    (setf (node-open current) t)))

(defun close-node ()
  (with-slots (current) *browser*
    (setf (node-open current) nil)))

(defun close-all-subnodes-command ()
  (close-all-subnodes (current *browser*)))

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

(defun next-node ()
  "Set current to the next node in the linearized tree."
  (with-slots (current bottom-node top scroll-hint) *browser*
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

(defun previous-node ()
  "Set current to the previous node in the linearized tree."
  (with-slots (current top scroll-hint) *browser*
    (setf scroll-hint :up)
    (when (eq current top)
      (setf top (or (find-previous-node top) top)))
    (let ((prev (find-previous-node current)))
      (when prev
	(setf current prev)))))

(defun forward-some (&optional (n 15))
  (with-slots (current top bottom-node scroll-hint) *browser*
    (setf (scroll-hint *browser*) :down)
    (loop :with after-bottom
       :for i :from 1 :to n
       :while (next-node)
       :do
       (when (eq current bottom-node)
	 (setf after-bottom t))
       (when after-bottom
	 (setf top (find-next-node top))))))

(defun backward-some (&optional (n 15))
  (setf (scroll-hint *browser*) :up)
  (loop :for i :from 1 :to n
     :while (previous-node)))

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

(defun next-hierarchical-node ()
  (with-slots (current bottom-node top scroll-hint) *browser*
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

(defun previous-hierarchical-node ()
  (with-slots (current top scroll-hint) *browser*
    (setf scroll-hint :up)
    (when (eq current top)
      (setf top (or (find-previous-node top) top)))
    (let ((prev (find-previous-hierarchical-node current)))
      (when prev
	(setf current prev)))))

;; Page movement is approximate.

(defun next-page ()
  (with-slots (scroll-hint) *browser*
    (setf (scroll-hint *browser*) :down)
    (loop :for i :from 1 :to curses:*lines*
       :while (next-node))))

(defun previous-page ()
  (backward-some curses:*lines*))

(defun shift-left ()
  (decf (left *browser*) 10))

(defun shift-right ()
  (incf (left *browser*) 10))

(defun goto-first-node ()
  (with-slots (current root top) *browser*
    (setf current root
	  top root)))

(defun goto-bottom-node ()
  (with-slots (current root top scroll-hint) *browser*
    (let ((last (last-open-subnode (node-branches root)))) 
      (setf current last
	    ;; We could jus set scroll-hint :down here, but that can be very
	    ;; inefficient, so let's go to the end and then go backwards some.
	    top current)
      (backward-some *lines*)
      (setf current last
	    scroll-hint :down))))

(defun toggle-modeline ()
  (setf (show-modeline *browser*) (not (show-modeline *browser*))))

(defun node-info ()
  (with-slots (current) *browser*
    (fui:display-text
     "Node Info"
     (list (format nil "Node: ~a" current)
	   (format nil " open     : ~a" (node-open current))
	   (format nil " branches : ~a" (node-branches current))
	   (format nil " parent   : ~a" (get-parent current))))))

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
      (:home		. goto-first-node)
      (#\>		. goto-bottom-node)
      (:end		. goto-bottom-node)
      (:left		. shift-left)
      (:right	     	. shift-right)
      ;; Miscellaneous
      (#\m		. toggle-modeline)
      (,(ctrl #\L)	. redraw)
      (#\i		. node-info)))

(defun show-message (format-string &rest format-args)
  "Display a formatted message."
  (move (- *lines* 1) 2)
  (clrtoeol)
  (addstr (apply #'format nil format-string format-args)))

(defun message (format-string &rest format-args)
  (setf (message-string *browser*)
	(apply #'format nil format-string format-args)))

(defun message-pause (format-string &rest format-args)
  "Display a formatted message and wait for a key."
  (apply #'show-message format-string format-args)
  (refresh)
  (fui:get-char))

(defun perform-key (key &optional (keymap *tree-keymap*))
  ;; Convert positive integer keys to characters
  (when (and (integerp key) (>= key 0))
    (setf key (code-char key)))
  (let ((binding (key-binding key keymap)))
    (cond
      ((not binding)
       (message "No binding for ~a" key))
      ((symbolp binding)
       (cond
	 ((fboundp binding)
	  (funcall binding))
	 ((keymap-p (symbol-value binding))
	  (show-message (princ-to-string (nice-char key)))
	  (perform-key (fui:get-char) (symbol-value binding)))
	 (t
	  (error "Unbound symbol ~s in keymap" binding))))
      ((consp binding)
       (apply (car binding) (cdr binding)))
      (t
       (error "Weird thing ~s in keymap" binding)))))

(defgeneric display-indent (node level)
  (:documentation
   "Display the normal indentation for a node."))

(defmethod display-indent ((node node) level)
  "Display the normal indentation for a node."
  (addstr (format nil "~v,,,va  " (* level (indent *browser*)) #\space "")))

(defgeneric display-prefix (node level)
  (:documentation
   "Display the normal indentation and open / close indicator."))

(defmethod display-prefix ((node node) level)
  "Display the normal indentation and open / close indicator."
  (addstr
   (format nil "~v,,,va~c " (* level (indent *browser*)) #\space ""
	   (if (node-branches node)
	       (if (node-open node) #\- #\+)
	       #\space))))

(defgeneric display-node (node level)
  (:documentation "Display a node."))

(defmethod display-node :before ((node node) level)
  (when (eq node (current *browser*))
    (setf (current-position *browser*) (getcury *stdscr*)))
  (setf (bottom-node *browser*) node))

(defmethod display-node ((node object-node) level)
  "Display an object node. The object is printed to a string as with PRINC,
and indented properly for multi-line objects."
  (let ((lines (split-sequence #\newline (princ-to-string (node-object node)))))
    (when (eq node (current *browser*))
      (attron +a-bold+))
    (display-prefix node level)
    (addstr (format nil "~a~%" (first lines)))
    (when (eq node (current *browser*))
      (attroff +a-bold+))
    (loop :for l :in (cdr lines) :do
       (display-indent node level)
       (addstr (format nil "~a~%" l)))))

(defgeneric display-tree (browser tree level)
  (:documentation "Display a tree brower tree."))

(defvar *display-start* nil
  "True if we hit the top node.")

(defmethod display-tree ((browser tree-browser) tree level)
  "Display a tree."
  (with-slots (#|current current-position left |# top bottom) *browser*
    (when (< (getcury *stdscr*) bottom)
      (when (eq tree top)
	(setf *display-start* t))
      (when *display-start*
	(display-node tree level))
      (if (and (node-open tree) (node-branches tree))
	  (loop :for n :in (node-branches tree) :do
	     (set-parent n tree)
	     (display-tree browser n (1+ level)))))))

(defun redraw ()
  (with-slots (top current root) *browser*
    (clear)
    (move 0 0)
    (let ((*display-start* nil))
      (display-tree *browser* root 0))
    (setf top current)))

;; @@@ just for debugging
(defun node-abbrev (node)
  (when node
    (let ((str (format nil "~w" (node-object node))))
      (subseq str 0 (min 15 (length str))))))

(defun browse-tree (tree)
  "Look at a tree, with expandable and collapsible branches."
  (fui:with-curses
    (when (listp tree)
      (setf tree (convert-tree tree)))
    (let ((*browser* (make-instance 'tree-browser
				    :root tree
				    :bottom (- *lines* 2))))
      (with-slots (root quit-flag picked-object current left top bottom
		   bottom-node current-position message-string scroll-hint)
	  *browser*
	(loop :do
	   (tagbody
	    again
	      (move 0 0)
	      (erase)
	      (setf current-position nil)
	      (let ((*display-start* nil))
		(display-tree *browser* root 0))
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
		(show-message message-string)
		(setf message-string nil))
	      (when (show-modeline *browser*)
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
	      (perform-key (fui:get-char))
	      ;; bounds checking
	      (when (< left 0)
		(setf left 0)))
	   :while (not quit-flag))
	picked-object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some examples

(defclass foo-node (object-node)
  ())

(defmethod display-node ((node foo-node) level)
  "Display a foo node."
  (addstr 
   (format nil "~v,,,va~c ~a~%" (* level (indent *browser*)) #\space ""
	   (if (node-branches node)
	       (if (node-open node) #\- #\+)
	       #\space)
	   (node-object node)))
  (when (node-open node)
    (let* ((info (nos:stat (node-object node)))
	   (prefix (format nil "~v,,,va  "
			   (* level (indent *browser*)) #\space ""))
	  (string
	   (with-slots ((size nos::size)
			(access-time nos::access-time)
			(modify-time nos::modify-time)
			(change-time nos::change-time)
			(birth-time nos::birth-time)) info
	     (with-output-to-string (str)
	       (format str "~a Size:        ~d~%" prefix size)
	       (format str "~a Access time: ~a~%" prefix
		       (dlib-misc:date-string
			:time (nos:unix-to-universal-time
			       (nos:timespec-seconds access-time))))
	       (format str "~a Mod time:    ~a~%" prefix
		       (dlib-misc:date-string
			:time (nos:unix-to-universal-time
			       (nos:timespec-seconds modify-time))))
	       (format str "~a Change time: ~a~%" prefix
		       (dlib-misc:date-string
			:time (nos:unix-to-universal-time
			       (nos:timespec-seconds change-time))))
	       (format str "~a Birth time:  ~a~%" prefix
		       (dlib-misc:date-string
			:time (nos:unix-to-universal-time
			       (nos:timespec-seconds birth-time))))
	       (finish-output str)))))
      (addstr string))))

(defun browse-foo ()
  (browse-tree
   (convert-tree (fui:make-tree "." #'fui::subdirs)
		 :type 'foo-node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass lisp-node (object-node) ())
(defclass environment-node (object-node) ())
(defclass systems-node (object-node) ())
(defclass packages-node (object-node) ())
(defclass classes-node (object-node) ())
(defclass commands-node (object-node) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Code browsing

(defclass code-node (object-node)
  ())

(defmacro with-fg ((color) &body body)
  `(progn
     (color-set (fui:color-index ,color +color-black+)
		(cffi:null-pointer))
     ,@body
     (color-set (fui:color-index +color-white+ +color-black+)
		(cffi:null-pointer))))

(defmethod display-node ((node code-node) level)
  (addstr
   (format nil "~v,,,va~c "
	   (* level (indent *browser*)) #\space ""
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
	       (with-fg (+color-magenta+)
		 (addstr (format nil "~(~s~) " (node-object node))))
	       (with-fg (+color-green+)
		 (addstr (format nil "~(~a~)" name)))
	       (addstr (format nil " (~{~(~w~)~^ ~})~%" args))))
	 ((defstruct defclass deftype defvar defparameter defconstant)
	  (with-fg (+color-magenta+)
	    (addstr (format nil "~(~s~)" (node-object node))))
	  (with-fg (+color-green+)
	    (addstr (format nil " ~(~a~)~%" (get-name node)))))
      	 (otherwise
	  (let* ((pkg (symbol-package (node-object node)))
		 (pkg-name (and pkg (package-name pkg))))
	    (cond
	      ((equal pkg-name "COMMON-LISP")
	       (with-fg (+color-magenta+)
		 (addstr (format nil "~(~s~)~%" (node-object node)))))
	      ((equal pkg-name "KEYWORD")
	       (with-fg (+color-blue+)
		 (addstr (format nil "~(~s~)~%" (node-object node)))))
	      (t
	       (addstr (format nil "~(~s~)~%" (node-object node)))))))))
      ((stringp (node-object node))
       (with-fg (+color-cyan+)
	 (addstr (format nil "~s~%" (node-object node)))))
      (t
       (let ((*print-case* :downcase))
	 (addstr (format nil "~s~%" (node-object node))))))))

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
    (read stream nil nil)))

(defun fake-code-browse (&optional (file (fui:pick-file)))
  "This shows why s-exps are cool."
  (with-open-file (stm file)
    (browse-tree
     (convert-tree
      (append (list file)
	      (loop :with exp
		 :while (setf exp (safer-read stm))
		 :collect exp))
      :type 'code-node))))

;; (defun fake-code-browse-dir (&optional (directory "."))
;;   "This shows why s-exps are cool."
;;   (browse-tree
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

(defun fake-code-browse-files (&optional files)
  "This shows why s-exps are cool."
  (browse-tree
   (convert-tree
    (append (list "Files")
	    (loop :for file :in (or files (glob:glob "*.lisp"))
	       :collect
	       (with-open-file (stm file)
		 (append (list file)
			 (loop :with exp
			    :while
			    (setf exp (safer-read stm))
			    :collect exp)))))
    :type 'code-node)))

;; EOF
