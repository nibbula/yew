;;;
;;; view-org.lisp - View Org Mode trees
;;;

(defpackage :view-org
  (:documentation "View Org Mode trees.")
  (:use :cl :dlib :collections :tree-viewer :terminal :inator :file-inator
	:table :table-print :terminal-table :ostring)
  (:export
   #:read-org-mode-file
   #:!view-org
   #:view-org
   ))
(in-package :view-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode

(defclass org-node (node)
  ()
  (:default-initargs
   :branches nil)
  (:documentation
   "Generic Org mode node."))

(defclass heading-node (org-node)
  ((heading
    :initarg :heading :accessor heading :initform "" :type string
    :documentation "Node heading."))
  (:documentation "Org heading node."))

(defmethod node-object ((node heading-node))
  (heading node))

(defclass body-node (org-node)
  ()
  (:documentation "An abstract node in the body of a heading."))

(defclass text-like-node (body-node)
  ((text
    :initarg :text :accessor text :initform nil
    :documentation "Text of node."))
  (:documentation "Generic node with text lines."))

(defmethod node-object ((node text-like-node))
  (text node))

(defclass text-node (text-like-node)
  ()
  (:default-initargs
   :open t)
  (:documentation "Org text lines."))

(defmethod (setf node-open) (value (node text-node))
  ;; Force it to always be open.
  (setf (slot-value node 'tb::open) t))

(defclass block-node (text-like-node)
  ((begin-tag :initarg :begin-tag :accessor begin-tag)
   (end-tag   :initarg :end-tag   :accessor end-tag)
   (language
    :initarg :language :accessor org-block-language :initform nil
    :documentation "Language the block is in."))
  (:documentation "Org mode collapsible block node."))

(defclass table-node (body-node)
  ((table
    :initarg :table :accessor table :initform nil
    :documentation "The table."))
  (:documentation "Org table node."))

(defmethod node-object ((node table-node))
  (table node))

(defparameter *default-colors*
  '(:white :red :green :yellow :blue :magenta :cyan))

(defun colors ()
  "Return a collection of colors used for level lines."
  (or (theme:value '(:program :org-mode :colors))
      *default-colors*))

(defun org-node-indicator (node)
  (if (node-has-branches node)
      (if (node-open node) #\- #\+)
      (if (text node)
	  (if (node-open node) #\- #\+)
	  #\·)))

#|
(defmethod print-object ((object org-node) stream)
  "Print an org-node to ‘stream’."
  (if (or *print-readably* *print-escape*)
      (print-unreadable-object (object stream)
	(format stream "org-node ~w" (node-object object)))
      (let ((*standard-output* stream))
	(format stream "~a ~a" (org-node-indicator object)
		(trim (org-node-heading object)))
	(when (node-open object)
	  (loop :for l :in (reverse (org-node-text object)) :do
	     (format stream "~%  ~a" (trim l)))
	  (terpri stream)))))
|#

(defclass org-table-renderer (terminal-box-table-renderer)
  ()
  (:documentation "Render org-mode tables."))

(defmethod write-box-line ((renderer org-table-renderer) style sizes)
  (multiple-value-bind (row col) (terminal-get-cursor-position *destination*)
    (declare (ignore col))
    ;; @@@ render clipping should really be solved at another level
    (when (< row (1- (terminal-window-rows *destination*)))
      (call-next-method))))

(defmethod table-output-row ((renderer org-table-renderer)
			     table row row-number &key sizes)
  (declare (ignorable sizes))
  (when (< (terminal-get-cursor-position *destination*)
	   (1- (terminal-window-rows *destination*)))
    (call-next-method)))

(defmethod table-output-row-separator ((renderer org-table-renderer) table
				       n &key width sizes)
  (declare (ignore width sizes))
  (when (< (terminal-get-cursor-position *destination*)
	   (1- (terminal-window-rows *destination*)))
    (call-next-method)))

(defmethod display-node ((node heading-node) level)
  "Display an org-node heading node."
  (with-accessors ((indent tb::indent)) *viewer*
    (let ((fake-level (max 0 (1- level))))
      (tt-color (elt (colors) (mod level (length (colors)))) :black)
      (display-node-line
       node (format nil "~v{~a~:*~} ~a ~a~%"
		    (* fake-level indent) '(#\space)
		    (if (node-has-branches node)
			(if (node-open node) #\- #\+)
			(if (node-branches node)
			    (if (node-open node) #\- #\+)
			    #\·))
		    (trim (heading node))))
      (tt-color :default :default))))

(defmethod display-node ((node text-node) level)
  "Display an org-node heading node."
  (with-accessors ((indent tb::indent)) *viewer*
    (let ((fake-level (max 0 (1- level))))
      (tt-set-rendition
       (style:styled-char
	(theme:value '(:program :org-mode :default :style)) #\x))
      (when (node-open node)
	(loop :for l :in (reverse (text node)) :do
          (display-node-line
	   node (format nil "~v{~a~:*~} ~a~%"
			(* fake-level indent) '(#\space)
			(trim (princ-to-string l)))))))))

(defmethod display-node ((node block-node) level)
  "Display an org-block node."
  (with-accessors ((indent tb::indent)) *viewer*
    (let ((fake-level (max 0 (1- level))))
      (flet ((display-tag (tag indicator)
	       (tt-color (elt (colors) (mod level (length (colors)))) :black)
	       (display-node-line node
	         (format nil "~v{~a~:*~} ~a~@[~a~]~%"
			 (* fake-level indent) '(#\space)
			 (trim tag)
			 (when indicator
			   (when (not (node-open node))
			     "..."))))
	       (tt-color :default :default)))
	(display-tag (begin-tag node) t)
	(tt-set-rendition
	 (style:styled-char
	  (theme:value '(:program :org-mode :default :style)) #\x))
	(when (node-open node)
	  (loop :for line :in (reverse (text node)) :do
	    ;; Just display with one space for our cursor, two for the parent
	    ;; node toggle, otherwise indent.
	    (display-node-line node (format nil "   ~a~%" line)))
	  (display-tag (end-tag node) nil))))))

(defmethod display-node ((node table-node) level)
  "Display an org-table node."
  (with-accessors ((indent tb::indent)) *viewer*
    (let ((fake-level (max 0 (1- level)))
	  (table (table node)))
      ;; @@@ It would probably be cooler if we could invoke the
      ;; table-editor here.
      (output-table table
		    ;; (make-instance 'terminal-table-renderer)
		    ;; (make-instance 'terminal-box-table-renderer
		    (make-instance 'org-table-renderer
				   :box-style table-print:*fancy-box*
				   :box-color #(:rgb 0 1 1)
				   :x (+ (* fake-level indent) 3))
		    *terminal*
		    :print-titles
		    ;; @@@ bogus
		    (string/=
		     (column-name (first (table-columns table)))
		     "Column0")
		    :long-titles t
		    ;; :max-width max-width
		    ;; :trailing-spaces trailing-spaces
		    ))))

(defun get-heading (line)
  "Return the part after the stars."
  (let ((start (position-if (_ (char/= _ #\*)) line)))
    (if (and start (< start (length line)))
	(subseq line start)
	"")))

(defun count-depth (line)
  "Count the number of stars at the beginning of the line."
  (loop :with i = 0 :and len = (length line)
     :while (and (< i len) (char= (char line i) #\*))
     :do (incf i)
     :finally (return i)))

(defstruct state
  "State of org parsing."
  stream			; org file
  root				; root node
  current			; current node we're building
  parent			; parent node
  parent-depth			;
  (depth 0)			;
  trimmed-line)			; the line without surrounding whitespace

(defun add-sub-node (state node &key new-depth)
  "Add a ‘node’ to the tree, as a sub-node of the current. If ‘new-depth’ is not
given, it defaults to one more the the current depth. ‘state’ is the tree
state structure."
  (with-slots (current parent parent-depth depth) state
    (push current parent)
    (push depth parent-depth)
    (setf (node-branches current)
	  (append (node-branches (first parent)) (list node))
	  current node
	  depth (or new-depth (1+ depth)))))

(defun add-sibling-node (state node)
  "Add ‘node’ as a sibling to the current node, and make it the current.
‘state’ is the tree state structure."
  (with-slots (current parent) state
    (setf (node-branches (first parent))
	  (append (node-branches (first parent)) (list node))
	  current node)))

(defgeneric add-node (state node)
  (:documentation "Add a node to the tree."))

(defmethod add-node (state (node body-node))
  "Add a body node."
  (etypecase (state-current state)
    (heading-node
     (add-sub-node state node))
    (body-node
     (add-sibling-node state node))))

(defun add-heading (state line)
  (with-slots (depth current parent parent-depth) state
    (let ((new-depth (count-depth line))
	  new)
      ;; (maybe-add-table state)
      ;; (format t "new-depth = ~a~%" new-depth)
      (cond
	((> new-depth depth)
	 (add-sub-node state (make-instance 'heading-node
					    :heading (get-heading line))
		       :new-depth new-depth))
	((= new-depth depth)
	 ;; (format t "=~%")
	 (add-sibling-node state
			   (make-instance 'heading-node
					  :heading (get-heading line))))
	((< new-depth depth)
	 ;; (format t "<~%")
	 (loop :do
		  (pop parent)
		  ;; (format t "pop ~a~%" (pop parent-depth))
		  (pop parent-depth)
		  ;; (format t "parent-depth ~a~%" parent-depth)
	       :while (and (first parent-depth)
			   (<= new-depth (first parent-depth))))
	 (setf new (make-instance 'heading-node
				  :heading (get-heading line))
	       (node-branches (first parent))
	       (append (node-branches (first parent)) (list new))
	       current new
	       depth new-depth))))))

(defun read-table-line (table line)
  "Convert the #\\| separated fields in ‘line’ to a trimmed list. If table is
not NIL, pad out the line with NILs to the table columns. If the column is all
dashes (a.k.a. #\\hyphen-minus), convert it to the symbol |-|."
  (let* ((cols (mapcar
		(_ (let ((result (trim _)))
		     (if (every (_ (char= #\- _)) result)
		      	 '- result)))
		;; (split-sequence #\| line)
		(ppcre:split "[|+]" line)
		))
	 (line-length (length cols))
	 (full-length (and table (length (table-columns table)))))
    (cond
      ((every (_ (eq '- _)) cols)
       nil)				; discard all dashes lines
      ((and table (< line-length full-length))
       (append cols (make-list (- full-length line-length))))
      (t
       cols))))

(defun add-table-line (state)
  (with-slots (trimmed-line current) state
    ;; Make the table node if we're not currently in a table.
    (when (not (subtypep (type-of current) 'table-node))
      (add-node state (make-instance 'table-node)))
    (let ((line
	    (progn
	      (setf trimmed-line (rtrim trimmed-line))
	      ;; Get rid of first bar, if the cell is empty.
	      (when (char= #\| (ochar trimmed-line 0))
		(setf trimmed-line (osubseq trimmed-line 1)))
	      ;; Get rid of last bar, if the cell is empty.
	      (when (and
		     (plusp (olength trimmed-line))
		     (char= #\| (ochar trimmed-line
				       (1- (olength trimmed-line)))))
		(setf trimmed-line
		      (osubseq trimmed-line
			       0 (1- (olength trimmed-line)))))
	      (read-table-line (table current) trimmed-line))))
      (when line
	(if (not (table current))
	    (setf (table current)
		  (make-table-from nil :column-names line))
	    (setf (container-data (table current))
		  ;; (nconc (container-data table) line)
		  (append (container-data (table current)) (list line))))))))

#|
(defun maybe-add-table (state)
  (with-slots (table current) state
    (when table
      (push table (node-branches current)))
    (setf table nil)))
|#

(defun add-block (state)
  (with-slots (current parent stream trimmed-line) state
    (let ((new (make-instance 'block-node
			      :begin-tag trimmed-line)))
      (add-node state new)
      (loop
	:with line :and got-end
        :while (and (not got-end)
		    (setf line (read-line stream nil nil)))
	:do
	(setf trimmed-line (ltrim line))
	(cond
	  ((begins-with "#+END" trimmed-line :test #'equalp)
	   (setf (end-tag new) trimmed-line
		 got-end t))
	  (t
	   (push line (text new))))))))

(defun add-text-line (state line)
  (with-slots (current parent) state
    ;; If we're not in a text node, make new text node sibling.
    (when (not (subtypep (type-of current) 'text-node))
      (add-node state (make-instance 'text-node)))
    (push line (text current))))

(defun read-org-mode-file (file)
  "Read ‘file’ and return the contents as a tree of ‘org-nodes’."
  (let ((state (make-state)))
    (with-slots (root current parent parent-depth depth table trimmed-line)
	state
      (setf root (make-instance 'heading-node :heading file)
	    current root
	    parent (list root))
      (with-open-file (stream file)
	(setf (state-stream state) stream)
	(loop
	  :with line
	  :while (setf line (read-line stream nil nil))
	  :do
	  ;; (format t "line = ~w~%" line)
	  (cond
	    ;; Headings
	    ((begins-with "*" line)
	     (add-heading state line))

	    ;; Tables
	    ((progn
	       (setf trimmed-line (ltrim line))
	       (and (plusp (length trimmed-line))
		    (char= (char trimmed-line 0) #\|)))
	     (add-table-line state))

	    ;; Blocks
	    ((begins-with "#+BEGIN" trimmed-line :test #'equalp)
	     (add-block state))

	    ;; Normal paragraph lines
	    (t
	     ;; (format t "text~%")
	     ;; (maybe-add-table state)
	     (add-text-line state line)
	     ))))
      root)))

(defun view-org (file)
  "View an org-mode ‘file’ with the tree viewer."
  (with-terminal ()
    (let ((tree (read-org-mode-file file)))
      (setf (node-open tree) t)
      (view-tree tree :viewer (make-instance 'org-viewer
					     :root tree
					     :file-name file)))))

(defclass org-viewer (tree-viewer file-inator)
  ()
  (:default-initargs
   :keymap `(,tree-viewer::*tree-keymap*
	     ,*default-file-inator-keymap*
	     ,*default-inator-keymap*
	     ))
  (:documentation "A viewer for Emacs Org Mode files."))

(defmethod revert-file ((o org-viewer))
  (with-slots ((file-name         file-inator::file-name)
	       (root              tree-viewer::root)
	       (current           tree-viewer::current)
	       (current-position  tree-viewer::current-position)
	       (top               tree-viewer::top)
	       (bottom            tree-viewer::bottom)
	       (left              tree-viewer::left)) o
    (if file-name
	(let ((new-root (read-org-mode-file file-name)))
	  (setf root new-root
		;; @@@ maybe we should have a freshen method?
		top root
		current top
		current-position nil
		;; bottom nil
		left 0)
	  (redraw o))
	(message o "This org tree doesn't seem to have a file name."))))

#+lish
(lish:defcommand view-org
  ((org-files pathname :repeating t :help "Org-mode files to view."))
  "View an Emacs Org mode file with the tree-viewer."
  (when (and lish:*input* (typep lish:*input* '(or string pathname list)))
    (setf org-files (append
		     (if (listp lish:*input*) lish:*input* (list lish:*input*))
		     org-files)))
  (block nil
    (with-file-list (file org-files)
      (with-terminal ()
	(let ((tree (read-org-mode-file file)))
	  (setf (node-open tree) t)
	  (when (= 1 (length (multiple-value-list
			      (view-tree tree
					 :viewer (make-instance
						  'org-viewer
						  :root tree
						  :file-name file)))))
	    (return)))))))

;; EOF
