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
  ((heading
    :initarg :heading :accessor org-node-heading :initform "" :type string
    :documentation "Node heading.")
   (text
    :initarg :text :accessor org-node-text :initform nil
    :documentation "Text of node."))
  (:default-initargs
   :branches nil)
  (:documentation
   "Org mdoe node."))

(defparameter *org-colors*
  ;; `(,+color-white+ ,+color-red+ ,+color-green+ ,+color-yellow+ ,+color-blue+
  ;;   ,+color-magenta+ ,+color-cyan+))
  '(:white :red :green :yellow :blue :magenta :cyan))

(defmethod node-object ((node org-node))
  (org-node-heading node))

(defun org-node-indicator (node)
  (if (node-has-branches node)
      (if (node-open node) #\- #\+)
      (if (org-node-text node)
	  (if (node-open node) #\- #\+)
	  #\·)))

(defmethod print-object ((object org-node) stream)
  "Print a system-node to STREAM."
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

(defmethod display-node ((node org-node) level)
  "Display an org-node."
  (with-accessors ((indent tb::indent)) *viewer*
    (let ((fake-level (max 0 (1- level))))
      (tt-color (elt *org-colors* (mod level (length *org-colors*))) :black)
      (display-node-line node (s+ (format nil "~v,,,va "
					  (* fake-level indent)
					  #\space "")
				  (if (node-has-branches node)
				      (if (node-open node) #\- #\+)
				      (if (org-node-text node)
					  (if (node-open node) #\- #\+)
					  #\·))
				  #\space
				  (trim (org-node-heading node))
				  #\newline))
      (tt-color :white :black)
      (when (node-open node)
	(loop :for l :in (reverse (org-node-text node)) :do
	   (typecase l
	     (table
	      ;; @@@ It would probably be cooler if we could invoke the
	      ;; table-editor here.
	      (output-table l
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
			     (column-name (first (table-columns l)))
			     "Column0")
			    :long-titles t
			    ;; :max-width max-width
			    ;; :trailing-spaces trailing-spaces
			    ))
	     (t
	      (display-node-line node (s+ (format nil "~v,,,va "
						  (* fake-level indent)
						  #\space "")
					  "  "
					  (trim (princ-to-string l))
					  #\newline)))))))))

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

(defun read-table-line (table line)
  "Convert the #\\| separated fields in LINE to a trimmed list. If table is
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
       nil)
      ((and table (< line-length full-length))
       (append cols (make-list (- full-length line-length))))
      (t
       cols))))

(defun read-org-mode-file (file)
  "Read FILE and return the contents as a tree of ORG-NODES."
  (let* ((root (make-instance 'org-node :heading file))
	 (cur root)
	 (parent (list root))
	 (parent-depth '())
	 (depth 0)
	 table trimmed-line
	 new)
    (flet ((maybe-add-table ()
	     (when table
	       (push table (org-node-text cur)))
	       (setf table nil)))
      (with-open-file (stream file)
	(loop :with line
	   :while (setf line (read-line stream nil nil))
	   :do
	   ;; (format t "line = ~w~%" line)
	   (cond
	     ((begins-with "*" line)
	      (maybe-add-table)
	      (let ((new-depth (count-depth line)))
		;; (format t "new-depth = ~a~%" new-depth)
		(cond
		  ((> new-depth depth)
		   ;; (format t ">~%")
		   (push cur parent)
		   (push depth parent-depth)
		   ;; (format t "parent-depth ~a~%" parent-depth)
		   (setf new (make-instance 'org-node
					    :heading (get-heading line))
			 (node-branches cur)
			 (append (node-branches cur) (list new))
			 cur new
			 depth new-depth))
		  ((= new-depth depth)
		   ;; (format t "=~%")
		   (setf new (make-instance 'org-node
					    :heading (get-heading line))
			 (node-branches (first parent))
			 (append (node-branches (first parent)) (list new))
			 cur new))
		  ((< new-depth depth)
		   ;; (format t "<~%")
		   (loop :do
		      (pop parent)
		      ;; (format t "pop ~a~%" (pop parent-depth))
		      (pop parent-depth)
		      ;; (format t "parent-depth ~a~%" parent-depth)
		      :while (and (first parent-depth)
				  (<= new-depth (first parent-depth))))
		   (setf new (make-instance 'org-node
					    :heading (get-heading line))
			 (node-branches (first parent))
			 (append (node-branches (first parent)) (list new))
			 cur new
			 depth new-depth)))))
	     ((progn
		(setf trimmed-line (ltrim line))
		(and (plusp (length trimmed-line))
		     (char= (char trimmed-line 0) #\|)))
	      (let ((line
		     (progn
		       (setf trimmed-line (rtrim trimmed-line))
		       ;; Get rid of first bar, if the cell is empty.
		       (when (char= #\| (ochar trimmed-line 0))
			 (setf trimmed-line (osubseq trimmed-line 1)))
		       ;; Get rid of last bar, if the cell is empty.
		       (when (char= #\| (ochar trimmed-line
					       (1- (olength trimmed-line))))
			 (setf trimmed-line
			       (osubseq trimmed-line
					0 (1- (olength trimmed-line)))))
		       (read-table-line table trimmed-line))))
		(when line
		  (if (not table)
		      ;; (setf table (make-table-from (list line)))
		      (setf table (make-table-from nil :column-names line))
		      (setf (container-data table)
			    ;; (nconc (container-data table) line)
			    (append (container-data table) (list line))
			    )))))
	     (t ;; Just some other text line
	      (progn
		;; (format t "text~%")
		(maybe-add-table)
		(push line (org-node-text cur))))))))
    root))

(defun view-org (file)
  "View an org-mode FILE with the tree viewer."
  (let ((tree (read-org-mode-file file)))
    (view-tree tree :viewer (make-instance 'org-viewer :root tree
					   :file-name file))))

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
      (let ((tree (read-org-mode-file file)))
	(when (= 1 (length (multiple-value-list
			    (view-tree tree
				       :viewer (make-instance
						'org-viewer
						:root tree
						:file-name file)))))
	  (return))))))

;; EOF
