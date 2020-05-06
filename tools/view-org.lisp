;;
;; view-org.lisp - View Org Mode trees
;;

(defpackage :view-org
  (:documentation "View Org Mode trees.")
  (:use :cl :dlib :tree-viewer :terminal :inator :file-inator)
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
	   (display-node-line node (s+ (format nil "~v,,,va "
					       (* fake-level indent)
					       #\space "")
				       "  "
				       (trim l)
				       #\newline)))))))

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

(defun read-org-mode-file (file)
  (let* ((root (make-instance 'org-node :heading file))
	 (cur root)
	 (parent (list root))
	 (parent-depth '())
	 (depth 0)
	 new)
    (with-open-file (stream file)
      (loop :with line
	 :while (setf line (read-line stream nil nil))
	 :do
	 ;; (format t "line = ~w~%" line)
	 (if (begins-with "*" line)
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
			depth new-depth))))
	     ;; Just a text line
	     (progn
	       ;; (format t "text~%")
	       (push line (org-node-text cur))))))
    root))

(defun view-org (file)
  "View an org-mode FILE with the tree viewer."
  (view-tree (read-org-mode-file file)))

(defclass org-viewer (tree-viewer file-inator)
  ()
  (:documentation "A viewer for Emacs Org Mode files."))

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
						:root tree)))))
	  (return))))))

;; EOF
