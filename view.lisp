;;
;; view/open.lisp - Look at something.
;;

(defpackage :view
  (:documentation "Look at something.")
  (:use :cl :dlib :magic :pick-list)
  (:export
   #:view
   #:view-things
   #:!view
   #:!open
   ))
(in-package :view)

(defparameter *viewer-alist*
  `(
    (("text" . "html")       . (:view-html :view-html))
    (("application" . "xml") . (:view-html :view-html))
    ("image"                 . (:view-image :view-image))
    ("text"                  . (:pager :pager))
    ))

(defun get-viewer-func (type)
  "Return the viewer function for TYPE, or NIL if none."
  (labels ((get-func (cat name list)
	     (cdr (find-if
		   (_
		    (let ((thing (car _)))
		      (cond
			((atom thing)
			 (equal cat thing))
			((consp thing)
			 (and (equal cat (car thing))
			      (equal name (cdr thing)))))))
		   list))))
    ;; @@@ extend to support sublists of name and description
    (get-func (content-type-category type)
	      (content-type-name type)
	      *viewer-alist*)))

(defun view (thing)
  "Look at something."
  (let* ((type (guess-file-type thing))
	 (viewer (get-viewer-func type))
	 (pkg (first viewer))
	 (func (second viewer)))
    (if viewer
	(progn
	  (when (not (find-package pkg))
	    (asdf:oos 'asdf:load-op pkg))
	  (symbol-call pkg func thing))
	(cerror "Skip the thing."
		"No viewer for ~a of type ~a / ~a"
		thing
		(content-type-category type)
		(content-type-name type)))))

(defun view-things (things)
  (loop :for thing :in things :do
     (with-simple-restart (continue "View the next thing.")
       (view thing))))

;; This is really stubby yet.
#+lish
(lish:defcommand view
  ((things pathname :repeating t :optional t :help "The things to view."))
  "Look at something."
  (when (not things)
    (setf things (list (pick-file))))
  (view-things things))

;; EOF
