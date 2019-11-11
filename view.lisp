;;
;; view/open.lisp - Look at something.
;;

(defpackage :view
  (:documentation "Look at something.")
  (:use :cl :view-generic :dlib :magic :pick-list)
  (:export
   #:view-things
   #:view
   #:!view
   #:!open
   ))
(in-package :view)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

;; @@@ This and the other data here, should really be kept somewhere else,
;; and probably done in a much more sophisticated way.
(defparameter *viewer-alist*
  `(
    (("text" . "html")       . (:view-html :view-html))
    (("application" . "xml") . (:view-html :view-html))
    (("text" . "org")        . (:view-org :view-org))
    (("text" . "csv")        . (:table-viewer :view-table-thing))
    (("text" . "neox")       . (:neox :neox-on-file))
    ("image"                 . (:view-image :view-image))
    ("text"                  . (:pager :pager))
    ))

(defparameter *file-name-types*
  `(("csv"          . ("text"  . "csv"))
    ("org"	    . ("text"  . "org"))
    ("nx"	    . ("text"  . "neox"))
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

;; @@@ This is very wrong.
(defun guess-file-name-type (thing)
  "Return the mime type for THING, which should probably be a filename."
  (loop :for (ext . type) :in *file-name-types*
     :when (ends-with (s+ "." ext) thing :test #'equalp)
     :do (return (make-content-type :name (cdr type) :category (car type)))))

(defun guess-type (thing)
  (or (guess-file-name-type thing)
      (guess-file-type thing)))

(defun view-file (thing)
  "Look at a file."
  (let* ((type (guess-type thing))
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

(defmethod view ((thing string))
  (view-file thing))

(defmethod view ((thing pathname))
  (view-file thing))

(defmethod view ((thing stream))
  (view-file thing))

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
    (setf things (or (and lish:*input*
			  (if (listp lish:*input*)
			      lish:*input*
			      (list lish:*input*)))
		     (list (pick-file)))))
  (view-things things))

;; EOF
