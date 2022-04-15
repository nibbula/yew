;;;
;;; view/open.lisp - Look at something.
;;;

(defpackage :view
  (:documentation "Look at something.")
  (:use :cl :view-generic :dlib :magic :pick-list :ostring)
  (:export
   #:*viewer-alist*
   #:*file-name-types*
   #:viewer-for
   #:set-viewers
   #:view-things
   #:view
   #:!view
   #:!open
   ))
(in-package :view)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

(defclass viewer ()
  ()
  (:documentation "Something which can view things with a MIME type."))

(defclass function-viewer (viewer)
  ((package
    :initarg :package :accessor function-viewer-package :initform nil
    :type symbol
    :documentation "A keyword designating the package of the function.")
   (function-name
    :initarg :function-name :accessor function-viewer-function-name
    :initform nil :type symbol
    :documentation "A keyword naming the function."))
  (:documentation "A viewer which is a Lisp function."))

(defclass command-viewer (viewer)
  ((command
    :initarg :command :accessor command-viewer-command :initform nil
    :documentation "A command to be invoked by a shell."))
  (:documentation "A viewer which is a system or shell command."))

;; @@@ This and the other data here, should really be kept somewhere else,
;; and probably done in a much more sophisticated way.
(defparameter *viewer-alist*
  `(
    (("text" . "html")       . (:view-html :view-html))
    (("application" . "xml") . (:view-html :view-html))
    (("text" . "org")        . (:view-org :view-org))
    (("text" . "csv")        . (:table-viewer :view-table-thing))
    (("text" . "neox")       . (:neox :neox-on-file))
    (("application" . "json") . (:view-json :view-json))
    (("application" . "x-directory") . (:dired :dired))
    (("application" . "octet-stream") . (:pager :binary-pager))
    ("video"		     . (:view-video :view-video))
    ("audio"		     . (:view-audio :view-audio))
    ("image"                 . (:view-image :view-image))
    ("text"                  . (:pager :pager))
    ))

(defparameter *file-name-types*
  `(("csv"          . ("text"  . "csv"))
    ("org"	    . ("text"  . "org"))
    ("nx"	    . ("text"  . "neox"))
    ("json"	    . ("application"  . "json"))
    ))

(defun find-in-list (cat name list)
  "Find the the type with ‘cat’ and ‘name’ in ‘list’."
  (let ((%cat (string-downcase cat))
	(%name (string-downcase name)))
    (find-if (_ (let ((thing (car _)))
		  (cond
		    ((atom thing)
		     (equal %cat thing))
		    ((consp thing)
		     (and (equal %cat (car thing))
			  (equal %name (cdr thing)))))))
	     list)))

(defun find-viewer (type)
  "Return the viewer function for ‘type’, or NIL if none."
    ;; @@@ extend to support sublists of name and description
  (cdr (find-in-list (content-type-category type)
		     (content-type-name type)
		     *viewer-alist*)))

(defun as-content-type (thing)
  (etypecase thing
    (content-type
     (find-viewer thing))
    (cons
     (make-content-type :category (car thing) :name (cdr thing)))))

(defun viewer-for (type-designator)
  "Return the viewer function for ‘type’, or NIL if none."
  (find-viewer (as-content-type type-designator)))

(defun set-viewer-for (type-designator viewer-designator)
  "Set the viewer for ‘type’ to ‘viewer-designator’, wher ‘type’ is a
content-type, and ‘viewer-designator’ can something accepted by
‘make-viewer-from’."
  (let* ((type (as-content-type type-designator))
	 (cat (string-downcase (content-type-category type)))
	 (name (and (content-type-name type)
		    (string-downcase (content-type-name type))))
	 (item (find-in-list cat name *viewer-alist*)))
    (if item
	(setf (cdr item) viewer-designator)
	(push (cons (if name
			(cons cat name)
			cat)
		    viewer-designator)
	      *viewer-alist*))))

(defsetf viewer-for set-viewer-for)

(defun set-viewers (list)
  "Set the viewers in ‘list’ which should be a list of conses of
(<type-designator> . <viewer-designator>)."
  (loop :for (type . viewer) :in list :do
    (setf (viewer-for type) viewer)))

;; @@@ This is very wrong.
(defun guess-file-name-type (thing)
  "Return the mime type for THING, which should probably be a filename."
  (loop :for (ext . type) :in *file-name-types*
     :when (ends-with (s+ "." ext) thing :test #'equalp)
     :do (return (make-content-type :name (cdr type) :category (car type)))))

(defun guess-type (thing)
  (or (guess-file-name-type thing)
      (guess-file-type thing)))

(define-condition no-viewer-error (simple-error)
  ()
  (:documentation "We can't find a viewer for something."))

(defgeneric invoke-viewer (viewer thing)
  (:documentation "Invoke a viewer for ‘thing’."))

(defmethod invoke-viewer ((viewer function-viewer) thing)
  (with-slots (package function-name) viewer
    (when (not (find-package package))
      (asdf:oos 'asdf:load-op package))
    (symbol-call package function-name thing)))

(defmethod invoke-viewer ((viewer command-viewer) thing)
  (with-slots (command) viewer
    (if (find-package :lish)
	(apply (find-symbol "!=" :lish) `(,@command ,thing))
	(apply #'dlib:run-system-command `(,@command ,thing)))))

(defun make-viewer-from (thing)
  "Return a viewer object from the viewer-designator ‘thing’, which can
designate a function, as a list of keywords (:<package> :<function-name>),
or a system command, designated by either a string, or a list of
(! command [arg]...) for commands with separate arguments."
  (cond
    ((or (stringp thing) (and (consp thing) (symbolp (first thing))
			      (equal (symbol-name (first thing)) "!")))
     (make-instance 'command-viewer
		    :command (if (stringp thing)
				 ;; this is troublesome
				 (split-sequence #\space thing :omit-empty t)
				 (rest thing))))
    ((and (consp thing) (keywordp (first thing)) (keywordp (second thing)))
     (make-instance 'function-viewer
		    :package (first thing) :function-name (second thing)))
    (t
     (error "Sorry, but I don't know how to make a viewer out of ~s." thing))))

(defun view-file (thing)
  "Look at a file."
  (let* ((type (guess-type thing))
	 (designator (find-viewer type))
	 (viewer))
    (if (and designator (setf viewer (make-viewer-from designator)))
	(invoke-viewer viewer thing)
	(cerror "Skip the thing."
		'no-viewer-error
		:format-control
		"No viewer for ~a of type ~a / ~a"
		:format-arguments
		`(,thing ,(content-type-category type)
			 ,(content-type-name type))))))

(defmethod view ((thing string))
  (view-file thing))

(defmethod view ((thing ostring))
  (view-file (ostring-simplify thing)))

(defmethod view ((thing pathname))
  (view-file (namestring thing)))

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
