;;
;; view-html.lisp - View HTML as a tree.
;;

;; This uses the excellent PLUMP library to read HTML/XML and the tree-viewer
;; to display it. Using the TREE-VIEWER generally consists of making node
;; type subclasses and display methods for them. Here we make one HTML-NODE
;; subclass of the tree-viewer's CACHED-DYNAMIC-NODE and use PLUMP methods to
;; dynamically pull out content from the parsed file. We make the PLUMP node
;; be the tree-viewer's OBJECT-NODE object.

;; TODO:
;;  - hook into pager to:
;;    - view as text
;;    - view as source text

(defpackage :view-html
  (:documentation "View HTML as a tree.")
  (:use :cl :dlib :dlib-misc :rl :inator :file-inator :tree-viewer
	:terminal :fui :char-util :keymap)
  (:export
   #:view-html
   #:*user-agent*
   ))
(in-package :view-html)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-windows (add-feature :use-drakma))

#+use-drakma
(defvar *user-agent* "Drakma"
  "User-agent for requests, defaulting not to spilling too much information.")

(defun html-node-contents (node)
  "Get the contents of an HTML-NODE, which is a list of PLUMP node children."
  (when (and node
	     (typep node 'plump::node)
	     (plump::has-child-nodes node))
    ;; The
    (map 'list #'identity
	 (remove-if (lambda (x)
		      (and (or (plump:text-node-p x)
			       (plump:textual-node-p x))
			   (= (length (dlib:trim (plump:text x))) 0)))
		    (plump::children node)))))

(defclass html-node (cached-dynamic-node)
  ()
  (:default-initargs
   :func #'html-node-contents)
  (:documentation "A tree node for plump HTML."))

(defstruct history-node
  "Record the place and time."
  resource
  time)

(defclass location-node (object-node)
  ((parent
    :initarg :parent :accessor location-node-parent
    :documentation "Parent node in the location tree."))
  (:documentation "Node in the location tree. The object is the resource name."))

(defclass html-viewer (tree-viewer)
  ((visited
    :initarg :visited :accessor html-viewer-visited
    :initform (make-hash-table :test #'equal) :type hash-table
    :documentation "Table of places visited.")
   (history
    :initarg :history :accessor html-viewer-history :initform nil :type list
    :documentation
    "A list of resources vistited. This records a path through the location
tree.")
   (location-tree
    :initarg :location-tree :accessor html-viewer-location-tree :initform nil
    :documentation "A navigation tree of where we've visted.")
   (location
    :initarg :location :accessor html-viewer-location :initform nil
    ;; :type location-node
    :documentation "The current location.")
   (cookies
    :initarg :cookies :accessor html-viewer-cookies :initform nil
    :documentation "The cookie jar."))
  (:documentation "A tree view of HTML/XML."))

(defmethod initialize-instance
    :after ((o html-viewer) &rest initargs &key &allow-other-keys)
  "Initialize a html-viewer."
  (declare (ignore initargs))
  ;; Add our own keymap.
  (when (not (find *view-html-keymap* (inator-keymap o)))
    (push *view-html-keymap* (inator-keymap o)))
  (setf (slot-value o 'cookies) (make-instance 'drakma:cookie-jar)))

(defun reset-view ()
  "Reset the viewer for viewing a different tree."
  (with-slots ((current tree-viewer::current)
	       (root    tree-viewer::root)
	       (top     tree-viewer::top)
	       (parents tree-viewer::parents)) *viewer*
    (setf parents (make-hash-table :test #'equal)
	  current root
	  top root)))

(defun add-to-history (uri)
  "Add a location to the linear history."
  (with-slots (visited history) *viewer*
    (let ((node (make-history-node :resource uri :time (get-dtime))))
      (setf (gethash uri visited) node)
      (push node history))))

(defun make-location-current (uri &key parent)
  (with-slots (location) *viewer*
    (setf location
	  (make-instance 'location-node :object uri :parent parent))))

(defun register-visit (uri &key parent)
  (with-slots (location) *viewer*
    (add-to-history uri)
    (make-location-current uri :parent (or parent location))))

(defun relative-uri (relative)
  "Return the full URI from a possibly relative URI."
  (with-slots (location) *viewer*
    (let* ((base (node-object location))
	   (base-uri (handler-case (puri:parse-uri base)
		       (puri:uri-parse-error (c)
			 (declare (ignore c))
			 nil)))
	   merged)
      (cond
	((and base-uri (member (puri:uri-scheme base-uri) '(:http :https)))
	 (setf merged (puri:merge-uris relative base-uri))
	 (values (puri:render-uri merged nil) merged))
	((and (not (puri:uri-scheme base-uri)) ; no scheme
	      (nos:file-exists base))
	 ;; @@@ should we even do this???? security issue?
	 (let ((new (nos:path-append (nos:path-directory-name base) relative)))
	   (and (nos:file-exists new)
		(values new nil))))))))

(defun coerce-to-parseable (file)
  "Make FILE into something parseable by PLUMP:PARSE."
  (block nil
    (etypecase file
      (string
       (let* ((path (pathname (nos:quote-filename file)))
	      uri)
	 (or (and path (nos:file-exists path) path)
	     (and (setf uri (handler-case
				(puri:parse-uri file)
			      (puri:uri-parse-error (c)
				(declare (ignore c))
				nil)))
		  (member (puri:uri-scheme uri) '(:http :https))
		  ;; @@@ We should probably check something like:
		  ;; (sb-unicode:confusable-p url
		  ;;   (sb-unicode:canonically-deconfuse url))
		  ;; and some site blacklist or something.
		  #+use-drakma
		  (drakma:http-request uri :user-agent *user-agent*)))))
      ((or pathname stream)
       file)
      (null
       #| (read-filename :prompt "HTML file: ") |#
       (pathname (nos:quote-filename
		  (or (pick-list:pick-file)
		      (return nil))))))))

(defun make-tree-from-document (location document parsed-document)
  "Make a tree-viewer tree from a parsed plump document."
  (make-instance
   'object-node
   :object (or (and location (princ-to-string location))
	       (princ-to-string document))
   :branches
   (loop :for n :in (map 'list #'identity (plump::children parsed-document))
      :if (or (not (or (plump:text-node-p n)
		       (plump:textual-node-p n)))
	      (> (length (dlib:trim (plump:text n))) 0))
      :collect
      (make-instance 'html-node :object n))))

(defun load-document (location)
  "Load the document at LOCATION. Return the tree."
  (with-slots ((root tree-viewer::root)) *viewer*
    (let* ((document (or (coerce-to-parseable location)
			 (return-from load-document nil)))
	   (parsed-document (plump:parse document))
	   (tree (make-tree-from-document location document parsed-document)))
      (setf root tree)
      (reset-view)
      ;; (register-visit location)
      tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Displaying

(defgeneric display-thing (obj stream)
  (:documentation "Display a plump element OBJ to STREAM."))

(defmethod display-thing ((obj t) stream)
  "Display an anything."
  (format stream "~(~a~) " (type-of obj)))

(defun print-attrubutes (obj stream)
  "Display an anything."
  (loop :for k :being :the :hash-keys :of (plump:attributes obj) :using
     (hash-value v) :do
     (format stream "~a='~a' " k v)))

(defparameter *attr-tags*
  #(:meta :link :script :a :form :input :img :div :span :frame)
  "Tags to print attributes for.")

(defmethod limited-display-thing ((obj plump-dom:element) stream)
  (let ((tag (keywordify (plump:tag-name obj))))
    (format stream "~(~a~) " (plump:tag-name obj))
    (when (position tag *attr-tags*)
      (print-attrubutes obj stream))))

(defmethod display-thing ((obj plump-dom:element) stream)
  (format stream "~(~a~) " (plump:tag-name obj))
  (print-attrubutes obj stream))

(defmethod display-thing ((obj plump-dom:doctype) stream)
  (format stream "~(~a~) ~a" (type-of obj) (plump-dom:doctype obj)))

(defmethod print-object ((object html-node) stream)
  "Print an html-node to STREAM."
  (let ((obj (node-object object)))
    (if (or *print-readably* *print-escape*)
	(print-unreadable-object (object stream)
	  (format stream "html-node ~w" obj))
	(let ((*standard-output* stream))
	  (when (or (plump:text-node-p obj) (plump:textual-node-p obj))
	    (justify-text (dlib:trim (plump:text obj)) :stream stream))))))

(defmethod display-node ((node html-node) level)
  "Display an HTML node."
  (let* ((obj (node-object node))
         (str (with-output-to-string (stream)
                (cond
                  ((plump:text-node-p obj)
		   (justify-text
		    (dlib:trim (plump:text obj))
		    :stream stream
		    ;;:prefix (make-string level :initial-element #\space)
		    ))
                  ((plump:textual-node-p obj)
                   ;;(format stream "~a" (dlib:trim (plump:text obj))))
                   (format stream "textual-node-p~%")
		   (justify-text
		    (dlib:trim (plump:text obj))
		    :stream stream
		    ;;:prefix (make-string level :initial-element #\space)
		    ))
                  (t
                   (display-thing obj stream))))))
    (display-object node str level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visiting

(defgeneric visit-thing (thing)
  (:documentation
   "Visit a thing, most likely in a URL, in a way that replaces
the current view with it, but lets us back."))

(defmethod visit-thing ((thing t))
  (display-text
   "What is it?"
   (list (format nil "I don't know how to visit a ~a." (type-of thing)))))

(defmacro external-visit (&body body)
  `(unwind-protect
	(progn
	  (terminal-end *terminal*)
	  (handler-case
	      ,@body
	    (simple-error (c)
	      (message *viewer* "Error: ~a" c))))
     (terminal-start *terminal*)
     (tt-clear)
     (tt-finish-output)))

(defmethod visit-thing ((thing plump-dom:element))
  (case (symbolify (plump:tag-name thing) :package :view-html)
    ((a link)
     (let* ((relative (gethash "href" (plump:attributes thing)))
	    (location (relative-uri relative)))
       (display-text
	"Visiting location"
	(list (format nil "Visiting location ~s -> ~a" relative location)
	      (with-output-to-string (s)
		(describe (html-viewer-location *viewer*) s))
	      (format nil "~s" (node-object (html-viewer-location *viewer*)))
	      ))
       ;; (view-html location)
       (load-document location)
       (register-visit location)
       ))
    (img
     (let ((relative (gethash "src" (plump:attributes thing))))
       (multiple-value-bind (location real-uri) (relative-uri relative)
	 (display-text
	  "Visiting image"
	  (list (format nil "Visiting image ~s -> ~a" relative location)
		(with-output-to-string (s)
		  (describe (html-viewer-location *viewer*) s))
		(s+ real-uri)
		(format nil "~s" (node-object (html-viewer-location *viewer*)))))
	 (add-to-history location)
	 (external-visit
	  (view-image:view-image
	   (if (puri:uri-p real-uri)
	       #+use-drakma (drakma:http-request real-uri
						 :user-agent *user-agent*)
	       #-use-drakma nil
	       location))))))
    (script
     (let ((relative (gethash "src" (plump:attributes thing))))
       (multiple-value-bind (location real-uri) (relative-uri relative)
	 (display-text
	  "Visiting script"
	  (list (format nil "Visiting script ~s -> ~a" relative location)
		(with-output-to-string (s)
		  (describe (html-viewer-location *viewer*) s))
		(format nil "~s" (node-object (html-viewer-location *viewer*)))))
	 (add-to-history location)
	 (external-visit
	  (pager:pager
	   (if (puri:uri-p real-uri)
	       #+use-drakma (drakma:http-request real-uri
						 :user-agent *user-agent*
						 :want-stream t)
	       #-use-drakma nil
	       location))))))
    (otherwise
     (display-text
      "Unknown tag"
      (list (format nil "I don't know how to visit a ~s tag."
		    (plump:tag-name thing)))))))

(defmethod visit-thing-command ((o html-viewer))
  (visit-thing (node-object (tree-viewer::current o))))

(defun back-command (o)
  "Load the previously viewed document."
  (with-slots (history) o
    (when history
      (pop history) ;; current
      (when history
	(let ((location (history-node-resource (first history))))
	  (load-document location)
	  (make-location-current location))))))

(defun history-command (o)
  (with-slots (history) o
    ;; (display-text
    ;;  "History"
    ;;  (loop :for h :in history
    ;; 	:collect (history-node-resource h)))
    (pick-list:pick-list (loop :for h :in history
			    :when h
			    :collect (history-node-resource h))
			 :popup t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defkeymap *view-html-ctrl-x-keymap*
  `((,(ctrl #\s)	. save-file)
    (,(ctrl #\f)	. load-file)))

(defkeymap *view-html-keymap*
  `((,(ctrl #\x)	. *view-ctrl-x-keymap*)
    (#\h		. history-command)
    (#\b		. back-command)
    (#\v		. visit-thing-command)))

;; Here we parse the whole file with PLUMP and make a tree-viewer tree out
;; of it.

(defun view-html (&optional file)
  (let ((*viewer* (or *viewer* (make-instance 'html-viewer)))
	tree)
    (unwind-protect
	 (progn
	   (setf tree (load-document file))
	   (make-location-current file)
	   (view-tree tree :viewer *viewer*))
      (when (streamp file)
	(close file)))))

#+lish
(lish:defcommand view-html
  ((things pathname :repeating t :help "File or URL to view as HTML."))
  "View HTML as a tree."
  (if (not things)
      (view-html *standard-input*)
      (with-file-list (file things)
	(when (= 1 (length (multiple-value-list (view-html file))))
	  (return)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plist viewr

#|
(defclass plist-node (html-node)
  ()
  (:default-initargs
   :func #'html-node-contents)
  (:documentation "A tree node for Apple PLists in XML."))

(defmethod display-node ((node plist-node) level)
  "Display a plist node."
  (addstr
   (format nil "~v,,,va~c "
	   (* level (indent *viewer*)) #\space ""
	   (if (node-branches node)
	       (if (node-open node) #\- #\+)
	       #\space)))
  (let* ((obj (node-object node)))
    (cond
      ((or (plump:text-node-p obj) (plump:textual-node-p obj))
       (addstr (dlib:trim (plump:text obj)))
       (addch (code-char #\newline)))
      (t
       (typecase obj
	 (plump-dom:element
	  (let ((tag (keywordify (plump:tag-name obj))))
	    (case tag
	      (:key
	       (addstr (format nil "~a: " (plump:tag-name obj))))
	      (:string
	       )
	      (:integer
	       )

    (display-object node str level)))

  
(defun view-plist (&optional file)
  (let* ((ff (or file
		 #| (read-filename :prompt "HTML file: ") |#
		 (pick-list:pick-file)
		 ))
	 (hh (plump:parse (pathname ff))))
    (view-tree
     (make-instance
      'object-node
      :object ff
      :branches
      (loop :for n :in (map 'list #'identity (plump::children hh))
	 :if (or (not (or (plump:text-node-p n)
			  (plump:textual-node-p n)))
		 (> (length (dlib:trim (plump:text n))) 0))
	 :collect
	 (make-instance 'plist-node :object n))))))

#+lish
(lish:defcommand view-html
    (("file" pathname))
  "View HTML as a tree."
  (view-html file))

|#

;; EOF
