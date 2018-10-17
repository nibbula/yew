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
  (:use :cl :dlib :dlib-misc :rl :inator :tree-viewer)
  (:export
   #:view-html
   #:*user-agent*
   ))
(in-package :view-html)

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

(defmethod display-thing ((obj plump-dom:element) stream)
  (let ((tag (keywordify (plump:tag-name obj))))
    (format stream "~(~a~) " (plump:tag-name obj))
    (when (position tag *attr-tags*)
      (print-attrubutes obj stream))))

(defmethod display-thing ((obj plump-dom:doctype) stream)
  (format stream "~(~a~) ~a" (type-of obj) (plump-dom:doctype obj)))

(defmethod print-object ((object html-node) stream)
  "Print a system-node to STREAM."
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

;; Here we parse the whole file with PLUMP and
;; 

(defun view-html (&optional file)
  (let* ((ff (or (coerce-to-parseable file) (return-from view-html nil)))
	 (hh (plump:parse ff)))
    (unwind-protect
      (progn
	(view-tree
	 (make-instance
	  'object-node
	  :object (or (and file (princ-to-string file))
		      (princ-to-string ff))
	  :branches
	  (loop :for n :in (map 'list #'identity (plump::children hh))
	     :if (or (not (or (plump:text-node-p n)
			      (plump:textual-node-p n)))
		     (> (length (dlib:trim (plump:text n))) 0))
	     :collect
	     (make-instance 'html-node :object n)))))
      (when (streamp file)
	(close file)))))

#+lish
(lish:defcommand view-html
  ((things pathname :repeating t :help "File or URL to view as HTML."))
  "View HTML as a tree."
  (with-file-list (file things)
    (when (= 1 (length (multiple-value-list (view-html file))))
      (return))))

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
