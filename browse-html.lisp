;;
;; browse-html.lisp - Browse HTML as a tree.
;;

;; This uses the excellent PLUMP library to read HTML/XML and the tree-browser
;; to display it. Using the TREE-BROWSER generally consists of making node
;; type subclasses and display methods for them. Here we make one HTML-NODE
;; subclass of the tree-browser's CACHED-DYNAMIC-NODE and use PLUMP methods to
;; dynamically pull out content from the parsed file. We make the PLUMP node
;; be the tree-browser's OBJECT-NODE object.

(defpackage :browse-html
  (:documentation "Browse HTML as a tree.")
  (:use :cl :dlib :dlib-misc :tiny-rl :tree-browser)
  (:export
   #:browse-html
   ))
(in-package :browse-html)

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
  #(:meta :link :script :a :form :input :img :div :span)
  "Tags to print attributes for.")

(defmethod display-thing ((obj plump-dom:element) stream)
  (let ((tag (keywordify (plump:tag-name obj))))
    (format stream "~(~a~) " (plump:tag-name obj))
    (when (position tag *attr-tags*)
      (print-attrubutes obj stream))))

(defmethod display-thing ((obj plump-dom:doctype) stream)
  (format stream "~(~a~) ~a" (type-of obj) (plump-dom:doctype obj)))

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

;; Here we parse the whole file with PLUMP and
;; 

(defun browse-html (&optional file)
  (let* ((ff (or file
		 #| (read-filename :prompt "HTML file: ") |#
		 (pick-list:pick-file)
		 ))
	 (hh (plump:parse (pathname ff))))
    (browse-tree
     (make-instance
      'object-node
      :object ff
      :branches
      (loop :for n :in (map 'list #'identity (plump::children hh))
	 :if (or (not (or (plump:text-node-p n)
			  (plump:textual-node-p n)))
		 (> (length (dlib:trim (plump:text n))) 0))
	 :collect
	 (make-instance 'html-node :object n))))))

#+lish
(lish:defcommand browse-html
    (("file" pathname))
  "Browse HTML as a tree."
  (browse-html file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plist browser

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
	   (* level (indent *browser*)) #\space ""
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

  
(defun browse-plist (&optional file)
  (let* ((ff (or file
		 #| (read-filename :prompt "HTML file: ") |#
		 (pick-list:pick-file)
		 ))
	 (hh (plump:parse (pathname ff))))
    (browse-tree
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
(lish:defcommand browse-html
    (("file" pathname))
  "Browse HTML as a tree."
  (browse-html file))

|#

;; EOF
