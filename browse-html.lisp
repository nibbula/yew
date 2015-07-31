;;
;; browse-html.lisp - Browse HTML as a tree.
;;

(defpackage :browse-html
  (:documentation "Browse HTML as a tree.")
  (:use :cl :dlib :tiny-rl :tree-browser)
  (:export
   #:browse-html
   ))
(in-package :browse-html)

(defun html-node-contents (node)
  (when (and node
	     (typep node 'plump::node)
	     (plump::has-child-nodes node))
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
  (:documentation "An tree node for plump HTML."))

(defgeneric display-thing (obj stream)
  (:documentation "Display a plump element OBJ to STREAM."))

(defmethod display-thing ((obj t) stream)
  (format stream "~(~a~) " (type-of obj)))

(defun print-attrubutes (obj stream)
  (loop :for k :being :the :hash-keys :of (plump:attributes obj) :using
     (hash-value v) :do
     (format stream "~a='~a' " k v)))

(defmethod display-thing ((obj plump-dom:element) stream)
  (cond
    ((equal (plump:tag-name obj) "meta")
     (format stream "~(~a~) " (plump:tag-name obj))
     (print-attrubutes obj stream))
    (t
     (format stream "~(~a~)" (plump:tag-name obj)))))

(defmethod display-thing ((obj plump-dom:doctype) stream)
  (format stream "~(~a~) ~a" (type-of obj) (plump-dom:doctype obj)))

(defmethod display-node ((node html-node) level)
  "Display an HTML node."
  (let* ((obj (node-object node))
	 (str (with-output-to-string (stream)
		(cond
		  ((plump:element-p obj)
		   (display-thing obj stream))
		  ((plump:text-node-p obj)
		   (format stream "~a" (dlib:trim (plump:text obj))))
		  ((plump:textual-node-p obj)
		   (format stream "~a" (dlib:trim (plump:text obj))))
		  (t
		   (display-thing obj stream))))))
    (display-object node str level)))

(defun browse-html (&optional file)
  (let* ((ff (or file (read-filename :prompt "HTML file: ")))
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

;; EOF
