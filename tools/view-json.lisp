;;;
;;; view-json.lisp - foo
;;;

(defpackage :view-json
  (:documentation "View JavaScript Object Notation stupid bullshit.")
  (:use :cl :dlib-misc :opsys :unicode :lish :table)
  (:export
   #:json-to-table
   #:view-json
   #:!view-json
   ))
(in-package :view-json)

(defun json-to-table (string)
  "Convert the json string into a table. The json must be an array of objects."
  (let* ((cc (cdadr (jsown:parse string)))
	 (cols (let ((result nil))
                 (loop :for c :in cc :do
		   (when (not (eq (car c) :obj))
		     (error "This JSON doesn't seem like an object array."))
		   (loop :for (name . value) :in (cdr c) :do
		     (pushnew name result :test #'equal)))
                 (loop :for i :from 0
                       :for c :in (nreverse result)
                   :collect (cons c i)))))
    (make-table-from
     (loop :for c :in cc
       :collect
       (let ((row (make-array (length cols) :initial-element nil))
             (n 0))
         (loop :for (name . value) :in (cdr c)
           :do
           (if (setf n (cdr (assoc name cols :test #'equal)))
               (setf (aref row n) value)
	       (error "Property names changed?")))
	 row))
     :column-names (mapcar #'car cols))))

(defun view-json (thing &key as-table)
  "Look at JSON with the tree viewer."
  ;; CL-JSON version
  ;; (car (cl-json:decode-json stream))
  (let ((tree
	  (jsown:parse
	   (typecase thing
	     ((or pathname stream) (slurp thing))
	     (string
	      (if (nos:file-exists thing)
		  (slurp (nos:quote-filename thing))
		  thing))
	     (vector
	      (when (equal (array-element-type thing) '(unsigned-byte 8))
		(utf8b-bytes-to-string thing)))))))
    (if as-table
	(table-viwer:view-table
	(tree-viewer:view-tree tree))))

#+lish
(lish:defcommand view-json
  ((as-table boolean :short-arg #\t
    :help "View as a table. This will only work with suitable files.")
   (file input-stream-or-filename :default nil
    :help "A JSON file to view."))
  "Look at a JSON file with the tree viewer. Accepts a path name or a stream,
or uses *standard-input*."
  :accepts '(or stream string pathname)
  (let ((in (or file lish:*input*)))
    (cond
     ((and (vectorp in)
	   (equal (array-element-type in) '(unsigned-byte 8)))
      (view-json (utf8b-bytes-to-string in) :as-table as-table))
     (t
      (with-streamlike-input (file :use-stdin t)
	(view-json file :as-table as-table))))))

;; End
