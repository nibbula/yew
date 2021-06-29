;;;
;;; view-json.lisp - foo
;;;

(defpackage :view-json
  (:documentation "View JavaScript Object Notation stupid bullshit.")
  (:use :cl :dlib-misc :opsys :unicode :lish)
  (:export
   #:view-json
   #:!view-json
   ))
(in-package :view-json)

(defun view-json (thing)
  "Look at JSON with the tree viewer."
  (tree-viewer:view-tree
   ;; CL-JSON version
   ;; (car (cl-json:decode-json stream))
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

#+lish
(lish:defcommand view-json
  ((file input-stream-or-filename :default nil
    :help "A JSON file to view."))
  "Look at a JSON file with the tree viewer. Accepts a path name or a stream,
or uses *standard-input*."
  :accepts '(or stream string pathname)
  (let ((in (or file lish:*input*)))
    (when (and (vectorp in)
	       (equal (array-element-type in) '(unsigned-byte 8)))
      (view-json (utf8b-bytes-to-string in))))
  (with-streamlike-input (file :use-stdin t)
    (view-json file)))

;; End
