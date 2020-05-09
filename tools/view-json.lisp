;;;
;;; view-json.lisp - foo
;;;

(defpackage :view-json
  (:documentation "View JavaScript Object Notation stupid bullshit.")
  (:use :cl :dlib-misc :lish)
  (:export
   #:view-json
   #:!view-json
   ))
(in-package :view-json)

(defun view-json (file)
  "Look at JSON with the tree viewer."
  (tree-viewer:view-tree
   ;; CL-JSON version
   ;; (car (cl-json:decode-json stream))
   (jsown:parse (slurp file))
   ))

#+lish
(lish:defcommand view-json
  ((file input-stream-or-filename :default nil
    :help "A JSON file to view."))
  "Look at a JSON file with the tree viewer. Accepts a path name or a stream,
or uses *standard-input*."
  :accepts '(or stream string pathname)
  (view-json (or file lish:*input* *standard-input*)))

;; End
