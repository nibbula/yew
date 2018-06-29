;;;								-*- Lisp -*-
;;; view-lisp.asd - System definition for view-lisp
;;;

(defsystem view-lisp
    :name               "view-lisp"
    :description        "View things in the Lisp system."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "View things in the Lisp system."
    :depends-on (:dlib :tree-viewer :dlib-interactive)
    :components
    ((:file "view-lisp")))
