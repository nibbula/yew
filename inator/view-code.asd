;;;								-*- Lisp -*-
;;; view-code.asd - System definition for view-code
;;;

(defsystem view-code
    :name               "view-code"
    :description        "View Lisp code with the tree viewer. "
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "This is an interesting experiment, but not very useful yet."
    :depends-on (:terminal :tree-viewer :reader-ext :tree-editor)
    :components
    ((:file "view-code")))
