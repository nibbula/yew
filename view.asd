;;;								-*- Lisp -*-
;;; view.asd - System definition for view
;;;

(defsystem view
    :name               "view"
    :description        "Look at something."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "Blah, blah, frobulation, la, la, projection, blah, blah, presentation, ..."
    :depends-on (:view-generic :dlib :magic :pick-list)
    :components
    ((:file "view")))
