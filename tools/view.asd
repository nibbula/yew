;;;								-*- Lisp -*-
;;; view.asd - System definition for view
;;;

(defsystem view
    :name               "view"
    :description        "Look at something."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "Blah, blah, frobulation, la, la, projection, blah, blah, presentation, ..."
    :depends-on (:view-generic :dlib :magic :pick-list :los-config)
    :components
    ((:file "view")))
