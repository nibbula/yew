;;;								-*- Lisp -*-
;;; view-json.asd - System definition for view-json
;;;

(defsystem view-json
    :name               "view-json"
    :description        "View JSON as a tree."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "View JSON as a tree."
    :depends-on (:dlib :dlib-misc :lish :tree-viewer :jsown)
    :components
    ((:file "view-json")))
