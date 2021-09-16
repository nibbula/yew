;;;								-*- Lisp -*-
;;; id.asd - System definition for id
;;;

(defsystem id
    :name               "id"
    :description        "Print user information."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Print user information."
    :depends-on (:dlib :opsys :collections :table :grout :lish)
    :components
    ((:file "id")))
