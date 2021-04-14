;;;								-*- Lisp -*-
;;; tee.asd - System definition for tee
;;;

(defsystem tee
    :name               "tee"
    :description        "Copy input to multiple outputs."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Copy input to multiple outputs."
    :depends-on (:dlib :cat)
    :components
    ((:file "tee")))
