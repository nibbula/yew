;;;								-*- Lisp -*-
;;; fake-dlib.asd - System definition for fake-dlib
;;;

(defsystem fake-dlib
    :name               "fake-dlib"
    :description        "Stuff from dlib to eliminate the dependency."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Stuff from dlib to eliminate the dependency."
    ;; :depends-on () DON'T ADD DEPENDENCIES.
    :components
    ((:file "fake-dlib")))
