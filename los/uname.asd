;;;								-*- Lisp -*-
;;; uname.asd - System definition for uname
;;;

(defsystem uname
    :name               "uname"
    :description        "Print system type information."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Print system type information."
    :depends-on (:dlib :opsys :lish)
    :components
    ((:file "uname")))
