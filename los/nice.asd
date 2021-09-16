;;;								-*- Lisp -*-
;;; nice.asd - System definition for nice
;;;

(defsystem nice
    :name               "nice"
    :description        "Modify scheduling priority."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Run a system command with modified scheduling priority."
    :depends-on (:dlib :opsys :lish)
    :components
    ((:file "nice")))
