;;;								-*- Lisp -*-
;;; sed.asd - System definition for sed
;;;

(defsystem sed
    :name               "sed"
    :description        "Stream editor."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "A stream editor, not compaitble with unix sed."
    :depends-on (:dlib :collections :cl-ppcre)
    :components
    ((:file "sed")))
