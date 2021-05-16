;;;								-*- Lisp -*-
;;; parse-util.asd - System definition for parse-util
;;;

(defsystem parse-util
    :name               "parse-util"
    :description        "Parsing utilities."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Minimal cheapo parsing junk."
    :depends-on (:dlib :collections)
    :components
    ((:file "parse-util")))
