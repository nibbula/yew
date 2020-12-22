;;;								-*- Lisp -*-
;;; parse-util.asd - System definition for parse-util
;;;

(defsystem parse-util
    :name               "parse-util"
    :description        "Parsing utilities."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Minimal cheapo parsing junk."
    :depends-on (:dlib)
    :components
    ((:file "parse-util")))
