;;;								-*- Lisp -*-
;;; los-util.asd - System definition for los-util
;;;

(defsystem los-util
    :name               "los-util"
    :description        "Common utilities for LOS programs."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Common utilities for LOS programs."
    :depends-on (:dlib :opsys :lish)
    :components
    ((:file "los-util")))
