;;;								-*- Lisp -*-
;;; rm.asd - System definition for rm
;;;

(defsystem rm
    :name               "rm"
    :description        "Delete files and directories."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "Delete files and directories. Provides something like the unix rm and
rmdir commands."
    :depends-on (:opsys)
    :components
    ((:file "rm")))
