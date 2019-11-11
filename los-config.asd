;;;								-*- Lisp -*-
;;; los-config.asd - System definition for los-config
;;;

(defsystem los-config
    :name               "los-config"
    :description        "Configuration for LOS"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Configuration for LOS"
    :depends-on (:config)
    :components
    ((:file "los-config")))
