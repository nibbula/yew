;;;								-*- Lisp -*-
;;; opsys-config.asd - System definition for opsys-config
;;;

(defsystem opsys-config
    :name               "opsys-config"
    :description        "Configuration for OPSYS"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Configuration for OPSYS"
    :depends-on (:config)
    :components
    ((:file "opsys-config")))
