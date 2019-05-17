;;;								-*- Lisp -*-
;;; config.asd - System definition for config
;;;

(defsystem config
    :name               "config"
    :description        "Package build configuration."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Package build configuration."
    :depends-on (:cffi)
    :components
    ((:file "config")))
