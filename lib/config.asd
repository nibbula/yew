;;;								-*- Lisp -*-
;;; config.asd - System definition for config
;;;

(defsystem config
    :name               "config"
    :description        "Package build configuration."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Package build configuration."
    :depends-on ((:feature (:not :mezzano) :cffi))
    :components
    ((:file "config")))
