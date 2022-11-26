;;;								-*- Lisp -*-
;;; theme-default.asd - System definition for theme-default
;;;

(defsystem theme-default
    :name               "theme-default"
    :description        "Defaults for theme."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Defaults for theme."
    :depends-on (:fatchar :theme)
    :components
    ((:file "theme-default")))
