;;;								-*- Lisp -*-
;;; locale-data.asd - System definition for locale-data
;;;

(defsystem locale-data
    :name               "locale-data"
    :description        "Localization data."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Localization data."
    :depends-on (:locale :calendar)
    :components
    ((:file "locale-data")))
