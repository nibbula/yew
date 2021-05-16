;;;								-*- Lisp -*-
;;; locale.asd - System definition for locale
;;;

(defsystem locale
    :name               "locale"
    :description        "Localization"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Localization"
    :depends-on (:dlib :opsys)
    :components
    ((:file "locale")))
