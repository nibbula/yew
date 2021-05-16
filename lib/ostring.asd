;;;								-*- Lisp -*-
;;; ostring.asd - System definition for ostring
;;;

(defsystem ostring
    :name               "ostring"
    :description        "Objectable strings."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Objectable strings for the downtrodden masses."
    :depends-on (:ochar)
    :components
    ((:file "ostring")))
