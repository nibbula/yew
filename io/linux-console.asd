;;;								-*- Lisp -*-
;;; linux-console.asd - System definition for linux-console
;;;

(defsystem linux-console
    :name               "linux-console"
    :description        "Interface to Linux console devices."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Interface to Linux console devices."
    :depends-on (:dlib :opsys :cffi :dlib-misc :dcolor :terminal)
    :components
    ((:file "linux-console")))
