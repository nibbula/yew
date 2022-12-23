;;;								-*- Lisp -*-
;;; dgray.asd - System definition for dgray
;;;

(defsystem dgray
    :name               "dgray"
    :description        "Thin portability around Gray streams."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Thin portability around Gray streams."
    ;; :depends-on ()
    :components
    ((:file "dgray")))
