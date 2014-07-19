;;;								-*- Lisp -*-
;;; ansiterm.asd -- System definition for ansiterm
;;;

(defpackage :ansiterm-system
    (:use :common-lisp :asdf))

(in-package :ansiterm-system)

(defsystem ansiterm
    :name               "ansiterm"
    :description        "Deal with ANSI-like terminals."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "None"
    :long-description   "Deal with ANSI-like terminals."
    :depends-on (:termios :opsys :cffi)
    :components
    ((:file "ansiterm")))
