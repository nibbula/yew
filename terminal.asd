;;;								-*- Lisp -*-
;;; terminal.asd -- System definition for terminal
;;;

(defpackage :terminal-system
    (:use :common-lisp :asdf))

(in-package :terminal-system)

(defsystem terminal
    :name               "terminal"
    :description        "Generic terminality."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Manipulate an imaginary thing once called a terminal."
    :depends-on (:opsys)
    :components
    ((:file "terminal")))
