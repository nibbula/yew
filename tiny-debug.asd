;;;								-*- Lisp -*-
;;; tiny-debug.asd -- System definition for tiny-debug
;;;

(defpackage :tiny-debug-system
    (:use :common-lisp :asdf))

(in-package :tiny-debug-system)

(defsystem tiny-debug
    :name               "tiny-debug"
    :description        "Command line debugger"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "None"
    :long-description   "bleh"
    :depends-on (:tiny-repl)
    :components
    ((:file "tiny-debug")))
