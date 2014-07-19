;;;								-*- Lisp -*-
;;; keymap.asd -- System definition for keymap
;;;

(defpackage :keymap-system
    (:use :common-lisp :asdf))

(in-package :keymap-system)

(defsystem keymap
    :name               "keymap"
    :description        "keymap package"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "None"
    :long-description   "Associate functions with keys."
    :depends-on (:char-util)
    :components
    ((:file "keymap")))
