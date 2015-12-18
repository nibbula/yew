;;;								-*- Lisp -*-
;;; grout.asd -- System definition for grout
;;;

(defpackage :grout-system
    (:use :common-lisp :asdf))

(in-package :grout-system)

(defsystem grout
    :name               "grout"
    :description        "Generic Rectilinear Output Und Text"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "None"
    :long-description   "Generic Rectilinear Output Und Text"
    :depends-on (:dlib :dlib-misc :char-util :opsys :terminal :terminal-ansi)
    :components
    ((:file "grout")))
