;;;								-*- Lisp -*-
;;; puca.asd -- System definition for PUCA
;;;

(defpackage :puca-system
    (:use :common-lisp :asdf))

(in-package :puca-system)

(defsystem puca
    :name               "puca"
    :description        "Putative Muca"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "None"
    :long-description   "Putative Muca (A very simple interface to CVS)"
    :depends-on (:dlib :dlib-misc :opsys :curses :pager :fui)
    :components
    ((:file "puca")))
