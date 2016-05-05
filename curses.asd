;;;								-*- Lisp -*-
;;; curses.asd -- System definition for CURSES package
;;;

(defpackage :curses-system
    (:use :common-lisp :asdf))

(in-package :curses-system)

(defsystem curses
    :name               "curses"
    :description        "Interface to curses library."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3"
    :long-description   "Not even close to being complete."
    :depends-on (:cffi)
    :components
    ((:file "curses")))
