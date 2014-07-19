;;;								-*- Lisp -*-
;;; curses.asd -- System definition for CURSES package
;;;

;;; $Header: /bog/cvs2/whirl/lisp/curses.asd,v 1.6 2012/09/06 16:14:49 dan Exp $

(defpackage :curses-system
    (:use :common-lisp :asdf))

(in-package :curses-system)

(defsystem curses
    :name               "curses"
    :description        "Interface to curses library."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula@yahoo.com>"
    :licence            "None"
    :long-description   "Not even close to being complete."
    :depends-on (:cffi)
    :components
    ((:file "curses")))
