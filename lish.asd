;;;								-*- Lisp -*-
;;; lish.asd -- System definition for LISH package
;;;

;;; $Revision: 1.4 $

(defpackage :lish-system
    (:use :common-lisp :asdf))

(in-package :lish-system)

(defsystem lish
    :name               "lish"
    :description        "Lispy system command shell."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula@yahoo.com>"
    :licence            "None"
    :long-description   "I don't recommend using this."
    :depends-on (:tiny-rl :cl-ppcre :opsys :dlib :dlib-misc :stretchy :glob)
    :components
    ((:file "lish")))
