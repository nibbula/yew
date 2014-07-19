;;;								-*- Lisp -*-
;;; opsys.asd -- System definition for OPSYS package
;;;

;;; $Revision: 1.1 $

(defpackage :opsys-system
    (:use :common-lisp :asdf))

(in-package :opsys-system)

(defsystem opsys
    :name               "opsys"
    :description        "Interface to the operating system."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula@yahoo.com>"
    :licence            "None"
    :long-description   "The cessation of the repetition of \"Never Again\"."
    :depends-on (:cffi)
    :components
    ((:file "opsys")))
