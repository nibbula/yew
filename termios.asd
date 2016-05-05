;;;								-*- Lisp -*-
;;; termios.asd -- System definition for TERMIOS package
;;;

(defpackage :termios-system
    (:use :common-lisp :asdf))

(in-package :termios-system)

(defsystem termios
    :name               "termios"
    :description
    "Interface to POSIX termios terminal driver manipulation functions."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3"
    :source-control	:git
    :long-description   "Kickin' it old school."
    :depends-on (:cffi :opsys #| :dlib-misc :char-util |#)
    :components
    ((:file "termios")))
