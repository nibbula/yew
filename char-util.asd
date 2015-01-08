;;;								-*- Lisp -*-
;;; char-util.asd -- System definition for char-util
;;;

(defpackage :char-util-system
    (:use :common-lisp :asdf))

(in-package :char-util-system)

(defsystem char-util
    :name               "char-util"
    :description        "General utilites for characters."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :long-description   "General utilites for characters."
;    :depends-on ()
    :components
    ((:file "char-util")))
