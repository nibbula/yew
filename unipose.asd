;;;								-*- Lisp -*-
;;; unipose.asd -- System definition for unipose
;;;

(defpackage :unipose-system
    (:use :common-lisp :asdf))

(in-package :unipose-system)

(defsystem unipose
    :name               "unipose"
    :description        "Compose unicode characters"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "None"
    :long-description   "Compose unicode characters."
;    :depends-on ()
    :components
    ((:file "unipose")))
