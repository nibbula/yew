;;;								-*- Lisp -*-
;;; completion.asd -- System definition for completion
;;;

(defpackage :completion-system
    (:use :common-lisp :asdf))

(in-package :completion-system)

(defsystem completion
    :name               "completion"
    :description        "Complete your words. Fill the gap. Type less."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "None"
    :long-description   "Blah blah blah"
    :depends-on (:opsys)
    :components
    ((:file "completion")))
