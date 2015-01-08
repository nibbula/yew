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
    :license            "GPLv3"
    :long-description   "Blah blah blah"
    :depends-on (:dlib :opsys :glob :dlib-misc :syntax-lisp :ansiterm :cl-ppcre)
    :components
    ((:file "completion")))
