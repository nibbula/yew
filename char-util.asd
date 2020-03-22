;;;								-*- Lisp -*-
;;; char-util.asd -- System definition for char-util
;;;

(defsystem char-util
    :name               "char-util"
    :description        "General utilites for characters."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "General utilites for characters."
    :depends-on (:dlib :stretchy :ochar :unicode
		       #-sbcl :cl-unicode
		       #-sbcl :uax-15
		       )
    :components
    ((:file "char-util")))
