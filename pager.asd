;;;								-*- Lisp -*-
;;; pager.asd -- System definition for pager
;;;

(defpackage :pager-system
    (:use :common-lisp :asdf))

(in-package :pager-system)

(defsystem pager
    :name               "pager"
    :description        "something like more or less"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :long-description   "We can only see so much at one time."
    :depends-on (:dlib :dlib-misc :curses :opsys :fui :stretchy :keymap
		 :char-util :cl-ppcre)
    :components
    ((:file "pager")))
