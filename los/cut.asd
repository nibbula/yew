;;;								-*- Lisp -*-
;;; cut.asd - System definition for cut
;;;

(defsystem cut
    :name               "cut"
    :description        "Cut pieces from lines."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "Cut pieces from lines, something like the traditional Unix command."
    :depends-on (:dlib :lish :stretchy :char-util :unicode :table :cl-ppcre
		 :los-config)
    :components
    ((:file "cut")))
