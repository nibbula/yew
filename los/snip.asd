;;;								-*- Lisp -*-
;;; snip.asd -- System definition for snip
;;;

(defsystem snip
    :name               "snip"
    :description        "Cut off part of a stream."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Cut off part of a stream."
    :depends-on (:dlib :cl-ppcre :opsys :stretchy :lish :los-config
		 :utf8b-stream)
    :components
    ((:file "snip")))
