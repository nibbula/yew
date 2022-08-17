;;;								-*- Lisp -*-
;;; find.asd -- System definition for FIND
;;;

(defsystem find
    :name               "find"
    :description        "Find files."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "This will probably never be done."
    :depends-on (:dlib :opsys :collections :cl-ppcre :magic #| :lparallel |#
		 :char-util :los-config)
    :components
    ((:file "find")))
