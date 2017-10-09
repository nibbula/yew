;;;								-*- Lisp -*-
;;; dlib-misc.asd -- System definition for DLIB-MISC
;;;

(defsystem dlib-misc
    :name               "dlib-misc"
    :description        "Dan's library of miscellaneous useful function."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3"
    :source-control	:git
    :long-description
    "Dan's library of miscellaneous useful function. This is for things that
are nice, but not essential."
    :depends-on (:opsys :dlib :char-util :table :cl-ppcre :glob)
    :components
    ((:file "dlib-misc")))
