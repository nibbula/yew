;;;								-*- Lisp -*-
;;; dlib-misc.asd -- System definition for DLIB-MISC
;;;

(defsystem dlib-misc
    :name               "dlib-misc"
    :description        "Library of miscellaneous useful functions."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3"
    :source-control	:git
    :long-description
    "A library of miscellaneous useful functions. This is for things that
are nice, but not essential."
    :depends-on (:opsys :dlib :char-util :glob :cl-ppcre :table)
    :components
    ((:file "dlib-misc")))
