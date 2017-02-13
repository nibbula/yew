;;;								-*- Lisp -*-
;;; dlib-misc.asd -- System definition for DLIB-MISC
;;;

(defsystem dlib-misc
    :name               "dlib-misc"
    :description        "Dan's library of miscellaneous useful function."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "None"
    :long-description
    "Dan's library of miscellaneous useful function. This is for things that
are nice, but not essential."
    :depends-on (:opsys :dlib :char-util :table :glob)
    :components
    ((:file "dlib-misc")))
