;;;								-*- Lisp -*-
;;; libmagic.asd - System definition for magic-ffi
;;;

(defsystem libmagic
    :name               "libmagic"
    :description        "Interface to libmagic."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Interface to libmagic."
    :depends-on (:cffi :opsys)
    :components
    ((:file "libmagic")))
