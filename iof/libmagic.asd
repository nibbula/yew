;;;								-*- Lisp -*-
;;; libmagic.asd - System definition for magic-ffi
;;;

(defsystem libmagic
    :name               "libmagic"
    :description        "Interface to libmagic."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Interface to libmagic."
    :depends-on (:cffi :opsys)
    :components
    ((:file "libmagic")))
