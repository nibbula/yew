;;;								-*- Lisp -*-
;;; keymap.asd -- System definition for keymap
;;;

(defsystem keymap
    :name               "keymap"
    :description        "keymap package"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Associate functions with keys."
    :depends-on (:char-util)
    :components
    ((:file "keymap")))
