;;;								-*- Lisp -*-
;;; locate.asd - System definition for locate
;;;

(defsystem locate
    :name               "locate"
    :description        "Try to somehow locate files."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Try to somehow locate files."
    :depends-on (:dlib :dlib-misc :cl-ppcre #| :trie |# :opsys)
    :components
    ((:file "locate")))
