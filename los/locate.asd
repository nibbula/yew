;;;								-*- Lisp -*-
;;; locate.asd - System definition for locate
;;;

(defsystem locate
    :name               "locate"
    :description        "Try to somehow locate files."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Try to somehow locate files."
    :depends-on (:dlib :dlib-misc :cl-ppcre #| :trie |# :opsys :los-config)
    :components
    ((:file "locate")
     (:module "cmds"
      :pathname ""
      :if-feature :lish
      :components ((:file "locate-cmds"))
      :depends-on ("locate"))))

