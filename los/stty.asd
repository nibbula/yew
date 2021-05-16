;;;								-*- Lisp -*-
;;; stty.asd -- System definition for stty
;;;

(defsystem stty
    :name               "stty"
    :description        "Show and set terminal settings."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Show and set terminal settings."
    :depends-on (:dlib :opsys :dlib-misc :char-util :los-config :completion)
    :components
    ((:file "stty")))
