;;;								-*- Lisp -*-
;;; apropos.asd - System definition for apropos
;;;

(defsystem apropos
    :name               "apropos"
    :description        "A mixture of both Unix and Lisp apropos."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Knowing what you want can be half the problem."
    :depends-on (:cl-ppcre :dlib :collections :char-util :table :grout :syntax
		 :syntax-lisp :man :los-config :lish)
    :components
    ((:file "apropos")))