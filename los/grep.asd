;;;								-*- Lisp -*-
;;; grep.asd -- System definition for grep
;;;

(defsystem grep
    :name               "grep"
    :description        "Regular expression search in streams."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Regular expression search in streams."
    :depends-on (:cl-ppcre :dlib :opsys :stretchy :grout :los-config :fatchar
		 :char-util :collections :table :theme :style :view-generic
		 :result)
    :components
    ((:file "grep")
     (:module "cmds"
      :pathname ""
      :if-feature :lish
      :components ((:file "grep-cmds"))
      :depends-on ("grep"))))
