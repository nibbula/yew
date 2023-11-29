;;;								-*- Lisp -*-
;;; pick-list.asd -- System definition for pick-list
;;;

(defsystem pick-list
    :name               "pick-list"
    :description        "Choose things from a list."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Choose things from a list."
    :depends-on (:dlib :char-util :stretchy :keymap :opsys :inator :terminal
		 :collections :ostring :fatchar
		 :terminal-inator
		 ;; #+unix :terminal-curses
		 :fui :view-generic)
    :components
    ((:file "pick-list")
     (:module "cmds"
      :pathname ""
      :if-feature :lish
      :components ((:file "pick-list-cmds"))
      :depends-on ("pick-list"))))
