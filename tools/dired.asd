;;;								-*- Lisp -*-
;;; dired.asd - System definition for dired
;;;

(defsystem dired
    :name               "dired"
    :description        "Directory editor."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Edit directories, like dired from a famous editor."
    :depends-on (:dlib :opsys :keymap :char-util :terminal :inator
		 :terminal-inator :terminal-table :table-viewer :lish :ls
		 :view-generic :view :rl :fui :rl-widget)
    :components
    ((:file "dired")))
