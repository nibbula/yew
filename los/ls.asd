;;;								-*- Lisp -*-
;;; ls.asd - System definition for ls
;;;

(defsystem ls
    :name               "ls"
    :description        "List files."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "Why would you ever want to use this command? You should know what files you
     have by now. This command is *so* overused, I can't even believe it. Do you
     really want to rummage through all your stuff yet again?"
    :depends-on (:dlib :dlib-misc :char-util :opsys :dtime :terminal
		 :terminal-ansi :grout :table :table-print :terminal-table
		 :fatchar :fatchar-io :theme :style :magic :los-config)
    :components
    ((:file "ls")))
