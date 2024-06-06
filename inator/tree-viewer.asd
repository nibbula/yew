;;;								-*- Lisp -*-
;;; tree-viewer.asd - System definition for tree-browser
;;;

(defsystem tree-viewer
    :name               "tree-viewer"
    :description        "Browse trees."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "A user interface for viewing generic trees."
    :depends-on (:dlib :opsys :dlib-misc :dtime :char-util :keymap :pick-list
		 :glob :collections :ostring :inator :terminal :terminal-inator
		 :fui :rl :fatchar-io :theme
		 ;; #+unix :terminal-curses
		 :view-generic)
    :components
    ((:file "tree-viewer")))
