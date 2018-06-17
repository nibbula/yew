;;;								-*- Lisp -*-
;;; tree-viewer.asd - System definition for tree-browser
;;;

(defsystem tree-viewer
    :name               "tree-viewer"
    :description        "Browse trees."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "A user interface for viewing generic trees."
    :depends-on (:dlib :opsys :dlib-misc :char-util :keymap :pick-list
		 :glob :dlib-interactive :inator :terminal :terminal-inator :rl
		 #+unix :terminal-curses)
    :components
    ((:file "tree-viewer")))
