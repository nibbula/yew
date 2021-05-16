;;;								-*- Lisp -*-
;;; view-html.asd -- System definition for view-html
;;;

(defsystem view-html
    :name               "view-html"
    :description        "View HTML as a tree."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "View HTML as a tree, using the tree-browser."
    :depends-on (:dlib :dlib-misc :dtime :rl :tree-viewer :pick-list
		 :plump :puri #-windows :drakma
		 :terminal :keymap :fui :pager :view-image :rl-widget
		 :terminal-inator :fatchar :char-util :ostring :unicode
		 :parse-util)
    :components
    ((:file "view-html")))
