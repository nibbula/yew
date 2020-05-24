;;;								-*- Lisp -*-
;;; view-html.asd -- System definition for view-html
;;;

(defsystem view-html
    :name               "view-html"
    :description        "View HTML as a tree."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "View HTML as a tree, using the tree-browser."
    :depends-on (:dlib :dlib-misc :rl :tree-viewer :pick-list
		 :plump :puri #-windows :drakma
		 :terminal :keymap :fui :pager :view-image :rl-widget)
    :components
    ((:file "view-html")))
