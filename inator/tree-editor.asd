;;;								-*- Lisp -*-
;;; tree-editor.asd - System definition for tree-editor
;;;

(defsystem tree-editor
    :name               "tree-editor"
    :description        "Tree editor."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Tree editor."
    :depends-on (:tree-viewer :fui :rl-widget :keymap :ostring)
    :components
    ((:file "tree-editor")))
