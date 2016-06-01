;;;								-*- Lisp -*-
;;; tree-viewer.asd - System definition for tree-browser
;;;

(defsystem tree-viewer
    :name               "tree-viewer"
    :description        "Browse trees."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "None"
    :long-description   "A user interface for viewing generic trees."
    :depends-on (:dlib :dlib-misc :curses :char-util :keymap :fui :pick-list
		 :glob)
    :components
    ((:file "tree-viewer")))
