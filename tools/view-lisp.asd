;;;								-*- Lisp -*-
;;; view-lisp.asd - System definition for view-lisp
;;;

(defsystem view-lisp
    :name               "view-lisp"
    :description        "View things in the Lisp system."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "Use the tree viewer to browse through some of the multitudinous entities
residing in your running Lisp system. This can help you pretend that your
memory is a nice neat tree, rather than the vast corrals of scattered refuse
that it is."
    :depends-on (:dlib :tree-viewer :dlib-interactive :syntax :syntax-lisp
		 :terminal :collections :fatchar :fatchar-io :completion
		 :terminal-utils :fancy-inator)
    :components
    ((:file "view-lisp")))
