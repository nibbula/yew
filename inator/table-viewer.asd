;;;								-*- Lisp -*-
;;; table-viewer.asd - System definition for table-viewer
;;;

(defsystem table-viewer
    :name               "table-viewer"
    :description        "View tables."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "View tables."
    :depends-on (:dlib :collections :table :table-print :keymap :inator
		 :terminal :terminal-inator :dtt :char-util :fui :fatchar
		 :fatchar-io :grout :terminal-table :ostring :view-generic
		 :terminal-null)
    :components
    ((:file "table-viewer")))
