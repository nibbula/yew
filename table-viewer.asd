;;;								-*- Lisp -*-
;;; table-viewer.asd - System definition for table-viewer
;;;

(defsystem table-viewer
    :name               "table-viewer"
    :description        "View tables."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "View tables."
    :depends-on (:collections :table :table-print :keymap :inator :terminal
		 :terminal-inator)
    :components
    ((:file "table-viewer")))
