;;;								-*- Lisp -*-
;;; du.asd -- System definition for du
;;;

(defsystem du
    :name               "du"
    :description        "Disk usage"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Calculate and explore disk usage"
    :depends-on (:dlib :opsys :find :curses :fui :tree-viewer
		 :terminal :terminal-ansi :view)
    :components
    ((:file "du")))
