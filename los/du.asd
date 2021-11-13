;;;								-*- Lisp -*-
;;; du.asd -- System definition for du
;;;

(defsystem du
    :name               "du"
    :description        "Disk usage"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Calculate and explore disk usage"
    :depends-on (:dlib :opsys :find :terminal :view-generic
		 :los-config :view :unicode :tree-viewer :table-viewer)
    :components
    ((:file "du")))
