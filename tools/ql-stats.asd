;;;								-*- Lisp -*-
;;; ql-stats.asd - System definition for ql-stats
;;;

(defsystem ql-stats
    :name               "ql-stats"
    :description        "Grab Quicklisp statistics files."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Grab Quicklisp statistics files."
    :depends-on (:dlib :dlib-misc :opsys :collections :glob :dtt :table-viewer
		 :fui :drakma :chart)
    :components
    ((:file "ql-stats")))
