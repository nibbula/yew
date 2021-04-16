;;;								-*- Lisp -*-
;;; cal.asd - System definition for cal
;;;

(defsystem cal
    :name               "cal"
    :description        "Print a calendar."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Print a calendar."
    :depends-on (:dlib :calendar :table :table-print :grout :fatchar :char-util
		 :collections)
    :components
    ((:file "cal")))
