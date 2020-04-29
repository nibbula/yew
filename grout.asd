;;;								-*- Lisp -*-
;;; grout.asd -- System definition for grout
;;;

(defsystem grout
    :name               "grout"
    :description        "Generic Rectilinear Output Und Text"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Generic Rectilinear Output Und Text"
    :depends-on (:dlib :dlib-misc :char-util :opsys :terminal :terminal-ansi
		 :terminal-dumb-color
		 :table-print :terminal-table :fatchar)
    :components
    ((:file "grout")))
