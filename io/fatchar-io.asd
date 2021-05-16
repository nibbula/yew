;;;								-*- Lisp -*-
;;; fatchar-io.asd - System definition for fatchar-io
;;;

(defsystem fatchar-io
    :name               "fatchar-io"
    :description        "Outputing fat characters."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Outputing fat characters."
    :depends-on (:dlib :stretchy :char-util :fatchar :terminal
		 :trivial-gray-streams)
    :components
    ((:file "fatchar-io")))
