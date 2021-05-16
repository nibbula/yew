;;;								-*- Lisp -*-
;;; chart.asd - System definition for chart
;;;

(defsystem chart
    :name               "chart"
    :description        "Draw charts."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Draw charts."
    :depends-on (:dlib :collections :char-util :table :terminal :keymap :inator
		 :terminal-inator)
    :components
    ((:file "chart")))
