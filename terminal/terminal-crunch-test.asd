;;;								-*- Lisp -*-
;;; terminal-crunch-test.asd - System definition for terminal-crunch-test
;;;

(defsystem terminal-crunch-test
    :name               "terminal-crunch-test"
    :description        "Tests for terminal-crunch."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Tests for terminal-crunch."
    :depends-on (:dlib :dlib-misc :terminal :terminal-crunch :terminal-ansi
		 :fatchar :table :table-print :rl :lish)
    :components
    ((:file "terminal-crunch-test")))
