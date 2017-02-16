;;;								-*- Lisp -*-
;;; terminal-test.asd -- System definition for terminal-test
;;;

(defsystem terminal-test
    :name               "terminal-test"
    :description        "Test the generic terminal library."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Test the generic terminal library."
    :depends-on (:dlib :terminal :terminal-ansi :terminal-curses)
    :components
    ((:file "terminal-test")))
