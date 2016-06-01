;;;								-*- Lisp -*-
;;; terminal-ansi.asd -- System definition for terminal-ansi
;;;

(defsystem terminal-ansi
    :name               "terminal-ansi"
    :description        "Standard terminal frobbing."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "Once there was a clunkly thing called a terminal.
It was last stop on the bit train. Now there is an imaginary protocol to
display your fixed width text in a box with colors. This may help."
    :depends-on (:dlib :terminal :cffi :opsys)
    :components
    ((:file "terminal-ansi")))
