;;;								-*- Lisp -*-
;;; terminal-ansi.asd -- System definition for terminal-ansi
;;;

(defsystem terminal-ansi
    :name               "terminal-ansi"
    :description        "Standard terminal frobbing."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "Once there was a clunkly thing called a terminal.
It was last stop on the bit train. Now there is an imaginary protocol to
display your fixed width text in a box with colors. This may help."
    :depends-on (:cffi :dlib :dlib-misc :terminal :char-util :unicode :opsys
		 :trivial-gray-streams :fatchar :dcolor :ansi :terminal-dumb
		 :terminal-crunch :cl-base64)
    :components
    ((:file "terminal-ansi")))
