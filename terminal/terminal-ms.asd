;;;								-*- Lisp -*-
;;; terminal-ms.asd - System definition for terminal-ms
;;;

(defsystem terminal-ms
    :name               "terminal-ms"
    :description        "Microsoft console as a terminal."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "Another in the increasingly long list of terminals implementations.
This one works in a Windows console window, and probably not anywhere else.
It has the charming feature of not having any character attributes except
for 16 colors. How quaint and rustic. And it's very slow."
    :depends-on (:cffi :dlib :dlib-misc :terminal :char-util :opsys
		 :trivial-gray-streams :fatchar :dcolor :terminal-crunch)
    :components
    ((:file "terminal-ms")))
