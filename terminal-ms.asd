;;;								-*- Lisp -*-
;;; terminal-ms.asd - System definition for terminal-ms
;;;

(defsystem terminal-ms
    :name               "terminal-ms"
    :description        "Microsoft console as a terminal."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "Another in the increasingly long list of terminals implementations.
This one works in a Windows console window, and probably not anywhere else.
It has the chaming feature of not having any character attributes except
for 16 colors."
    :depends-on (:opsys)
    :components
    ((:file "terminal-ms")))
