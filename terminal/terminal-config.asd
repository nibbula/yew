;;;								-*- Lisp -*-
;;; terminal-config.asd - System definition for terminal-config
;;;

(defsystem terminal-config
    :name               "terminal-config"
    :description        "Build configuration for terminal. "
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Build configuration for terminal. "
    :depends-on (:config)
    :components
    ((:file "terminal-config")))
