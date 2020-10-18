;;;								-*- Lisp -*-
;;; rl-config.asd - System definition for rl-config
;;;

(defsystem rl-config
    :name               "rl-config"
    :description        "Configuration for RL"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Configuration for RL"
    :depends-on (:config)
    :components
    ((:file "rl-config")))
