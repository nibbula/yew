;;;								-*- Lisp -*-
;;; rl-widget.asd - System definition for rl-widget
;;;

(defsystem rl-widget
    :name               "rl-widget"
    :description        "An editor widget."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Just using RL as text box in terminals."
    :depends-on (:rl :fui)
    :components
    ((:file "widget")))
