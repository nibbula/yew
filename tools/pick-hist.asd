;;;								-*- Lisp -*-
;;; pick-hist.asd - System definition for pick-hist
;;;

(defsystem pick-hist
    :name               "pick-hist"
    :description        "Pick an history item using pick-list."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Pick an history item using pick-list."
    :depends-on (:dlib :collections :keymap :terminal :inator :rl :pick-list)
    :components
    ((:file "pick-hist")))
