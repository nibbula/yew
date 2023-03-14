;;;								-*- Lisp -*-
;;; fancy-inator.asd - System definition for fancy-inator
;;;

(defsystem fancy-inator
  :name               "fancy-inator"
  :description        "A terminal-inator with fancy features."
  :version            "0.1.0"
  :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
  :license            "GPL-3.0-only"
  :source-control	:git
  :long-description   "A terminal-inator with fancy features."
  :depends-on (:inator :char-util :keymap :fatchar :terminal :terminal-inator
	       :fui :tiny-repl)
  :components
  ((:file "fancy-inator")))
