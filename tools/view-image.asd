;;;								-*- Lisp -*-
;;; view-image.asd - System definition for view-image
;;;

(defsystem view-image
    :name               "view-image"
    :description        "View an image."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "View an image."
    :depends-on (:dlib :dlib-misc :dtime :keymap :fatchar :char-util :terminal
		 :terminal-ansi :terminal-crunch :terminal-inator :image
		 :image-ops :dcolor :magic :grout :flexi-streams
		 :view-image-popi :glob :rl-widget :fui)
    :components
    ((:file "view-image")))
