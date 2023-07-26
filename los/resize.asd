;;;								-*- Lisp -*-
;;; resize.asd - System definition for resize
;;;

(defsystem resize
    :name               "resize"
    :description        "Resize the window."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "If you have interest in a mostly pointless command, this might suffice."
    :depends-on (:terminal :char-util :keymap :inator :terminal-inator :fui)
    :components
    ((:file "resize")
     (:module "cmds"
      :pathname ""
      :if-feature :lish
      :components ((:file "resize-cmds"))
      :depends-on ("resize"))))
