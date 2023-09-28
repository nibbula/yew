;;;								-*- Lisp -*-
;;; touch.asd - System definition for touch
;;;

(defsystem touch
    :name               "touch"
    :description        "Change file times."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Change a file times or create it."
    :depends-on (:opsys :calendar)
    :components
    ((:file "touch")
     (:module "cmds"
      :pathname ""
      :if-feature :lish
      :components ((:file "touch-cmds"))
      :depends-on ("touch"))))
     
