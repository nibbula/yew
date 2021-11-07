;;;								-*- Lisp -*-
;;; view-audio.asd - System definition for view-audio
;;;

(defsystem view-audio
    :name               "view-audio"
    :description        "Audio viewer."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Audio viewer."
    :depends-on (:lish)
    :components
    ((:file "view-audio")))
