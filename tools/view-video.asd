;;;								-*- Lisp -*-
;;; view-video.asd - System definition for view-video
;;;

(defsystem view-video
    :name               "view-video"
    :description        "Video viewer."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Video viewer."
    :depends-on (:lish)
    :components
    ((:file "view-video")))
