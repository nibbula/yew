;;;								-*- Lisp -*-
;;; view-image.asd - System definition for view-image
;;;

(defsystem view-image
    :name               "view-image"
    :description        "View an image."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "View an image."
    :depends-on (:dlib :keymap :char-util :terminal :terminal-ansi
    		:terminal-inator :magic :cl-jpeg :png-read :skippy)
    :components
    ((:file "view-image")))
