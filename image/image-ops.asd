;;;								-*- Lisp -*-
;;; image-ops.asd - System definition for image-ops
;;;

(defsystem image-ops
    :name               "image-ops"
    :description        "Image operations."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Operations on images."
    :depends-on (:image :lparallel)
    :components
    ((:file "image-ops")))
