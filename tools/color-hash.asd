;;;								-*- Lisp -*-
;;; color-hash.asd - System definition for color-hash
;;;

(defsystem color-hash
    :name               "color-hash"
    :description        "List file color hashes."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "List file color hashes."
    :depends-on (:dlib :opsys :collections :table :fatchar :find :md5
		 :color-stripe)
    :components
    ((:file "color-hash")))
