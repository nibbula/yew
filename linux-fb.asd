;;;								-*- Lisp -*-
;;; linux-fb.asd - System definition for linux-fb
;;;

(defsystem linux-fb
    :name               "linux-fb"
    :description        "Interface to the Linux framebuffer device."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Interface to the Linux framebuffer device."
    :depends-on (:dlib :opsys :cffi :table :table-print :collections :color
		 :image)
    :components
    ((:file "linux-fb")))