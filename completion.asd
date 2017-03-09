;;;								-*- Lisp -*-
;;; completion.asd -- System definition for completion
;;;

(defsystem completion
    :name               "completion"
    :description        "Complete your words. Fill the gap. Type less."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Blah blah blah"
    :depends-on (:dlib :opsys :glob :dlib-misc :syntax-lisp
		 :terminal :terminal-ansi :cl-ppcre)
    :components
    ((:file "completion")))
