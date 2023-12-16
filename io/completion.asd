;;;								-*- Lisp -*-
;;; completion.asd -- System definition for completion
;;;

(defsystem completion
    :name               "completion"
    :description        "Complete your words. Fill the gap. Type less."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Blah blah blah"
    :depends-on (:dlib :opsys :glob :collections :char-util :dlib-misc
		 :string-expand :reader-ext :syntax-lisp :terminal :prefix-tree
		 #+use-re :re
		 #-use-re :cl-ppcre
		 :theme :fatchar :style)
    :components
    ((:file "completion")))
