;;;								-*- Lisp -*-
;;; string-expand.asd - System definition for string-expand
;;;

(defsystem string-expand
  :name               "string-expand"
  :description        "Expand variables in strings."
  :version            "0.1.0"
  :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
  :license            "GPL-3.0-only"
  :source-control     :git
  :long-description   "Expand variables in strings."
  :depends-on (:dlib :opsys)
  :components
  ((:file "string-expand")))
