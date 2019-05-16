;;;								-*- Lisp -*-
;;; rl.asd -- System definition for RL package
;;;

(defsystem rl
  :name               "rl"
  :description        "A line editor."
  :version            "0.1.0"
  :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
  :licence            "GPLv3"
  :source-control     :git
  :long-description   "A line editor which is not so tiny."
  :depends-on (:dlib :dlib-misc :dl-list :stretchy :char-util
	       :opsys :terminal :terminal-ansi :terminal-dumb :terminal-crunch
	       :collections :ochar :fatchar :fatchar-io :completion :keymap
	       :syntax-lisp :unipose :inator :terminal-inator :theme :ostring
	       :clsql :clsql-sqlite3)
  :serial t	; not entirely correct, but convenient
  :components
  ((:file "package")
   (:file "editor")
   (:file "history")
   (:file "history-store")
   (:file "undo")
   (:file "buffer")
   (:file "display")
   (:file "complete")
   (:file "commands")
   (:file "rl"))
  :in-order-to ((test-op (load-op "rl-test")))
  :perform (test-op (o c) (symbol-call :rl-test :run)))
