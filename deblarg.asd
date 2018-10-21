;;;								-*- Lisp -*-
;;; deblarg.asd -- System definition for deblarg
;;;

(defsystem deblarg
    :name               "deblarg"
    :description        "Command line Lisp debugger."
    :version            "0.2.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "The “Dynamic Environment Belated Lisp Activation Record Grappler”.

This exists because I wanted command line editing in the debugger from my
REPL. It does afford one that modicum of efficacy, but scant else. Another
smidgeon of utility is a uniform interface between platforms. Otherwise, it is
quite lacking of features."
    :depends-on
    (:dlib :char-util :keymap :table-print :opsys :terminal :terminal-ansi
     :terminal-crunch :terminal-table :rl :collections :fatchar :fatchar-io
     :tiny-repl #+sbcl :sb-introspect)
    :serial t
    :components
    ((:file "package")
     (:file "base")
     (:file "deblarg-sbcl"   :if-feature :sbcl)
     (:file "deblarg-ccl"    :if-feature :ccl)
     (:file "deblarg-clisp"  :if-feature :clisp)
     (:file "deblarg-others" :if-feature (:not (:or :sbcl :ccl :clisp)))
     (:file "deblarg")))
