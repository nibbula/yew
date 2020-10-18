;;
;; package.lisp - Deblarg package definition
;;

(defpackage :deblarg
  (:documentation
   "A crappy half-assed debugger for your enjoyment and frustration. But at
least you can type things using RL.")
  (:use :cl :dlib :char-util :table :table-print :keymap :terminal
	:terminal-ansi :terminal-table :rl :collections :fatchar :fatchar-io
	:tiny-repl #+sbcl :sb-introspect :reader-ext :source-path)
  (:export
   #:deblarg
   #:*default-interceptor*
   #:*interceptor-condition*
   #:*visual-mode*
   #:toggle
   #:active-p
   #:activate
   #:deactivate
   #:with-debugger-io
   #:debugger-backtrace
   #:debugger-wacktrace
   ))
(in-package :deblarg)

;; EOF
