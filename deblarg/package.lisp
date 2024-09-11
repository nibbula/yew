;;;
;;; package.lisp - Deblarg package definition
;;;

(defpackage :deblarg
  (:documentation
   "A crappy half-assed debugger for your enjoyment and frustration. But at
least you can type things using RL.")
  (:use :cl :dlib :char-util :table :dlib-misc :table-print :keymap :terminal
	:terminal-ansi :terminal-table :rl :collections :ochar :fatchar
        :fatchar-io :tiny-repl #+sbcl :sb-introspect :reader-ext :source-path)
  (:export
   #:deblargger
   #:deblargger-current-frame
   #:deblargger-saved-frame
   #:deblargger-condition
   #:deblargger-term
   #:deblargger-visual-mode
   #:deblargger-visual-term
   #:*deblarg*
   #:*condition*
   #:*debug-term*
   #:*deblargger-entry-hook*
   #:deblarg
   #:*default-interceptor*
   #:toggle
   #:active-p
   #:activate
   #:deactivate
   #:with-debugger-io
   #:debugger-backtrace
   #:debugger-wacktrace
   #:invoke-command
   ))
(in-package :deblarg)

;; End
