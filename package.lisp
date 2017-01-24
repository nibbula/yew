;;
;; package.lisp - RL package definition.
;;

;; Copyright © 2007-2017 Nibby Nebbulous
;; Licensed under the GPL (See file LICENSE for details).

(defpackage :rl
  (:documentation "A line editor.")
  (:use :cl :dlib :dlib-misc :keymap :char-util :dl-list :stretchy
	;; :cffi
	:opsys :terminal :terminal-ansi :terminal-curses :fatchar
	:completion :syntax-lisp :unipose)
  (:export
   ;; main functionality
   #:rl-read-line
   #:rl
   #:line-editor
   #:line-editor-p
   #:make-line-editor
   #:screen-row
   #:screen-col
   #:line-editor-terminal
   #:line-editor-terminal-device-name
   #:line-editor-terminal-class
   #:line-editor-input-callback
   #:line-editor-output-callback
   #:line-editor-debug-log
   #:line-editor-keymap
   #:line-editor-local-keymap
   #:show-history
   #:history-clear
   #:*line-editor*
   #:*default-prompt*
   #:*completion-list-technique*
   #:*completion-really-limit*
   #:*completion-short-divisor*
   #:*normal-keymap*
   #:*ctlx-keymap*
   #:*escape-keymap*
   #:*terminal-name*
   ;; misc
   #:get-lone-key
   #:read-filename
   #:read-choice
   #:complete-filename-command
   ))
(in-package :rl)

;; EOF
