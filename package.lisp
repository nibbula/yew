;;
;; package.lisp - RL package definition.
;;

(defpackage :rl
  (:documentation "A line editor.")
  (:use :cl :dlib :dlib-misc :keymap :char-util :dl-list :stretchy
	;; :cffi
	:opsys :terminal :terminal-ansi :terminal-curses :fatchar
	:completion :syntax-lisp :unipose)
  (:export
   ;; Main functionality
   #:rl
   #:rl-read-line
   ;; Misc dorky crap
   #:read-filename
   #:read-choice
   ;; Things that one might want to use externally, like in external commands.
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
   #:get-buffer-string
   #:replace-buffer
   #:show-history
   #:history-clear
   #:complete-filename-command
   ;; Variables?
   #:*line-editor*
   #:*normal-keymap*
   #:*ctlx-keymap*
   #:*escape-keymap*
   #:*default-prompt*
   #:*completion-list-technique*
   #:*completion-really-limit*
   #:*completion-short-divisor*
   #:*terminal-name*
   ))
(in-package :rl)

;; EOF
