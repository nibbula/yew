;;;
;;; package.lisp - RL package definition.
;;;

(defpackage :rl
  (:documentation "A line editor.")
  (:use :cl :dlib :dlib-misc :dl-list :stretchy :char-util
	:opsys :terminal :terminal-ansi :collections :ochar :fatchar :fatchar-io
	:completion :keymap :syntax-lisp :unipose :inator :terminal-inator
	:theme :ostring)
  (:import-from :inator
		#:point #:mark #:clipboard #:quit-flag #:command #:last-command)
  (:export
   ;; Main functionality
   #:rl
   #:rl-read-line
   ;; Misc dorky crap
   #:read-filename
   #:read-choice
   #:edit-value
   ;; Things that one might want to use externally, like in external commands.
   #:line-editor
   #:line-editor-p
   #:make-line-editor
   #:screen-relative-row
   #:screen-col
   #:line-editor-terminal
   #:line-editor-terminal-device-name
   #:line-editor-terminal-class
   #:line-editor-input-callback
   #:line-editor-output-callback
   #:line-editor-debug-log
   #:line-editor-keymap
   #:line-editor-local-keymap
   #:line-editor-highlight-region
   #:line-editor-highlight-attr
   #:line-editor-allow-history-duplicates
   #:line-editor-allow-history-blanks
   #:get-buffer-string
   #:replace-buffer
   #:*history-context*
   #:history-entry-time
   #:history-entry-line
   #:history-entry-modified
   #:history-clear
   #:history-nth
   #:history-add
   #:history-current
   #:history-file-name
   #:show-history
   #:history-table
   #:history-save
   #:history-load
   #:history-store
   #:history-store-file-name
   #:history-store-save
   #:history-store-load
   #:history-store-start
   #:history-store-done
   #:history-store-default-file-name
   #:text-history-store
   #:db-history-store
   #:db-history-store-connection
   #:complete-filename-command
   ;; Variables?
   #:*line-editor*
   #:*normal-keymap*
   #:*ctlx-keymap*
   #:*escape-keymap*
   #:*vi-insert-mode-keymap*
   #:*vi-command-mode-keymap*
   #:*default-prompt*
   #:*completion-list-technique*
   #:*completion-really-limit*
   #:*completion-short-divisor*
   #:*terminal-name*
   #:*entry-hook*
   #:*exit-hook*
   ))
(in-package :rl)

;; EOF
