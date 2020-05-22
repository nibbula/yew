;;;
;;; package.lisp - RL package definition.
;;;

(defpackage :rl
  (:documentation "A line editor.")
  (:use :cl :dlib :dlib-misc :dl-list :stretchy :char-util
	:opsys :terminal :terminal-ansi :collections :ochar :fatchar :fatchar-io
	:completion :keymap :syntax-lisp :unipose :inator :terminal-inator
	:theme :ostring :spot :compound-string :style)
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
   #:*line-editor-prototype*
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
   #:line-editor-auto-suggest-p
   #:line-editor-auto-suggest-rendition
   #:first-point
   #:use-context
   #:use-first-context
   #:with-context
   #:do-contexts
   #:set-all-points
   #:freshen
   #:save-excursion
   #:get-buffer-string
   #:replace-buffer
   #:*history-context*
   #:get-history
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
   ;; Things for defining commands
   #:with-external
   #:defmulti
   #:defmulti-method
   #:defsingle
   #:defsingle-method
   ;; Commands
   #:backward-word
   #:mark-backward-word
   #:forward-word
   #:mark-forward-word
   #:backward-char
   #:mark-backward-char
   #:forward-char
   #:mark-forward-char
   #:beginning-of-line
   #:beginning-of-buffer
   #:end-of-line
   #:end-of-buffer
   #:previous-history
   #:forward-line
   #:previous-line
   #:previous-line-or-history
   #:next-line
   #:next-history
   #:next-line-or-history
   #:beginning-of-history
   #:end-of-history
   #:accept-line
   #:copy-region
   #:set-mark
   #:kill-region
   #:exchange-point-and-mark
   #:redraw-command
   #:insert
   #:delete-region
   #:delete-backward-char
   #:delete-char
   #:delete-char-or-exit
   #:backward-kill-word
   #:kill-word
   #:kill-line
   #:backward-kill-line
   #:yank
   #:downcase-region
   #:upcase-region
   #:downcase-word
   #:upcase-word
   #:capitalize-word
   #:un-studly-cap
   #:delete-horizontal-space
   #:transpose-characters
   #:quote-region
   #:finish-line
   #:pop-to-lish
   #:abort-command
   #:toggle-debugging
   #:quoted-insert
   #:self-insert
   #:newline
   #:set-key-command
   #:what-cursor-position
   #:exit-editor
   #:beep-command
   #:bracketed-paste
   #:char-picker-command
   #:unipose-command
   #:insert-file
   #:add-cursor-on-next-line
   #:just-one-context
   #:next-like-this
   #:reset-stuff
   #:complete-filename-command
   #:show-filename-completions-command
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
