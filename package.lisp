;;
;; package.lisp - Package definition for Lish
;;

(defpackage :lish
  (:documentation
   "Unix Shell & Lisp somehow smushed together.

Lish is a program designed to make typing operating system commands, and
Common Lisp expressions, convienient. It combines the features of a
traditional operating system shell with a Lisp REPL. It's designed to
hopefully have little annoyance to people familair with a POSIX shell. But it
does not have exact compatibility with POSIX shells. In particular Lish can
run any operating system programs, but commands can also be defined in Lisp,
which usually run directly in the same process.

The motivation for writing Lish came from the annoyance of having to swtich
between a Lisp REPL and a Unix shell.
But you can just use Lish for it's shell features.
You might also want to use Lish for it's enhanced programabilty, compared
to most shells. For example by loading other software, Lish can simultaneously
be a shell and a web server, text editor, symbolic math solver, music composer,
etc.

Lish does have some novel features compared to other shells.

- Commands may be compiled to optimized machine code, depending on your Lisp compiler, and therefore may be somewhat faster than other shells.
- 

")
  (:use :cl :dlib :dlib-misc :opsys :stretchy :glob :completion :tiny-rl
	:cl-ppcre :terminal :terminal-ansi)
  (:export
   ;; Main entry point(s)
   #:lish
   #:shell-toplevel
   ;; variables
   #:*lish-level*
   #:*shell*
   #:*old-pwd*
   #:*dir-list*
   #:*shell-path*
   #:*accepts*
   #:*output*
   #:*input*
   ;; (installation)
   #:make-standalone
   ;; shell options
   #:lish-prompt-char
   #:lish-prompt-string
   #:lish-prompt-function
   #:lish-sub-prompt
   #:lish-ignore-eof
   #:lish-debug
   #:make-prompt
   ;; shell object
   #:shell
   #:lish-aliases
   #:lish-editor
   #:lish-old-pwd
   #:lish-dir-list
   #:lish-suspended-jobs
   #:lish-options
   ;; arguments
   #:argument
   #:arg-name #:arg-type #:arg-value #:arg-default #:arg-repeating
   #:arg-optional #:arg-hidden #:arg-prompt #:arg-help #:arg-short-arg
   #:arg-long-arg
   ;; argument types
   #:arg-boolean #:arg-number #:arg-integer #:arg-float #:arg-string
   #:arg-keyword #:arg-object #:arg-date #:arg-pathname
   #:arg-choice #:arg-choices #:arg-choice-labels
   #:arg-lenient-choice
   ;; argument generics
   #:convert-arg #:argument-choices
   ;; commands
   #:command #:command-name #:command-function #:command-arglist
   #:command-built-in-p #:command-loaded-from #:command-accepts
   #:defcommand
   #:!cd #:!pwd #:!pushd #:!popd #:!dirs #:!suspend #:!history #:!echo
   #:!help #:!alias #:!unalias #:!type #:!exit #:!source #:!debug #:!bind
   #:!times #:!time #:!ulimit #:!wait #:!export #:!format
   #:!read #:!kill #:!umask #:!jobs #:!exec #:|!:| #:!hash #:!opt
   ;; convenience / scripting
   #:set-alias #:unset-alias #:get-alias
   #:command-pathname
   #:command-paths
   #:input-line-words
   #:command-output-words
   #:command-output-list
   ;; magic punctuation
   #:! #:!? #:!! #:!$ #:!$$ #:!_ #:!-
   #:!and #:!or #:!bg
   #:!> #:!>> #:!>! #:!>>!
   #:!< #:!!<
   ;; internal-ish things that might want to be used
   #:get-command
   #:command-to-lisp-args
   #:posix-to-lisp-args
   #:shell-read
   #:shell-eval
   #:format-prompt
   #:load-file
   #:suspend-job
   ))

;; EOF
