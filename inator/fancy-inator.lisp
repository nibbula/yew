;;;
;;; fancy-inator.lisp - A terminal-inator with fancy features.
;;;

(defpackage :fancy-inator
  (:documentation "A terminal-inator with fancy features.")
  (:use :cl :inator :char-util :keymap :fatchar :terminal :terminal-inator)
  (:export
   ;; Classes
   #:fancy-inator
   ;; Generic functions
   #:eval-expression
   ;; Keymaps
   ;; #:*default-fancy-inator-keymap*
   #:*default-fancy-inator-escape-keymap*
   ;; #:*default-fancy-inator-ctrl-x-keymap*
   ))
(in-package :fancy-inator)

;; Right now the fancy features are just eval-expression.
;; :terminal-inator :fui :tiny-repl

(defkeymap *default-fancy-inator-escape-keymap* ()
  `((#\escape . eval-expression-command)
    (#\+      . set-key-command)))

(defkeymap *default-fancy-inator-keymap* ()
  `((#\escape . *default-fancy-inator-escape-keymap*)))

(defclass fancy-inator (terminal-inator)
  ()
  (:default-initargs
   :default-keymap *default-fancy-inator-keymap*)
  (:documentation "An inator with fancy features."))

(defgeneric eval-expression-command (inator)
  (:documentation "Prompt for an expression an evaluate it.")
  (:method ((o inator))
    (tt-move-to (1- (tt-height)) 0)
    (tt-finish-output)
    (with-simple-restart (abort "Go back to the inator.")
      (handler-case
	  (fui:display-text
	   "Eval results"
	   (list (with-output-to-string (stream)
		   (prog1
		       (let ((*standard-output* stream))
			 (tiny-repl:tiny-repl :prompt-string "Eval: "
					      :quietly t :once t
					      :output stream
					      :terminal *terminal*))
		     (redraw o)
		     (update-display o))))
	   :justify nil :min-width 16)
	(error (c)
	  (if (fui:popup-y-or-n-p
	       (span-to-fat-string
		`((:red "Error: ") ,(apply #'format nil "~a" (list c))
		  #\newline #\newline "Enter the debugger?"))
	       :default #\N)
	      (invoke-debugger c)
	      (invoke-restart (find-restart 'abort))))))
    (redraw o)))

(defgeneric set-key-command (inator)
  (:documentation "Bind a key interactively.")
  (:method ((o fancy-inator))
    (prompt o "Set key: ")
    (let* ((key-seq (read-key-sequence o))
	   (cmd (rl::ask-function-name (format nil "Set key ~a to command: "
					       (key-sequence-string key-seq)))))
      (if cmd
	  (progn
	    ;; @@@ maybe inators should have an ensure-local-keymap method
	    (unless (inator-local-keymap o)
	      (pushnew
	       (setf (inator-local-keymap o)
		     (make-instance 'keymap :name "Fancy Inator Local Keymap"))
	       (inator-keymap o)))
	    (set-key key-seq cmd (inator-local-keymap o)))
	  (message o "Not a function.")))))

;; End
