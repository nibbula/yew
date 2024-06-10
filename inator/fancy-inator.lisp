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

(defun ask-bindable (&optional (prompt "Expression: "))
  "Prompt for an expression suitable for binding to a key and return it."
  (let ((expr (rl::ask-expr prompt)))
    (or (and (symbolp expr) (fboundp expr) expr)
	(and (consp expr) (member (car expr) '(function lambda))
	     (eval expr)))))

(defgeneric ensure-local-keymap (o)
  (:documentation "Make a local keymap for inator ‘o’ if it doesn't have one.")
  (:method ((o inator))
    (unless (inator-local-keymap o)
      (pushnew
       (setf (inator-local-keymap o)
	     (make-instance 'keymap :name "Fancy Inator Local Keymap"))
       (inator-keymap o)))))

(defgeneric set-key-command (inator)
  (:documentation "Bind a key interactively. Prompts for a key press and binding,
which can be a function or a lambda expression. The ‘universal-arguemnt’ can
change which keymap it's bound in:
  - Without universal-argument set, bind it in the local keymap of this instance.
  - With universal-argument set to 4 (one C-u), bind it in the default keymap
    for this class of inator.
  - With universal-argument 16 (two C-u), set it in the keymap for all
    fancy-inators.
  - With universal-argument 65 (three C-u), set it in the global keymap for all
    inators.
")
  (:method ((o fancy-inator))
    (multiple-value-bind (keymap description)
	(case (inator-universal-argument o)
	  ((nil) (values (ensure-local-keymap o)        "local"))
	  (4     (values (inator-default-keymap o)      "class"))
	  (16    (values *default-fancy-inator-keymap*  "fancy"))
	  (64    (values *default-inator-keymap*        "global"))
	  (t     (values (inator-default-keymap o)      "local")))
      (prompt o (format nil "~:(~a~) ~s set key: " description
			(inator-universal-argument o)))
      (let* ((key-seq (read-key-sequence o))
	     (command (ask-bindable
		       (format nil "~:(~a~) set key ~a to command: "
			       description
			       (key-sequence-string key-seq)))))
	(if command
	    (progn
	      (set-key key-seq command keymap)
	      (notify o "~:(~a~) bound ~a to ~a." description
		       (key-sequence-string key-seq) command))
	    (notify o "Not a function."))))))

;; End
