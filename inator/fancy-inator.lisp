;;;
;;; fancy-inator.lisp - A terminal-inator with fancy features.
;;;

(defpackage :fancy-inator
  (:documentation "A terminal-inator with fancy features.")
  (:use :cl :inator :char-util :keymap :fatchar :terminal)
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

(defclass fancy-inator (inator)
  ()
  (:documentation "An inator with fancy features."))

(defkeymap *default-fancy-inator-escape-keymap* ()
  `((#\escape . *default-fancy-inator-escape-keymap*)))

(defgeneric eval-expression-command (inator)
  (:documentation "Prompt for an expression an evaluate it.")
  (:method ((o inator))
    (tt-move-to (1- (tt-height)) 0)
    (with-simple-restart (abort "Go back to the inator.")
      (handler-case
	  (fui:display-text
	   "Eval results"
	   (list (with-output-to-string (stream)
		   (prog1
		       (let ((*standard-output* stream))
			 (tiny-repl:tiny-repl :prompt-string "Eval: "
					      :quietly t :once t
					      :output stream))
		     (redraw o)
		     (update-display o)
		     ;; (setf (tt-input-mode) :char)
		     )))
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

;; End
