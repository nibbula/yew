;;;
;;; resize-cmds.lisp - Commands for resize.
;;;

(in-package :resize)

(lish:defcommand resize
  ((old boolean :short-arg #\o
    :help "Act like the unix resize for old shells.")
   (interactive boolean :short-arg #\i :help "Resize the window interactively.")
   (width integer :help "Width in character cells to resize to.")
   (height integer :help "Height in character cells to resize to."))
  :args-as args
  "Resize the window or get the window size."
  (when (not terminal:*terminal*)
    (error "You have to run this in a terminal."))
  ;; default from *input*
  (when (and (not width) (consp lish:*input*) (integerp (car lish:*input*)))
    (setf width (car lish:*input*)))
  (when (and (not height)
	     (consp lish:*input*)
	     (or (integerp (cdr lish:*input*))
		 (integerp (second lish:*input*))))
    (setf height (if (integerp (cdr lish:*input*))
		     (cdr lish:*input*)
		     (second lish:*input*))))
  (setf lish:*output* (apply 'resize-terminal args)))

;; End
