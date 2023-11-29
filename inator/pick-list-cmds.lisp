;;;
;;; pick-list-commands.lisp - Commands for pick-list.
;;;

(in-package :pick-list)

#+lish
(lish:defcommand pick-list
  ((multiple boolean :short-arg #\m :help "True to pick multiple results.")
   (print boolean :short-arg #\p :help "True to force printing the results.")
   (lines string :repeating t))
  :accepts (:stream :list)
  "Pick something from the list of lines of input."
  ;; (when lish:*input*
  ;;   (format t "pick-list *input* = ~s~%" lish:*input*))
  (setf lish:*output*
	(pick-list
	 (or lines
	     (and (listp lish:*input*) lish:*input*)
	     (lish:input-line-list (and (streamp lish:*input*) lish:*input*)))
	 :multiple multiple))
  (cond
    ((lish:accepts :sequence 'list)
     lish:*output*)
    ((or print (lish:accepts :stream :grotty-stream :unspecified))
     (if (listp lish:*output*)
	 (loop :for o :in lish:*output* :do (princ o) (terpri))
	 (progn (princ lish:*output*) (terpri))))))

;; End
