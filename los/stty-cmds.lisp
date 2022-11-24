;;;
;;; stty-cmds.lisp - Commands for stty.
;;;

(in-package :stty)

;; (defclass arg-stty-setting (lish:arg-choice)
;;   ()
;;   (:documentation "Terminal setting name.")))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (lish:defargtype stty-setting (lish:arg-lenient-choice)
    "Terminal setting name."
    ()))

(defmethod lish:convert-arg ((arg lu::arg-stty-setting) (value string)
			     &optional quoted)
  (declare (ignore quoted))
  (let ((check-value (or (and value (plusp (length value))
			      (find (char value 0) "-+")
			      (subseq value 1))
			 value))
	(choices (lish:argument-choices arg))
	choice)
    (unless choices
      (error "Choice argument has no choices ~a." (lish:arg-name arg)))
    (if (setf choice (find check-value choices
			   :test (lish:arg-choice-test arg)))
	choice
	(error "~s is not one of the choices for the argument ~:@(~a~)."
	       value (lish:arg-name arg)))))

(defun complete-setting (context pos all &key parsed-exp)
  "Complete stty settings."
  (declare (ignore parsed-exp))
  (let ((word (subseq context 0 pos))
	minus)
    (when (and (not (zerop (length word)))
	       (char= (char word 0) #\-))
      (setf word (subseq word 1)
	    minus t))
    (if all
	(if minus
	    (mapcar (_ (s+ #\- _))
		    (string-completion-list word *all-setting-strings*))
	    (string-completion-list word *all-setting-strings*))
	(let ((result (string-completion word *all-setting-strings*)))
	  (when minus
	    (setf (completion-result-completion result)
		  (s+ #\- (completion-result-completion result))))
	  result))))

(lish:defcommand stty
  ((settings stty-setting :optional t :repeating t :rest t
    :choices *all-setting-strings*
    :help "Terminal setting to effect.")
   (all boolean :short-arg #\a
    :help "True to show all settings.")
   (device pathname :short-arg #\d
    :default *default-console-device-name*
    ;; :completion-function 'complete-setting
    :help "Terminal device to operate on."))
  "Show and change terminal settings."
  (if settings
      (multiple-value-bind (set-modes unset-modes control-chars)
	  (gather-settings settings)
	(set-tty :device device :set-modes set-modes :unset-modes unset-modes
		 :control-chars control-chars))
      (describe-tty :format (if all :stty :nice) :device device)))

;; End
