;;
;; terminal-inator.lisp - What is even going on?
;;

(defpackage :terminal-inator
  (:documentation "I'll be back.")
  (:use :cl :dlib :keymap :inator :terminal :fui)
  (:export
   #:terminal-inator
   ))
(in-package :terminal-inator)

(defclass terminal-inator (inator)
  ()
  (:documentation "A terminal-inator."))

(defmethod initialize-instance
    :after ((o terminal-inator) &rest initargs &key &allow-other-keys)
  "Initialize a terminal-inator."
  (declare (ignore initargs)))

;; (defmethod start-inator ((i terminal-inator))
;;   "Start a TERMINAL-INATOR."
;;   (terminal-start i)
;;   (call-next-method))

;; (defmethod finish-inator ((i terminal-inator))
;;   "Stop a TERMINAL-INATOR."
;;   (terminal-end i)
;;   (call-next-method))

(defmethod update-display ((i terminal-inator))
  "Update the view of a TERMINAL-INATOR."
  (call-next-method)
  (tt-finish-output))

(defmethod await-event ((i terminal-inator))
  "Get an event from a TERMINAL-INATOR."
  (declare (ignore i))
  (tt-get-key))

(defmethod message ((i terminal-inator) format-string &rest args)
  "Display a short message."
  (tt-move-to (1- (terminal-window-rows *terminal*)) 0)
  (tt-erase-to-eol)
  ;; We use terminal-format here because tt-format is a macro.
  (apply #'terminal-format *terminal* format-string args))

(defmethod prompt ((i terminal-inator) format-string &rest args)
  "Display a short message, asking the user for input."
  (apply #'message i format-string args)
  (tt-finish-output))

(defun inator-doc-finder (i func)
  "Find documentation for an inator (subclass) method."
  (when (fboundp func)
    (let ((method
	   (and (typep (symbol-function func) 'generic-function)
		(find-method (symbol-function func) '()
			     (list (class-of i)) nil))))
      (when method (documentation method t)))))

(defmethod help ((i terminal-inator))
  "Show help for the inator."
  (typecase (inator-keymap i)
    (keymap
     (display-text "Help"
		   (help-list (inator-keymap i) (_ (inator-doc-finder i _)))
		   :justify nil))
    (list
     (display-text "Help"
		   (loop :for k :in (inator-keymap i)
		      :append
		      (help-list k (_ (inator-doc-finder i _))))
		   :justify nil))))

(defmethod redraw ((i terminal-inator))
  "Redraw the screen."
  (tt-clear)
  (tt-finish-output)
  (update-display i))

(defmethod read-key-sequence ((i terminal-inator))
  "Read a key sequence."
  (get-key-sequence (λ () (terminal-get-key *terminal*))
		    (inator-keymap i)))

(defmethod describe-key-briefly ((i terminal-inator))
  (prompt i "Press a key to describe: ")
  (let* ((key-seq (read-key-sequence i))
	 (action (key-sequence-binding key-seq (inator-keymap i))))
    (if action
	(message i "~a is bound to ~a" (key-sequence-string key-seq) action)
	(message i "~a is not defined" (key-sequence-string key-seq)))))

;; EOF
