;;;
;;; terminal-inator.lisp - Inator methods for terminals.
;;;

(defpackage :terminal-inator
  (:documentation "I'll be back.")
  (:use :cl :dlib :keymap :inator :terminal :fui)
  (:export
   #:terminal-inator
   #:terminal-inator-last-event
   #:terminal-inator-last-event-untranslated
   #:with-terminal-inator
   ))
(in-package :terminal-inator)

(defclass terminal-inator (inator)
  ((last-event
    :initarg :last-event :accessor terminal-inator-last-event :initform nil
    :documentation "The last event we got.")
   (last-event-untranslated
    :initarg :last-event-untranslated
    :accessor terminal-inator-last-event-untranslated
    :initform nil
    :documentation "The untranslated last event."))
  (:documentation "A terminal-inator."))

(defmethod initialize-instance
    :after ((o terminal-inator) &rest initargs &key &allow-other-keys)
  "Initialize a terminal-inator."
  (declare (ignore initargs))
  ;; Allow resize events by default.
  (tt-enable-events :resize))

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
  (when (next-method-p)
    (call-next-method))
  (tt-finish-output))

(defmethod await-event ((i terminal-inator))
  "Get an event from a TERMINAL-INATOR."
  (with-slots (last-event (ev last-event-untranslated)) i
    (setf ev (tt-get-key))
    ;; Translate some events
    (typecase ev
      (tt-mouse-button-event
       (case (tt-mouse-button ev)
	 (:button-4 (setf last-event :scroll-up))
	 (:button-5 (setf last-event :scroll-down))
	 (t
	  (setf last-event
		(keywordify
		 (if (typep ev 'tt-mouse-button-release)
		     :button-release
		     (s+ (string (tt-mouse-button ev))
			 "-press")))))))
      (t (setf last-event ev)))
    last-event))

(defmethod message ((i terminal-inator) format-string &rest args)
  "Display a short message."
  (tt-move-to (- (terminal-window-rows *terminal*)
		 ;; @@@ This is such a kludgey hack
		 (length (dlib-misc:calculate-line-endings
			  (apply #'format nil format-string args)
			  0 (tt-width) nil nil nil))
		 1)
	      0)
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
  (let* ((doc-string (documentation (class-of i) t))
	 (inator-doc (or (and doc-string (list doc-string "")) nil)))
    (typecase (inator-keymap i)
      (keymap
       (display-text
	(format nil "~:(~a~) Help" (type-of i))
	`(,@inator-doc
	  ,@(help-list (inator-keymap i) (_ (inator-doc-finder i _))))
	:justify nil))
      (list
       (display-text
	(format nil "~:(~a~) Help" (type-of i))
	`(,@inator-doc
	  ,@(loop :for k :in (inator-keymap i)
	      ;; :append (list (princ-to-string k))
	      :append
		(help-list k (_ (inator-doc-finder i _)))))
	:justify nil)))))

(defmethod redraw ((i terminal-inator))
  "Redraw the screen."
  (tt-clear)
  (tt-finish-output)
  (update-display i))

(defmethod read-key-sequence ((i terminal-inator))
  "Read a key sequence."
  ;; (get-key-sequence (lambda () (terminal-get-key *terminal*))
  (get-key-sequence (lambda () (await-event i))
		    (inator-keymap i)))

(defmethod describe-key-briefly ((i terminal-inator))
  (prompt i "Press a key to describe: ")
  (let* ((key-seq (read-key-sequence i))
	 (action (key-sequence-binding key-seq (inator-keymap i))))
    (if action
	(message i "~a is bound to ~a" (key-sequence-string key-seq) action)
	(message i "~a is not defined" (key-sequence-string key-seq)))))

(defmacro with-terminal-inator ((var type &rest args) &body body)
  "Evaluate BODY with a new terminal-inator of type TYPE, made with ARGS passed
to MAKE-INSTANCE, with VAR bound to the new instance."
  (with-names (result)
    `(let ((,result
	    (with-terminal ()
	      (with-immediate ()
		(with-enabled-events ('(:resize :mouse-buttons))
		  (let ((,var (make-instance ,type ,@args)))
		    ,@body))
		;; Move to the bottom of the screen on exit.
		(tt-move-to (1- (tt-height)) 0)))))
       ,result)))

;; EOF
