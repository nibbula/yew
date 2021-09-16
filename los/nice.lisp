;;;
;;; nice.lisp - Modify scheduling priority.
;;;

(defpackage :nice
  (:documentation "Run a system command with modified scheduling priority.")
  (:use :cl :dlib :opsys :lish)
  (:export
   #:!nice
   #:!renice
   ))
(in-package :nice)

(defun coerce-to-uid (thing what)
  (typecase thing
    (string
     (multiple-value-bind (n pos) (parse-integer thing :junk-allowed t)
       (if n
	   (if (= pos (length thing))
	       n
	       (nos:user-id :name thing))
	   (nos:user-id :name thing))))

    (integer thing)
    (t
     (error "~a should be string or integer, not a ~s." what (type-of thing)))))

(defun check-priority (priority)
  "Error of the priority is out of range."
  (let ((high (max nos:*os-process-most-favorable-priority*
		   nos:*os-process-least-favorable-priority*))
	(low (min nos:*os-process-most-favorable-priority*
		  nos:*os-process-least-favorable-priority*)))
    (when (or (> priority high) (< priority low))
      (error "priority should be between ~s (most favorable) and ~s ~
              (least favorable)."
	     nos:*os-process-most-favorable-priority*
	     nos:*os-process-least-favorable-priority*))))

;; @@@ arg dependent completion for id?
(defcommand renice
  ((priority integer :optional nil :help "The priority to set.")
   (group boolean :short-arg #\g :help "Set priority for the process group.")
   (pid   boolean :short-arg #\p :help "Set priority for the process ID.")
   (user  boolean :short-arg #\u :help "Set priority for the user.")
   (id object :help "What to set the priority of."))
  "Alter scheduling priority."
  (when user
    (setf id (coerce-to-uid id "ID")))
  (check-priority priority)
  (let ((key (cond
	      (group :group)
	      (user :user)
	      (pid :pid))))
    (setf (nos:os-process-priority key id) priority)))

(defmacro with-adjusted-priority ((original adjustment) &body body)
  #+unix
  `(let ((uos:*post-fork-hook*
	  (append uos:*post-fork-hook*
		  (list
		   (lambda ()
		     (setf (nos:os-process-priority :pid (uos:getpid))
			   (+ ,adjustment ,original)))))))
     ,@body)
  #-unix
  `(progn ,@body))

(defcommand nice
  ((adjustment integer :short-arg #\n :default 10
    :help
    "Integer to add to the process priority, making it more nice (lower scheduling priority).")
   (arguments string :rest t :help "Like the arguments to env."))
  "Run a system command with modified scheduling priority. With no arguments,
just print the current process priority."
  (if arguments
      (let* ((pid (nos:current-process-id))
	     (original (nos:os-process-priority :pid pid)))
	(check-priority (+ original adjustment))
	(with-adjusted-priority (original adjustment)
	  (lish:!env :arguments arguments)))
      (let ((result (nos:os-process-priority :pid (nos:current-process-id))))
	(format t "~d~%" result)
	(setf *output* result))))

;; End
