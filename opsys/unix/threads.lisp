;;;
;;; threads.lisp - Things dealing wtih threads.
;;;

(in-package :opsys-unix)

(defctype pthread-t :unsigned-long)

;; @@@ This isn't even close to the fully stupid interface.

(defcfun pthread-create :int
  "Create a new thread running ‘start-routine’ passed ‘arg’ with attributes
described in ‘attr’. A handle is stored in ‘newthread’."
  (newthread (:pointer pthread-t))
  ;; (attr (:pointer pthread-attr-t))
  (attr :pointer)
  (start_routine :pointer)		; void *(*start_routine) (void *)
  (arg (:pointer :void)))

(defcfun pthread-exit :void
  "Terminate the calling thread and return ‘retval’. As you might guess, this
function should never return."
  (retval (:pointer :void)))

(defcfun pthread-join :int
  "Make the calling thread wait for termination of the ‘thread’. The
 exit status is stored in ‘thread_return’, if it's not NULL."
  (thread pthread-t)
  (thread_return :pointer))

;; Supporting threads on the language level can be different than supporting
;; operating system threads. Here we make a minimal interface to language
;; level thread support, mostly by wrapping what the implementation provides.

;; Previously we just used bordeaux-threads, but since they removed the
;; ability to even load on non-threaded lisps, we have to do this. It
;; reinforces the idea that trivial dependencies are bad, and every dependency
;; is a risk. Why would a modern Lisp be non-threaded? Well, single core
;; machines still exist, and even if they didn't, it could be running on a
;; single core virtual machine. Why would one want it to load on non-thread
;; supporting Lisps? Because the possibility that the same compiled code may
;; run on threaded and non-threaded Lisps, and therefore should adapt to that.
;; But beyond that, why the fuck would you remove functionality that wasn't
;; harming anything, especially when doing so could make your users' software
;; fail and maintaining compatibility on Lisp is usually very easy?

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *supports-threads-p*
    (feature-expression
     '(or (and sbcl sb-thread)
       (and openmcl openmcl-native-threads)
       (and ecl threads)
       (and mt clisp)
       ;; not done only out of lazyness
       ;; lispworks
       ;; (and allegro multiprocessing)
       )))
  (when *supports-threads-p*
    (config-feature :os-t-threads)))

;; This is so fucking stupid. Why can't we just get beyond 1994.

;; I think this has to be at top level.
#+os-t-threads
(defparameter *thread-type*
  #+sbcl 'sb-thread:thread
  #+ccl 'ccl:process
  #+ecl 'mp:process
  #+clisp 'mt:thread
  #-(or sbcl ccl ecl clisp) nil)

#-os-t-threads
(defparameter *thread-type* nil)

(cond
  #+os-t-threads
  (*supports-threads-p*
   (defun threadp (thread)
     (typep thread *thread-type*))
   (defun make-thread (function &key name)
     #+sbcl (sb-thread:make-thread function :name name)
     #+ccl (ccl:process-run-function name function)
     #+ecl (mp:process-run-function name function)
     #+clisp (mt:make-thread function
			     :name name
			     :initial-bindings mt:*default-special-bindings*)
     )
   (defun join-thread (thread)
     #+sbcl (sb-thread:join-thread thread)
     #+ccl (ccl:join-process thread)
     #+ecl (mp:process-join thread)
     #+clisp (values-list (mt:thread-join thread))
     )
   (defun destroy-thread (thread)
     #+sbcl (sb-thread:terminate-thread thread)
     #+ccl (ccl:process-kill thread)
     #+ecl (mp:process-kill thread)
     #+clisp (mt:thread-interrupt thread :function t)
     )
   (defun thread-name (thread)
     #+sbcl (sb-thread:thread-name thread)
     #+ccl (ccl:process-name thread)
     #+ecl (mp:process-name thread)
     #+clisp (mt:thread-name thread)
     )
   (defun all-threads ()
     #+sbcl (sb-thread:list-all-threads)
     #+ccl (ccl:all-processes)
     #+ecl (mp:all-processes)
     #+clisp (mt:list-threads)
     )
   (defun current-thread ()
     #+sbcl sb-thread:*current-thread*
     #+ccl ccl:*current-process*
     #+ecl mp::*current-process*
     #+clisp (mt:current-thread)
     )
   (defun thread-alive-p (thread)
     #+sbcl (sb-thread:thread-alive-p thread)
     #+ccl (not (ccl:process-exhausted-p thread))
     #+ecl (mp:process-active-p thread)
     #+clisp (mt:thread-active-p thread)
     ))
  (t
   (defun threadp (thread)
     (declare (ignore thread))
     nil)
   (defun make-thread (function &key name)
     (declare (ignore function name)))
   (defun join-thread (thread)
     (declare (ignore thread)))
   (defun destroy-thread (thread)
     (declare (ignore thread)))
   (defun thread-name (thread)
     (declare (ignore thread)))
   (defun all-threads ())
   (defun current-thread ())
   (defun thread-alive-p (thread) (declare (ignore thread)))))

;; End
