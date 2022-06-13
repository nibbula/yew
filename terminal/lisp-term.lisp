;;;
;;; lisp-term.lisp - The outer part of a terminal emulator.
;;;

(defpackage :lisp-term
  (:documentation "The outer part of a terminal emulator.")
  (:use :cl :dlib :terminal :ansi-terminal :cffi :opsys :opsys-unix
        :terminal-ansi)
  (:export
   #:emulate
   ))
(in-package :lisp-term)

(defstruct term
  "State for the terminal emulator."
  master				; File descriptor of the control side
  slave					; File descriptor of the terminal side
  pid					; Process ID of the child
  out-term				; Terminal for output (and input)
  ansi					; The emulator stream
  (quit-flag nil :type boolean)		; True to stop
  input-buffer)				; Foreign byte buffer for input

(defvar *term* nil
  "The dynamic state.")

(defvar *buf-size* 1024
  "Size of buffer for reading from the pty.")

(defun use-the-fd (fd)
  "Use the file descriptor ‘fd’ as the processes controlling terminal and the
standard input, output, and error. Also put the process in it's own session.
Finally close the original ‘fd’."
  (let ((pid (uos:getpid)))
    (declare (ignore pid))
    (uos:setsid)
    (posix-ioctl fd uos::+TIOCSCTTY+ (null-pointer))
    ;; @@@ what if it fails?
    (flet ((d2 (std-fd)
	     (loop :while (and (= -1 (posix-dup2 fd std-fd))
			       (= uos:*errno* +EBUSY+)))))
      (d2 0)
      (d2 1)
      (d2 2))
    (when (> fd 2)
      (posix-close fd)))) ;; @@@ check for fail?

(defun close-other-fds (fd)
  "Close every (or at least some) file descriptors that aren't stdio and ‘fd’."
  (loop :for i :from 1024 :downto 3 ;; @@@ Of course this isn't really
    :when (/= i fd)
    :do (posix-close i)))

(defun run-the-program (fd program &optional args #| (environment nil env-p) |#)
  "Run the ‘program’ in the terminal slave ‘fd’ with ‘args’ as the list of
string arguments."
  (let* ((cmd-and-args (cons program args))
	 (argc (length cmd-and-args))
	 child-pid)
    (with-foreign-object (argv :pointer (1+ argc))
      (with-foreign-string (path program)
	(unwind-protect
	  (progn
	    ;; Fill argv with C allocated strings
	    (loop :with i = 0
		  :for arg :in cmd-and-args :do
		     (setf (mem-aref argv :pointer i)
			   (foreign-string-alloc arg))
		     (incf i))
	    ;; Null terminate the array
	    (setf (mem-aref argv :pointer argc) (null-pointer))

	    (setf child-pid (fork))
	    (when (= child-pid 0)
	      ;; in the child
	      (close-other-fds fd)
	      (use-the-fd fd)
	      (when (= (uos::execvp path argv) -1)
		(write-string "Exec of ")
		(write-string program)
		(write-string " failed")
		(write-char #\newline)
		(uos::_exit 1)))
	    ;; in the parent
	    (uos::error-check child-pid)
	    (dbug "pid = ~s~%" child-pid)
	    (posix-close fd)) ;; close the slave
	  ;; Free argv strings
	  (loop :for i :from 0 :below argc
		:unless (null-pointer-p (mem-aref argv :pointer i))
		:do (foreign-free (mem-aref argv :pointer i))))))
    child-pid))

(defun push-it (stream bytes)
  "The terminal pushback callback."
  (declare (ignore stream))
  (dbug "push-it ~s~%" bytes)
  (with-slots (input-buffer master) *term*
    (loop
      :for c :across bytes
      :for i := 0 :then (1+ i)
      :do
	 (setf (mem-aref input-buffer :unsigned-char i) c))
    ;; @@@ how about an error check?
    (uos:posix-write master input-buffer (length bytes))))

(defun process-keys (term)
  "Process any key input coming from the terminal."
  (with-slots (master slave pid out-term ansi quit-flag input-buffer) term
    (let (key)
      (loop
	:while (terminal-listen-for out-term 0)
	:do
	   (dbug "before get-key~%")
	   (setf key (terminal-get-key out-term))
	   (dbug "key ~s ~s~%" key (ignore-errors (char-code key)))
	   (cond
	     ((keywordp key)
	      (cond
		((eq key :s-f12) ;; escape hatch
		 (setf quit-flag t)
		 (uos:kill pid uos:+SIGKILL+)
		 (uos::wait-and-report pid))
		((eq key :resize)
		 (dbug " ---- RESIZE ---- ~%")
		 (let ((rows (terminal-window-rows out-term))
		       (cols (terminal-window-columns out-term)))
		   (uos:set-window-size-struct
		    slave
		    (nos:make-window-size :rows rows :columns cols))))
		(t
		 (let ((s (key-string ansi key)))
		   (when s
		     (loop
		       :for c :across s
		       :for i := 0 :then (1+ i)
		       :do
			  (setf (mem-aref input-buffer :unsigned-char i) c))
		     (uos:posix-write master input-buffer (length s)))))))
	     ((characterp key)
	      (setf (cffi:mem-ref input-buffer :unsigned-char)
		    (char-code key))
	      (dbug "write char ~s~%" (char-name key))
	      (uos:posix-write master input-buffer 1)))))))

;; Currently this is like screen or tmux, but with only one process, so it's
;; only really useful for testing our emulation.

(defun emulate (device &key (program "/bin/bash"))
  "Run ‘program’ in a terminal on ‘device’."
  (let* ((out-term (make-instance 'terminal-ansi
				  :device-name device))
	 (ansi (make-instance 'ansi-terminal:ansi-stream
			      :terminal out-term
			      :pushback-function #'push-it))
	 (*term* (make-term
		  :out-term out-term
		  :ansi ansi))
	 fds master slave tty input buf pid)
    (unwind-protect
      (progn
	(terminal-enable-event out-term '(:resize :mouse-buttons))
	(terminal-start out-term)
	(multiple-value-setq (master slave)
	  (uos:open-pseudo-terminal
	   :window-size (nos:make-window-size
			 :rows (terminal-window-rows out-term)
			 :columns (terminal-window-columns out-term))))
	(dbug "master = ~s slave = ~s~%" master slave)
	(setf (term-master *term*) master
	      (term-slave *term*) slave
	      tty (terminal-file-descriptor (term-out-term *term*)))
	(dbug "tty fd = ~s~%" tty)
	(setf fds `((,tty :read)
		    (,master :read))
	       input (cffi:foreign-alloc :unsigned-char :count 100)
	      (term-input-buffer *term*) input
	      buf (cffi:make-shareable-byte-vector *buf-size*))

	(dbug "before run~%")
	(setf pid (run-the-program slave program)
	      (term-pid *term*) pid)
	(dbug "after run~%")

	(with-foreign-object (status-ptr :int 1)
	  (setf (mem-ref status-ptr :int) 0)
	  (cffi:with-pointer-to-vector-data (bufp buf)
            (uos:with-nonblocking-io (master)
	      (with-simple-restart (abort "Return to the terminal main loop.")
		(loop :with results
                :while (not (term-quit-flag *term*))
                :do
		 (finish-output ansi)
		 (terminal-finish-output out-term)
		 (dbug "before select ~s~%" fds)
		 ;; (when (terminal-listen-for out-term 0)
		 ;;   (process-keys *term*))
                 (setf results (uos:lame-select fds nil))
		 (dbug "after select = ~s~%" results)
		 (when (/= 0 (uos::real-waitpid pid status-ptr
						uos::+WAIT-NO-HANG+))
			   ;; (member (multiple-value-list
			   ;; 	(uos::wait-return-status
			   ;; 	(mem-ref status-ptr :int))
			   ;;     '(:exited :coredump :
		   ;;; @@@ bogus
		   (dbug "wait ~s~%" (uos::wait-return-status
				      (mem-ref status-ptr :int)))
		   (setf (term-quit-flag *term*) t))
		 (dbug "before io loop~%")
                 (loop :for r :in results :do
                   ;; output from the master
		   (when (eq (car r) master)
		     (case (second r)
		       (:read
			(dbug "reading from master~%")
			(let ((rr (uos:posix-read master bufp *buf-size*)))
			  (dbug "got from master ~s~%" rr)
			  (cond
                            ((eql rr -1)
			     (case uos:*errno*
			       (uos:+EAGAIN+
				#| no prob |#
				)
			       (t
				(format *debug-io* "read error ~s ~s~%"
					uos:*errno*
					(nos:error-message uos:*errno*)))))
                            ((eql rr 0)
                             ;; nothing read?
                             )
                            ((plusp rr)
                             ;; Feed to the the ANSI emulator
			     (dbug "hanky ~s~%" (type-of buf))
                             (write-sequence
			      (unicode:utf8b-bytes-to-string
			       (displaced-subseq buf 0 rr))
			      ansi)
			     ;; (finish-output ansi)
			     (dbug "hokay~%")
			     ))))
		       (:write
			(dbug "master writable?~%"))
		       (t
			(dbug "master something else? ~s~%" (second r)))))

                     ;; Input from the outer terminal
		   (when (eq (car r) tty)
		     (case (second r)
		       (:read
			(dbug "tty read~%")
			(process-keys *term*))
		       (:write
			(dbug "tty write~%")
			(finish-output ansi)
			(terminal-finish-output out-term))
		       (t
			(dbug "tty ~s~%" (second r))))))))))))
      (cffi:foreign-free input)
      (uos:posix-close slave)
      (uos:posix-close master)
      (terminal-done out-term))))

;; End
