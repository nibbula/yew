;;;
;;; lisp-term.lisp - The outer part of a terminal emulator.
;;;

(defpackage :lisp-term
  (:documentation "The outer part of a terminal emulator.")
  (:use :cl :dlib :terminal :ansi-terminal :cffi :opsys :opsys-unix)
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
  (step-p nil :type boolean)		; True to single step
  debug-term				; Terminal for debugging I/O
  input-buffer)				; Foreign byte buffer for input

(defvar *term* nil
  "The dynamic state.")

(defvar *buf-size* 1024
  "Size of buffer for reading from the pty.")

(defun use-the-fd (fd)
  "Use the file descriptor ‘fd’ as the processes controlling terminal and the
standard input, output, and error. Also put the process in it's own session.
Finally close the original ‘fd’."
  (flet ((report (m)
	   (let ((s (format nil "error ~s ~s~%" m uos:*errno*)))
             (posix-write 1 s (length s)))))
    (let ((pid (uos:getpid))
	  (sid (uos:setsid)))
      (when (eql -1 sid)
	(setf sid (getsid pid)))
      (when (eql -1 (posix-ioctl fd uos::+TIOCSCTTY+ (null-pointer)))
	(report "TIOSCTTY"))

      ;; @@@ what if it fails?
      (flet ((d2 (std-fd)
	       (loop :while (and (= -1 (posix-dup2 fd std-fd))
				 (= uos:*errno* +EBUSY+)))))
	(d2 0)
	(d2 1)
	(d2 2))
      (when (> fd 2)
	(posix-close fd))))) ;; @@@ check for fail?

(defun close-other-fds (fd)
  "Close every (or at least some) file descriptors that aren't stdio and ‘fd’."
  (loop :for i :from 1024 :downto 3 :by 1 ;; @@@ Of course this isn't really
    :when (/= i fd)
    :do (posix-close i)))

(defun run-the-program (fd program &optional args (environment nil #|env-p|#))
  "Run the ‘program’ in the terminal slave ‘fd’ with ‘args’ as the list of
string arguments."
  (let* ((cmd-and-args (cons program args))
	 (argc (length cmd-and-args))
	 child-pid)
    (with-foreign-objects ((argv :pointer (1+ argc))
			   #| (msg-str :string 80) |#)
      ;; (setf *msg-str* msg-str)
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
	      (when environment
		(loop :for (name value) :in environment
		      :do (setf (nos:env name) value)))
	      ;; in the child
	      ;; (msg (s+ "FOOOB" #\newline))
	      (close-other-fds fd)
	      ;; (msg (s+ "BAAR" #\newline))
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

;; @@@ consider merging with one terminal-x11 and putting in terminal-utils
(defun start-in-terminal (new-term func)
  (let* ((*terminal* new-term)
	 (*standard-output* new-term)
	 (*standard-input* new-term)
	 (*error-output* new-term)
	 (*query-io* new-term)
	 ;; (*trace-output* new-term)
	 ;; (*debug-io* *trace-output*) ;; @@@
	 (*terminal-io* new-term)
	 ;;(deblarg::*dont-use-a-new-term* t)
	 ;; (terminal:*default-terminal-type* :x11)
	 )
    (funcall func)))

(defun call-program (fd device program)
  (let ((inner (make-instance 'terminal-ansi:terminal-ansi
			      :file-descriptor fd
			      :device-name device)))
    (with-new-terminal (:crunch *terminal* :wrapped-terminal inner)
      (start-in-terminal *terminal* program))))

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

(defun single-step-write (buffer)
  "Step through output for debugging."
  (with-slots (ansi debug-term) *term*
    (let ((i 0)
	  (len (length buffer))
	  key)
      (flet ((getter ()
	       (when (< i len)
		 (prog1 (aref buffer i)
		   (incf i))))
	     (setter (c)
	       (write-char c ansi)))
	(loop :while (and (< i len) (not (eql #\q key))) :do
	  (terminal-save-cursor debug-term)
	  (terminal-move-to debug-term 12 60)
	  (terminal-write-span debug-term '(:red ">> "))
	  ;; flush output to the underlying terminal
	  (when (eq (ansi-terminal::state ansi) :start)
	    (terminal-finish-output (ansi-terminal::ansi-stream-terminal ansi)))
	  (with-immediate ()
	    (setf key (terminal-get-key debug-term)))
	  (terminal-move-to debug-term 12 60)
	  (terminal-write-string debug-term "   ")
	  (terminal-restore-cursor debug-term)
	  (terminal-finish-output debug-term)
	  (unicode:get-utf8b-char #'getter #'setter))
	(when (eql key #\q)
	  ;; output the remaining bytes without further mugicha
	  (loop :while (< i len) :do
	    (unicode:get-utf8b-char #'getter #'setter)))))))

(defun safer-utf8-write (buffer)
  ;; @@@ Shouldn't we just be able to write bytes? Also I think we get glitches
  ;; when we get a partial character.
  ;;
  ;; We should detect if we're in the
  ;; middle of a character, and see if we can read enough more bytes to finish
  ;; out the character, before converting and sending.
  (write-sequence (unicode:utf8b-bytes-to-string buffer) (term-ansi *term*)))

(defun process-keys (term)
  "Process any key input coming from the terminal."
  (with-slots (master slave pid out-term ansi quit-flag step-p debug-term
	       input-buffer) term
    (let (key)
      (loop
	:do (terminal-finish-output out-term)
	:while (terminal-listen-for out-term 0)
	:do
	   ;; (dbug "before get-key~%")
	   (terminal-finish-output out-term)
	   (setf key (terminal-get-key out-term))
	   (dbug "key ~s ~s~%" key (ignore-errors (char-code key)))
	   (cond
	     ((keywordp key)
	      (cond
		((eq key :s-f12) ;; escape hatch
		 (setf quit-flag t)
		 (uos:kill pid uos:+SIGKILL+)
		 (uos::wait-and-report pid))
		((eq key :s-f1) ;; toggle debugging single step
		 (if debug-term
		     (progn
		       (setf step-p (not step-p))
		       (dbug "Turning single stepping ~:[OFF~;ON~]~%" step-p))
		     (warn "Single stepping won't work without a debug-term.")))
		((eq key :resize)
		 (dbug " ---- RESIZE ---- ~%")
		 (let ((rows (terminal-window-rows out-term))
		       (cols (terminal-window-columns out-term)))
		   (uos:set-window-size-struct
		    slave
		    (nos:make-window-size :rows rows :columns cols))
		   (uos:kill pid uos:+SIGWINCH+)))
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
	      (case key
		;; (#\etx
		;;  ;; wrong?
		;;  (dbug " --- ^C ---~%")
		;;  (uos:kill (- pid) uos:+SIGINT+))
		(t
		 (setf (cffi:mem-ref input-buffer :unsigned-char)
		       (char-code key))
		 (dbug "write char ~s~%" (char-name key))
		 (uos:posix-write master input-buffer 1)))))))))

;; Currently this is like screen or tmux, but with only one process, so it's
;; only really useful for testing our emulation.

(defun emulate (&key (program (or (env "SHELL") "/bin/bash"))
		  x device debug-term (term-env "xterm-256color"))
  "Run ‘program’ in a terminal on ‘device’."
  (cond
    ((or x device)
     ;; If we are using a separate terminal, use the current terminal as
     ;; the debug device
     (setf debug-term *terminal*))
    (t
     ;; When not using X11 or given a separate device, use the current tty.
     (setf device (uos:ttyname 0))))
  (let* ((out-term (make-instance (if x
				      'terminal-x11:terminal-x11
				      'terminal-ansi:terminal-ansi)
				  :device-name device))
	 (ansi (make-instance 'ansi-terminal:ansi-stream
			      :terminal out-term
			      :pushback-function #'push-it))
	 (*term* (make-term
		  :out-term out-term
		  :ansi ansi
		  :debug-term debug-term))
	 (dlib:*dbug-output* (or debug-term dlib:*dbug-output*))
	 fds master slave tty input buf pid)
    (unwind-protect
      (progn
	(terminal-start out-term)
	(terminal-enable-event out-term '(:resize :mouse-buttons))
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
	(setf fds `((,tty :read #|:write|#)
		    (,master :read))
	       input (cffi:foreign-alloc :unsigned-char :count 100)
	      (term-input-buffer *term*) input
	      buf (cffi:make-shareable-byte-vector *buf-size*))

	(when (typep out-term 'terminal-ansi:terminal-ansi)
	  (nos:set-terminal-mode
	   (terminal-file-descriptor out-term)
	   :raw t))

	;;(break)

	(dbug "before run~%")
	(typecase program
	  (string
	   ;; Break it into words with the shell and run it as an executable.
	   (let ((all-args (lish::shell-words-to-list
			    (lish::shell-expr-words
			     (lish:shell-read program))))
		 (env `(("TERM" ,term-env))))
	     (when (typep (term-out-term *term*) 'terminal-x11:terminal-x11)
	       (push `("WINDOWID"
		       ,(format nil "~d" (xlib:window-id
					  (terminal-x11::window
					   (term-out-term *term*)))))
		     env))
	     (setf pid (run-the-program slave (car all-args) (cdr all-args) env)
		   (term-pid *term*) pid)))
	  (symbol
	   (unless (fboundp program)
	     (error "Program symbol ~s isn't bound to function." program))
	   (call-program slave (uos:ttyname slave) program))
	  (function
	   (call-program slave (uos:ttyname slave) program))
	  (t
	   (error "I don't know how to run a ‘program’ of type ~s."
		  (type-of program))))
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
		 (when (terminal-listen-for out-term 0)
		   (process-keys *term*))
		 (when x
		   (dbug "event-listen -@-@-@-@- ~s~%"
			 (xlib:event-listen (terminal-x11::display out-term) 0)))
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
		 ;; (dbug "before io loop~%")
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
			     ;; (dbug "hanky ~s~%" (type-of buf))
			     (if (term-step-p *term*)
				 (single-step-write (displaced-subseq buf 0 rr))
				 (safer-utf8-write (displaced-subseq buf 0 rr))
				 ;; (finish-output ansi)
				 ;; (dbug "hokay~%")
				 )))))
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
      ;; We shouldn't have to do this, right?
      ;; (when (typep out-term 'terminal-ansi:terminal-ansi)
      ;; 	(nos:set-terminal-mode
      ;; 	 (terminal-file-descriptor out-term)
      ;; 	 :raw nil))
      (terminal-done out-term))))

;; End
