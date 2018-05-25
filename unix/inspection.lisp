;;
;; unix/inspection.lisp - Unix interface to debuging, profiling, and inspection.
;;

(in-package :opsys-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profiling and debugging?
;; Should use other lispy tools?
;; For profiling you probably need to use tools specific to the implementation.

;; profil
;; ptrace

(defparameter *ptrace-requests* nil "Requests type for ptrace.")
(define-to-list *ptrace-requests*
#(
#(+PTRACE-TRACEME+      0 "Yo, yo, yo! Trace me bro.")
#(+PTRACE-PEEKTEXT+     1 "Return the word in the process's text space at address ADDR.")
#(+PTRACE-PEEKDATA+     2 "Return the word in the process's data space at address ADDR.")
#(+PTRACE-PEEKUSER+     3 "Return the word in the process's user area at offset ADDR.")
#(+PTRACE-POKETEXT+     4 "Write the word DATA into the process's text space at address ADDR.")
#(+PTRACE-POKEDATA+     5 "Write the word DATA into the process's data space at address ADDR.")
#(+PTRACE-POKEUSER+     6 "Write the word DATA into the process's user area at offset ADDR.")
#(+PTRACE-CONT+	        7 "Continue the process.")
#(+PTRACE-KILL+	        8 "Kill the process.")
#(+PTRACE-SINGLESTEP+   9 "Single step the process. This is not supported on all machines.")
#(+PTRACE-GETREGS+     12 "Get all general purpose registers used by a processes.")
#(+PTRACE-SETREGS+     13 "Set all general purpose registers used by a processes.")
#(+PTRACE-GETFPREGS+   14 "Get all floating point registers used by a processes.")
#(+PTRACE-SETFPREGS+   15 "Set all floating point registers used by a processes.")
#(+PTRACE-ATTACH+      16 "Attach to a process that is already running.")
#(+PTRACE-DETACH+      17 "Detach from a process attached to with PTRACE_ATTACH.")
#(+PTRACE-GETFPXREGS+  18 "Get all extended floating point registers used by a processes.")
#(+PTRACE-SETFPXREGS+  19 "Set all extended floating point registers used by a processes.")
#(+PTRACE-SYSCALL+     24 "Continue and stop at the next (return from) syscall.")
#(+PTRACE-SETOPTIONS+  #x4200 "Set ptrace filter options.")
#(+PTRACE-GETEVENTMSG+ #x4201 "Get last ptrace message.")
#(+PTRACE-GETSIGINFO+  #x4202 "Get siginfo for process.")
#(+PTRACE-SETSIGINFO+  #x4203 "Set new siginfo for process.")
#(+PTRACE-GETREGSET+   #x4204 "Get register content.")
#(+PTRACE-SETREGSET+   #x4205 "Set register content.")
#(+PTRACE-SEIZE+       #x4206 "Like PTRACE_ATTACH, but do not force tracee to trap and do not affect signal or group stop state.")
#(+PTRACE-INTERRUPT+   #x4207 "Trap seized tracee.")
#(+PTRACE-LISTEN+      #x4208 "Wait for next group event.")
#(+PTRACE-PEEKSIGINFO+ #x4209 "")
#(+PTRACE-GETSIGMASK+  #x420a "")
#(+PTRACE-SETSIGMASK+  #x420b "")
#(+PTRACE-SECCOMP-GET-FILTER+ #x420c "")))

#| alternate names
(defconstant +PT_TRACE_ME+    +PTRACE_TRACEME+)
(defconstant +PT_READ_I+      +PTRACE_PEEKTEXT+)
(defconstant +PT_READ_D+      +PTRACE_PEEKDATA+)
(defconstant +PT_READ_U+      +PTRACE_PEEKUSER+)
(defconstant +PT_WRITE_I+     +PTRACE_POKETEXT+)
(defconstant +PT_WRITE_D+     +PTRACE_POKEDATA+)
(defconstant +PT_WRITE_U+     +PTRACE_POKEUSER+)
(defconstant +PT_CONTINUE+    +PTRACE_CONT+)
(defconstant +PT_KILL+        +PTRACE_KILL+)
(defconstant +PT_STEP+        +PTRACE_SINGLESTEP+)
(defconstant +PT_GETREGS+     +PTRACE_GETREGS+)
(defconstant +PT_SETREGS+     +PTRACE_SETREGS+)
(defconstant +PT_GETFPREGS+   +PTRACE_GETFPREGS+)
(defconstant +PT_SETFPREGS+   +PTRACE_SETFPREGS+)
(defconstant +PT_ATTACH+      +PTRACE_ATTACH+)
(defconstant +PT_DETACH+      +PTRACE_DETACH+)
(defconstant +PT_GETFPXREGS+  +PTRACE_GETFPXREGS+)
(defconstant +PT_SETFPXREGS+  +PTRACE_SETFPXREGS+)
(defconstant +PT_SYSCALL+     +PTRACE_SYSCALL+)
(defconstant +PT_SETOPTIONS+  +PTRACE_SETOPTIONS+)
(defconstant +PT_GETEVENTMSG+ +PTRACE_GETEVENTMSG+)
(defconstant +PT_GETSIGINFO+  +PTRACE_GETSIGINFO+)
(defconstant +PT_SETSIGINFO+  +PTRACE_SETSIGINFO+)
|#

(defconstant +PTRACE-SEIZE-DEVEL+ #x80000000 "Flag for PTRACE_LISTEN.")

(defparameter *ptrace-setoptions* nil "Options set using PTRACE_SETOPTIONS.")
(define-to-list *ptrace-setoptions*
#(
#(+PTRACE-O-TRACESYSGOOD+    #x00000001 "")
#(+PTRACE-O-TRACEFORK+	     #x00000002 "")
#(+PTRACE-O-TRACEVFORK+	     #x00000004 "")
#(+PTRACE-O-TRACECLONE+	     #x00000008 "")
#(+PTRACE-O-TRACEEXEC+	     #x00000010 "")
#(+PTRACE-O-TRACEVFORKDONE+  #x00000020 "")
#(+PTRACE-O-TRACEEXIT+	     #x00000040 "")
#(+PTRACE-O-TRACESECCOMP+    #x00000080 "")
#(+PTRACE-O-EXITKILL+	     #x00100000 "")
#(+PTRACE-O-SUSPEND-SECCOMP+ #x00200000 "")
#(+PTRACE-O-MASK+	     #x003000ff "")
))

(defparameter *ptrace-eventcodes* nil "Wait extended result codes for the above trace options.")
(define-to-list *ptrace-eventcodes*
#(
#(+PTRACE-EVENT-FORK+	      1 "")
#(+PTRACE-EVENT-VFORK+	      2 "")
#(+PTRACE-EVENT-CLONE+	      3 "")
#(+PTRACE-EVENT-EXEC+	      4 "")
#(+PTRACE-EVENT-VFORK-DONE+   5 "")
#(+PTRACE-EVENT-EXIT+	      6 "")
#(+PTRACE-EVENT-SECCOMP+      7 "")
))

(defcfun ptrace :long
  (request :unsigned-int) (pid pid-t) (addr :pointer) (data :pointer))

(defun process-attach (pid)
  (syscall (ptrace +PTRACE-ATTACH+ pid (null-pointer) (null-pointer))))

(defun process-detach (pid)
  (syscall (ptrace +PTRACE-DETACH+ pid (null-pointer) (null-pointer))))

;; End
