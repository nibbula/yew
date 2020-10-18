;;
;; unix/inspection.lisp - Unix interface to debuging, profiling, and inspection.
;;

(in-package :opsys-unix)

(declaim #.`(optimize ,.(getf opsys-config::*config* :optimization-settings)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BPF

;; @@@ not even close yet

;; Instruction classes
(defun bpf-class (code) (logand code #x07))
(declaim (inline bpf-class))

(defparameter *bpf-classes* nil "BPF instruction classes.")
(define-to-list *bpf-classes*
    #(#(+BPF-LD           #x00 "Load")
      #(+BPF-LDX          #x01 "Load X")
      #(+BPF-ST           #x02 "Store")
      #(+BPF-STX          #x03 "Store X")
      #(+BPF-ALU          #x04 "Arithmetic and logical")
      #(+BPF-JMP          #x05 "Jump")
      #(+BPF-RET          #x06 "Return")
      #(+BPF-MISC         #x07 "Miscellaneous or ALU 64")))

(defun bpf-size (code) (logand code #x18))
(declaim (inline bpf-size))

;; ld/ldx fields
(defparameter *bpf-sizes* nil "BPF data sizes.")
(define-to-list *bpf-sizes*
    #(#(+BPF-W #x00 "Word ?")
      #(+BPF-H #x08 "Half-word ?")
      #(+BPF-B #x10 "Byte ?")
      #(+BPF-DW #x18 "Double word")
      ))

(defun bpf-mode (code) (logand code #xe0))
(declaim (inline bpf-mode))

(defparameter *bpf-modes* nil "BPF memory access modes.")
(define-to-list *bpf-modes*
    #(#(+BPF-IMM+ #x00 "Immediate")
      #(+BPF-ABS+ #x20 "Absolute")
      #(+BPF-IND+ #x40 "Indirect")
      #(+BPF-MEM+ #x60 "Memory?")
      #(+BPF-LEN+ #x80 "Length")
      #(+BPF-MSH+ #xa0 "MSH?")))

;; alu/jmp fields
(defun bpf-op (code) (logand code #xf0))
(declaim (inline bpf-op))

(defparameter *bpf-ops* nil "BPF operations.")
(define-to-list *bpf-ops*
    #(#(+BPF+ADD+   #x00 "Add")
      #(+BPF+SUB+   #x10 "Subtract")
      #(+BPF+MUL+   #x20 "Multiply")
      #(+BPF+DIV+   #x30 "Divide")
      #(+BPF+OR+    #x40 "Or")
      #(+BPF+AND+   #x50 "And")
      #(+BPF+LSH+   #x60 "Left shift")
      #(+BPF+RSH+   #x70 "Right shift")
      #(+BPF+NEG+   #x80 "Negate")
      #(+BPF+MOD+   #x90 "Modulus")
      #(+BPF+XOR+   #xa0 "Exclusive or")
      ;;
      #(+BPF+JA+    #x00 "Jump always?")
      #(+BPF+JEQ+   #x10 "Jump if equal")
      #(+BPF+JGT+   #x20 "Jump if greater than")
      #(+BPF+JGE+   #x30 "Jump if greater than or equal")
      #(+BPF+JSET+  #x40 "")
      ;;
      #(+BPF+JNE+   #x50 "Jump if not equal")
      #(+BPF+JSGT+  #x60 "Jump if signed greater than")
      #(+BPF+JSGE+  #x70 "Jump if signed greater than or equal")
      #(+BPF+CALL+  #x80 "Call a function")
      #(+BPF+EXIT+  #x90 "Function return")
      ))

;; Sources
(defun bpf-src (code) (logand code #x08))
(declaim (inline bpf-src))

(defparameter *bpf-sources* nil "BPF sources.")
(define-to-list *bpf-sources*
    #(#(+BPF-K+ #x00 "")
      #(+BPF-X+ #x08 "")))

(defconstant +BPF-MAXINSNS+ 4096
  "Maximum instructions.")

;; Commands
(defparameter *bpf-commands* "Commands for the bpf system call.")
(define-enum-list *bpf-commands*
    #(#(+BPF-MAP-CREATE+       "Create a map.")
      #(+BPF-MAP-LOOKUP-ELEM+  "Lookup a map element.")
      #(+BPF-MAP-UPDATE-ELEM+  "Update a map element.")
      #(+BPF-MAP-DELETE-ELEM+  "Delete a map element.")
      #(+BPF-MAP-GET-NEXT-KEY+ "Get the next map key.")
      #(+BPF-PROG-LOAD+        "Load a program.")
      #(+BPF-OBJ-PIN+          "Pin an object.")
      #(+BPF-OBJ-GET+          "Get an object.")))

(defparameter *bpf-map-types* "Type of bpf map.")
(define-enum-list *bpf-map-types*
    #(#(+BPF-MAP-TYPE-UNSPEC+           "")
      #(+BPF-MAP-TYPE-HASH+             "")
      #(+BPF-MAP-TYPE-ARRAY+            "")
      #(+BPF-MAP-TYPE-PROG-ARRAY+       "")
      #(+BPF-MAP-TYPE-PERF-EVENT-ARRAY+ "")))

(defparameter *bpf-program-types* "Type of bpf program.")
(define-enum-list *bpf-program-types*
    #(#(+BPF-PROG-TYPE-UNSPEC+          "Unspecified")
      #(+BPF-PROG-TYPE-SOCKET-FILTER+   "Socket filter")
      #(+BPF-PROG-TYPE-KPROBE+          "Kernel probe")
      #(+BPF-PROG-TYPE-SCHED-CLS+       "Scheduling class")
      #(+BPF-PROG-TYPE-SCHED-ACT+       "Scheduling act")))

;; Flags for map-update-element
(defconstant +BPF-ANY+	   0 "Create new element or update existing.")
(defconstant +BPF-NOEXIST+ 1 "Create new element if it didn't exist.")
(defconstant +BPF-EXIST+   2 "Update existing element.")

(defun dest-regs (regs)    (ash (logand regs #b11110000) -4))
(defun source-regs (regs)        (logand regs #b00001111))
(defun regs (source dest)
  (logior (ash (logand dest   #b1111) 4)
	       (logand source #b1111)))

(defcstruct bpf-instruction
  (code       :uint8)			; opcode
  (regs       :uint8)			; source & dest register
  (off        :int16)			; signed offset
  (imm        :int32))			; signed immediate constant

(defparameter *bpf-helpers* nil "Definitions of helper functions.")
(defparameter *bpf-helper-count* 0 "Count of helper functions.")

(defstruct helper
  name
  args
  return-type
  id)

#|

;; The order in which these are defined much match the kernel.
(defmacro defhelper (name (&rest args) return-type &optional docstring)
  (with-unique-names (h)
    (let ((bpf-name (symbolify (s+ "bpf-" name)))
	  (lisp-args (mapcar #'car args))
	  (func-args @@@@@))
      `(let ((,h (make-helper :name ,name
			      :args ,args
			      :return-type return-type
			      :id (incf *bpf-helper-count*))))
	 (push *bpf-helpers* ,helper)
	 (defun ,bpf-name ,lisp-args
	   ,@(when docstring (list docstring))
	   (nconc *bpf-program*
		  `(+BPF-CALL+
		    ,(helper-id ,h)
		    ,,@func-args)))))))

(defhelper map-lookup-element ((map :pointer) (key :poiinter)) :pointer
	   "Loop up an element in a map.")
(defhelper map-update-element ((map :pointer) (key :pointer) (value :pointer)
			    (flags :int)) :int
	   "Update a map element.")
(defhelper map-delete-element ((map :pointer) (key :pointer)) :int "")
(defhelper probe-read ((dst (:pointer :void))
		       (size :int)
		       (src (:pointer :void)) :int "")
(defhelper ktime-get-ns () :u64 "")
(defhelper trace-printk ((fmt :string) (fmt-size :int) &rest args) :int "")
(defhelper get-prandom-u32 () :u32 "")
(defhelper get-smp-processor-id () :u32 "")
(defhelper skb-store-bytes
  ((skb    :pointer)
   (offset :int)
   (from   :pointer)
   (len    :int)
   (flags  :int))
  :int "Store bytes into packet.
   (skb    :pointer) ; pointer to skb
   (offset :int)     ; offset within packet from skb->mac_header
   (from   :pointer) ; pointer where to copy bytes from
   (len    :int)     ; number of bytes to store into packet
   (flags  :int))    ; if bit 0, recompute skb->csum, other bits reserved")

(defhelper l3-csum-replace
    ((skb     :pointer)
     (offset  :int)
     (from    :int)
     (to      :int)
     (flags   :int)) :int
   "recompute IP checksum
(skb     pointer to skb
(offset  offset within packet where IP checksum is located
(from    old value of header field
(to      new value of header field
(flags   bits 0-3 - size of header field
         other bits - reserved
Return 0 on success")

(defhelper l4-csum-replace
    ((skb    :pointer)
     (offset :int)
     (from   :int)
     (to     :int)
     (flags  :int)) :int
    "recompute TCP/UDP checksum
  skb     pointer to skb
  offset  offset within packet where TCP/UDP checksum is located
  from    old value of header field
  to      new value of header field
  flags   bits 0-3 - size of header field
          bit 4 - is pseudo header
  Return 0 on success")

(defhelper tail-call ((ctx :pointer) (prog-array-map :pointer) (index :int))
  :int
  "Jump into another BPF program.
 ctx             Context pointer passed to next program
 prog_array_map  Pointer to map which type is BPF_MAP_TYPE_PROG_ARRAY
 index           Index inside array that selects specific program to run.
Return 0 on success")

(defhelper clone-redirect ((skb :pointer) (ifindex :int) (flags :int)) :int
  "redirect to another netdev
skb       pointer to skb
ifindex   ifindex of the net device
flags     bit 0 - if set, redirect to ingress instead of egress
Return 0 on success.")

(defhelper get-current-pid-tgid () :uint64
  "Return current->tgid << 32 | current->pid")

(defhelper get-current-uid-gid () :uint64
  "Return current_gid << 32 | current_uid")

(defhelper get-current-comm ((buf :string) (size-of-buf :int)) :int
  "Stores current->comm into buf. Return 0 on success.")

(defhelper get-cgroup-classid ((skb :pointer)) :int
  "Retrieve a proc's classid. Return classid if != 0.")

(defhelper skb-vlan-push ((skb :pointer) (vlan-proto :int) (vlan-tci :int)) :int)
(defhelper skb-vlan-pop  ((skb :pointer)) :int)

(defhelper skb-get-tunnel-key
    ((skb :pointer)
     (key (:pointer (:struct bpf-tunnel-key)))
     (size :int)
     (flags :int)) :int
     "Retrieve tunnel metadata.
 skb      pointer to skb
 key      pointer to 'struct bpf_tunnel_key'
 size     size of 'struct bpf_tunnel_key'
 flags    room for future extensions
 Retrun 0 on success.")

(defhelper skb-set-tunnel-key
    ((skb :pointer)
     (key (:pointer (:struct bpf-tunnel-key)))
     (size :int)
     (flags :int)) :int
     "Populate tunnel metadata.
 skb      pointer to skb
 key      pointer to 'struct bpf_tunnel_key'
 size     size of 'struct bpf_tunnel_key'
 flags    room for future extensions
 Retrun 0 on success.")

(defhelper perf-event-read ((map :pointer) (index :int)) :uint64 "")

(defhelper redirect (ifindex flags) :int
	   "redirect to another netdev
 ifindex: ifindex of the net device
 flags:   bit 0 - if set, redirect to ingress instead of egress
 Return: TC_ACT_REDIRECT")

(defhelper get-route-realm ((skb :pointer)) :int
    "retrieve a dst's tclassid")

(defhelper perf-event-output
    ((ctx (:pointer (:struct pt-regs)))
     (map :pointer)
     (index :int)
     (data :int)
     (size :int))
:int
"output perf raw sample
ctx    struct pt_regs*
map    pointer to perf_event_array map
index    index of event in the map
data    data on stack to be output as raw data
size    size of data
return    0 on success")

;;;;;;;;;;;;;;;
|#

(defcstruct bpf-map-spec
  (map-type    :uint32)
  (key-size    :uint32)
  (value-size  :uint32)
  (max-entries :uint32))

(defcstruct bpf-map
  (map-fd            :uint32)
  (key               :uint64) ;; aligned?
  (value-or-next-key :uint64) ;; aligned?
  (flags             :uint64))

(defcstruct bpf-program
  (program-type      :uint32)
  (instruction-count :uint32)
  (instructions      :uint64) ;; aligned (struct bpf_insn *)
  (license           :uint64) ;; aligned (char *)
  (log-level	     :uint32)
  (log-size	     :uint32)
  (log-buf	     :uint64) ;; aligned (char *)
  (kernel-version    :uint32))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defcunion bpf-attr ;; aligned(8)
    (map-spec (:struct bpf-map-spec))
    (map      (:struct bpf-map))
    (program  (:struct bpf-program))))

#|

(defmacro defbpf (name (&rest args) &body body)
  "Define a BPF function using a Lisp subset."
  )

(defun compile-bpf (bpf-function)
  "Compile a BFP function. Turn a function defined with defbpf into BFP
code instructions."
  )

(defmacro with-bpf ((fd function) &body body)
  "Evaluate the body with the BPF function active and bound to fd."
  )

(defmacro with-bpf-map ((stream) &body body)
  "Evaluate the body with the BPF map bound to fd."
  )

(defmacro with-socket-bpf ((socket function) &body body)
  "Evaluate the body with filter attached to the socket."
  )

|#

;; @@@@
#+(and linux 64-bit-target) (defparameter +SYS-BPF+ 357)

;; It's not actually in libc.
;;
;; (defcfun ("bpf" real-bpf) :int
;;   "Interface to the Berkely Packet Filter."
;;   (cmd :int)
;;   (attr (:pointer (:union bpf-attr)))
;;   (size :unsigned-int))

#+linux
(defun real-bpf (cmd attr size)
  (declare (ignorable cmd attr size)) ;; @@@ Why?
  (real-syscall +SYS-BPF+
		:int cmd
		(:pointer (:union bpf-attr)) attr
		:unsigned-int size))

(defun bpf (cmd &rest args)
  (case cmd
    (:create
     (with-foreign-object (attr '(:struct bpf-map-spec))
       (with-foreign-slots ((map-type key-size value-size max-entries) attr
			    (:struct bpf-map-spec))
	 (setf map-type    (or (getf args :type) +BPF-MAP-TYPE-ARRAY+)
	       key-size    (or (getf args :key-size)   0 #| :int @@@ wrong |#)
	       value-size  (or (getf args :value-size) 0 #| :int @@@ wrong |#)
	       max-entries (or (getf args :value-size) 10))
	 (syscall (real-bpf +BPF-MAP-CREATE+ attr
			    (foreign-type-size '(:struct bpf-map-spec)))))))
    (:lookup)
    (:update)
    (:delete)
    (:next-key)
    (:load)))

;; End
