;;
;; unix/memory.lisp - Unix interface to memory
;;

(in-package :opsys-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Memory mapping

#+linux (progn  ;; @@@ only on Linux so far
(defparameter *memory-permissions* nil "Permission bits for mmap and mprotect.")
(define-to-list *memory-permissions*
  #(
    #(+PROT_NONE+             #x0 "Pages may not be accessed.")
    #(+PROT_READ+             #x1 "Pages may be read.")
    #(+PROT_WRITE+            #x2 "Pages may be written.")
    #(+PROT_EXEC+             #x4 "Pages may be executed.")
    #(+PROT_GROWSDOWN+ #x01000000 "Extend change to start of growsdown vma")
    #(+PROT_GROWSUP+   #x02000000 "Extend change to end of growsup vma")
  ))

(defparameter *memory-flags* nil "Flag bits for mmap and mprotect.")
(define-to-list *memory-flags*
  #(
    #(+MAP_SHARED+      #x00001 "Share this mapping.")
    #(+MAP_PRIVATE+     #x00002 "Create a private copy-on-write mapping.")
    #(+MAP_TYPE+        #x0000f "Mask for type of mapping")
    #(+MAP_FIXED+       #x00010 "Interpret addr exactly")
    #(+MAP_ANONYMOUS+   #x00020 "don't use a file")
    #(+MAP_32BIT+       #x00040 "Only give out 32bit addresses")
    #(+MAP_GROWSDOWN+	#x00100 "stack-like segment")
    #(+MAP_DENYWRITE+	#x00800 "Ignored. Fail with ETXTBSY.")
    #(+MAP_EXECUTABLE+	#x01000 "Ignored. Mark it as an executable")
    #(+MAP_LOCKED+	#x02000 "Pages are locked")
    #(+MAP_NORESERVE+	#x04000 "Don't check for reservations")
    #(+MAP_POPULATE+	#x08000 "Populate (prefault) pagetables")
    #(+MAP_NONBLOCK+	#x10000 "Do not block on IO")
    #(+MAP_STACK+	#x20000
      "Give out an address that is best suited for process/thread stacks")
    #(+MAP_HUGETLB+	#x40000 "Create a huge page mapping")
    #(+MAP_UNINITIALIZED+
      ;; aka CONFIG_MMAP_ALLOW_UNINITIALIZED
      #+mmap-allows-uninitialized #x4000000
      #-mmap-allows-uninitialized #x0
      "For anonymous mmap, memory could be uninitialized")))

(defconstant +MREMAP_MAYMOVE+	1)
(defconstant +MREMAP_FIXED+	2)

(defconstant +OVERCOMMIT_GUESS+	 0)
(defconstant +OVERCOMMIT_ALWAYS+ 1)
(defconstant +OVERCOMMIT_NEVER+	 2)

(defconstant +MAP_HUGE_SHIFT+  26)
(defconstant +MAP_HUGE_MASK+   #x3f)
(defconstant +MAP_HUGE_2MB+    (ash 21 +MAP_HUGE_SHIFT+))
(defconstant +MAP_HUGE_1GB+    (ash 30 +MAP_HUGE_SHIFT+))

(defconstant +MLOCK_ONFAULT+   #x01
  "Lock pages in range after they are faulted in, do not prefault")

;; @@@ These will likely have to be changed for 32 vs 64 bits.

(defcfun mmap (:pointer :void)
  (addr (:pointer :void)) (length size-t) (prot :int) (flags :int) (fd :int)
  (offset off-t))

(defcfun munmap :int
  (addr (:pointer :void)) (length size-t))

(defconstant +MS_ASYNC+		1 "Sync memory asynchronously.")
(defconstant +MS_SYNC+		4 "Synchronous memory sync.")
(defconstant +MS_INVALIDATE+	2 "Invalidate the caches.")

(defcfun msync :int
  "Synchronize the region starting at ADDR and extending LENGTH bytes with the
file it maps. Filesystem operations on a file being mapped are unpredictable
before this is done."
  (addr (:pointer :void)) (length size-t) (flags :int))

(defcfun mprotect :int
  "Change the memory protection of the region starting at ADDR and extending
LENGTH bytes to PROT. Returns 0 if successful, -1 for errors (and sets errno)."
  (addr (:pointer :void)) (length size-t) (prot :int))

(defparameter *madvise-flags* nil "Flags for madvise.")
(define-to-list *madvise-flags*
  #(#(+MADV_NORMAL+	  0	"No further special treatment")
    #(+MADV_RANDOM+	  1	"Expect random page references")
    #(+MADV_SEQUENTIAL+	  2	"Expect sequential page references")
    #(+MADV_WILLNEED+	  3	"Will need these pages")
    #(+MADV_DONTNEED+	  4	"Don't need these pages")
    #(+MADV_REMOVE+	  9     "Remove these pages & resources")
    #(+MADV_DONTFORK+	  10    "Don't inherit across fork")
    #(+MADV_DOFORK+	  11    "Do inherit across fork")
    #(+MADV_MERGEABLE+    12    "KSM may merge identical pages")
    #(+MADV_UNMERGEABLE+  13    "KSM may not merge identical pages")
    #(+MADV_HUGEPAGE+	  14    "Worth backing with hugepages")
    #(+MADV_NOHUGEPAGE+	  15    "Not worth backing with hugepages")
    #(+MADV_DONTDUMP+     16
      "Explicity exclude from the core dump, overrides the coredump filter bits")
    #(+MADV_DODUMP+	  17    "Clear the MADV_DONTDUMP flag")
    #(+MADV_HWPOISON+	  100   "Poison a page for testing")
    #(+MADV_SOFT_OFFLINE+ 101   "Soft offline page for testing")
    ))

(defcfun madvise :int
  "Advise the system about particular usage patterns the program follows
   for the region starting at ADDR and extending LENGTH bytes."
  (addr (:pointer :void)) (length size-t) (advice :int))

(defcfun mlock :int
  "Try to guarantee all whole pages mapped by the range [ADDR, ADDR + LENGTH) to
   be memory resident."
  (addr (:pointer :void)) (length size-t))

(defcfun munlock :int
  "Unlock whole pages previously mapped by the range [ADDR, ADDR + LENGTH)."
  (addr (:pointer :void)) (length size-t))

(defcfun mlockall :int
  "Cause all currently mapped pages of the process to be memory resident
   until unlocked by a call to the `munlockall', until the process exits,
   or until the process calls `execve'."
  (flags :int))

(defcfun munlockall :void
  "All currently mapped pages of the process' address space become
   magically unlocked.")

(defcfun mincore :int
  "mincore returns the memory residency status of the pages in the
   current process's address space specified by [start, start + length).
   The status is returned in a vector of bytes.  The least significant
   bit of each byte is 1 if the referenced page is in memory, otherwise
   it is zero."
  (start (:pointer :void)) (length size-t) (vec (:pointer :unsigned-char)))

(defcfun mremap (:pointer :void)
  "Remap pages mapped by the range [ADDR,ADDR+OLD_LEN) to new length
   NEW_LEN. If MREMAP_MAYMOVE is set in FLAGS the returned address
   may differ from ADDR. If MREMAP_FIXED is set in FLAGS the function
   takes another parameter which is a fixed address at which the block
   resides after a successful call."
  (addr (:pointer :void)) (old-len size-t) (new-len size-t) (flags :int)
  &rest)

(defcfun remap-file-pages :int
  "Remap arbitrary pages of a shared backing store within an existing VMA."
  (start (:pointer :void)) (size size-t) (prot :int) (pgoff size-t) (flags :int))

;; @@@ Consider interfaces from trivial-mmap
;; @@@ Except what I would really like is a mmapped file as a gray-stream,
;; and an mmaped region as a displaced array, so it's probably better to
;; only provide access to the unix level stuff for now, and add those after
;; doing a portability study, especially examining the facilities on Windows.

#|
(defun map-file-to-memory (filename
			   &key length (mapping-type :map-private) (offset 0))
  "Maps a FILENAME into memory."
  (when (not (zerop (mod offset (memory-page-size))))
    (error 'posix-error
	   :format-control
	   "Offset must be a multiple of the memory page size ~d."
	   :format-args (memory-page-size)))

  (let* ((mapping-type-flag (ecase mapping-type
                              (:map-private +MAP_PRIVATE+)
                              (:map-shared +MAP_SHARED+)
			      ((+MAP_PRIVATE+ +MAP_SHARED+)
			       mapping-type)))
         fd file-length)
    (unwind-protect
	 (progn
	   (setf fd (posix-open filename +O_RDONLY+)
		 file-length (or length (file-status-size (fstat fd)))
		 pointer (mmap (cffi:null-pointer)
			       file-length
			       +PROT_READ+
			       mapping-type-flag
			       fd
			       offset))
	   (when (pointer-eq pointer (make-pointer +MAP_FAILED+))
	     (error 'posix-error :error-code *errno*
		    :format-control "mmap:"))
           (values pointer file-length))
      (posix-close fd))))
|#
)

;; swapon ??

;; End
