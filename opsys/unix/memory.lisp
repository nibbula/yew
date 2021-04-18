;;;
;;; unix/memory.lisp - Unix interface to memory
;;;

(in-package :opsys-unix)

(declaim #.`(optimize ,.(getf opsys-config::*config* :optimization-settings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Memory mapping

#+linux (progn  ;; @@@ only on Linux so far
(defparameter *memory-permissions* nil "Permission bits for mmap and mprotect.")
(define-to-list *memory-permissions*
  #(
    #(+PROT-NONE+             #x0 "Pages may not be accessed.")
    #(+PROT-READ+             #x1 "Pages may be read.")
    #(+PROT-WRITE+            #x2 "Pages may be written.")
    #(+PROT-EXEC+             #x4 "Pages may be executed.")
    #(+PROT-GROWSDOWN+ #x01000000 "Extend change to start of growsdown vma")
    #(+PROT-GROWSUP+   #x02000000 "Extend change to end of growsup vma")
  ))

(defparameter *memory-flags* nil "Flag bits for mmap and mprotect.")
(define-to-list *memory-flags*
  #(
    #(+MAP-SHARED+      #x00001 "Share this mapping.")
    #(+MAP-PRIVATE+     #x00002 "Create a private copy-on-write mapping.")
    #(+MAP-TYPE+        #x0000f "Mask for type of mapping")
    #(+MAP-FIXED+       #x00010 "Interpret addr exactly")
    #(+MAP-ANONYMOUS+   #x00020 "don't use a file")
    #(+MAP-32BIT+       #x00040 "Only give out 32bit addresses")
    #(+MAP-GROWSDOWN+	#x00100 "stack-like segment")
    #(+MAP-DENYWRITE+	#x00800 "Ignored. Fail with ETXTBSY.")
    #(+MAP-EXECUTABLE+	#x01000 "Ignored. Mark it as an executable")
    #(+MAP-LOCKED+	#x02000 "Pages are locked")
    #(+MAP-NORESERVE+	#x04000 "Don't check for reservations")
    #(+MAP-POPULATE+	#x08000 "Populate (prefault) pagetables")
    #(+MAP-NONBLOCK+	#x10000 "Do not block on IO")
    #(+MAP-STACK+	#x20000
      "Give out an address that is best suited for process/thread stacks")
    #(+MAP-HUGETLB+	#x40000 "Create a huge page mapping")
    #(+MAP-UNINITIALIZED+
      ;; aka CONFIG_MMAP_ALLOW_UNINITIALIZED
      #+mmap-allows-uninitialized #x4000000
      #-mmap-allows-uninitialized #x0
      "For anonymous mmap, memory could be uninitialized")))

(defconstant +MREMAP-MAYMOVE+	1)
(defconstant +MREMAP-FIXED+	2)

(defconstant +OVERCOMMIT-GUESS+	 0)
(defconstant +OVERCOMMIT-ALWAYS+ 1)
(defconstant +OVERCOMMIT-NEVER+	 2)

(defconstant +MAP-HUGE-SHIFT+  26)
(defconstant +MAP-HUGE-MASK+   #x3f)
(defconstant +MAP-HUGE-2MB+    (ash 21 +MAP-HUGE-SHIFT+))
(defconstant +MAP-HUGE-1GB+    (ash 30 +MAP-HUGE-SHIFT+))

(defconstant +MAP-FAILED+		;  ((void *) -1)
  #|
  #+32-bit-target #xffffffff
  #+64-bit-target #xffffffffffffffff)
  |#
  ;; Ever so boldly assuming two's complement.
  #.(loop :with r = 0
       :for i :from 0 :below (cffi:foreign-type-size :pointer)
       :do (setf r (logior (ash r 8) #xff))
       :finally (return r)))

(defconstant +MLOCK-ONFAULT+   #x01
  "Lock pages in range after they are faulted in, do not prefault")

;; @@@ These will likely have to be changed for 32 vs 64 bits.

(defcfun mmap (:pointer :void)
  (addr (:pointer :void)) (length size-t) (prot :int) (flags :int) (fd :int)
  (offset off-t))

(defcfun munmap :int
  (addr (:pointer :void)) (length size-t))

(defconstant +MS-ASYNC+		1 "Sync memory asynchronously.")
(defconstant +MS-SYNC+		4 "Synchronous memory sync.")
(defconstant +MS-INVALIDATE+	2 "Invalidate the caches.")

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
  #(#(+MADV-NORMAL+	  0	"No further special treatment")
    #(+MADV-RANDOM+	  1	"Expect random page references")
    #(+MADV-SEQUENTIAL+	  2	"Expect sequential page references")
    #(+MADV-WILLNEED+	  3	"Will need these pages")
    #(+MADV-DONTNEED+	  4	"Don't need these pages")
    #(+MADV-REMOVE+	  9     "Remove these pages & resources")
    #(+MADV-DONTFORK+	  10    "Don't inherit across fork")
    #(+MADV-DOFORK+	  11    "Do inherit across fork")
    #(+MADV-MERGEABLE+    12    "KSM may merge identical pages")
    #(+MADV-UNMERGEABLE+  13    "KSM may not merge identical pages")
    #(+MADV-HUGEPAGE+	  14    "Worth backing with hugepages")
    #(+MADV-NOHUGEPAGE+	  15    "Not worth backing with hugepages")
    #(+MADV-DONTDUMP+     16
      "Explicity exclude from the core dump, overrides the coredump filter bits")
    #(+MADV-DODUMP+	  17    "Clear the MADV-DONTDUMP flag")
    #(+MADV-HWPOISON+	  100   "Poison a page for testing")
    #(+MADV-SOFT-OFFLINE+ 101   "Soft offline page for testing")
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
;; @@@ But I need this now, so as usual, we'll have we'll have to figure out
;; a portable interface later.

(defun map-file (file &key length (offset 0) (mapping-type :private)
			(access '(:read)) address)
  "Maps FILE into memory. Return a foreign pointer and a length. FILE can be a
string, in which case it's taken as a file name, or an integer, in which case
it's taken as a file descriptor.
  LENGTH       - the length in octets to map, which probably shouldn't be longer
                 than the length of the file.
  OFFSET       - The offset in bytes from the beginning of the file to start
                 mapping from. Defaults to 0.
  MAPPING-TYPE - Either :map-private or :shared. Defaults to :private.
  ACCESS       - A list of cosisting of :read :write :exec. NIL for no access.
                 Defaults to :read.
  ADDRESS      - An address where you might like it to be mapped. It will
                 probably be adjusted to a page boundry, if it's even okay."
  (when (not (zerop (mod offset (memory-page-size))))
    (error 'posix-error
	   :format-control
	   "Offset must be a multiple of the memory page size ~d."
	   :format-args (memory-page-size)))

  (let* ((mapping-type-flag (ecase mapping-type
                              (:private +MAP-PRIVATE+)
                              (:shared +MAP-SHARED+)
			      ((+MAP-PRIVATE+ +MAP-SHARED+)
			       mapping-type)))
	 (prot (if (not access) +PROT-NONE+
		   (loop :with p = 0
		      :for a :in access :do
			(ecase a
			  (:read  (setf p (logior p +PROT-READ+)))
			  (:write (setf p (logior p +PROT-WRITE+)))
			  (:exec  (setf p (logior p +PROT-EXEC+))))
			:finally (return p))))
	 (file-access (if (not access) +O_RDONLY+
			  (if (find :write access)
			      +O_RDWR+
			      +O_RDONLY+)))
         fd file-length pointer we-opened)
    (unwind-protect
	 (progn
	   (setf fd (etypecase file
		      (string
		       (prog1
			   (error-check (posix-open file file-access 0)
					"Can't open file for mapping ~s" file)
			 (setf we-opened t)))
		      (integer fd))
		 file-length (or length (file-status-size (fstat fd))))
	   ;; (dbugf :mmap "mmap address ~s file-length ~s prot ~x ~
           ;;               mapping-type-flag ~s fd ~s offset ~s~%"
	   ;; 	  address file-length prot mapping-type-flag fd offset)
	   (setf pointer (mmap (or address (cffi:null-pointer))
			       file-length
			       prot
			       mapping-type-flag
			       fd
			       offset))
	   ;; (dbugf :mmap "mmap ZZ address ~s file-length ~s prot ~x ~
           ;;               mapping-type-flag ~s fd ~s offset ~s~%" address
           ;;               file-length prot mapping-type-flag fd offset)
	   (finish-output)
	   (when (pointer-eq pointer (make-pointer +MAP-FAILED+))
	     (error 'posix-error :error-code *errno*
		    :format-control "Failed to map file ~s"
		    :format-arguments `(,file)))
           (values pointer file-length))
      (when (and we-opened fd)
	(posix-close fd)))))
)

;; swapon ??

;; End
