;;
;; ms/processes.lisp - Windos interface to processes
;;

(in-package :opsys-ms)

(defclass ms-process-handle (process-handle)
  ()
  (:documentation "A Windows process handle."))

(defun suspend-process (&optional id)
  "Suspend the process with the given ID. If ID is NIL or not given, suspend
the current process."
  (declare (ignore id))
  ;;(DebugActiveProcess id)
  nil)

(defun resume-process (id)
  "Resume the suspended process with the given ID."
  (declare (ignore id))
  ;;(DebugActiveProcessStop id)
  nil)

(defun terminate-process (id)
  "Terminate the process with the given ID."
  (declare (ignore id))
  nil)

;; To get times for children, we have to put them in a job.
;; If we create a job for the shell with CreateJobObject and add any
;; children to it with AssignProcessToJobObject, then maybe we can get
;; cumulative execution times QueryInformationJobObject ??

#|
BOOL WINAPI QueryInformationJobObject(
  _In_opt_  HANDLE             hJob,
  _In_      JOBOBJECTINFOCLASS JobObjectInfoClass,
  _Out_     LPVOID             lpJobObjectInfo,
  _In_      DWORD              cbJobObjectInfoLength,
  _Out_opt_ LPDWORD            lpReturnLength
);

set JobObjectInfoClass to JobObjectBasicAccountingInformation (== 1)

typedef struct _JOBOBJECT_BASIC_ACCOUNTING_INFORMATION {
  LARGE_INTEGER TotalUserTime;
  LARGE_INTEGER TotalKernelTime;
  LARGE_INTEGER ThisPeriodTotalUserTime;
  LARGE_INTEGER ThisPeriodTotalKernelTime;
  DWORD         TotalPageFaultCount;
  DWORD         TotalProcesses;
  DWORD         ActiveProcesses;
  DWORD         TotalTerminatedProcesses;
} JOBOBJECT_BASIC_ACCOUNTING_INFORMATION,
*PJOBOBJECT_BASIC_ACCOUNTING_INFORMATION;

|#

(defcfun ("GetProcessTimes" %get-process-times)
    BOOL
  (process       HANDLE)		; in
  (creation-time LPFILETIME)		; out
  (exit-time     LPFILETIME)		; out
  (kernel-time   LPFILETIME)		; out
  (user-time     LPFILETIME))		; out

(defun process-times (handle)
  (with-foreign-objects ((creation-time '(:struct FILETIME))
			 (exit-time '(:struct FILETIME))
			 (kernel-time '(:struct FILETIME))
			 (user-time '(:struct FILETIME)))
    (syscall (%get-process-times
	      handle
	      creation-time exit-time kernel-time user-time))
    (values-list
     ;;(mapcar
      ;;#'filetime-to-os-time
      ;; (list (mem-ref creation-time '(:struct FILETIME))
      ;; 	    (mem-ref exit-time '(:struct FILETIME))
      ;; 	    (mem-ref kernel-time '(:struct FILETIME))
      ;; 	    (mem-ref user-time '(:struct FILETIME)))))))
      (list (mem-ref creation-time :uint64)
	    (mem-ref exit-time     :uint64)
	    (mem-ref kernel-time   :uint64)
	    (mem-ref user-time     :uint64)))))

(defcfun ("GetCommandLineW" %get-command-line) LPTSTR)

(defun get-command-line ()
  (wide-string-to-lisp (%get-command-line)))

(defconstant +GR-GDIOBJECTS+       0 "Count of GDI objects.")
(defconstant +GR-USEROBJECTS+      1 "Count of USER objects.")
(defconstant +GR-GDIOBJECTS-PEAK+  2 "Peak count of GDI objects.")
(defconstant +GR-USEROBJECTS-PEAK+ 4 "Peak count of USER objects.")

(defcfun ("GetGuiResources" %get-gui-resources)
    DWORD
  (process HANDLE)
  (flags DWORD))

(defconstant +NORMAL-PRIORITY-CLASS+       #x00000020)
(defconstant +IDLE-PRIORITY-CLASS+         #x00000040)
(defconstant +HIGH-PRIORITY-CLASS+         #x00000080)
(defconstant +REALTIME-PRIORITY-CLASS+     #x00000100)
(defconstant +BELOW-NORMAL-PRIORITY-CLASS+ #x00004000)
(defconstant +ABOVE-NORMAL-PRIORITY-CLASS+ #x00008000)

(defcfun ("GetPriorityClass" %get-priority-class)
    DWORD
  (process HANDLE))

(defcfun ("GetProcessHandleCount" %get-process-handle-count)
    BOOL
  (process HANDLE)			; in
  (handle-count PDWORD))		; in out

(defcfun ("GetProcessWorkingSetSize" %get-process-working-set-size)
    BOOL
  (process HANDLE)			; in
  (minimum-working-set-size PSIZE_T)	; out
  (maximum-working-set-size PSIZE_T))	; out

;; These might be in PSAPI.DLL or nowhere. So don't use them?
#|
(defcfun ("GetProcessImageFileNameW" %get-process-image-file-name)
    DWORD
 (process HANDLE)			; in
 (image-file-name LPTSTR)		; out
 (size DWORD))				; in

(defcfun ("EnumProcesses" %enum-processes)
    BOOL
  (process-ids (:pointer DWORD))	; out
  (cb DWORD)				; in
  (bytes-returned (:pointer DWORD)))	; out
|#

(defconstant +TH32CS-SNAPHEAPLIST+ #x00000001)
(defconstant +TH32CS-SNAPPROCESS+  #x00000002)
(defconstant +TH32CS-SNAPTHREAD+   #x00000004)
(defconstant +TH32CS-SNAPMODULE+   #x00000008)
(defconstant +TH32CS-SNAPMODULE32+ #x00000010)
(defconstant +TH32CS-INHERIT+      #x80000000)
(defconstant +TH32CS-SNAPALL+ 	   (logior +TH32CS-SNAPHEAPLIST+
					   +TH32CS-SNAPMODULE+
					   +TH32CS-SNAPPROCESS+
					   +TH32CS-SNAPTHREAD+))

(defcfun ("CreateToolhelp32Snapshot" %create-toolhelp32-snapshot)
    HANDLE
  (flags DWORD)				; in
  (process-id DWORD))			; in

(defcstruct PROCESSENTRY32
  (size		     DWORD)
  (usage	     DWORD)
  (process-id	     DWORD)
  (default-heap-id   ULONG_PTR)
  (module-id	     DWORD)
  (threads	     DWORD)
  (parent-process-id DWORD)
  (pri-class-base    LONG)
  (flags	     DWORD)
  (exe-file	     TCHAR :count #.+MAX-PATH+))

(defcfun ("Process32FirstW" %process32-first)
    BOOL
  (snapshot HANDLE)			      ; in
  (lppe (:pointer (:struct PROCESSENTRY32)))) ; in out

(defcfun ("Process32NextW" %process32-next)
    BOOL
  (snapshot HANDLE)				; in
  (lppe (:pointer (:struct PROCESSENTRY32))))	; in out

(defcfun ("QueryFullProcessImageNameW" %query-full-process-image-name)
    BOOL
 (process HANDLE)			; in
 (flags DWORD)				; in
 (exe-name LPTSTR)			; out
 (size PDWORD))				; in out

(defcfun ("QueryProcessCycleTime" %query-process-cycle-time)
    BOOL
  (process-handle HANDLE)		; in
  (cycle-time PULONG64))		; out

(defcfun ("QueryIdleProcessorCycleTime" %query-idle-processor-cycle-time)
    BOOL
  (buffer-length PULONG)		; in out
  (processor-idle-cycle-time PULONG64))	; out

;; Process access rights
(defconstant +PROCESS-TERMINATE+                 #x0001)
(defconstant +PROCESS-VM-READ+                   #x0010)
(defconstant +PROCESS-VM-WRITE+                  #x0020)
(defconstant +PROCESS-QUERY-INFORMATION+         #x0400)
(defconstant +PROCESS-SUSPEND-RESUME+            #x0800)
(defconstant +PROCESS-QUERY-LIMITED-INFORMATION+ #x1000)

(defcfun ("OpenProcess" %open-process)
    HANDLE
  (desired-access DWORD)		; in
  (inherit-handle BOOL)			; in
  (process-id DWORD))			; in

(defstruct ms-process
  pid
  parent-pid
  thread-count
  priority-class
  creation-time
  exit-time
  kernel-time
  user-time
  gui-resources
  handle-count
  max-working-set
  min-working-set
  name
  command
  )

(defun system-process-type ()
  'ms-process)

;; GlobalMemoryStatusEx
;; GetSystemTimes

(defun get-process-info (pid proc)
  (let (handle creation-time exit-time kernel-time user-time)
    (unwind-protect
	 (progn
	   (with-foreign-objects ((handle-count 'DWORD)
				  (min-ws 'SIZE_T)
				  (max-ws 'SIZE_T)
				  (path-size 'DWORD)
				  (path 'WCHAR +MAX-PATH+))
	     (setf handle (%open-process
			   +PROCESS-QUERY-INFORMATION+ +FALSE+ pid))
	     (when (null-pointer-p handle)
	       (setf handle
		     (%open-process
		      +PROCESS-QUERY-LIMITED-INFORMATION+ +FALSE+ pid))
	       (when (null-pointer-p handle)
		 ;; don't error
		 (return-from get-process-info proc)))
	     (dbugf :ms-proc "Yo~%")
	     (multiple-value-setq
		 (creation-time exit-time kernel-time user-time)
	       (process-times handle))
	     (%get-process-handle-count handle handle-count)
	     (%get-process-working-set-size handle min-ws max-ws)
	     (setf (mem-ref path-size 'DWORD) +MAX-PATH+)
	     (%query-full-process-image-name handle 0 path path-size)
	     (setf (ms-process-creation-time proc) creation-time
		   (ms-process-exit-time proc) exit-time
		   (ms-process-kernel-time proc) kernel-time
		   (ms-process-user-time proc) user-time
		   (ms-process-gui-resources proc)
		   (%get-gui-resources handle +GR-GDIOBJECTS+)
		   ;; (ms-process-priority-class proc)
		   ;; (%get-priority-class handle)
		   (ms-process-handle-count proc) (mem-ref handle-count 'DWORD)
		   (ms-process-max-working-set proc) (mem-ref max-ws 'SIZE_T)
		   (ms-process-min-working-set proc) (mem-ref min-ws 'SIZE_T)
		   (ms-process-command proc) (wide-string-to-lisp path))))
      (when (and handle (not (null-pointer-p handle)))
	(syscall (%close-handle handle))))
    proc))

;; A way using EnumProcesses, which probably isn't a good idea anyway.
#|
(defparameter *pid-array-size* 1000)
(defun get-process-list ()
  "Return a list of OS-PROCESS structures that represent the processes active
around the time of the call."
  (with-foreign-objects ((pids-size 'DWORD))
    (let (pids pids-bytes npids result)
      (unwind-protect
	   (progn
	     (setf pids (foreign-alloc 'DWORD :count *pid-array-size*))
	     (loop :for i :from 0 :below 4
		:do
		(setf pids-bytes (* *pid-array-size* (foreign-type-size 'DWORD)))
		(syscall (%enum-processes pids pids-bytes pids-size))
		:while (= (mem-ref pids-size 'DWORD) pids-bytes)
		:do (incf *pid-array-size* 1000)
		(foreign-free pids)
		(setf pids (foreign-alloc 'DWORD :count *pid-array-size*)))
	     ;; If there's more the 4k processes, you're just getting the
	     ;; first 4k, mmmkay?
	     (setf npids (/ (mem-ref pids-size 'DWORD)
			    (foreign-type-size 'DWORD))
		   result
		   (loop :for i :from 0 :below npids
		      :collect
		      (process-info (mem-aref pids 'DWORD i)))))
	(when pids
	  (foreign-free pids)))
      result)))
|#

;; Using "ToolHelp32"™ - "I pity the tool with no help! Now in 64™ bits®."
(defun system-process-list ()
  (let (snapshot result)
    (unwind-protect
	 (progn
	   (setf snapshot (%create-toolhelp32-snapshot +TH32CS-SNAPALL+ 0))
	   (when (= (pointer-address snapshot) +INVALID-HANDLE-VALUE+)
	     (error 'windows-error :error-code (get-last-error)
		    :format-control "create-toolhelp32-snapshot"))
	   (with-foreign-object (proc '(:struct PROCESSENTRY32))
	     (with-foreign-slots ((size usage process-id default-heap-id
				   module-id threads parent-process-id
				   pri-class-base flags exe-file) proc
				  (:struct PROCESSENTRY32))
	       (setf size (foreign-type-size '(:struct PROCESSENTRY32)))
	       (syscall (%process32-first snapshot proc))
	       (loop :with p
		  :do
		  (setf p (make-ms-process
			   :pid process-id
			   :parent-pid parent-process-id
			   :thread-count threads
			   :priority-class pri-class-base
			   :name (wide-string-to-lisp exe-file)))
		  (push (get-process-info process-id p) result)
		  :until (zerop (%process32-next snapshot proc)))
	       (when (not (equal (get-last-error) +ERROR-NO-MORE-FILES+))
		 (error 'windows-error :error-code (get-last-error)
			:format-control "get-process-list")))))
      (when snapshot
	(%close-handle snapshot)))
    (nreverse result)))

(defun process-list ()
  (mapcar
   (_ (make-os-process
       :id        (ms-process-pid _)
       :parent-id (ms-process-parent-pid _)
       :user      ""
       :size      (or (ms-process-max-working-set _) 0)
       :name      (or (ms-process-command _) (ms-process-name _) "")))
   (system-process-list)))

(defconstant +WAIT-OBJECT-0+       0)
(defconstant +WAIT-ABANDONED-0+    #x00000080)
(defconstant +WAIT-IO-COMPLETION+  #x000000C0)
(defconstant +WAIT-TIMEOUT+        #x00000102)
(defconstant +WAIT-FAILED+         #xFFFFFFFF)

(defcfun ("GetExitCodeProcess" %get-exit-code-process)
    BOOL
  (process HANDLE)
  (exit-code LPDWORD))

(defvar *all-process-handles* nil
  "List of all active process handles we created.")

(defun wait-and-chill (handle)
  "Wait for jobs to do something."
  (let ((handle-count (length *all-process-handles*))
	result result-handle done)
    (with-foreign-objects ((handles '(:pointer HANDLE) handle-count)
			   (exit-code 'DWORD))
      (loop :while (not done) :do
	(setf result (%wait-for-multiple-objects
		      handle-count handles 0 +INFINITE+))
	(format t "wait result ~x~%" result)
	(cond
	  ((= result +WAIT-FAILED+)
	   (return-from wait-and-chill (values (get-last-error) :error)))
	  ((= result +WAIT-TIMEOUT+)
	   (error 'windows-error :error-code 0
		  :format-control "Unexpected wait timeout."
		  #| :format-arguments args |#))
	  ((and (>= result +WAIT-OBJECT-0+)
		(<= result (+ +WAIT-OBJECT-0+ handle-count)))
	   (setf result-handle (nth (- result +WAIT-OBJECT-0+)
				    *all-process-handles*))
	   (syscall (%get-exit-code-process result-handle exit-code))
	   (when (and (eql result-handle handle)
		      (/= (mem-ref exit-code 'DWORD) +STILL-ACTIVE+))
	     (setf done t)))
	  ((and (>= result +WAIT-ABANDONED-0+)
		(<= result (+ +WAIT-ABANDONED-0+ handle-count)))
	   (setf result-handle (nth (- result +WAIT-ABANDONED-0+)
				    *all-process-handles*))
	   (syscall (%get-exit-code-process result-handle exit-code))
	   (when (and (eql result-handle handle)
		      (/= (mem-ref exit-code 'DWORD) +STILL-ACTIVE+)))
	     (setf done t))))
      ;; @@@ This might not be exactly right.
      ;; exit-code could be one of:
      ;; - The exit value specified in the ExitProcess or TerminateProcess
      ;;   function.
      ;; - The return value from the main or WinMain function of the process.
      ;; - The exception value for an unhandled exception that caused the
      ;;   process to terminate.
      ;; How do we tell between these?
      (values (mem-ref exit-code 'DWORD) :exited))))

(defun check-jobs (&optional hang)
  "Check if any sub-processes have changed status. Returns three values.
The PID of the process that changed, and the RESULT and STATUS as returned by
wait. Returns NILs if nothing changed."
  (declare (ignore hang))
  )

;; End
