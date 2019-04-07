;;
;; unix/environmental.lisp - Environmental information for unix
;;

(in-package :opsys-ms)

(defcfun ("GetEnvironmentStringsW" %get-environment-strings
				   :convention :stdcall)
    LPTCH)

(defcfun ("FreeEnvironmentStringsW" %free-environment-strings
				    :convention :stdcall)
    BOOL
  (env LPTCH))

(defun environment ()
  (let (env new-env)
    (unwind-protect
	 (prog ((i 0) c cc name value)
	    (setf env (%get-environment-strings))
	    (when (null-pointer-p env)
	      (error 'windows-error :error-code (get-last-error)
		     :format-control (error-message)))
	    (loop :while (not (zerop (mem-aref env 'WCHAR i)))
	       :do
	       (setf name
		     (with-output-to-string (str)
		       (loop :do (setf c (mem-aref env 'WCHAR i))
			  :while (and (not (zerop c))
				      (char/= #\=
					      (setf cc (wchar-to-character c))))
			  :do (princ cc str)
			  (dbugf :ms "c = ~s cc = ~s i = ~s~%" c cc i)
			  (incf i))))
	       (dbugf :ms "name=~s~%" name)
	       (if (not (zerop c))
		   (progn
		     (incf i) ;; past the #\=
		     (setf value
			   (with-output-to-string (str)
			     (loop :do (setf c (mem-aref env 'WCHAR i))
				:while (not (zerop c))
				:do (princ (wchar-to-character c) str)
				(incf i))))
		     (dbugf :ms "value=~s~%" value)
		     (push (cons (keywordify name) value) new-env))
		   (push (cons (keywordify name) nil) new-env))
	       (incf i))
	    (setf new-env (nreverse new-env)))
      (when env
	(syscall (%free-environment-strings env))))
    new-env))

(defcfun ("GetEnvironmentVariableW"
	  %get-environment-variable :convention :stdcall)
    DWORD
  (name LPCTSTR) (buffer LPTSTR) (size DWORD))

(defun environment-variable (name)
  (with-wide-string (w-name name)
    (let ((size (%get-environment-variable w-name (null-pointer) 0)))
      (if (and (zerop size)
	       (= (get-last-error) +ERROR-ENVVAR-NOT-FOUND+))
	  nil
	  (with-foreign-object (str 'WCHAR (1+ size))
	    (let ((result (%get-environment-variable w-name str size)))
	      (when (/= (1+ result) size)
		(error "environment-variable: ~s ~s" result size
		       ;; (error-message 1)
		       ))
	      (wide-string-to-lisp str result)))))))

(defalias 'env 'environment-variable)

(defcfun ("SetEnvironmentVariableW" %set-environment-variable
				   :convention :stdcall)
    BOOL
  (Name LPCTSTR) (Value LPCTSTR))

(defun set-environment-variable (var value)
  "Set the environtment variable named VAR to the string VALUE. If VALUE is
NIL, unset the VAR, using unsetenv."
  (with-wide-string (env-var var)
    (with-wide-string (env-value value)
      (syscall (%set-environment-variable env-var env-value))))
  value)

(defsetf environment-variable set-environment-variable
    "Set the environtment variable named VAR to the string VALUE.")

(defsetf env set-environment-variable
    "Set the environtment variable named VAR to the string VALUE.")

(defcstruct foreign-processor-arch
  (processor-architecture WORD)
  (reserved WORD))

(defcunion foreign-arch
  (oem-id DWORD)
  (proc-arch (:struct foreign-processor-arch)))

(defcstruct SYSTEM_INFO
  (processor-arch             (:union foreign-arch))
  (page-size		       DWORD)
  (minimum-application-address LPVOID)
  (maximum-application-address LPVOID)
  (active-processor-mask       DWORD_PTR)
  (number-of-processors	       DWORD)
  (processor-type	       DWORD)
  (allocation-granularity      DWORD)
  (processor-level	       WORD)
  (processor-revision	       WORD))

(defctype LPSYSTEM_INFO (:pointer (:struct SYSTEM_INFO)))

(defcfun ("GetSystemInfo" %get-system-info)
    :void
  (system-info LPSYSTEM_INFO))

(defun memory-page-size ()
  (with-foreign-object (sys-info 'LPSYSTEM_INFO)
    (%get-system-info sys-info)
    (foreign-slot-value sys-info '(:struct SYSTEM_INFO) 'page-size)))

(defun processor-count ()
  (with-foreign-object (sys-info 'LPSYSTEM_INFO)
    (%get-system-info sys-info)
    (foreign-slot-value sys-info '(:struct SYSTEM_INFO) 'number-of-processors)))

;; This is probably the same as (machine-instance), and therefore unnecessary.
;; (defcfun ("GetComputerName" real-get-computer-name :convention :stdcall) BOOL
;;    (buffer LPTSTR) (size LPDWORD))

(defcfun ("GetComputerNameW" real-get-computer-name :convention :stdcall) BOOL
  (buffer LPTSTR) (size LPDWORD))

(defconstant +MAX-COMPUTERNAME-LENGTH+ 15)

(defun get-computer-name ()
  (with-foreign-objects ((str 'WCHAR (1+ +MAX-COMPUTERNAME-LENGTH+))
			 (len 'DWORD))
    (setf (mem-ref len 'DWORD) +MAX-COMPUTERNAME-LENGTH+)
    (let ((result (real-get-computer-name str len)))
      (when (zerop result)
	;; we could check for ERROR-BUFFER-OVERFLOW and expand? but why?
	;;(error "get-computer-name: ~s" (error-message))
	(error "get-computer-name: ")
	)
      (wide-string-to-lisp str))))

;; void GetNativeSystemInfo(
;;   LPSYSTEM_INFO lpSystemInfo
;; );

;; typedef struct _SYSTEM_INFO {
;;   union {
;;     DWORD dwOemId;
;;     struct {
;;       WORD wProcessorArchitecture;
;;       WORD wReserved;
;;     } DUMMYSTRUCTNAME;
;;   } DUMMYUNIONNAME;
;;   DWORD     dwPageSize;
;;   LPVOID    lpMinimumApplicationAddress;
;;   LPVOID    lpMaximumApplicationAddress;
;;   DWORD_PTR dwActiveProcessorMask;
;;   DWORD     dwNumberOfProcessors;
;;   DWORD     dwProcessorType;
;;   DWORD     dwAllocationGranularity;
;;   WORD      wProcessorLevel;
;;   WORD      wProcessorRevision;
;; } SYSTEM_INFO, *LPSYSTEM_INFO;

;; PROCESSOR_ARCHITECTURE_AMD64	       9  "x64 (AMD or Intel)"
;; PROCESSOR_ARCHITECTURE_ARM	       5  "ARM"
;; PROCESSOR_ARCHITECTURE_ARM64	      12  "ARM64"
;; PROCESSOR_ARCHITECTURE_IA64	       6  "Intel Itanium-based"
;; PROCESSOR_ARCHITECTURE_INTEL	       0  "x86"
;; PROCESSOR_ARCHITECTURE_UNKNOWN 0xffff  "Unknown architecture"

(defun os-machine-instance ()
  (get-computer-name))

(defun os-machine-type ()
  "x86")

(defun os-machine-version ()
  nil)

(defun os-software-type ()
  "Windows")

(defun os-software-version ()
  "")

;; End
