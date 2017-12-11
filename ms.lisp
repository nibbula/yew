;;
;; ms.lisp - Interface to Microsoft systems.
;;

;; Notes:
;;  - Keep Windows names in StudlyCaps. This facilitates translation from
;;    from other code. Things that we invent, or for the generic interface,
;;    should be in the usual lisp hyphenated-identifier-style.
;;
;;  - Don't try to apdapt Unix concepts to windows (e.g. signals), or vice
;;    versa. Find a generic concept that works on both systems, or if the
;;    faciliity really doesn't exist, only provide system specific versions.
;;
;;  - Imagine you are creating a hypothetical new operating system that has
;;    the features of both, but is better, and has a better lispy API.
;;    Especially don't be biased by Unix's (or Windows') historical
;;    crumminess. Accept that sometimes, Windows (or Unix, or neither) has the
;;    better way.

;; Conventions:
;;   - Names that conflict should be given the prefix "MS-".
;;   - Type names should try to follow Microsoft style, because they're quite
;;     terse, complicated and could be very confusing otherwise.
;;   - Slot, function, constant, and variable names are much nicer to deal with
;;     when converted to Lisp hyphenated and earmuffed style.

(defpackage :ms
  (:documentation "Interface to Microsoft systems.")
  (:use :cl :cffi :dlib :opsys-base)
  (:nicknames :os-ms :wos)
  (:export
   ;; Things that opsys imports:
   #:error-message
   #:environment
   #:environment-variable
   #:memory-page-size
   #:get-user-info
   #:user-name
   #:user-home
   #:user-id
   #:user-full-name
   #:user-name-char-p
   #:valid-user-name
   #:get-next-user
   #:user-list
   #:refresh-user-list
   #:is-administrator
   #:users-logged-in
   #:get-file-info
   #:file-exists
   #:simple-delete-file
   #:with-os-file
   #:read-directory
   #:change-directory
   #:current-directory
   #:make-directory
   #:delete-directory
   #:probe-directory
   #:without-access-errors
   #:hidden-file-name-p
   #:superfluous-file-name-p
   #:is-executable
   #:config-dir
   #:data-path
   #:config-path
   #:cache-dir
   #:runtime-dir
   #:suspend-process
   #:resume-process
   #:terminate-process
   #:process-times
   #:process-list
   #:wait-and-chill
   #:check-jobs
   #:get-time
   #:set-time
   #:listen-for
   #:mounted-filesystems
   #:mount-point-of-file
   #:file-handle-terminal-p
   #:file-handle-terminal-name
   #:*default-console-device-name*
   #:open-terminal
   #:close-terminal
   #:read-terminal-char
   #:read-terminal-byte
   #:read-until
   #:write-terminal-char
   #:write-terminal-string
   #:slurp-terminal
   #:set-terminal-mode
   #:get-terminal-mode
   #:reset-terminal-modes
   #:terminal-query
   #:get-window-size
   ;; Extra stuff:
   #:windows-error
   #:get-computer-name
   ))
(in-package :ms)

#|
(define-foreign-library (kernel32 :stdcall) (t "kernel32"))
(use-foreign-library kernel32)

(define-foreign-library (user32 :stdcall) (t "user32"))
(use-foreign-library user32)
|#

;; (define-foreign-library kernel32 (t "kernel32.dll"))
;; (define-foreign-library user32 (t "user32.dll"))
;; (use-foreign-library kernel32)
;; (use-foreign-library user32)

(use-foreign-library "kernel32.dll")
(use-foreign-library "user32.dll")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(eval-when (:compile-toplevel :load-toplevel :execute)
  (config-feature :ms-unicode) ;; ?? pre-NT or what?
  ;; (not (equal (machine-type) "x86"))
  #+64-bit-target (config-feature :ms-win64)

  (defparameter *windows-major-version*
    (parse-integer (initial-span (software-version) ".")))

  (when (< *windows-major-version* 10)
    (config-feature :t-os-old-windows)))

;; (defctype WINAPI) __stdcall
;; presumably using :stdcall on the library will take care of this.
;; Also one could specify something like:
;;   (defcfun ("WinFoo" win-foo :cconv :stdcall) DWORD (LPSTSTR derp))

(defctype wchar-t :int16)
(defctype VOID :void)
(defctype BOOL :int)
(defctype INT :int)
(defctype UINT :unsigned-int)
(defctype INT8  :char)
(defctype INT16 :short)
(defctype INT32 :int)
(defctype INT64 :int64)
(defctype LONG :long)
(defctype WORD :unsigned-short)
(defctype DWORD :unsigned-long)
(defctype DWORD32 :unsigned-int)
(defctype DWORD64 :uint64)
(defctype DWORDLONG :uint64)
(defctype MS-BYTE :unsigned-char)
(defctype MS-BOOLEAN MS-BYTE)
(defctype MS-FLOAT :float)
(defctype MS-CHAR :char)
(defctype MS-SHORT :short)
(defctype CCHAR :char)
(defctype WCHAR wchar-t)
(defctype LPWCH (:pointer WCHAR))
(defctype LPTCH LPWCH)
(defctype LPSTR (:pointer MS-CHAR))
(defctype LPCSTR (:pointer MS-CHAR))
(defctype LPWSTR (:pointer WCHAR))
(defctype LPCWSTR (:pointer WCHAR))
#+ms-unicode
(progn
  (defctype LPTSTR LPWSTR)
  (defctype LPCTSTR LPCWSTR))
#-ms-unicode
(progn
  (defctype LPTSTR LPSTR)
  (defctype LPCTSTR LPCSTR))
(defctype PVOID (:pointer :void))
(defctype LPVOID (:pointer :void))
(defctype LPCVOID (:pointer :void))
(defctype LPDWORD (:pointer DWORD))
#+ms-win64
(progn
  (defctype INT_PTR :int64)
  (defctype ULONG_PTR :uint64))
#-ms-win64
(progn
  (defctype INT_PTR :int)
  (defctype ULONG_PTR :unsigned-long))
(defctype DWORD_PTR ULONG_PTR)
(defctype HANDLE PVOID)
(defctype HFILE :int)
(defctype HLOCAL HANDLE)

;; Utilities

(defun wide-string-to-lisp (wide-string &optional n)
  "Convert a Windows wide string (LPTSTR or LPWSTR) to a Lisp string.
If N isn't given, assume WIDE-STRING is terminated by a zero character."
  (if n
      (with-output-to-string (str)
	(loop :for i :from 0 :below n
	   :do (princ (code-char (mem-aref wide-string 'WCHAR i)) str)))
      (with-output-to-string (str)
	(loop :with c :and i = 0
	   :while (not (zerop (setf c (mem-aref wide-string 'WCHAR i))))
	   :do (princ (code-char c) str)
	   (incf i)))))

(defmacro with-wide-string ((var string) &body body)
  "Make a Windows wide string (LPTSTR or LPWSTR) out of a Lisp string."
  (with-unique-names (i cc)
    `(with-foreign-object (,var 'WCHAR (1+ (length ,string)))
       (let ((,i 0))
	 (loop :while (< ,i (length ,string))
	    :do
	    (setf (cffi:mem-aref ,var 'WCHAR ,i)
		  (let ((,cc (char-code (char ,string ,i))))
		    (if (> ,cc #xffff)
			(error "I didn't write UTF-16 conversion yet! c=#x~x"
			       ,cc)
			,cc)))
	    (incf ,i))
	 (setf (cffi:mem-aref ,var 'WCHAR ,i) 0))
       ,@body)))

(defcfun ("LocalFree" local-free)
    HLOCAL
    (mem HLOCAL))

(defcfun ("HeapFree" heap-free)
    BOOL
  (heap HANDLE)
  (flags DWORD)
  (mem LPVOID))

(defcfun ("GetProcessHeap" get-process-heap) HANDLE)

(defun dork-free (ptr)
  #+t-os-old-windows
  (let ((result (local-free ptr)))
    (dbugf :ms "local-free ~s~%" result)
    (when (not (null-pointer-p result))
      (error "dork-free failed ~s" ptr))) ;; @@@ call (error-message)       
  #-t-os-old-windows
  (let* ((heap (get-process-heap))
	 (result (heap-free heap 0 ptr)))
    (dbugf :ms "heap-free ~s~%" result)
    (when (zerop result)
      (error "dork-free failed ~s" ptr)))) ;; @@@ call (error-message)

(defconstant LANG_NEUTRAL #x00)
(defconstant SUBLANG_DEFAULT #x01)

(defun MAKELANGID (primary sublang)
  (logior (ash (logior #xffff sublang) 10)
	  (logior #xffff primary)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error handling

(defcfun ("GetLastError" get-last-error) DWORD)

(defconstant +FORMAT-MESSAGE-ALLOCATE-BUFFER+ #x00000100)
(defconstant +FORMAT-MESSAGE-ARGUMENT-ARRAY+  #x00002000)
(defconstant +FORMAT-MESSAGE-FROM-HMODULE+    #x00000800)
(defconstant +FORMAT-MESSAGE-FROM-STRING+     #x00000400)
(defconstant +FORMAT-MESSAGE-FROM-SYSTEM+     #x00001000)
(defconstant +FORMAT-MESSAGE-IGNORE-INSERTS+  #x00000200)
(defconstant +FORMAT-MESSAGE-MAX-WIDTH-MASK+  #x000000FF)

(defcfun ("FormatMessageW" format-message :convention :stdcall)
    DWORD
  (flags	DWORD)
  (source	LPCVOID)
  (message-id	DWORD)
  (language-id	DWORD)
  (buffer	LPTSTR)
  (size		DWORD)
  (va-list      PVOID) ; ??? _In_opt_ va_list *Arguments
  )

(defun error-message (&optional (error-code (get-last-error)))
  "Return a string describing the error code." 
  (with-foreign-object (message '(:pointer LPTSTR))
  ;;(with-foreign-object (message 'LPWSTR)
    (unwind-protect
       (progn
	 (when (zerop (format-message
		       (logior +FORMAT-MESSAGE-FROM-SYSTEM+
			       +FORMAT-MESSAGE-ALLOCATE-BUFFER+
			       +FORMAT-MESSAGE-IGNORE-INSERTS+)
		       (null-pointer)
		       error-code
		       (MAKELANGID LANG_NEUTRAL SUBLANG_DEFAULT)
		       message
		       0
		       (null-pointer)))
	   (error "FormatMessage failed: ~s ~s" error-code (get-last-error)))
	 (wide-string-to-lisp (mem-ref message 'LPTSTR)))
      (when (not (null-pointer-p message))
	(dork-free (mem-ref message 'LPTSTR))))))

(define-condition windows-error (opsys-error)
  ()
  (:documentation "An error from calling a Windows function."))

(defun error-check (c &optional fmt &rest args)
  "Check if a BOOL function fails (returns FALSE i.e. zero) and signal an
appropriate error."
  (if (zerop c)
      (error 'windows-error :error-code (get-last-error)
	     :format-control fmt :format-arguments args)
      c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environmental

(defconstant +ERROR-ENVVAR-NOT-FOUND+ 203)

(defcfun ("GetEnvironmentStringsW" %get-environment-strings
				   :convention :stdcall)
    LPTCH)

(defcfun ("FreeEnvironmentStringsW" %free-environment-strings
				    :convention :stdcall)
    BOOL
  (env LPTCH))

(defcfun ("GetEnvironmentVariableW"
	  real-get-environment-variable :convention :stdcall)
    DWORD
  (name LPCTSTR) (buffer LPTSTR) (size DWORD))

(defcfun ("SetEnvironmentVariableW" %set-environment-variable
				   :convention :stdcall)
    BOOL
  (Name LPCTSTR) (Value LPCTSTR))

;; (ms:environment)

(defun environment ()
  (let (env new-env)
    (unwind-protect
	 (prog ((i 0) c cc name value)
	    ;;(setf env (mem-ref (%get-environment-strings) '(:pointer :int16)))
	    (setf env (%get-environment-strings))
	    (loop :while (not (zerop (mem-aref env 'WCHAR i)))
	    ;;(loop :while (not (zerop (mem-aref env :char i)))
	       :do
	       (setf name
		     (with-output-to-string (str)
		       (loop :do (setf c (mem-aref env 'WCHAR i))
		       ;;(loop :do (setf c (mem-aref env :char i))
			  :while (and (not (zerop c))
				      (char/= #\= (setf cc (code-char c))))
			  :do (princ cc str)
			  (dbugf :ms "c = ~s cc = ~s i = ~s~%" c cc i)
			  (incf i))))
	       (dbugf :ms "name=~s~%" name)
	       (if (not (zerop c))
		   (progn
		     (setf value
			   (with-output-to-string (str)
			     (loop :do (setf c (mem-aref env 'WCHAR i))
				:while (not (zerop c))
				:do (princ (code-char c) str)
				(incf i))))
		     (dbugf :ms "value=~s~%" value)
		     (push (cons name value) new-env))
		   (push (cons name nil) new-env)))
	    (setf new-env (nreverse new-env)))
      (when env
	(%free-environment-strings env)))
    new-env))

(defun environment-variable (name)
  (with-wide-string (w-name name)
    (let ((size (real-get-environment-variable w-name (null-pointer) 0)))
      (if (and (zerop size)
	       (= (get-last-error) +ERROR-ENVVAR-NOT-FOUND+))
	  nil
	  (with-foreign-object (str 'WCHAR (1+ size))
	    (let ((result (real-get-environment-variable w-name str size)))
	      (when (/= (1+ result) size)
		(error "environment-variable: ~s ~s" result size
		       ;; (error-message 1)
		       ))
	      (wide-string-to-lisp str result)))))))

(defun set-environment-variable (var value)
  "Set the environtment variable named VAR to the string VALUE. If VALUE is
NIL, unset the VAR, using unsetenv."
  (declare (ignore var value))
  ;; @@@ 
  )

(defsetf environment-variable set-environment-variable
    "Set the environtment variable named VAR to the string VALUE.")

(defun memory-page-size ()
  4096)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Users

#|
(defun is-administrator ()
  "Return true if you have administrator privileges."

  ;; Possibilities:
  ;;
  ;; Win9x: Everyone is "admin"
  ;;
  ;; NT4: OpenThreadToken/OpenProcessToken +
  ;;
  ;; GetTokenInformation(...,TokenGroups,...) on DOMAIN_ALIAS_RID_ADMINS SID
  ;; in a loop
  ;;
  ;; 2000+: OpenThreadToken/OpenProcessToken + CheckTokenMembership on
  ;; DOMAIN_ALIAS_RID_ADMINS SID
  ;;
  ;; Other alternatives:
  ;;
  ;; AccessCheck()
  ;; IsUserAnAdmin ()
  )

BOOL IsUserAdmin(VOID)
/*++ 
Routine Description: This routine returns TRUE if the caller's
process is a member of the Administrators local group. Caller is NOT
expected to be impersonating anyone and is expected to be able to
open its own process and process token. 
Arguments: None. 
Return Value: 
   TRUE - Caller has Administrators local group. 
   FALSE - Caller does not have Administrators local group. --
*/ 
{
BOOL b;
SID_IDENTIFIER_AUTHORITY NtAuthority = SECURITY_NT_AUTHORITY;
PSID AdministratorsGroup; 
b = AllocateAndInitializeSid(
    &NtAuthority,
    2,
    SECURITY_BUILTIN_DOMAIN_RID,
    DOMAIN_ALIAS_RID_ADMINS,
    0, 0, 0, 0, 0, 0,
    &AdministratorsGroup); 
if(b) 
{
    if (!CheckTokenMembership( NULL, AdministratorsGroup, &b)) 
    {
         b = FALSE;
    } 
    FreeSid(AdministratorsGroup); 
}

return(b);
}
  
  nil)

(defun users-logged-in ()
  "Return a list of names of logged in users."
  #|
DirectoryEntry localMachine = new DirectoryEntry("WinNT://" + Environment.MachineName);
DirectoryEntry admGroup = localMachine.Children.Find("users","group");
object members = admGroup.Invoke("members", null);
foreach (object groupMember in (IEnumerable)members)
{
    DirectoryEntry member = new DirectoryEntry(groupMember);
    lstUsers.Items.Add(member.Name);
}
 |#
  )
|#

(defun get-user-info (&key name id)
  (declare (ignore name id))
  (make-user-info :name "dan"
		  :id 1024
		  :full-name "Nibby Nebbulous"
		  :home-directory "e:\\"
		  :shell "lish"
		  :primary-group-id 1
		  :guid "123-456-789"
		  :picture "D E R P!"))

(defun user-name (&optional id)
  (declare (ignore id))
  "dan")

(defun user-home (&optional (user (user-name)))
  (declare (ignore user))
  "e:\\")

(defun user-id (&key name effective)
  "Return the ID of the user with NAME, which defaults to the current user."
  (declare (ignore name effective))
  1024)

(defun user-full-name (&optional id)
  "Return the full name of user with ID, which defaults to the current user."
  (declare (ignore id))
  "Nibby Nebbulous")

(defun user-name-char-p (c)
  "Return true if C is a valid character in a user name."
  (alphanumericp c))

(defun valid-user-name (username)
  "Return true if USERNAME could be a valid user name, but not that the user
actually exists."
  (declare (ignore username))
  t)

(defun get-next-user ()
  "Return the next user structure from the user database."
  nil)

(defun user-list ()
  "Return the list of all users."
  (list (get-user-info)))

(defun refresh-user-list ()
  "Make GET-NEXT-GROUP or GROUP-LIST return potentially updated data."
  )

(defun is-administrator ()
  "Return true if you are root, or effectively root."
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Groups

;; No such thing.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Login/accounting database

(defun users-logged-in ()
  "Return a list of names of logged in users."
  "dan")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files

(defun get-file-info (path &key (follow-links t))
  "Return information about the file described by PATH in a FILE-INFO
structure. If FOLLOW-LINKS is true (the default), then if PATH is a symbolic
link, return information about the file it's linked to, otherwise return
information about the link itself."
  (declare (ignore path follow-links))
  (make-file-info
   :type :regular
   :size 1024
   :flags nil
   :creation-time 0
   :access-time 0
   :modification-time 0))

(defun file-exists (filename)
  "Check that a file with FILENAME exists at the moment. But it might not exist
for long."
  (declare (ignore filename))
  t)

(defun simple-delete-file (pathname)
  "Delete a file. Doesn't monkey with the name, which should be a string.
Doesn't operate on streams."
  (declare (ignore pathname))
  nil)

(defmacro with-os-file ((var filename &key
			     (direction :input)
			     (if-exists :error)
			     (if-does-not-exist :error)) &body body)
  "Evaluate the body with the variable VAR bound to a posix file descriptor
opened on FILENAME. DIRECTION, IF-EXISTS, and IF-DOES-NOT-EXIST are simpler
versions of the keywords used in Lisp open.
  DIRECTION         - supports :INPUT, :OUTPUT, and :IO.
  IF-EXISTS         - supports :ERROR and :APPEND.
  IF-DOES-NOT-EXIST - supports :ERROR, and :CREATE.
"
  `(with-open-file (,var ,filename
			 :direction ,direction
			 :if-exists ,if-exists
			 :if-does-not-exist ,if-does-not-exist)
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directories

;; @@@ maybe we should rename this to directory? or directory-contents?
(defun read-directory (&key dir append-type full omit-hidden)
  "Return a list of the file names in DIR as strings. DIR defaults to the ~
current directory. If APPEND-TYPE is true, append a character to the end of ~
the name indicating what type of file it is. Indicators are:
  / : directory
  @ : symbolic link
  | : FIFO (named pipe)
  = : Socket
  > : Doors
If FULL is true, return a list of dir-entry structures instead of file name ~
strings. Some dir-entry-type keywords are:
  :unknown :pipe :character-device :dir :block-device :regular :link :socket
  :whiteout :undefined
If OMIT-HIDDEN is true, do not include entries that start with ‘.’.
"
  (declare (ignore dir append-type full omit-hidden))
  nil
  )

(defun change-directory (&optional path)
  "Change the current directory to DIR. Defaults to (user-homedir-pathname) ~
if not given."
  (declare (ignore path))
  nil)

(defun current-directory ()
  "Return the full path of the current working directory as a string."
  nil)

(defun make-directory (path &key (mode #o755))
  "Make a directory."
  (declare (ignore path mode))
  nil)

(defun delete-directory (path)
  "Delete a directory."
  (declare (ignore path))
  nil)

(defun probe-directory (dir)
  "Something like probe-file but for directories."
  (declare (ignore dir))
  nil)

(defmacro without-access-errors (&body body)
  "Evaluate the body while ignoring typical file access error from system
calls. Returns NIL when there is an error."
  `(ignore-errors ,@body))

(defun hidden-file-name-p (name)
  "Return true if the file NAME is normally hidden."
  (declare (ignore name))
  nil)

(defun superfluous-file-name-p (name)
  "Return true if the file NAME is considered redundant. On POSIX file
systems, this means \".\" and \"..\"."
  (declare (ignore name))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System Commands?

(defun is-executable (path &key user regular)
  "Return true if the PATH is executable by the USER. USER defaults to the
current effective user. If REGULAR is true also check if it's a regular file."
  (declare (ignore path user regular))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application paths

;; App name isn't really optional.
(defvar *app-name* nil)
(defun app-name ()
  (or *app-name* (setf *app-name* "WinBogo"))) ;; XXX @@@

(defun config-dir (&optional (app-name (app-name)))
  "Where user specific configuration files should be stored."
  (s+ "%USERPROFILE%\\AppData\\Local\\" app-name "\\"))

(defun data-path (&optional app-name)
  "Search path for user specific data files."
  (declare (ignore app-name))
  nil)

(defun config-path (&optional (app-name (app-name)))
  "Search path for user specific configuration files."
  (list
   (s+ "%USERPROFILE%\\AppData\\Local\\" app-name "\\")
   (s+ "%PROGRAMDATA%\\" app-name "\\config\\")))

(defun cache-dir (&optional app-name)
  "Directory where user specific non-essential data files should be stored."
  (declare (ignore app-name))
  nil)

(defun runtime-dir (&optional app-name)
  "Directory where user-specific non-essential runtime files and other file
objects should be stored."
  (declare (ignore app-name))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processes

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

;; A FILETIME contains a 64-bit value representing the number of 100-nanosecond
;; intervals since January 1, 1601 (UTC).

;; To get times for children, we have to put them in a job.
;; If we create a job for the shell with CreateJobObject and add any
;; children to it with AssignProcessToJobObject, then maybe we can get
;; cumulative execution times QueryInformationJobObject
;;

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

(defun process-times (id)
  (declare (ignore id))
#|
BOOL WINAPI GetProcessTimes(
  _In_  HANDLE     hProcess,
  _Out_ LPFILETIME lpCreationTime,
  _Out_ LPFILETIME lpExitTime,
  _Out_ LPFILETIME lpKernelTime,
  _Out_ LPFILETIME lpUserTime
);
  (with-objects (creation-time exit-time kernel-time user-time)
    (GetProcessTimes (GetCurrentProcess)
		     creation-time exit-time kernel-time user-time)
  |#
  nil)

(defun process-list ()
  "Return a list of OS-PROCESS structures that represent the processes active
around the time of the call."
  nil)

(defun wait-and-chill ()
  "Wait for jobs to do something.")

(defun check-jobs ()
  "Check if any sub-processes have changed status. Returns three values.
The PID of the process that changed, and the RESULT and STATUS as returned by
wait. Returns NILs if nothing changed."
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timers / Timing

(defun get-time ()
  "Return the time in seconds and nanoseconds. The first value is seconds in
so-called “universal” time. The second value is nanoseconds."
  (values (get-universal-time) 0))

(defun set-time (seconds nanoseconds)
  "Set time in seconds and nanoseconds. Seconds are in so-called
“universal” time."
  (declare (ignore seconds nanoseconds))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; poll

(defun listen-for (seconds &optional (fd 0))
  "Listen on the OS file descriptor for at most N seconds or until input is ~
available."
  (declare (ignore seconds fd))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System administration

(defun mounted-filesystems ()
  "Return a list of filesystem info."
  nil)

(defun mount-point-of-file (file)
  "Try to find the mount of FILE. This might not always be right."
  (declare (ignore file))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminals

;; The way terminals work on Windows probably would be better dealt with
;; by a whole terminal-* driver. Unfortunately, to have an O/S specific
;; terminal-* type breaks our current model. It's proabably okay to break that
;; idea a bit, but we still want to have this opsys level interface for the time
;; being and just cope with the mismatchedness.

(defstruct ms-term
  "A dumb way to deal with it."
  in-handle
  out-handle
  mode
  width
  height)

;; (defconstant +STD-INPUT-HANDLE+  -10) ; CONIN$
;; (defconstant +STD-OUTPUT-HANDLE+ -11) ; CONOUT$
;; (defconstant +STD-ERROR-HANDLE+  -12) ; CONOUT$
(defmacro as-32bit-unsigned (n) `(logand (1- (expt 2 32)) (lognot (1- (- ,n)))))
(defconstant +STD-INPUT-HANDLE+  (as-32bit-unsigned -10)) ; CONIN$
(defconstant +STD-OUTPUT-HANDLE+ (as-32bit-unsigned -11)) ; CONOUT$
(defconstant +STD-ERROR-HANDLE+  (as-32bit-unsigned -12)) ; CONOUT$

(defcfun ("GetStdHandle" %get-std-handle :convention :stdcall)
    HANDLE
   (nStdHandle DWORD)) ; in

;; a.k.a: ((HANDLE)~(ULONG_PTR)0) or the maximum pointer value.
(defconstant +INVALID-HANDLE-VALUE+
  #+ms-win64 (1- (expt 2 64))
  #-ms-win64 (1- (expt 2 32)))

(defcstruct COORD
  (x MS-SHORT)
  (y MS-SHORT))

(defctype PCOORD (:pointer (:struct COORD)))

(defcunion foreign-uchar
  (unicode-char WCHAR)
  (ascii-char MS-CHAR))

(defcstruct foreign-key-event
  (key-down 	       BOOL)
  (repeat-count        WORD)
  (virtual-key-code    WORD)
  (virtual-scan-code   WORD)
  (uchar               (:union foreign-uchar))
  (control-key-state   DWORD))

(defconstant +FROM-LEFT-1ST-BUTTON-PRESSED+ #x0001)
(defconstant +RIGHTMOST-BUTTON-PRESSED+     #x0002)
(defconstant +FROM-LEFT-2ND-BUTTON-PRESSED+ #x0004)
(defconstant +FROM-LEFT-3RD-BUTTON-PRESSED+ #x0008)
(defconstant +FROM-LEFT-4TH-BUTTON-PRESSED+ #x0010)

(defconstant +RIGHT-ALT-PRESSED+  #x0001)
(defconstant +LEFT-ALT-PRESSED+   #x0002)
(defconstant +RIGHT-CTRL-PRESSED+ #x0004)
(defconstant +LEFT-CTRL-PRESSED+  #x0008)
(defconstant +SHIFT-PRESSED+      #x0010)
(defconstant +NUMLOCK-ON+         #x0020)
(defconstant +SCROLLLOCK-ON+      #x0040)
(defconstant +CAPSLOCK-ON+        #x0080)
(defconstant +ENHANCED-KEY+       #x0100)

(defconstant +MOUSE-MOVED+ 	  #x0001)
(defconstant +DOUBLE-CLICK+ 	  #x0002)
(defconstant +MOUSE-WHEELED+ 	  #x0004)
(defconstant +MOUSE-HWHEELED+ 	  #x0008)

(defcstruct foreign-mouse-event
  (mouse-position    (:struct COORD))
  (button-state      DWORD)
  (control-key-state DWORD)
  (event-flags	     DWORD))

(defcstruct foreign-buffer-size-event
    (size (:struct COORD)))

(defcstruct foreign-menu-event
    (command-id UINT))

(defcstruct foreign-focus-event
    (set-focus BOOL))

(defconstant +KEY-EVENT+                #x0001)
(defconstant +MOUSE-EVENT+ 	        #x0002)
(defconstant +WINDOW-BUFFER-SIZE-EVENT+ #x0004)
(defconstant +MENU-EVENT+ 	        #x0008)
(defconstant +FOCUS-EVENT+              #x0010)

(defcunion foreign-event-union
  (key-event                 (:struct foreign-key-event))
  (mouse-event               (:struct foreign-mouse-event))
  (window-buffer-size-event  (:struct foreign-buffer-size-event))
  (menu-event                (:struct foreign-menu-event))
  (focus-event		     (:struct foreign-focus-event)))

(defcstruct foreign-input-record
  (event-type WORD)
  (event (:union foreign-event-union)))

(defctype PINPUT_RECORD (:pointer (:struct foreign-input-record)))

(defcfun ("ReadConsoleInputW" %read-console-input :convention :stdcall)
    BOOL
   (console-input HANDLE)    		; in
   (buffer PINPUT_RECORD)   		; out
   (length DWORD)	      		; in
   (number-of-events-read LPDWORD))	; out

(defconstant +ENABLE-PROCESSED-INPUT+        #x0001)
(defconstant +ENABLE-LINE-INPUT+             #x0002)
(defconstant +ENABLE-ECHO-INPUT+             #x0004)
(defconstant +ENABLE-WINDOW-INPUT+           #x0008)
(defconstant +ENABLE-MOUSE-INPUT+            #x0010)
(defconstant +ENABLE-INSERT-MODE+            #x0020)
(defconstant +ENABLE-QUICK-EDIT-MODE+        #x0040)
(defconstant +ENABLE-VIRTUAL-TERMINAL-INPUT+ #x0200)

(defconstant +NORMAL-INPUT-MODES+ (logior +ENABLE-PROCESSED-INPUT+
					  +ENABLE-LINE-INPUT+
					  +ENABLE-ECHO-INPUT+
					  +ENABLE-MOUSE-INPUT+
					  +ENABLE-INSERT-MODE+
					  +ENABLE-QUICK-EDIT-MODE+
					  ;; +ENABLE-VIRTUAL-TERMINAL-INPUT+
					  ))

(defconstant +ENABLE-PROCESSED-OUTPUT+		  #x0001)
(defconstant +ENABLE-WRAP-AT-EOL-OUTPUT+	  #x0002)
(defconstant +ENABLE-VIRTUAL-TERMINAL-PROCESSING+ #x0004)
(defconstant +DISABLE-NEWLINE-AUTO-RETURN+	  #x0008)
(defconstant +ENABLE-LVB-GRID-WORLDWIDE+	  #x0010)

(defconstant +NORMAL-OUTPUT-MODES+ (logior +ENABLE-PROCESSED-OUTPUT+
					   +ENABLE-WRAP-AT-EOL-OUTPUT+))

(defcfun ("GetConsoleMode" %get-console-mode :convention :stdcall)
    BOOL
  (console-handle HANDLE)		; in
  (mode LPDWORD))			; out

(defcfun ("SetConsoleMode" %set-console-mode :convention :stdcall)
    BOOL
  (console-handle HANDLE)		; in
  (mode DWORD))				; in

(defun file-handle-terminal-p (fd)
  "Return true if the system file descriptor FD is attached to a terminal."
  (with-foreign-object (ms-mode 'DWORD)
    (not (zerop (%get-console-mode fd ms-mode)))))

(defun file-handle-terminal-name (fd)
  "Return the device name of the terminal attached to the system file
descriptor FD."
  (declare (ignore fd))
  ;;(GetFileInformationByHandleEx fd)
  nil)

(defvar *default-console-device-name* "CON" ;; @@@ or should it be CONIN$ ?
  "Name of the default console device.")

(defcstruct SECURITY_ATTRIBUTES
  (length              DWORD)
  (security-descriptor LPVOID)
  (inherit-handle      BOOL))

(defctype PSECURITY_ATTRIBUTES (:pointer (:struct SECURITY_ATTRIBUTES)))
(defctype LPSECURITY_ATTRIBUTES (:pointer (:struct SECURITY_ATTRIBUTES)))

(defcfun ("CreateFileW" %create-file)
    HANDLE
  (file-name 		 LPCTSTR)
  (desired-access 	 DWORD)
  (share-mode 		 DWORD)
  (security-attributes   LPSECURITY_ATTRIBUTES)
  (creation-disposition  DWORD)
  (flags-and-attributes  DWORD)
  (template-file 	 HANDLE))

(defconstant +GENERIC-READ+  #x80000000)
(defconstant +GENERIC-WRITE+ #x40000000)

(defconstant +FILE-SHARE-READ+  #x00000001)
(defconstant +FILE-SHARE-WRITE+ #x00000002)

(defconstant +OPEN-EXISTING+ 3)

(defun open-real-console (direction)
  (flet ((open-it (name)
	   (with-wide-string (nn name)
	     (%create-file nn
			   (logior +GENERIC-READ+ +GENERIC-WRITE+)
			   (logior +FILE-SHARE-READ+ +FILE-SHARE-WRITE+)
			   (null-pointer) ;; @@@ maybe we should set inherit?
			   +OPEN-EXISTING+
			   0 ;; very unspecified flags?
			   (null-pointer)))))
    (let ((handle
	   (ecase direction
	     (:input  (open-it "CONIN$"))
	     (:output (open-it "CONOUT$")))))
      (when (= (pointer-address handle) +INVALID-HANDLE-VALUE+)
	(error 'windows-error :error-code (get-last-error)
	       :format-control "Failed to open the real console."))
      handle)))

(defun open-terminal (device-name direction)
  "Open a terminal. Return the system file handle."
  (declare (ignore device-name))
  (ecase direction
    (:input
     (let (in-h out-h tty)
       ;; Input handle
       (setf in-h (%get-std-handle +STD-INPUT-HANDLE+))
       (when (= (pointer-address in-h) +INVALID-HANDLE-VALUE+)
	 (error 'windows-error :error-code (get-last-error)
		:format-control "Failed to get terminal input handle."))
       (dbugf :ms "open-terminal input handle = #x~x~%" (pointer-address in-h))

       ;; Output handle
       (setf out-h (%get-std-handle +STD-OUTPUT-HANDLE+))
       (when (= (pointer-address out-h) +INVALID-HANDLE-VALUE+)
	 (error 'windows-error :error-code (get-last-error)
		:format-control "Failed to get terminal output handle."))
       (dbugf :ms "open-terminal output handle = #x~x~%" (pointer-address out-h))

       (setf tty (make-ms-term :in-handle in-h :out-handle out-h))

       ;; Test handles to try to see if they're usable consoles.
       (when (not (file-handle-terminal-p in-h))
	 (dbugf :ms "terminal handles aren't real~%")
	 (setf (ms-term-in-handle tty) (open-real-console :input)
	       (ms-term-out-handle tty) (open-real-console :output)))
       (dbugf :ms "ms-term = ~s~%" tty)
       tty))
    (:output *terminal-io*)))

(defun close-terminal (terminal-handle)
  "Close a terminal."
  (declare (ignore terminal-handle))
  ;; We don't really need to close a standard handle.
  ;; @@@ But perhaps if we were to open (or attach) to another terminal device
  ;; we might need to close that.
  nil)

(defun read-console-input (terminal)
  (let (c)
    (with-slots (in-handle width height) terminal
      (with-foreign-objects ((buf '(:pointer (:struct foreign-input-record)))
			     (events-read 'DWORD))
	(loop :do
	   (setf c nil)
	   (let ((result (%read-console-input in-handle buf 1 events-read)))
	     (when (zerop result)
	       (error "Failed to read console input."))
	     (with-foreign-slots ((event-type event)
				  ;;(mem-ref buf '(:struct foreign-input-record))
				  buf
				  (:struct foreign-input-record))
	       (dbugf :ms "event-type ~s~%" event-type)
	       (cond
		 ((equal event-type +KEY-EVENT+)
		  (with-foreign-slots ((key-down uchar) event
				       (:struct foreign-key-event))
		    (dbugf :ms "key-down ~a uchar = ~a~%" key-down uchar)
		    (when (= 1 key-down)
		      (setf c (foreign-slot-value uchar '(:union foreign-uchar)
						  'unicode-char)))
		    (dbugf :ms "c = ~s~%" c)))
		 ((equal event-type +WINDOW-BUFFER-SIZE-EVENT+)
		  (with-foreign-slots ((size) event
				       (:struct foreign-buffer-size-event))
		    (setf width (foreign-slot-value size '(:struct COORD) 'x)
			  height (foreign-slot-value size '(:struct COORD) 'y))))
		 ((equal event-type +MOUSE-EVENT+) #| @@@ ignore |# )
		 ((equal event-type +MENU-EVENT+) #| @@@ ignore |# )
		 ((equal event-type +FOCUS-EVENT+) #| @@@ ignore |# )
		 (t
		  (format t "Unknown event type from console #x~x~%" event-type)
		  ;;(error "Unknown event type from console."))
		  ))))
	   :while (not c))))
    c))

(defun read-terminal-char (terminal &key timeout)
  "Return a character read from the terminal TERMINAL-HANDLE.
If there's a problem, it will signal a READ-CHAR-ERROR. If the terminal is
resized it will signal an OPSYS-RESIZED. If the program is continued from
being suspended, it will signal an OPSYS-RESUMED. Usually this means the
caller should handle these possibilites. Returns the character read or NIL if it
the timeout is hit."
  (declare (ignore timeout))
  (code-char (read-console-input terminal)))

(defun read-terminal-byte (terminal &key timeout)
  "Return an unsigned byte read from the terminal TERMINAL-HANDLE.
If there's a problem, it will signal a READ-CHAR-ERROR. If the terminal is
resized it will signal an OPSYS-RESIZED. If the program is continued from
being suspended, it will signal an OPSYS-RESUMED. Usually this means the
caller should handle these possibilites. Returns the byte read or NIL if it
the timeout is hit."
  (declare (ignore timeout))
  (read-console-input terminal))

(defun read-until (tty stop-char &key timeout)
  "Read until STOP-CHAR is read. Return a string of the results.
TTY is a file descriptor."
  (declare (ignore tty stop-char timeout))
  ;; @@@ taking the lazy slow way out
  ;; (loop :with c = (read-terminal-char tty)
  ;;    :while (char/= c stop-char))
     )

(defcfun ("WriteConsole" %write-console)
    BOOL
  (console-output HANDLE)		; in
  (buffer (:pointer VOID))		; in
  (number-of-chars-to-write DWORD)	; in
  (number-of-chars-written LPDWORD)	; out
  (reserved LPVOID))			; reserved

(defun write-terminal-char (terminal char)
  "Write CHAR to the terminal designated by TERMINAL."
  (cond
    ((output-stream-p terminal)
     (write-char char terminal))
    ((ms-term-p terminal)
     (write-char char *terminal-io*))))	; @@@ taking the easy way out

(defun write-terminal-string (terminal string)
  "Write STRING to the terminal designated by TERMINAL-HANDLE."
  #| (cond
    ((typep terminal 'output-stream)
     (write-string string terminal))
    ((ms-term-p terminal)
     ;;(write-string string *terminal-io*)))) ; @@@ taking the easy way out
     (write-string string *standard-output*)))) ; @@@ taking the easy way out
  |#
  nil)

(defun slurp-terminal (tty &key timeout)
  "Read until EOF. Return a string of the results. TTY is a file descriptor."
  (declare (ignore tty timeout))
  ;; @@@ XXX not done?
  "")

(defun set-terminal-mode (tty &key
				(echo    nil echo-supplied)
				(line    nil line-supplied)
				(raw     nil raw-supplied)
				(timeout nil timeout-supplied)
				(mode    nil mode-supplied))
  "Set the terminal mode. Arguments are:
  ECHO makes input automatically output back, so you can see what you typed.
  LINE makes input wait for a newline until returning.
  RAW ingores normal processing, like interrupt keys.
  TIMEOUT is the time in milliseconds to wait before returning with no input.
  MODE is a TERMINAL-MODE structure to take settings from.
The individual settings override the settings in MODE."
  (with-slots (in-handle (our-mode mode)) tty
    (when mode-supplied
      ;; Copy modes from the given mode
      (setf (terminal-mode-echo our-mode) (terminal-mode-echo mode)
	    (terminal-mode-line our-mode) (terminal-mode-line mode)
	    (terminal-mode-raw our-mode) (terminal-mode-raw mode)
	    (terminal-mode-timeout our-mode) (terminal-mode-timeout mode)))
    (when echo-supplied    (setf (terminal-mode-echo our-mode) echo))
    (when line-supplied    (setf (terminal-mode-line our-mode) line))
    (when raw-supplied     (setf (terminal-mode-raw our-mode) raw))
    (when timeout-supplied (setf (terminal-mode-timeout our-mode) timeout))
    (with-foreign-object (ms-mode 'DWORD)
      (when (zerop (%get-console-mode in-handle ms-mode))
	(error 'windows-error :error-code (get-last-error)
	       :format-control "Can't get the console mode."))
      (let ((m (mem-ref ms-mode 'DWORD)))
	(dbugf :ms "console mode was ~s~%" m)
	(when (terminal-mode-echo our-mode)
	  (setf m (logior m +ENABLE-ECHO-INPUT+)))
	(when (terminal-mode-line our-mode)
	  (setf m (logior m +ENABLE-LINE-INPUT+)))
	(when (terminal-mode-raw our-mode)
	  (setf m (logand m (lognot +ENABLE-PROCESSED-INPUT+))))
	;; @@@ set timeout??
	(dbugf :ms "setting console mode ~s ~s~%" in-handle m)
	(when (zerop (%set-console-mode in-handle m))
	  (error 'windows-error :error-code (get-last-error)
		 :format-control "Can't set the console mode.")))))
  tty)

(defun get-terminal-mode (tty)
  "Return a TERMINAL-MODE structure with the current terminal settings."
  (with-slots (in-handle mode) tty
    (with-foreign-object (ms-mode 'DWORD)
      (let ((result (%get-console-mode in-handle ms-mode)))
	(dbugf :ms "get-console-mode = ~s mode = #x~x in-handle = ~s~%"
	       result (mem-ref ms-mode 'DWORD) in-handle)
	(when (zerop result)
	  (error 'windows-error :error-code (get-last-error)
		 :format-control "Can't get the console mode."))
	(let ((m (mem-ref ms-mode 'DWORD)))
	  (setf mode (make-terminal-mode
		      :echo (plusp (logand m +ENABLE-ECHO-INPUT+))
		      :line (plusp (logand m +ENABLE-LINE-INPUT+))
		      :raw (zerop (logand m +ENABLE-PROCESSED-INPUT+))
		      :timeout nil)))))))

(defcstruct SMALL_RECT
  (left   MS-SHORT)
  (top 	  MS-SHORT)
  (right  MS-SHORT)
  (bottom MS-SHORT))

(defcstruct CONSOLE_SCREEN_BUFFER_INFO
  (size                (:struct COORD))
  (cursor-position     (:struct COORD))
  (attributes          WORD)
  (window              (:struct SMALL_RECT))
  (maximum-window-size (:struct COORD)))

(defctype PCONSOLE_SCREEN_BUFFER_INFO (:pointer
				       (:struct CONSOLE_SCREEN_BUFFER_INFO)))

(defcfun ("GetConsoleScreenBufferInfo" get-console-screen-buffer-info)
    BOOL
  (console-output HANDLE)				    ; in 
  (console-screen-buffer-info PCONSOLE_SCREEN_BUFFER_INFO)) ; out

(defun get-window-size (tty)
  "Get the window size. The first value is columns, second value is rows."
  (dbugf :ms "get-window-size tty = ~s~%" tty)
  (with-slots (out-handle width height) tty
    (with-foreign-object (buf '(:struct CONSOLE_SCREEN_BUFFER_INFO))
      (when (zerop (get-console-screen-buffer-info out-handle buf))
	(error 'windows-error :error-code (get-last-error)
	       :format-control "Can't get console screen size."))
      (with-foreign-slots ((window) buf (:struct CONSOLE_SCREEN_BUFFER_INFO))
	;; (with-foreign-slots ((left top right bottom)
	;; 		     ;; (foreign-slot-value
	;; 		     ;;  buf '(:struct CONSOLE_SCREEN_BUFFER_INFO) 'window)
	;; 		     window
	;; 		     (:struct SMALL_RECT))
	(dbugf :ms "window = ~s~%" window)
	(setf width (- (getf window 'right) (getf window 'left))
	      height (- (getf window 'bottom) (getf window 'top)))))
    (values width height)))

(defun reset-terminal-modes (&optional tty)
  "Set the terminal modes to a normal starting state."
  (if (not tty)
      (let ((in-h (%get-std-handle +STD-INPUT-HANDLE+)))
	(dbugf :ms "resetting terminal modes to ~s~%" +NORMAL-INPUT-MODES+)
	(when (zerop (%set-console-mode in-h +NORMAL-INPUT-MODES+))
	  (error 'window-error :error-code (get-last-error)
		 :format-control "Can't set console mode.")
	  ;; @@@ but we don't reset the saved ms-term modes!!
	  ))
      (with-slots (in-handle mode) tty
	(dbugf :ms "resetting terminal modes to ~s~%" +NORMAL-INPUT-MODES+)
	(when (zerop (%set-console-mode in-handle +NORMAL-INPUT-MODES+))
	  (error 'window-error :error-code (get-last-error)
		 :format-control "Can't set console mode."))
	(setf mode (make-terminal-mode :echo t :line t :raw nil :timeout nil))))
  (values))

(defun terminal-query (query &key max)
  "Output the string to the terminal and wait for a response. Read up to MAX
characters. If we don't get anything after a while, just return what we got."
  (declare (ignore query max))
  ;; @@@ XXX
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; EOF
