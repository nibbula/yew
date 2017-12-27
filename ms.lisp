;;
;; ms.lisp - Interface to Microsoft systems.
;;

;; Notes:
;;  - Don't try to apdapt Unix concepts to windows (e.g. signals), or vice
;;    versa. Find a generic concept that works on both systems, or if the
;;    faciliity really doesn't exist, only provide system specific versions.
;;
;;  - Imagine you are creating a hypothetical new operating system that has
;;    the features of both, but is better, and has a better lispy API.
;;    Especially don't be biased by Unix's (or Windows') historical
;;    crumminess. Accept that sometimes, Windows (or Unix, or neither) has the
;;    better way. This really applies to the whole OPSYS system and not just
;;    this package.

;; Conventions:
;;   - Names that conflict should be given the prefix "MS-".
;;   - Type names should try to follow Microsoft style, because they're quite
;;     terse, complicated and could be very confusing otherwise.
;;   - Slot, function, constant, and variable names are much nicer to deal with
;;     when converted to Lisp hyphenated and earmuffed style.
;;   - We generally convert Windows interface function names from StudlyCaps to
;;     %hyphenated-identifier-style, and perhaps provide a function without
;;     the '%' for calling from other Lisp code.

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
   #:lock-file
   #:unlock-file
   #:with-locked-file
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
   ;; Extra Windows specific stuff:
   #:windows-error
   #:get-binary-type #:*binary-types*
   #:binary-type-description
   #:get-command-line
   #:get-computer-name
   ;; Console stuff:
   #:get-console-info
   #:get-window-size
   #:get-cursor-position
   #:get-cursor-info
   #:set-cursor-state
   #:set-cursor-position
   #:scroll-console
   #:fill-console-char
   #:fill-console-attribute
   #:get-attributes
   #:set-console-attribute
   #:+FOREGROUND-BLUE+ #:+FOREGROUND-GREEN+ #:+FOREGROUND-RED+
   #:+FOREGROUND-INTENSITY+ #:+BACKGROUND-BLUE+ #:+BACKGROUND-GREEN+
   #:+BACKGROUND-RED+ #:+BACKGROUND-INTENSITY+
   ))
(in-package :ms)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

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
;; Constants widely used

(defconstant +MAX-PATH+ 260)

(defconstant +ERROR-FILE-NOT-FOUND+ 2)
(defconstant +ERROR-PATH-NOT-FOUND+ 3)
(defconstant +ERROR-ENVVAR-NOT-FOUND+ 203)

(defconstant +GENERIC-READ+  #x80000000)
(defconstant +GENERIC-WRITE+ #x40000000)

;; a.k.a: ((HANDLE)~(ULONG_PTR)0) or the maximum pointer value.
(defconstant +INVALID-HANDLE-VALUE+
  #+ms-win64 (1- (expt 2 64))
  #-ms-win64 (1- (expt 2 32)))

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

(defctype wchar-t :uint16)
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
  (defctype TCHAR WCHAR)
  (defctype LPTSTR LPWSTR)
  (defctype LPCTSTR LPCWSTR))
#-ms-unicode
(progn
  (defctype TCHAR MS-CHAR)
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

;; Widely used structs.

(defcstruct FILETIME
  "100-nanosecond intervals since January 1, 1601 (UTC)."
  (low-date-time DWORD)
  (high-date-time DWORD))
(defctype PFILETIME (:pointer (:struct FILETIME)))
(defctype LPFILETIME (:pointer (:struct FILETIME)))

(defcstruct foreign-offset
  (offset-low DWORD)
  (offset-high DWORD))

(defcunion foreign-offset-pointer
  (offset (:struct foreign-offset))
  (pointer PVOID))

(defcstruct OVERLAPPED
  (internal ULONG_PTR)
  (internal-high ULONG_PTR)
  (offset-pointer (:union foreign-offset-pointer))
  (event HANDLE))
(defctype LPOVERLAPPED (:pointer (:struct OVERLAPPED)))

(defcstruct SECURITY_ATTRIBUTES
  (length DWORD)
  (security-descriptor LPVOID)
  (inherit-handle BOOL))

(defctype PSECURITY_ATTRIBUTES (:pointer (:struct SECURITY_ATTRIBUTES)))
(defctype LPSECURITY_ATTRIBUTES (:pointer (:struct SECURITY_ATTRIBUTES)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

;;(declaim (inline wchar-to-character))
(defun wchar-to-character (c)
  (if (and (> c #xd7ff) (< c #xe000))
      (error "I didn't write UTF-16 conversion yet! c=#x~x" c)
      (code-char c)))

(defun character-to-wchar (c)
  (let ((cc (char-code c)))
    (if (or (> cc #xffff)
	    (and (> cc #xd7ff) (< cc #xe000)))
	(error "I didn't write UTF-16 conversion yet! c=#x~x" cc)
	cc)))

;;(declaim (inline set-wchar))
(defun set-wchar (wchar-mem i character)
  (setf (mem-aref wchar-mem 'WCHAR i)
	(character-to-wchar character)))

(defun wide-string-to-lisp (wide-string &optional n)
  "Convert a Windows wide string (LPTSTR or LPWSTR) to a Lisp string.
If N isn't given, assume WIDE-STRING is terminated by a zero character."
  ;; @@@ XXX This is totally wrong. We need to do UTF-16 un-conversino.
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
  (with-unique-names (i)
    `(with-foreign-object (,var 'WCHAR (1+ (length ,string)))
       (let ((,i 0))
	 ;; @@@ XXX This is totally wrong. We need to do UTF-16 conversino.
	 (loop :while (< ,i (length ,string))
	    :do
	    (set-wchar ,var ,i (char ,string ,i))
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

;; There's many more of these, but I suppose this suffices for the current?
(defconstant LANG_NEUTRAL #x00)
(defconstant SUBLANG_DEFAULT #x01)

;; This is a macro in C code.
(defun MAKELANGID (primary sublang)
  (logior (ash (logand #xffff sublang) 10)
	  (logand #xffff primary)))

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

(defcfun ("FormatMessageW" %format-message :convention :stdcall)
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
  (let (result)
    (with-foreign-object (message '(:pointer LPTSTR))
      (unwind-protect
	   (let ((bytes-stored
		  (%format-message
		   (logior +FORMAT-MESSAGE-FROM-SYSTEM+
			   +FORMAT-MESSAGE-ALLOCATE-BUFFER+
			   +FORMAT-MESSAGE-IGNORE-INSERTS+)
		   (null-pointer)
		   error-code
		   (MAKELANGID LANG_NEUTRAL SUBLANG_DEFAULT)
		   message
		   0
		   (null-pointer))))
	     (when (zerop bytes-stored)
	       (error "FormatMessage failed: ~s ~s ~s"
		      bytes-stored  error-code (get-last-error)))
	     (setf result
		   (wide-string-to-lisp (mem-ref message 'LPTSTR))))
	(when (not (null-pointer-p message))
	  (dork-free (mem-ref message 'LPTSTR)))))))

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

(defmacro syscall ((func &rest args))
  "Call a system function that returns BOOL false on failure and signal a
windows-error with an appropriate error message if it fails."
  `(error-check (,func ,@args)
		,(concatenate 'string (string-downcase func) ":")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environmental

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

;; No such thing. Or is there?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Login/accounting database

(defun users-logged-in ()
  "Return a list of names of logged in users."
  "dan")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files

(defconstant GetFileExInfoStandard 0 "Indicates a WIN32_FILE_ATTRIBUTE_DATA.")
(defctype GET_FILEEX_INFO_LEVELS :int32) ; XXX whatever

(defcstruct WIN32_FILE_ATTRIBUTE_DATA
  (file-attributes DWORD)
  (creation-time (:struct FILETIME))
  (last-access-time (:struct FILETIME))
  (last-write-time (:struct FILETIME))
  (file-size-high DWORD)
  (file-size-low DWORD))

(defctype LPWIN32_FILE_ATTRIBUTE_DATA
    (:pointer (:struct WIN32_FILE_ATTRIBUTE_DATA)))

(defcfun ("GetFileAttributesExW" %get-file-attributes-ex)
    BOOL
  (file-name LPCTSTR)
  (info-level-id GET_FILEEX_INFO_LEVELS)
  (file-information LPVOID))

(defconstant +windows-to-universal-time+
  9435484800 ;; Calculated by comparing the actual times.
  ;; 9435456000 ;; Calculated by comparing the actual times. Off by TZ!?!
  ;; @@@ This can't be right? Leap years? etc.
  ;; (* (- 1900 1601) (* 60 60 24 (+ 365 1/4)))
  "Value to subtract from a 1601 based Windows time in seconds, to get a
Common Lisp 1900 based universal time.")

(defun filetime-to-universal-time (filetime)
  "Convert from a (:struct FILETIME) to a CL universal-time."
  ;; FILETIME is in 100-nanosecond intervals since January 1, 1601 (UTC).
  (let* ((low-date-time (getf filetime 'low-date-time))
	 (high-date-time (getf filetime 'high-date-time))
	 (100-nsec (logior (ash high-date-time 32) low-date-time))
	 (sec (truncate 100-nsec (expt 10 7))))
    (- sec +windows-to-universal-time+)))

(defun filetime-to-universal-time-and-nsec (filetime)
  "Convert from a (:struct FILETIME) to a CL universal-time and nanosecods."
  ;; FILETIME is in 100-nanosecond intervals since January 1, 1601 (UTC).
  (let* ((low-date-time (getf filetime 'low-date-time))
	 (high-date-time (getf filetime 'high-date-time))
	 (100-nsec (logior (ash high-date-time 32) low-date-time)))
    (multiple-value-bind (new-sec new-100nsec)
	(truncate 100-nsec (expt 10 7))
      (values (- new-sec +windows-to-universal-time+)
	      (* new-100nsec 100)))))

(defun filetime-to-derp-time (filetime)
  (multiple-value-bind (sec nano)
      (filetime-to-universal-time-and-nsec filetime)
    (make-derp-time :seconds sec :nanoseconds nano)))

 ;; :immutable :compressed :hidden
(defun attr-to-flags (attr)
  (let (flags)
    (when (plusp (logand attr +FILE-ATTRIBUTE-READONLY+))
      (push :immutable flags))
    (when (plusp (logand attr +FILE-ATTRIBUTE-HIDDEN+))
      (push :hidden flags))
    (when (plusp (logand attr +FILE-ATTRIBUTE-COMPRESSED+))
      (push :compressed flags))
    ;; @@@ What about the others? Or are we just doing least common denominator?
    ;; These seem like the could be important or something.
    (when (plusp (logand attr +FILE-ATTRIBUTE-SYSTEM+))
      (push :system flags))
    (when (plusp (logand attr +FILE-ATTRIBUTE-ENCRYPTED+))
      (push :encrypted flags))
    (when (plusp (logand attr +FILE-ATTRIBUTE-ARCHIVE+))
      (push :archive flags))))

(defun get-file-info (path &key (follow-links t))
  "Return information about the file described by PATH in a FILE-INFO
structure. If FOLLOW-LINKS is true (the default), then if PATH is a symbolic
link, return information about the file it's linked to, otherwise return
information about the link itself."
  (declare (ignore follow-links)) ;; @@@
  (with-wide-string (w-path path)
    (with-foreign-object (info '(:struct WIN32_FILE_ATTRIBUTE_DATA))
      (syscall (%get-file-attributes-ex w-path GetFileExInfoStandard info))
      (with-foreign-slots ((file-attributes creation-time last-access-time
			    last-write-time file-size-low file-size-high)
			   info (:struct WIN32_FILE_ATTRIBUTE_DATA))
	(make-file-info
	 :type (attr-to-dir-entry-type file-attributes)
	 :size (+ (ash file-size-high 32) file-size-low)
	 :flags (attr-to-flags file-attributes)
	 :creation-time (filetime-to-derp-time creation-time)
	 :access-time (filetime-to-derp-time last-access-time)
	 :modification-time (filetime-to-derp-time last-write-time))))))

(defcfun ("GetFileAttributesW" %get-file-attributes)
    DWORD
  (file-name LPCTSTR))

(defconstant +INVALID-FILE-ATTRIBUTES+ #xffffffff)

(defun file-exists (filename)
  "Check that a file with FILENAME exists at the moment. But it might not exist
for long."
  (with-wide-string (w-file filename)
    ;; I'm really just guessing with whole thing. For example, are there any
    ;; other errors which would constitute being not found?
    (let ((result (%get-file-attributes w-file)))
      (if (= result +INVALID-FILE-ATTRIBUTES+)
	  (let ((err (get-last-error)))
	    (if (or (= err +ERROR-FILE-NOT-FOUND+)
		    (= err +ERROR-PATH-NOT-FOUND+))
		nil
		(error 'windows-error :error-code err
		       :format-control "file-exists failed.")))
	  t))))

(defcfun ("DeleteFileW" %delete-file)
    BOOL
  (file-name LPCTSTR))

(defun simple-delete-file (pathname)
  "Delete a file. Doesn't monkey with the name, which should be a string.
Doesn't operate on streams."
  (with-wide-string (w-path pathname)
    (syscall (%delete-file w-path))))

#|
(defmacro with-windows-file ((var filename access &optional
				  share-mode
				  (security)
				  (flags))
			     &body body)
  "Evaluate the body with the variable VAR bound to a posix file descriptor
opened on FILENAME with FLAGS and MODE."
  (when (not share-mode)
    (setf share-mode (logior +FILE-SHARE-READ+ +FILE-SHARE-WRITE+
  `(let (,var)
     (unwind-protect
	  (progn
	 (with-wide-string (,w-filename ,filename)
	  (setf ,var (%create-file ,w-filename ,access
				   ,share-mode ,security
				   DISPOSITION
				   ,flags
				   TEMPLATE
				   ))
	 ,@body)
       (if (>= ,var 0)
	   (%close-handle ,var)
	   (error-check ,var)))))
|#

;; @@@ not done yet
(defmacro with-os-file ((var filename &key
			     (direction :input)
			     (if-exists :error)
			     (if-does-not-exist :error)) &body body)
  "Evaluate the body with the variable VAR bound to a Windows file handle
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

(defparameter *file-attributes* nil "FILE_ATTRIBUTE_* constants")
(define-to-list *file-attributes*
 #(#(+FILE-ATTRIBUTE-READONLY+              #x00000001)
   #(+FILE-ATTRIBUTE-HIDDEN+                #x00000002)
   #(+FILE-ATTRIBUTE-SYSTEM+                #x00000004)
   #(+FILE-ATTRIBUTE-DIRECTORY+             #x00000010)
   #(+FILE-ATTRIBUTE-ARCHIVE+               #x00000020)
   #(+FILE-ATTRIBUTE-DEVICE+                #x00000040)
   #(+FILE-ATTRIBUTE-NORMAL+                #x00000080)
   #(+FILE-ATTRIBUTE-TEMPORARY+             #x00000100)
   #(+FILE-ATTRIBUTE-SPARSE-FILE+           #x00000200)
   #(+FILE-ATTRIBUTE-REPARSE-POINT+         #x00000400 "symbolic link?")
   #(+FILE-ATTRIBUTE-COMPRESSED+            #x00000800)
   #(+FILE-ATTRIBUTE-OFFLINE+               #x00001000)
   #(+FILE-ATTRIBUTE-NOT-CONTENT-INDEXED+   #x00002000)
   #(+FILE-ATTRIBUTE-ENCRYPTED+             #x00004000)
   #(+FILE-ATTRIBUTE-INTEGRITY-STREAM+      #x00008000)
   #(+FILE-ATTRIBUTE-VIRTUAL+               #x00010000)
   #(+FILE-ATTRIBUTE-NO-SCRUB-DATA+         #x00020000)
   #(+FILE-ATTRIBUTE-RECALL-ON-OPEN+        #x00040000)
   #(+FILE-ATTRIBUTE-RECALL-ON-DATA-ACCESS+ #x00400000)))


(defcstruct WIN32_FIND_DATA
  (file-attributes DWORD)
  (creation-time (:struct FILETIME))
  (last-access-time (:struct FILETIME))
  (last-write-time (:struct FILETIME))
  (file-size-high DWORD)
  (file-size-low DWORD)
  (reserved0 DWORD)
  (reserved1 DWORD)
  (file-name TCHAR :count #.+MAX-PATH+)
  (alternate-file-name TCHAR :count 14))

(defctype PWIN32_FIND_DATA (:pointer (:struct WIN32_FIND_DATA)))
(defctype LPWIN32_FIND_DATA (:pointer (:struct WIN32_FIND_DATA)))

(defcfun ("FindFirstFileW" %find-first-file)
    HANDLE
  (file-name LPCTSTR)
  (find-file-data LPWIN32_FIND_DATA))

(defconstant +ERROR-NO-MORE-FILES+ 18)

(defcfun ("FindNextFileW" %find-next-file)
    BOOL
  (find-file HANDLE)
  (find-file-data LPWIN32_FIND_DATA))

(defcfun ("FindClose" %find-close)
    BOOL
  (find-file HANDLE))

;; @@@ If we wanted to be more complete we could open the file and call
;; GetFileType, but I imagine it would slow things quite a bit.
(defun attr-to-dir-entry-type (attr)
  "Return a dir-entry-type value given a file-attribute value."
  (cond
    ((plusp (logand attr +FILE-ATTRIBUTE-DIRECTORY+))     :directory)
    ((or (= attr +FILE-ATTRIBUTE-NORMAL+)
	 ;; It doesn't have any other flags than these:
	 (zerop (logand attr (lognot (logior +FILE-ATTRIBUTE-ARCHIVE+
					     +FILE-ATTRIBUTE-HIDDEN+
					     +FILE-ATTRIBUTE-READONLY+)))))
     :regular)
    ((plusp (logand attr +FILE-ATTRIBUTE-DEVICE+))        :device)
    ((plusp (logand attr +FILE-ATTRIBUTE-REPARSE-POINT+)) :link)
    (t :other)))

;; @@@ Perhaps we should use FindFirstFileExW on Windows 7 and above since it's
;; supposedly faster.

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
  (declare (ignore append-type))
  (when (not dir)
    (setf dir "."))			; XXX ???

  (setf dir (s+ dir "\\*"))
  (let (handle dir-list)
    (unwind-protect
      (with-foreign-object (find-data '(:struct WIN32_FIND_DATA))
	(with-wide-string (w-dir dir)
	  (setf handle (%find-first-file w-dir find-data))
	  (when (= (pointer-address handle) +INVALID-HANDLE-VALUE+)
	    (error 'windows-error :error-code (get-last-error)
		   :format-control
		   "read-directory failed to read first file.")))
	(setf dir-list
	      (loop
		 :with entry :and name
		 :do
		 (setf entry nil)
		 (with-foreign-slots ((file-name file-attributes) find-data
				      (:struct WIN32_FIND_DATA))
		   (setf name (wide-string-to-lisp file-name))
		   (when (or (not omit-hidden)
			     (and (zerop (logand file-attributes
						 +FILE-ATTRIBUTE-HIDDEN+))
				  (not (hidden-file-name-p name))))
		     (setf entry
			   (if full
			       (make-dir-entry
				:name name
				:type (attr-to-dir-entry-type file-attributes)
				:inode nil)
			       name))))
		 :when entry
		 :collect entry
		 :while (not (zerop (%find-next-file handle find-data)))))
	(when (/= (get-last-error) +ERROR-NO-MORE-FILES+)
	  (error 'windows-error :error-code (get-last-error)
		 :format-control "read-directory failed to read next file.")))
      (when (and handle (/= (pointer-address handle) +INVALID-HANDLE-VALUE+))
	(syscall (%find-close handle))))
  dir-list))

(defcfun ("SetCurrentDirectoryW" %set-current-directory)
    BOOL
  (path-name LPCTSTR))

;; @@@ This isn't OS specific, but implementation specific, so it should be
;; moved to base.lisp or opsys.lisp or something.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Set on implementations where we need to *default-pathname-defaults*
  ;; when we change the system current directory, so that load, open, etc.
  ;; will work as expected.
  #+(or sbcl excl abcl) (config-feature :t-os-cd-dpd))

(defun change-directory (&optional (path (user-homedir-pathname)))
  "Change the current directory to DIR. Defaults to (user-homedir-pathname) ~
if not given."
  ;; @@@ We should put a thread lock around this if we want it to be thread safe.
  (let ((our-path
	 (typecase path
	   (pathname (safe-namestring path))
	   (string path))))
    (when (char/= #\\ (aref our-path (1- (length our-path))))
      (setf our-path (s+ our-path "\\")))
    (with-wide-string (w-path our-path)
      (syscall (%set-current-directory w-path))
      #+t-os-cd-dpd
      (let ((tn (ignore-errors (truename path))))
	(when tn
	  (setf *default-pathname-defaults* tn))))))

(defcfun ("GetCurrentDirectoryW" %get-current-directory)
    DWORD
  (buffer-length DWORD)
  (buffer LPTSTR))

(defun current-directory ()
  "Return the full path of the current working directory as a string."
  (let ((len (%get-current-directory 0 (null-pointer)))
	result)
    (with-foreign-object (dir 'TCHAR len)
      (setf result (%get-current-directory len dir))
      (when (/= result (1- len))
	(error 'windows-error :error-code (get-last-error)
	       :format-control "Failed to get the current directory."))
      (wide-string-to-lisp dir))))

(defcfun ("CreateDirectoryW" %create-directory)
    BOOL
  (path-name LPCTSTR)
  (security-attributes LPSECURITY_ATTRIBUTES))

(defun make-directory (path &key (mode #o755))
  "Make a directory."
  (declare (ignore mode))
  (with-wide-string (w-path path)
    (syscall (%create-directory w-path (null-pointer)))))

(defcfun ("RemoveDirectoryW" %remove-directory)
    BOOL
  (path-name LPCTSTR))

(defun delete-directory (path)
  "Delete a directory."
  (with-wide-string (w-path path)
    (syscall (%remove-directory w-path))))

;; This has similar issues as file-exists.
(defun probe-directory (dir)
  "Something like probe-file but for directories."
  (with-wide-string (w-file dir)
    (let ((result (%get-file-attributes w-file)))
      (if (= result +INVALID-FILE-ATTRIBUTES+)
	  (let ((err (get-last-error)))
	    (if (or (= err +ERROR-FILE-NOT-FOUND+)
		    (= err +ERROR-PATH-NOT-FOUND+))
		nil
		(error 'windows-error :error-code err
		       :format-control "file-exists failed.")))
	  (if (plusp (logand result +FILE-ATTRIBUTE-DIRECTORY+))
	      t
	      nil)))))

(defmacro without-access-errors (&body body)
  "Evaluate the body while ignoring typical file access error from system
calls. Returns NIL when there is an error."
  `(ignore-errors ,@body))

;; Since this and the following are SO FAR the same as on POSIX, perhaps we
;; should move them to opsys.lisp?

(defun hidden-file-name-p (name)
  "Return true if the file NAME is normally hidden."
  (and name (> (length name) 0) (equal (char name 0) #\.)))

(defun superfluous-file-name-p (name)
  "Return true if the file NAME is considered redundant. On POSIX file
systems, this means \".\" and \"..\"."
  (and name (> (length name) 0)
       (or (and (= (length name) 1)
		(equal (char name 0) #\.))
	   (and (= (length name) 2)
		(equal (char name 0) #\.)
		(equal (char name 1) #\.)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file locking

(defun set-overlapped (overlapped offset-low-in offset-high-in event-handle)
  (with-foreign-slots ((offset-pointer event) overlapped (:struct OVERLAPPED))
    (with-foreign-slots ((offset) offset-pointer
			 (:union foreign-offset-pointer))
      (with-foreign-slots ((offset-low offset-high) offset
			   (:struct foreign-offset))
	(setf offset-low offset-low-in
	      offset-high offset-high-in)))
    (setf event event-handle)))

(defconstant +LOCKFILE-EXCLUSIVE-LOCK+ #x00000002
  "Request an exclusive lock. Otherwise a shared lock is requested.")
(defconstant +LOCKFILE-FAIL-IMMEDIATELY+ #x00000001
  "The function returns immediately if it is unable to acquire the requested
lock. Otherwise, it waits.")

(defcfun ("LockFileEx" %lock-file-ex)
    BOOL
  (file HANDLE)
  (flags DWORD)
  (reserved DWORD)
  (number-of-bytes-to-lock-low DWORD)
  (number-of-bytes-to-lock-high DWORD)
  (overlapped LPOVERLAPPED))

(defcfun ("UnlockFileEx" %unlock-file-ex)
    BOOL
  (file HANDLE)
  (reserved DWORD)
  (number-of-bytes-to-unlock-low DWORD)
  (number-of-bytes-to-unlock-high DWORD)
  (overlapped LPOVERLAPPED))

(defcfun ("CreateFileW" %create-file)
    HANDLE
  (file-name 		 LPCTSTR)
  (desired-access 	 DWORD)
  (share-mode 		 DWORD)
  (security-attributes   LPSECURITY_ATTRIBUTES)
  (creation-disposition  DWORD)
  (flags-and-attributes  DWORD)
  (template-file 	 HANDLE))

(defconstant +FILE-SHARE-READ+  #x00000001)
(defconstant +FILE-SHARE-WRITE+ #x00000002)

(defconstant +OPEN-EXISTING+ 3)

(defcfun ("CloseHandle" %close-handle)
    BOOL
   (object HANDLE))

(defmacro with-locked-file ((pathname &key (lock-type :write) (timeout 3)
				      (increment .1))
			    &body body)
  "Evaluate BODY with PATHNAME locked. Only wait for TIMEOUT seconds to get a
lock, checking at least every INCREMNT seconds."
  (declare (ignore timeout increment))
  (with-unique-names (locked flags handle overlapped the-lock-type w-path)
    `(let ((,locked nil) (,handle nil) (,the-lock-type ,lock-type))
       (unwind-protect
         (let ((,flags (logior (ecase ,the-lock-type
				 (:write 0)
				 (:read +LOCKFILE-EXCLUSIVE-LOCK+))
			       +LOCKFILE-FAIL-IMMEDIATELY+)))
	   (with-wide-string (,w-path ,pathname)
	     (setf ,handle (%create-file
			    ,w-path
			    (logior +GENERIC-READ+ +GENERIC-WRITE+)
			    (if (eq ,the-lock-type :write)
				+FILE-SHARE-READ+
				0)
			    +OPEN-EXISTING+
			    0
			    (null-pointer))))
	   (with-foreign-object (,overlapped '(:struct OVERLAPPED))
	     (set-overlapped ,overlapped 0 0 0)
	     ;; We lock the maximum number of bytes to effectively lock the
	     ;; whole file.
	     (syscall (%lock-file-ex ,handle ,flags #xffffffff #xffffffff
				     ,overlapped)))
	   (setf ,locked t)
	   ,@body)
	 (when ,locked
	   (syscall (%unlock-file-ex ,handle #xffffffff #xffffffff
				     ,overlapped)))
	 (when ,handle
	   (syscall %close-handle ,handle))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System Commands?

(defcfun ("GetBinaryTypeW" %get-binary-type)
    BOOL
  (application-name LPCTSTR)
  (binary-type LPDWORD))

(defconstant +SCS_32BIT_BINARY+ 0 "A 32-bit Windows application")
(defconstant +SCS-DOS-BINARY+   1 "An MS-DOS application.")
(defconstant +SCS-WOW-BINARY+   2 "A 16-bit Windows application.")
(defconstant +SCS-PIF-BINARY    3 "A PIF file that executes an MS-DOS application.")
(defconstant +SCS-POSIX-BINARY+ 4 "A POSIX application.")
(defconstant +SCS-OS216-BINARY  5 "A 16-bit OS/2 application")
(defconstant +SCS_64BIT_BINARY+ 6 "A 64-bit Windows application.")

;; This relys on the positional nature of the above constants.
(defparameter *binary-types*
  #(#(:32BIT "A 32-bit Windows application")
    #(:DOS   "An MS-DOS application.")
    #(:WOW   "A 16-bit Windows application.")
    #(:PIF   "A PIF file that executes an MS-DOS application.")
    #(:POSIX "A POSIX application.")
    #(:OS216 "A 16-bit OS/2 application")
    #(:64BIT "A 64-bit Windows application.")))

(defun get-binary-type (pathname)
  "Return the executable binary for PATHNAME. The return value is one of the
keywords from *BINARY-TYPE*."
  (with-wide-string (w-path pathname)
    (with-foreign-object (binary-type 'DWORD)
      (syscall (%get-binary-type w-path binary-type))
      (aref (aref *binary-types* (mem-ref binary-type 'DWORD)) 0))))

(defun binary-type-description (binary-type)
  "Return the description of BINARY-TYPE, as returned by GET-BINARY-TYPE."
  (aref (find binary-type *binary-types* :key (_ (aref _ 0))) 1))

(defun is-executable (path &key user regular)
  "Return true if the PATH is executable by the USER. USER defaults to the
current effective user. If REGULAR is true also check if it's a regular file."
  (declare (ignore path user regular))
  ;; @@@
  ;; I think checking the for the "Read & execute" permission is not really
  ;; what we mean here. But neither is get-binary-type, since that won't
  ;; capture things like *.bat files.
  ;; We really mean something that will ‘work’ with %create-process.
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

(defcfun ("GetCommandLineW" %get-command-line) LPTSTR)

(defun get-command-line ()
  (wide-string-to-lisp (%get-command-line)))

(defun process-list ()
  "Return a list of OS-PROCESS structures that represent the processes active
around the time of the call."
  nil)

(defcfun ("WaitForMultipleObjects" %wait-for-multiple-objects)
    DWORD
  (count DWORD)
  (handles (:pointer HANDLE))
  (wait-all BOOL)
  (milliseconds DWORD))

(defun wait-and-chill (child-pid)
  "Wait for jobs to do something.")

(defun check-jobs (&optional hang)
  "Check if any sub-processes have changed status. Returns three values.
The PID of the process that changed, and the RESULT and STATUS as returned by
wait. Returns NILs if nothing changed."
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timers / Timing

(defcstruct SYSTEMTIME
  (year WORD)
  (month WORD)
  (day-of-week WORD)
  (day WORD)
  (hour WORD)
  (minute WORD)
  (second WORD)
  (milliseconds WORD))
(defctype PSYSTEMTIME (:pointer (:struct SYSTEMTIME)))
(defctype LPSYSTEMTIME (:pointer (:struct SYSTEMTIME)))

(defcfun ("GetSystemTimeAsFileTime" %get-system-time-as-file-time)
    :void
  (system-time-as-file-time LPFILETIME))

(defcfun ("GetSystemTime" %get-system-time)
    :void 
  (system-time LPSYSTEMTIME))

(defcfun ("SetSystemTime" %set-system-time)
    BOOL
  (system-time (:pointer (:struct SYSTEMTIME))))

(defcfun ("SystemTimeToFileTime" %system-time-to-file-time)
    BOOL
  (system-time (:pointer (:struct SYSTEMTIME)))
  (file-time LPFILETIME))

(defcfun ("FileTimeToLocalFileTime" %file-time-to-local-file-time)
    BOOL
  (file-time (:pointer (:struct FILETIME)))
  (local-file-time LPFILETIME))

(defcfun ("GetLocalTime" %get-local-time)
    :void
  (system-time LPSYSTEMTIME))

(defcfun ("SetLocalTime" %set-local-time)
    BOOL
  (system-time (:pointer (:struct SYSTEMTIME))))

(defun get-time ()
  "Return the time in seconds and nanoseconds. The first value is seconds in
so-called “universal” time. The second value is nanoseconds."
  (with-foreign-objects ((sys-time '(:struct SYSTEMTIME))
			 (time '(:struct FILETIME)))
    (%get-local-time sys-time)
    (%system-time-to-file-time sys-time time)
    (filetime-to-universal-time-and-nsec time)))

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
  height
  not-console
  read-ahead)

(defmacro as-32bit-unsigned (n) `(logand (1- (expt 2 32)) (lognot (1- (- ,n)))))
(defconstant +STD-INPUT-HANDLE+  (as-32bit-unsigned -10)) ; CONIN$
(defconstant +STD-OUTPUT-HANDLE+ (as-32bit-unsigned -11)) ; CONOUT$
(defconstant +STD-ERROR-HANDLE+  (as-32bit-unsigned -12)) ; CONOUT$

(defcfun ("GetStdHandle" %get-std-handle :convention :stdcall)
    HANDLE
   (nStdHandle DWORD)) ; in

(defcstruct (COORD :class foreign-coord)
  (x MS-SHORT)
  (y MS-SHORT))

;; Shouldn't the be the default??!?!!!
(defmethod translate-into-foreign-memory (object (type foreign-coord) pointer)
  (with-foreign-slots ((x y) pointer (:struct COORD))
    (setf x (getf object 'x)
	  y (getf object 'y))))

(defun set-coord (coord o1 o2)
  (with-foreign-slots ((x y) coord (:struct COORD))
    (setf x o1 y o2)))

(defctype PCOORD (:pointer (:struct COORD)))

(defcunion foreign-uchar
  (unicode-char WCHAR)
  (ascii-char MS-CHAR))

(defparameter *keys* nil "List of Windows “Virtual” keys.")

(define-to-list *keys*
  #(#(+VK-LBUTTON+			#x01)
    #(+VK-RBUTTON+			#x02)
    #(+VK-CANCEL+			#x03)
    #(+VK-MBUTTON+			#x04)
    #(+VK-XBUTTON1+			#x05)
    #(+VK-XBUTTON2+			#x06)
    #(+VK-BACK+				#x08)
    #(+VK-TAB+				#x09)
    #(+VK-CLEAR+			#x0C)
    #(+VK-RETURN+			#x0D)
    #(+VK-SHIFT+			#x10)
    #(+VK-CONTROL+			#x11)
    #(+VK-MENU+				#x12)
    #(+VK-PAUSE+			#x13)
    #(+VK-CAPITAL+			#x14)
    #(+VK-KANA+				#x15)
    #(+VK-JUNJA+			#x17)
    #(+VK-FINAL+			#x18)
    #(+VK-HANJA+			#x19)
    #(+VK-ESCAPE+			#x1B)
    #(+VK-CONVERT+			#x1C)
    #(+VK-NONCONVERT+			#x1D)
    #(+VK-ACCEPT+			#x1E)
    #(+VK-MODECHANGE+			#x1F)
    #(+VK-SPACE+			#x20)
    #(+VK-PRIOR+			#x21)
    #(+VK-NEXT+				#x22)
    #(+VK-END+				#x23)
    #(+VK-HOME+				#x24)
    #(+VK-LEFT+				#x25)
    #(+VK-UP+				#x26)
    #(+VK-RIGHT+			#x27)
    #(+VK-DOWN+				#x28)
    #(+VK-SELECT+			#x29)
    #(+VK-PRINT+			#x2A)
    #(+VK-EXECUTE+			#x2B)
    #(+VK-SNAPSHOT+			#x2C)
    #(+VK-INSERT+			#x2D)
    #(+VK-DELETE+			#x2E)
    #(+VK-HELP+				#x2F)
    #(+VK-LWIN+				#x5B)
    #(+VK-RWIN+				#x5C)
    #(+VK-APPS+				#x5D)
    #(+VK-SLEEP+			#x5F)
    #(+VK-NUMPAD0+			#x60)
    #(+VK-NUMPAD1+			#x61)
    #(+VK-NUMPAD2+			#x62)
    #(+VK-NUMPAD3+			#x63)
    #(+VK-NUMPAD4+			#x64)
    #(+VK-NUMPAD5+			#x65)
    #(+VK-NUMPAD6+			#x66)
    #(+VK-NUMPAD7+			#x67)
    #(+VK-NUMPAD8+			#x68)
    #(+VK-NUMPAD9+			#x69)
    #(+VK-MULTIPLY+			#x6A)
    #(+VK-ADD+				#x6B)
    #(+VK-SEPARATOR+			#x6C)
    #(+VK-SUBTRACT+			#x6D)
    #(+VK-DECIMAL+			#x6E)
    #(+VK-DIVIDE+			#x6F)
    #(+VK-F1+				#x70)
    #(+VK-F2+				#x71)
    #(+VK-F3+				#x72)
    #(+VK-F4+				#x73)
    #(+VK-F5+				#x74)
    #(+VK-F6+				#x75)
    #(+VK-F7+				#x76)
    #(+VK-F8+				#x77)
    #(+VK-F9+				#x78)
    #(+VK-F10+				#x79)
    #(+VK-F11+				#x7A)
    #(+VK-F12+				#x7B)
    #(+VK-F13+				#x7C)
    #(+VK-F14+				#x7D)
    #(+VK-F15+				#x7E)
    #(+VK-F16+				#x7F)
    #(+VK-F17+				#x80)
    #(+VK-F18+				#x81)
    #(+VK-F19+				#x82)
    #(+VK-F20+				#x83)
    #(+VK-F21+				#x84)
    #(+VK-F22+				#x85)
    #(+VK-F23+				#x86)
    #(+VK-F24+				#x87)
    #(+VK-NUMLOCK+			#x90)
    #(+VK-SCROLL+			#x91)
    #(+VK-OEM-NEC-EQUAL+		#x92)
    #(+VK-OEM-FJ-JISHO+			#x92)
    #(+VK-OEM-FJ-MASSHOU+		#x93)
    #(+VK-OEM-FJ-TOUROKU+		#x94)
    #(+VK-OEM-FJ-LOYA+			#x95)
    #(+VK-OEM-FJ-ROYA+			#x96)
    #(+VK-LSHIFT+			#xA0)
    #(+VK-RSHIFT+			#xA1)
    #(+VK-LCONTROL+			#xA2)
    #(+VK-RCONTROL+			#xA3)
    #(+VK-LMENU+			#xA4)
    #(+VK-RMENU+			#xA5)
    #(+VK-BROWSER-BACK+			#xA6)
    #(+VK-BROWSER-FORWARD+		#xA7)
    #(+VK-BROWSER-REFRESH+		#xA8)
    #(+VK-BROWSER-STOP+			#xA9)
    #(+VK-BROWSER-SEARCH+		#xAA)
    #(+VK-BROWSER-FAVORITES+		#xAB)
    #(+VK-BROWSER-HOME+			#xAC)
    #(+VK-VOLUME-MUTE+			#xAD)
    #(+VK-VOLUME-DOWN+			#xAE)
    #(+VK-VOLUME-UP+			#xAF)
    #(+VK-MEDIA-NEXT-TRACK+		#xB0)
    #(+VK-MEDIA-PREV-TRACK+		#xB1)
    #(+VK-MEDIA-STOP+			#xB2)
    #(+VK-MEDIA-PLAY-PAUSE+		#xB3)
    #(+VK-LAUNCH-MAIL+			#xB4)
    #(+VK-LAUNCH-MEDIA-SELECT+		#xB5)
    #(+VK-LAUNCH-APP1+			#xB6)
    #(+VK-LAUNCH-APP2+			#xB7)
    #(+VK-OEM-1+			#xBA)
    #(+VK-OEM-PLUS+			#xBB)
    #(+VK-OEM-COMMA+			#xBC)
    #(+VK-OEM-MINUS+			#xBD)
    #(+VK-OEM-PERIOD+			#xBE)
    #(+VK-OEM-2+			#xBF)
    #(+VK-OEM-3+			#xC0)
    #(+VK-OEM-4+			#xDB)
    #(+VK-OEM-5+			#xDC)
    #(+VK-OEM-6+			#xDD)
    #(+VK-OEM-7+			#xDE)
    #(+VK-OEM-8+			#xDF)
    #(+VK-OEM-AX+			#xE1)
    #(+VK-OEM-102+			#xE2)
    #(+VK-ICO-HELP+			#xE3)
    #(+VK-ICO-00+			#xE4)
    #(+VK-PROCESSKEY+			#xE5)
    #(+VK-ICO-CLEAR+			#xE6)
    #(+VK-PACKET+			#xE7)
    #(+VK-OEM-RESET+			#xE9)
    #(+VK-OEM-JUMP+			#xEA)
    #(+VK-OEM-PA1+			#xEB)
    #(+VK-OEM-PA2+			#xEC)
    #(+VK-OEM-PA3+			#xED)
    #(+VK-OEM-WSCTRL+			#xEE)
    #(+VK-OEM-CUSEL+			#xEF)
    #(+VK-OEM-ATTN+			#xF0)
    #(+VK-OEM-FINISH+			#xF1)
    #(+VK-OEM-COPY+			#xF2)
    #(+VK-OEM-AUTO+			#xF3)
    #(+VK-OEM-ENLW+			#xF4)
    #(+VK-OEM-BACKTAB+			#xF5)
    #(+VK-ATTN+				#xF6)
    #(+VK-CRSEL+			#xF7)
    #(+VK-EXSEL+			#xF8)
    #(+VK-EREOF+			#xF9)
    #(+VK-PLAY+				#xFA)
    #(+VK-ZOOM+				#xFB)
    #(+VK-NONAME+			#xFC)
    #(+VK-PA1+				#xFD)
    #(+VK-OEM-CLEAR+			#xFE)))

;; Key aliases
(defconstant +VK-HANGEUL+ +VK-KANA+)
(defconstant +VK-HANGUL+  +VK-KANA+)
(defconstant +VK-KANJI+   +VK-HANJA+)

(defparameter *key-symbols* (make-hash-table))
(loop :for name :in *keys* :do
   (setf (gethash (symbol-value name) *key-symbols*) name))
(defun key-symbol (code)
  "Return the symbol name of a key given it's CODE."
  (gethash code *key-symbols*))
(defun key-name (key-symbol)
  "Return a string name of the key given by KEY-SYMBOL."
  (let ((n (symbol-name key-symbol))) (subseq n 4 (1- (length n)))))
(defun compatible-key-symbol (code)
  "Return a more compatible seeming key symbol."
  (keywordify (key-name (key-symbol code))))

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
	 ;; Actually we probably shouldn't do this, because we're likely
	 ;; running in a Cygwin terminal or something else that requires we
	 ;; do output through it's handles. We just won't be able to do many
	 ;; console-ish things.
	 ;; (setf (ms-term-in-handle tty) (open-real-console :input)
	 ;;       (ms-term-out-handle tty) (open-real-console :output))
	 (with-slots (not-console mode width height) tty
	   ;; Fake some stuff.
	   (setf not-console t
		 mode (make-terminal-mode)
		 width 80 height 24)))
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
  (let (result c)
    (with-slots (in-handle width height read-ahead) terminal
      (when read-ahead
	(return-from read-console-input (pop read-ahead)))
      (with-foreign-objects ((buf '(:pointer (:struct foreign-input-record)))
			     (events-read 'DWORD))
	(loop :do
	   (setf result nil)
	   (syscall (%read-console-input in-handle buf 1 events-read))
	   (with-foreign-slots ((event-type event)
				buf
				(:struct foreign-input-record))
	     (dbugf :ms "event-type ~s~%" event-type)
	     (cond
	       ((equal event-type +KEY-EVENT+)
		(with-foreign-slots ((key-down uchar virtual-key-code
					       control-key-state) event
				     (:struct foreign-key-event))
		  (dbugf :ms "key-down ~a uchar = ~a~%" key-down uchar)
		  (when (= 1 key-down)
		    (setf c (foreign-slot-value uchar '(:union foreign-uchar)
						'unicode-char))
		    (cond
		      ;; Convert Alt-<char> into #\Escape <Char>
		      ((plusp (logand control-key-state
				      (logior +RIGHT-ALT-PRESSED+
					      +LEFT-ALT-PRESSED+)))
		       (when (not (zerop c))
			 (setf read-ahead (append read-ahead (list c)))
			 (setf result (char-code #\escape))))
		      ((plusp virtual-key-code)
		       (setf result (compatible-key-symbol virtual-key-code))
		       (dbugf :ms "keycode = ~s~%" result))
		      (t
		       (when (not (zerop c))
			 (setf result c)))))
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
		)))
	   :while (not result))))
    result))

(defcfun ("ReadFile" %read-file)
    BOOL
   (file HANDLE)
   (buffer LPVOID)
   (number-of-bytes-to-read DWORD)
   (number-of-bytes-read LPDWORD)
   (overlapped LPOVERLAPPED))

;; Supposedly there's no way to tell if a handle was created with
;; FILE_FLAG_OVERLAPPED, but if you don't supply the OVERLAPPED to ReadFile,
;; it can mess up, generally by terminating prematurely if there is asynchronous
;; IO. This seems like a deep design problem.
;; ....
;; but...
;; try a zero byte ReadFile with a NULL lpOverlapped. If it fails with
;; ERROR_INVALID_PARAMETER assume it was opened with FILE_FLAG_OVERLAPPED.
;; ... O_o O rly??
;; [I haven't tested this out.]

(defun read-handle-input (handle)
  (with-foreign-objects ((buf :unsigned-char 1)
			 (bytes-read 'DWORD))
    (let ((result (%read-file handle buf 1 bytes-read (null-pointer))))
      (format *debug-io* "%read-file result = ~s bytes-read = ~s~%" result
	      (mem-ref bytes-read 'DWORD))
      (format *debug-io* "wchar = #x~x~%" (mem-aref buf :unsigned-char 0))
      (finish-output *debug-io*))
    (when (/= 1 (mem-ref bytes-read 'DWORD))
      (error 'windows-error :format-control "Fail to read read 1 byte."))
    (mem-aref buf :unsigned-char 0)))

(defun read-terminal-char (terminal &key timeout)
  "Return a character read from the terminal TERMINAL-HANDLE.
If there's a problem, it will signal a READ-CHAR-ERROR. If the terminal is
resized it will signal an OPSYS-RESIZED. If the program is continued from
being suspended, it will signal an OPSYS-RESUMED. Usually this means the
caller should handle these possibilites. Returns the character read or NIL if it
the timeout is hit."
  (declare (ignore timeout))
  (with-slots (in-handle not-console) terminal
    (wchar-to-character
     (if not-console
	 (read-handle-input in-handle)
	 (read-console-input terminal)))))

(defun read-terminal-byte (terminal &key timeout)
  "Return an unsigned byte read from the terminal TERMINAL-HANDLE.
If there's a problem, it will signal a READ-CHAR-ERROR. If the terminal is
resized it will signal an OPSYS-RESIZED. If the program is continued from
being suspended, it will signal an OPSYS-RESUMED. Usually this means the
caller should handle these possibilites. Returns the byte read or NIL if it
the timeout is hit."
  (declare (ignore timeout))
  (with-slots (in-handle not-console) terminal
    (if not-console
	(read-handle-input in-handle)
	(read-console-input terminal))))

(defun read-until (tty stop-char &key timeout)
  "Read until STOP-CHAR is read. Return a string of the results.
TTY is a file descriptor."
  (declare (ignore tty stop-char timeout))
  ;; @@@ taking the lazy slow way out
  ;; (loop :with c = (read-terminal-char tty)
  ;;    :while (char/= c stop-char))
     )

(defcfun ("WriteConsoleW" %write-console)
    BOOL
  (console-output HANDLE)		; in
  (buffer (:pointer VOID))		; in
  (number-of-chars-to-write DWORD)	; in
  (number-of-chars-written LPDWORD)	; out
  (reserved LPVOID))			; reserved

(defcfun ("WriteFile" %write-file)
    BOOL
  (file HANDLE)				; in
  (buffer LPCVOID)			; in
  (number-of-bytes-to-write DWORD)	; in
  (number-of-bytes-written LPDWORD)	; out opt
  (overlapped LPOVERLAPPED))		; in/out opt

(defun write-terminal-string (tty string)
  "Write STRING to the terminal designated by TERMINAL-HANDLE."
  (cond
    ((ms-term-p tty)
     (with-slots (out-handle not-console) tty
       (with-wide-string (str string)
	 (with-foreign-object (written 'DWORD)
	   (if not-console
	       (syscall (%write-file out-handle str (length string)
					 written (null-pointer)))
	       (syscall (%write-console out-handle str (length string)
					    written (null-pointer))))
	   ;; @@@ Should we complain if written != length ?
	   (mem-ref written 'DWORD)))))
    ((output-stream-p tty)
     (write-string string tty))))

(defun write-terminal-char (terminal char)
  "Write CHAR to the terminal designated by TERMINAL."
  (cond
    ((output-stream-p terminal)
     (write-char char terminal))
    ((ms-term-p terminal)
     (write-terminal-string terminal (string char)))))

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
  (with-slots (not-console in-handle (our-mode mode)) tty
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
    (when not-console
      (return-from set-terminal-mode tty))
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
  (with-slots (not-console in-handle mode) tty
    (when not-console
      (return-from get-terminal-mode mode))
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

(defun reset-terminal-modes (&key file-descriptor device)
  "Set the terminal modes to a normal starting state."
  (declare (ignore device)) ;; @@@
  (if (not file-descriptor)
      (let ((in-h (%get-std-handle +STD-INPUT-HANDLE+)))
	(dbugf :ms "resetting terminal modes to ~s~%" +NORMAL-INPUT-MODES+)
	(when (zerop (%set-console-mode in-h +NORMAL-INPUT-MODES+))
	  (error 'windows-error :error-code (get-last-error)
		 :format-control "Can't set console mode.")
	  ;; @@@ but we don't reset the saved ms-term modes!!
	  ))
      (with-slots (not-console in-handle mode) tty
	(setf mode (make-terminal-mode :echo t :line t :raw nil :timeout nil))
	(when not-console
	  (return-from reset-terminal-modes (values)))
	(dbugf :ms "resetting terminal modes to ~s~%" +NORMAL-INPUT-MODES+)
	(when (zerop (%set-console-mode in-handle +NORMAL-INPUT-MODES+))
	  (error 'windows-error :error-code (get-last-error)
		 :format-control "Can't set console mode."))))
  (values))

(defcstruct SMALL_RECT
  (left   MS-SHORT)
  (top 	  MS-SHORT)
  (right  MS-SHORT)
  (bottom MS-SHORT))

;; DAMNIT THIDS IS STUPDI!!!!
(defun set-rect (rect o1 o2 o3 o4)
  (with-foreign-slots ((left top right bottom)
		       rect (:struct SMALL_RECT))
    (setf left o1
	  top o2 
	  right o3
	  bottom o4)))

(defcstruct CONSOLE_SCREEN_BUFFER_INFO
  (size                (:struct COORD))
  (cursor-position     (:struct COORD))
  (attributes          WORD)
  (window              (:struct SMALL_RECT))
  (maximum-window-size (:struct COORD)))

(defctype PCONSOLE_SCREEN_BUFFER_INFO
    (:pointer (:struct CONSOLE_SCREEN_BUFFER_INFO)))

(defcfun ("GetConsoleScreenBufferInfo" %get-console-screen-buffer-info)
    BOOL
  (console-output HANDLE)				    ; in 
  (console-screen-buffer-info PCONSOLE_SCREEN_BUFFER_INFO)) ; out

(defun get-console-info (tty)
  "Get the window size. The first value is columns, second value is rows."
  (dbugf :ms "get-window-info tty = ~s~%" tty)
  (with-slots (out-handle width height) tty
    (let (x y attr)
      (with-foreign-object (buf '(:struct CONSOLE_SCREEN_BUFFER_INFO))
	(when (zerop (%get-console-screen-buffer-info out-handle buf))
	  (error 'windows-error :error-code (get-last-error)
		 :format-control "Can't get console screen size."))
	(with-foreign-slots ((window cursor-position attributes) buf
			     (:struct CONSOLE_SCREEN_BUFFER_INFO))
	  (dbugf :ms "window = ~s~%curs-pos = ~s ~%" window
		 cursor-position)
	  (setf width (1+ (- (getf window 'right) (getf window 'left)))
		height (1+ (- (getf window 'bottom) (getf window 'top)))
		x (getf cursor-position 'x)
		y (getf cursor-position 'y)
		attr attributes)
	  (values x y width height attr (getf window 'top)))))))

(defun get-window-size (tty)
  "Get the window size. The first value is columns, second value is rows."
  (dbugf :ms "get-window-size tty = ~s~%" tty)
  (with-slots (not-console width height) tty
    (when (not not-console)
      (multiple-value-bind (x y new-width new-height) (get-console-info tty)
	(declare (ignore x y))
	(setf width new-width
	      height new-height)))
    (values width height)))

(defun get-cursor-position (tty)
  "Get the cursor position. Return as two values, Y and X position."
  (multiple-value-bind (x y) (get-console-info tty)
    (values x y)))

(defun get-attributes (tty)
  "Get the current attributes as an integer."
  (multiple-value-bind (x y width height attr) (get-console-info tty)
    (declare (ignore x y width height))
    (values attr)))

(defcstruct CONSOLE_CURSOR_INFO
  (size DWORD)
  (visible BOOL))
(defctype PCONSOLE_CURSOR_INFO (:pointer (:struct CONSOLE_CURSOR_INFO)))

(defcfun ("GetConsoleCursorInfo" %get-console-cursor-info)
    BOOL
  (console-output HANDLE)
  (console-cursor-info PCONSOLE_CURSOR_INFO))

(defun get-cursor-info (tty)
  "Get the cursor info. Returns a size between 1 and 100 inclusive, and a
boolean indicating visibility."
  (with-slots (out-handle) tty
    (with-foreign-object (info '(:struct CONSOLE_CURSOR_INFO))
      (syscall (%get-console-cursor-info out-handle info))
      (values
       (foreign-slot-value info '(:struct CONSOLE_CURSOR_INFO) 'size)
       (plusp (foreign-slot-value info
				  '(:struct CONSOLE_CURSOR_INFO) 'visible))))))

(defcfun ("SetConsoleCursorInfo" %set-console-cursor-info)
    BOOL
  (console-output HANDLE)
  (console-cursor-info PCONSOLE_CURSOR_INFO))

(defun set-cursor-state (tty &key size (visible nil visible-provided-p))
  (with-slots (out-handle) tty
    (when (or (not size) (not visible-provided-p))
      (multiple-value-bind (old-size old-visible)
	  (get-cursor-info tty)
	(when (not size)
	  (setf size old-size))
	(when (not visible-provided-p)
	  (setf visible old-visible))))
    (when (not (and (integerp visible) (or (= visible 0) (= visible 1))))
      (setf visible (if visible 1 0)))
    ;; (when (not (and (integerp size) (>= size 0) (<= size 100)))
    ;;   (setf size 20))
    (with-foreign-object (info '(:struct CONSOLE_CURSOR_INFO))
      (setf (foreign-slot-value info '(:struct CONSOLE_CURSOR_INFO) 'size)
	    size
	    (foreign-slot-value info '(:struct CONSOLE_CURSOR_INFO) 'visible)
	    visible)
      (syscall (%set-console-cursor-info out-handle info)))))

(defcfun ("SetConsoleCursorPosition" %set-console-cursor-position
				     :convention :stdcall)
    BOOL
  (console-output HANDLE)
  (cursor-position (:struct COORD))
  )

(defun set-cursor-position (tty row col)
  (with-dbug :ms "set-cursor-position ~s ~s ~%" row col)
  (with-slots (out-handle) tty
    (syscall (%set-console-cursor-position out-handle `(x ,col y ,row)))))

(defcstruct CHAR_INFO
  (uchar (:union foreign-uchar))
  (attributes WORD))
(defctype PCHAR_INFO (:pointer (:struct CHAR_INFO)))

(defcfun ("ScrollConsoleScreenBufferW" %scroll-console-screen-buffer)
    BOOL
  (console-output HANDLE)			     ; in
  (scroll-rectangle (:pointer (:struct SMALL_RECT))) ; in
  (clip-rectangle (:pointer (:struct SMALL_RECT)))   ; in optional
  (destination-origin (:struct COORD))		     ; in
  (fill (:pointer (:struct CHAR_INFO))))	     ; in

(defun scroll-console (tty &key (left 0) (top 0) right bottom x y)
  (with-slots (out-handle) tty
    (with-foreign-objects ((scroll-rect '(:struct SMALL_RECT))
			   ;;(clip-rect '(:struct SMALL_RECT))
			   (fill-char '(:struct CHAR_INFO))
			   (stupid-uchar '(:union foreign-uchar))
			   (dest '(:struct COORD)))
      (set-wchar stupid-uchar 0 #\space)
      (with-foreign-slots ((uchar attributes)
			   fill-char (:struct CHAR_INFO))
	(setf attributes 0
	      uchar stupid-uchar))
      (set-rect scroll-rect left top right bottom)
      (set-coord dest x y)
#|
      (setf (mem-ref fill-char '(:struct CHAR_INFO))
	    (convert-to-foreign `(char ,uchar attributes 0)
				'(:struct CHAR_INFO))
	    (mem-ref scroll-rect '(:struct SMALL_RECT))
	    (convert-to-foreign `(left ,left top ,top
				  :right ,right :bottom, bottom)
				'(:struct CHAR_INFO))
	    ;; (mem-ref clip-rect '(:struct SMALL_RECT))
	    ;; (convert-to-foreign `(left ,left top ,top
	    ;;                       right ,right bottom ,bottom)
	    ;; 			'(:struct CHAR_INFO)))
	    (mem-ref scroll-rect '(:struct SMALL_RECT))
	    (convert-to-foreign `(left ,left top ,top
				  right ,right bottom, bottom)
				'(:struct SMALL_RECT))
|#
      (syscall (%scroll-console-screen-buffer
		out-handle
		scroll-rect (null-pointer)
		;;(mem-ref dest '(:struct COORD))
		`(x ,x y ,y)
		fill-char)))))

(defcfun ("FillConsoleOutputCharacterW" %fill-console-output-character
					:convention :stdcall)
    BOOL
  (console-output HANDLE)			  ; in
  (character TCHAR)				  ; in
  (length DWORD)				  ; in
  (write-coord (:struct COORD))			  ; in
  (number-of-chars-written LPDWORD))		  ; out

(defun fill-console-char (tty &key (char #\space) (x 0) (y 0) length)
  (when (not length)
    (multiple-value-bind (x y width height) (get-console-info tty)
      (declare (ignore x y))
      (setf length (* width height))))
  (with-slots (out-handle) tty
    (with-foreign-objects ((chars-written 'DWORD)
			   ;;(tchar 'TCHAR)
			   ;;(write-at '(:struct COORD))
			   )
      ;(set-wchar tchar 0 char)
      ;;(set-coord write-at x y)
      (syscall (%fill-console-output-character
		out-handle
		(character-to-wchar char)
		length
		;;(convert-to-foreign `(x ,x y ,y) '(:struct COORD))
		;;(mem-ref write-at '(:struct COORD))
		;;write-at
		`(x ,x y ,y)
		chars-written))
      (mem-ref chars-written 'DWORD))))

(defcfun ("SetConsoleTextAttribute" %set-console-text-attribute)
    BOOL
  (console-output HANDLE)
  (attributes WORD))

(defconstant +FOREGROUND-BLUE+      #x0001)
(defconstant +FOREGROUND-GREEN+     #x0002)
(defconstant +FOREGROUND-RED+       #x0004)
(defconstant +FOREGROUND-INTENSITY+ #x0008)
(defconstant +BACKGROUND-BLUE+      #x0010)
(defconstant +BACKGROUND-GREEN+     #x0020)
(defconstant +BACKGROUND-RED+       #x0040)
(defconstant +BACKGROUND-INTENSITY+ #x0080)

(defun set-console-attribute (tty attribute)
  (with-slots (out-handle) tty
    (%set-console-text-attribute out-handle attribute)))

(defcfun ("FillConsoleOutputAttribute" %fill-console-output-attribute)
    BOOL
  (console-output HANDLE)  			; in
  (attribute WORD)    			  	; in
  (length DWORD)   				; in
  (write-coord (:struct COORD))   		; in
  (number-of-attrs-written LPDWORD)) 		; out

(defun fill-console-attribute (tty &key (attribute 0) (x 0) (y 0) length)
  (when (not length)
    (multiple-value-bind (x y width height) (get-console-info tty)
      (declare (ignore x y))
      (setf length (* width height))))
  (with-slots (out-handle) tty
    (with-foreign-objects ((chars-written 'DWORD))
      (syscall (%fill-console-output-attribute
		out-handle
		attribute
		length
		`(x ,x y ,y)
		chars-written))
      (mem-ref chars-written 'DWORD))))

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
