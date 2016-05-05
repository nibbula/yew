;;
;; ms.lisp - Interface to Microsoft systems.
;;

;; Notes:
;;  - Keep Windows names in StudlyCaps. This facilitates translation from
;;    from other code. Things that we invent, or for the generic interface,
;;    should be in the usual lisp hyphenated-identifier-style.
;;
;;  - Don't try to apdapt unix concepts to windows (e.g. signals), or vice
;;    versa. Find a generic concept that works on both systems, or if the
;;    faciliity really doesn't exist, only provide system specific versions.
;;
;;  - Imagine you are creating a hypothetical new operating system that has
;;    the features of both, but is better, and has a better lispy API.
;;    Especially don't be biased by unix's (or windows') historical
;;    crumminess. Accept that sometimes, windows (or unix, or neither) has the
;;    better way.

(defpackage :ms
  (:documentation "Interface to Microsoft systems.")
  (:use :cl)
  (:export
   #:file-handle-terminal-p
   #:file-handle-terminal-name
   ))
(in-package :ms)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error handling

#|
DWORD WINAPI FormatMessage(
  _In_     DWORD   dwFlags,
  _In_opt_ LPCVOID lpSource,
  _In_     DWORD   dwMessageId,
  _In_     DWORD   dwLanguageId,
  _Out_    LPTSTR  lpBuffer,
  _In_     DWORD   nSize,
  _In_opt_ va_list *Arguments
);
|#


(defconstant FORMAT_MESSAGE_FROM_SYSTEM #x00001000)

;; see
;; https://msdn.microsoft.com/en-us/library/windows/desktop/ms680582.aspx

(defun error-message (error-code)
  ;; DWORD dw = GetLastError();
  (FormatMessage (logior FORMAT_MESSAGE_FROM_SYSTEM
			 FORMAT_MESSAGE_ALLOCATE_BUFFER
			 FORMAT_MESSAGE_IGNORE_INSERTS)
		 NULL
		 error-code
		 MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		 (LPTSTR) &lpMsgBuf,
		 0, NULL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment variables

(defun environment ()
  (GetEnvironmentStrings))

(defun environment-variable (name)
  (GetEnvironmentVariable name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Users

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

#|
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
|#
  
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processes

(defun suspend-process (&optional id)
  "Suspend the process with the given ID. If ID is NIL or not given, suspend
the current process."
  (DebugActiveProcess id))

(defun resume-process (id)
  "Resume the suspended process with the given ID."
  (DebugActiveProcessStop id))

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
#|
BOOL WINAPI GetProcessTimes(
  _In_  HANDLE     hProcess,
  _Out_ LPFILETIME lpCreationTime,
  _Out_ LPFILETIME lpExitTime,
  _Out_ LPFILETIME lpKernelTime,
  _Out_ LPFILETIME lpUserTime
);
  |#
  (with-objects (creation-time exit-time kernel-time user-time)
    (GetProcessTimes (GetCurrentProcess)
		     creation-time exit-time kernel-time user-time)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminals

(defun file-handle-terminal-p (fd)
  "Return true if the system file descriptor FD is attached to a terminal."
  (GetConsoleMode fd))

(defun file-handle-terminal-name (fd)
  "Return the device name of the terminal attached to the system file
descriptor FD."
  (GetFileInformationByHandleEx fd))

(defvar *default-console-device-name* "con"
  "Name of the default console device.")

;; EOF
