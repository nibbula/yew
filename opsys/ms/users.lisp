;;
;; ms/users.lisp - Windows interface to user accounts, groups and authenticaion.
;;

(in-package :opsys-ms)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Users

(defconstant +UNLEN+ 256 "Maximum length of a user name.")

(defparameter *extended-name-format* nil
  "List of extended user name “formats”.")

(define-to-list *extended-name-format*
  #(#(+NAME-UNKNOWN+              0 "An unknown format")
    #(+NAME-FULLY-QUALIFIED-D-N+  1 "A name with all the dots")
    #(+NAME-SAM-COMPATIBLE+       2 "Compatible with Security Account Manager")
    #(+NAME-DISPLAY+              3 "Name for displaying")
    #(+NAME-UNIQUE-ID+            6 "A unique identifier, probably a GUID")
    #(+NAME-CANONICAL+            7 "The normal name")
    #(+NAME-USER-PRINCIPAL+       8 "Principal for user authorization?")
    #(+NAME-CANONICAL-EX+         9 "Extended cannonical")
    #(+NAME-SERVICE-PRINCIPAL+   10 "Principal for service authorization?")
    #(+NAME-DNS-DOMAIN+          12 "Domain name service domain")
    #(+NAME-GIVEN-NAME+          13 "Individual given name, or first name")
    #(+NAME-SURNAME+             14 "Family surname, or last name")))

(defctype EXTENDED_NAME_FORMAT :int) ;; duh,what type should enum be?
(defctype PEXTENDED_NAME_FORMAT (:pointer EXTENDED_NAME_FORMAT))

;; (deftype extended-name-format ()
;;   `(member ,*extended-name-format*))

;; supposedly in Advapi32.dll
;; do we have to (use-foreign-library "advapi32.dll") in ms.lisp

(defcfun ("GetUserNameW" %get-user-name)
  BOOL
  (buffer LPSTR)			; out
  (length LPDWORD))			; in, out

;; do we really need SEC_ENTRY aka stdcall here?
(defcfun ("GetUserNameExW" %get-user-name-ex)
  MS-BOOLEAN
  (name-format EXTENDED_NAME_FORMAT)	; in
  (name-buffer LPSTR)			; out
  (size        PULONG))			; in, out

(defun user-name (&optional id)
  "Return the name of the user with ID, which defaults to the current user."
  (declare (ignore id))
  (with-foreign-objects ((buffer 'WCHAR +UNLEN+) ;; @@@ should it be (1+ UNLEN)?
			 (length 'DWORD))
    (setf (mem-ref length 'DWORD) +UNLEN+)
    (syscall (%get-user-name buffer length))
    (wide-string-to-lisp buffer (1- (mem-ref length 'DWORD)))))

(defun user-home (&optional (user (user-name)))
  (declare (ignore user))
  ;; @@@ This is wrong. We actually have to do a very complicated thing like: ??
  ;; - get the process handle
  ;; - get the process token
  ;; - call SHGetKnownFolderPath with FOLDERID_Profile ??
  (env "USERPROFILE")
  ;; "e:\\"
  )

(defun user-id (&key name effective)
  "Return the ID of the user with NAME, which defaults to the current user."
  (declare (ignore name effective))
  (or (ignore-errors (user-name-with-format +NAME-UNIQUE-ID+))
      (ignore-errors (user-name-with-format +NAME-SAM-COMPATIBLE+))))

(defun user-name-with-format (format &optional id)
  "Return the full name of user with ID, which defaults to the current user."
  (declare (ignore id))
  ;; (check-type format extended-name-format)
  (with-foreign-objects ((name-buffer 'WCHAR +UNLEN+)
			 (length 'DWORD))
    ;; we should handle getting ERROR_MORE_DATA and make a bigger buffer
    (setf (mem-ref length 'DWORD) +UNLEN+)
    (syscall (%get-user-name-ex format name-buffer length))
    (wide-string-to-lisp name-buffer (mem-ref length 'DWORD))))

;; where is what is in netplwiz properties comming from?

(defun user-full-name (&optional id)
  "Return the full name of user with ID, which defaults to the current user."
  (declare (ignore id))
  (or (ignore-errors (user-name-with-format +NAME-DISPLAY+)) ""))

(defun get-user-info (&key name id)
  "Return a user structure from the user database. You can look up by either
NAME or ID. If you specifiy both, it just uses the ID. If you specify neither,
it signals an error."
  (declare (ignore name id))
  (make-user-info :name (user-name)
		  :id (user-id)
		  :full-name (user-full-name)
		  :home-directory (user-home)
		  :shell "lish"
		  :primary-group-id 1
		  :guid (ignore-errors (user-name-with-format +NAME-UNIQUE-ID+))
		  :picture "<a lovely non-privacy-violating picture of you>"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Groups

;; No such thing. Or is there?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Login/accounting database

(defun users-logged-in ()
  "Return a list of names of logged in users."
  ;; @@@ stub
  (list (user-name)))

;; End
