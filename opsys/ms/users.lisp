;;
;; ms/users.lisp - Windows interface to user accounts, groups and authenticaion.
;;

(in-package :opsys-ms)

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

;; End
