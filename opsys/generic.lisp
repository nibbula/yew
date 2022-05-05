;;;
;;; generic.lisp - Generic operating system functions.
;;;

(defpackage :opsys-generic
  (:documentation "Generic operating system functions.")
  (:use :cl :opsys-base)
  (:nicknames :nos-generic)
  (:export
   #:file-info
   #:file-accessible-p
   #:file-exists
   #:os-delete-file
   #:os-rename-file
   #:set-file-time
   #:make-symbolic-link
   #:symbolic-link-target

   #:make-directory
   #:delete-directory
   #:probe-directory
   #:directory-p
   #:ensure-directory

   #:os-namestring
   #:path-parent
   #:path-directory-name
   #:path-file-name
   #:path-root
   ))
(in-package :opsys-generic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files

(defgeneric file-info (path &key follow-links)
  (:documentation
  "Return information about the file described by PATH in a FILE-INFO
structure. If FOLLOW-LINKS is true (the default), then if PATH is a symbolic
link, return information about the file it's linked to, otherwise return
information about the link itself."))

(defgeneric file-accessible-p (path &optional access)
  (:documentation
  "Return true if a PATH is accessible with ACCESS, which is a list consisiting
of the keywords :READ, :WRITE, or :EXECUTE. ACCESS defaults to :READ. Because of
race conditions, and many other peculiarities, it's best not to call this, since
something accessible now may not be accessible later."))

;; Sadly I find the need to do this because probe-file might be losing.
(defgeneric file-exists (path)
  (:documentation
  "Check that a file with FILENAME exists at the moment. But it might not exist
for long."))

(defgeneric os-delete-file (pathname)
  (:documentation
   "Delete a file. Doesn't monkey with the name, which should be a string.
Doesn't operate on streams."))

(defgeneric os-rename-file (from to)
  (:documentation
   "Rename a file called ‘from’ to be called ‘to’. Doesn't monkey with the names,
which should be a strings. It doesn't operate on streams. CAUTION: If ‘to’
already exists, it will be replaced, effectively deleting it."))

(defgeneric set-file-time (path &key access-time modification-time)
  (:documentation
   "Set the given times on PATH. The times are OS-TIME structures. Either
time can be :NOW to use the current time."))

(defgeneric make-symbolic-link (from to)
  (:documentation
   "Make a symbolic link from FROM to TO."))

(defgeneric symbolic-link-target (link-name)
  (:documentation
   "Return the target of the symbolic link."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directories

(defgeneric make-directory (path &key mode)
  (:documentation "Make a directory."))

(defgeneric delete-directory (path)
  (:documentation "Delete a directory."))

(defgeneric probe-directory (dir)
  (:documentation "Something like probe-file but for directories."))

(defgeneric directory-p (path)
  (:documentation "Return true if PATH is a directory."))

(defgeneric ensure-directory (directory &key make-parents mode)
  (:documentation
   "If ‘directory’ doesn't exist, create it.
 - ‘make-parents’   If true, create the parent diretories. (default t)
 - ‘mode’           POSIX style permissions mode for created directories.
                    Uses *default-directory-mode* if not specified."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths

(defgeneric os-namestring (path)
  (:documentation
  "Equivalent of ‘cl:namestring’ for opsys, accepting at least 
a ‘path-designator’."))

(defgeneric path-parent (path &key n)
  (:documentation
   "Return the parent directory of ‘path’, or NIL if it doesn't have one.
If N is given, return the Nth parent."))

(defgeneric path-directory-name (path)
  (:documentation
   "Return the directory portion of a ‘path’. This is similar to
‘directory-namestring’."))

(defgeneric path-file-name (path)
  (:documentation
   "Return the last portion of a ‘path’. This is similar to ‘file-namestring’."))

(defgeneric path-root (path)
  (:documentation
   "If the path is absolute, return the root directory of a ‘path’, otherwise
return NIL."))

;; End
