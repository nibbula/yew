;;;
;;; rm-cmds.lisp - Commands for rm.
;;;

(in-package :rm)

(lish:defcommand rmdir
  ((parents boolean :short-arg #\p
    :help "True to delete the parent directories if they are empty.")
   (dirs directory :optional nil :repeating t :help "Directories to delete."))
  "Delete directories."
  (loop :for dir :in dirs
     :do (rmdir dir :parents parents))
  (values))

(lish:defcommand rm
  ((force boolean :short-arg #\f
    :help "True to try to delete files even if you don't have write permission.")
   ;; Life is short. Make backups. Also, see section 15,16,17 of LICENSE.
   (recursive boolean :short-arg #\r
    :help "True to delete directories recursively.")
   (no-preserve-root boolean :short-arg nil :long-arg "no-preserve-root"
    :help "True to not treat '/' with special care.")
   (dry-run boolean :short-arg #\n
    :help
    "FOR RECURSIVE ONLY: Don't delete anything, just report what would be deleted.")
   (verbose boolean :short-arg #\v
    :help
    "FOR RECURSIVE ONLY: Print what is being deleted.")
   (files pathname :optional nil :repeating t :help "Files to delete."))
  "Delete directories."
  (declare (ignore force))
  (loop :with info
     :for file :in files
     :do
     (when (and (not no-preserve-root) (equal file "/"))
       (error "I won't delete the root directory, unless you specify ~
               no-preserve-root."))
     (setf info (get-file-info file :follow-links nil))
     (if (eq :directory (file-info-type info))
	 (rmdir file :recursive recursive :dry-run dry-run :verbose verbose)
	 (rm file)))
  (values))

;; End
