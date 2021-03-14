;;
;; rm.lisp - Delete files and directories.
;;

(defpackage :rm
  (:documentation "Delete files and directories.")
  (:use :cl :opsys)
  (:export
   #:rm
   #:rmdir
   #:!rm
   #:!rmdir
   ))
(in-package :rm)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

(defun rmdir (dir &key parents recursive dry-run verbose)
  "Remove directory DIR. If PARENTS is true, remove the parent directories in
the path."
  (assert dir)
  (if recursive
      ;; CAUTION: This is the super dangerous part!!
      (labels ((delete-entry (entry)
	        (let ((name (dir-entry-name entry)))
		  (when (not (superfluous-file-name-p (path-file-name name)))
		    (if (eq (dir-entry-type entry) :directory)
			(progn
			  (when (or verbose dry-run)
			    (format t "delete dir ~s~%" name))
			  ;; (when (not dry-run)
			  ;;   (nos:delete-directory name))
			  )
			(progn
			  (when (or verbose dry-run)
			    (format t "delete file ~s~%" name))
			  (when (not dry-run)
			    (os-delete-file name)))))))
	       (delete-dir (name)
	         (when (not (superfluous-file-name-p (path-file-name name)))
		   (when (or verbose dry-run)
		     (format t "delete dir ~s~%" name))
		   (when (not dry-run)
		     (nos:delete-directory name)))))
	(map-directory
	 #'delete-entry
	 :dir dir :recursive t :full t
	 :post-dir-function #'delete-dir))
      (nos:delete-directory dir))
  (when (and parents (not recursive))
    (loop
       :with splitsville = (split-path dir)
       :with parent-list = (if (path-absolute-p dir)
			       (cdr splitsville) ;; Don't try to delete root.
			       splitsville)
       :for dir :in parent-list
       :do (nos:delete-directory dir))))

#+lish
(lish:defcommand rmdir
  ((parents boolean :short-arg #\p
    :help "True to delete the parent directories if they are empty.")
   (dirs directory :optional nil :repeating t :help "Directories to delete."))
  "Delete directories."
  (loop :for dir :in dirs
     :do (rmdir dir :parents parents))
  (values))

(defun rm (path)
  "Remove a file."
  (os-delete-file path))

#+lish
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

;; EOF
