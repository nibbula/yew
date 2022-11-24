;;;
;;; rm.lisp - Delete files and directories.
;;;

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
  "Remove directory ‘dir’. If ‘parents’ is true, remove the parent directories
in the path."
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

(defun rm (path)
  "Remove a file."
  (os-delete-file path))

;; EOF
