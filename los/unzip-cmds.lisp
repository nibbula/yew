;;;
;;; unzip-cmds.lisp - Commands for unzip.
;;;

(in-package :unzip)

(lish:defcommand unzip
  ((overwrite boolean :short-arg #\o
    :help "True to overwrite files without asking.")
   (never-overwrite boolean :short-arg #\n
    :help "True to never overwrite.")
   (list-p boolean :short-arg #\l
    :help "True to list archive members in short format.")
   (verbose boolean :short-arg #\v
    :help "True to list archive members in verbose format.")
   (pipe boolean :short-arg #\p
    :help "True to extract files to standard output.")
   (exclude pathname :short-arg #\x :repeating t
    :help "Files to exclude from extracting or listing.")
   (archive pathname :optional nil
    :help "Zip archive file to operate on.")
   (files pathname :optional t :repeating t
    :help "File members to operate on."))
  :keys-as keys
  "Extract or list files from a ZIP archive."
  (when verbose
    (setf list-p t))
  (setf lish:*output*
	(apply #'unzip-command `(,@keys :extract ,(if list-p nil t)))))

;; End
