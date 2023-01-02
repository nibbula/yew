;;;
;;; cat-cmds.lisp - Commands for cat.
;;;

(in-package :cat)

;; I *NEVER* want to add -vet!

(lish:defcommand cat
  ((files pathname :repeating t :help "Files to concatenate to the output."))
  ;; :accepts (or string pathname stream list)
  :accepts t
  "Concatenate files. Copy streams."
  (lish:with-files-or-input (files :on-unknown-input-type
				   (when (and lish:*input*
					      (not (listen *standard-input*)))
				     (princ lish:*input*)
				     (terpri)))
    (apply #'cat files))
  (setf lish:*output* lish:*input*))

(lish:defcommand slurp
  ((files pathname :repeating t :help "Name of a file to slurp.")
   (binary boolean :short-arg #\b
   :help "Slurp as bytes. Return an array of (unsigned-byte 8).")
   (lines boolean :short-arg #\l :help "Slurp as lines."))
  "Convert input to return values. If no files are given read *standard-input*."
  (let* ((slurp-func
	   (cond
	     ((and lines binary) #'get-binary-lines)
	     ((and lines) #'get-lines)
	     (binary #'slurp-binary)
	     (t #'slurp)))
	 (count 0)
	 (result
	   (if files
	       (loop :for f :in files
		     :collect (funcall slurp-func f)
		     :do (incf count))
	       (funcall slurp-func *standard-input*))))
    (setf lish:*output* (if (= count 1)
			    (car result)
			    result))))

;; End
