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
  (when lish:*input*
    (typecase lish:*input*
      ((or string pathname stream)
       (push lish:*input* files))
      (list
       (setf files (append (list lish:*input*) files)))
      (t
       (princ lish:*input*)
       (terpri))))
  (apply #'cat files)
  (setf lish:*output* lish:*input*))


(lish:defcommand slurp
  ((files pathname :repeating t :help "Name of a file to slurp.")
   (binary boolean :short-arg #\b
   :help "Slurp as bytes. Return an array of (unsigned-byte 8)."))
  "Convert input to return values. If no files are given read *standard-input*."
  (let* ((slurp-func (if binary #'slurp-binary #'slurp))
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
