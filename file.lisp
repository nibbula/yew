;;
;; file.lisp - WTF is this file?
;;

(defpackage :file
  (:documentation "I can't believe you.")
  (:use :cl :dlib :dlib-misc :magic :table-print)
  (:export
   #:describe-file-type
   #:!file
   ))
(in-package :file)

(defun describe-file-type (file &key full (stream t))
  (if full
      (let ((o (guess-file-type file)))
	(print-properties
	 (append `((file ,file))
		 (mapcar (_ (list _ (funcall
				     (symbolify (s+ "content-type-" _)
						:package :magic) o)))
			 '(name category description file-name-match encoding
			   properties)))
	 :stream stream)
	(terpri))
      (format stream "~a~%" (content-type-description (guess-file-type file)))))

#+lish
(lish:defcommand file
  ;; BUG: we should be able to specify :optional nil and not get FILES twice
  ;; in the arg list.
  ((full boolean :short-arg #\f
    :help "True to show more information about the file.")
   (files pathname #|:optional nil XXX |# :repeating t
    :help "Files to identify."))
  "Try to guess what a file is."
  (if full
      (loop :for f :in files :do
	 (describe-file-type f :full t))
      (nice-print-table
       (loop :for f :in files
	  :collect (list (s+ f ":")
			 (content-type-description (guess-file-type f))))
       nil :trailing-spaces nil)))

;; EOF
