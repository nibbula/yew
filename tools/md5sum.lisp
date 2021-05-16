;;;
;;; md5sum.lisp - MD5 message digest checksums.
;;;

(defpackage :md5sum
  (:documentation "MD5 message digest checksums.")
  (:use :cl :dlib :collections)
  (:export
   #:md5sum
   #:!md5sum
   ))
(in-package :md5sum)

;; This is separate from checksum, so we don't have to depend on ironclad if we
;; don't need it.

(defun md5sum (files &key collect as-bytes)
  (flet ((sum-thing (thing)
	   (typecase thing
	     (pathname (md5:md5sum-file thing))
	     (stream (md5:md5sum-stream thing))
	     (string
	      (if (nos:file-exists thing)
		  (md5:md5sum-file thing)
		  (md5:md5sum-string thing)))
	     (sequence
	      (md5:md5sum-sequence thing))
	     (t
	      (error "Don't know how to MD5sum a ~s." (type-of thing))))))
  (grout:with-grout ()
    (let (results)
      (loop :with bytes :and sum
	 :for f :in files
	 :do
	 (with-simple-restart (continue "Skip to the next thing to checkum.")
	   (setf bytes (sum-thing f)
		 sum (with-output-to-string (str)
		       (map nil #'(lambda (x)
				    (format str "~(~2,'0x~)" x))
			    bytes)))
	   (grout:grout-format "~a  ~a~%" sum f)
	   (when collect
	     (push (if as-bytes bytes sum) results))))
      (when collect
	(nreverse results))))))

#+lish
(lish:defcommand md5sum
  ((collect boolean :short-arg #\c :help "Collect results in a list.")
   (as-bytes boolean :short-arg #\s :help "Return results as byte arrays.")
   (files pathname :repeating t :help "Files to sum."))
  "Print MD5 checksums. Use a stronger checksum for anything important."
  (md5sum (or files (and lish:*input* (list lish:*input*))
	      (list *standard-input*))
	  :collect collect :as-bytes as-bytes))

;; End
