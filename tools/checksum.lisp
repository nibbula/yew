;;;
;;; checksum.lisp - Print checksums.
;;;

(defpackage :checksum
  (:documentation "Print checksums.")
  (:use :cl :dlib :grout :ironclad)
  (:export
   #:checksum
   #:!sha256sum
   #:!sha512sum
   #:checksum
   ))
(in-package :checksum)

(defun checksum (files &key collect as-bytes (digest :sha512))
  (let ((ic-digest (symbolify (string digest) :package :ironclad :no-new t)))
    (flet ((sum-thing (thing)
	     (typecase thing
	       (pathname (digest-file ic-digest thing))
	       (stream
		;; @@@@@ try to convert stream to binary?
		(digest-stream ic-digest thing))
	       (string
		(if (nos:file-exists thing)
		    (digest-file ic-digest thing)
		  (digest-sequence ic-digest thing)))
	       (sequence
		(digest-sequence ic-digest thing))
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
	    (nreverse results)))))))

#+lish
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-digest-commands ()
    (let
     ((forms
       (loop :for name :in '(sha224 sha256 sha384 sha512)
         :collect
	 `(lish:defcommand ,(symbolify (s+ name "sum"))
	    ((collect boolean :short-arg #\c
	      :help "Collect results in a list.")
	     (as-bytes boolean :short-arg #\s
              :help "Return results as byte arrays.")
	     (files pathname :repeating t :help "Files to sum."))
	    ,(s+ "Print " name " checksums.")
	    (checksum (or files (and lish:*input* (list lish:*input*))
			  (list *standard-input*))
		      :digest ',name :collect collect :as-bytes as-bytes)))))
     `(progn ,@forms))))

#+lish
(define-digest-commands)

;; End
