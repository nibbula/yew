;;;
;;; tail-cmds.lisp - Commands for tail.
;;;

(in-package :tail)

;; @@@ I want to be able to support args like "-12" meaning "-n 12".
;; I could hack it in to the arg processing here, like with :keys-as, but I want
;; the automatic arg parsing to be able to support it.

(defun convert-plungas (value)
  (cons (if (and value (stringp value) (> (length value) 1))
	    (case (char value 0)
	      (#\+ '+)
	      (#\- '-)
	      (otherwise nil))
	    nil)
	(parse-integer value)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (lish:defargtype plungas-int (lish:argument)
    "A stupid argument type that is an integer that records whether it had a
preceding sign. It evaluates to a cons of (sign . integer). The sign is a
symbol, `+' '-' or nil."
    ()
    :convert t
    (convert-plungas lish::value)))

(lish:defcommand tail
  ((line-count plungas-int :short-arg #\n :default (cons nil 10)
    :help "Lines to show.")
   (byte-count integer :short-arg #\c
    :help "Bytes to show.")
   ;; ("count" integer :default 10
   ;;  :help "The number of units to show.")
   (forever boolean :short-arg #\f
    :help "True to keep displaying lines added to the end of the file.")
   (sleep-interval number :short-arg #\s :default 1
    :help "Interval in seconds to sleep between checking for output when forever is true.")
   ;; (list boolean :short-arg #\l :help "True to return lines as a list.")
   (files pathname :repeating t
    :help "Files to use as input."))
  "Output the last portion of input."
  (let* ((*plus-p* nil)
	 (real-line-count (case (car line-count)
			    (+ (setf *plus-p* t) (- (1- (cdr line-count))) )
			    (- (- (1- (cdr line-count))))
			    (otherwise (cdr line-count)))))
    (if byte-count
	(progn
	  (if files
	      (loop :for f :in files :do
		   (snip-bytes f byte-count :before))
	      (snip-bytes *standard-input* byte-count :before))
	  (when forever
	    (with-open-file-or-stream (stream (car (last files)))
	      (let ((seekable (really-seekable stream)))
		(when (not seekable)
		  (error "I didn't implement non-seekable streams yet."))
		(file-position stream (file-length stream))
		(tail-forever stream sleep-interval)))))
	(if files
	    (loop :with i = 0 :and len = (length files)
	       :for f :in files :do
		 (tail-lines f real-line-count
			     :forever (and forever (= i (1- len))
					   sleep-interval))
		 (incf i))
	    (tail-lines *standard-input* real-line-count
			:forever (and forever sleep-interval))))))

;; End
