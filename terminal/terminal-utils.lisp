;;;
;;; terminal-utils.lisp - Terminal utilities.
;;;

(defpackage :terminal-utils
  (:documentation "Terminal utilities.")
  (:use :cl :dlib :collections :fatchar :ostring :terminal :fatchar-io)
  (:export
   #:with-terminal-output-to-fat-string
   #:with-terminal-output-to-fat-strings
   ))
(in-package :terminal-utils)

(defmacro with-terminal-output-to-fat-strings ((&key (start 0) end trim)
					       &body body)
  "Return the output lines from terminal output in ‘body’ as fat-strings.
‘start’ and ‘end’ are line numbers limits. If ‘trim’ is true, trim whitespace
from the right of lines, and trim trailing blank lines."
  (with-names (null-term result %end %trim orig-term)
    `(let* ((,orig-term *terminal*) (,%trim ,trim)
	    *terminal* ,null-term ,%end ,result)
       (unwind-protect
         (progn
	   (setf ,null-term
		 (make-instance
		  'terminal-null:terminal-null
		  :window-rows (terminal-window-rows ,orig-term)
		  :window-columns (terminal-window-columns ,orig-term))
		 *terminal*
		 (make-instance 'terminal-crunch:terminal-crunch
				:wrapped-terminal ,null-term))
	   (terminal-start *terminal*)
	   (setf ,%end (or ,end (1- (terminal-window-rows *terminal*))))
	   (when (or (null ,%end) (not (numberp ,%end)))
	     (setf ,%end 23))
	   ;; (describe *terminal* *trace-output*)
	   ,@body
	   (tt-finish-output)
	   (setf ,result
		 (loop :with l
		       :for i :from ,start :below (1+ ,%end)
		       :do (setf l (terminal-output-line *terminal* i)
				 l (if ,%trim
				       (ostring:ostring-right-trim
					*whitespace* l)
				       l))
		       :when (or (not ,%trim)
				 (and ,%trim (not (zerop (olength l)))))
		       :collect l)))
	 (when *terminal*
	   (terminal-done *terminal*)))
       ,result)))

(defmacro with-terminal-output-to-fat-string ((&key (start 0) end trim)
					      &body body)
  "Return terminal output in ‘body’ as a fat-strings. ‘start’ and ‘end’ are
line number limits. If ‘trim’ is true, trim whitespace from the right of lines,
and trim trailing blank lines."
  (with-names (string line first)
    `(with-output-to-fat-string (,string)
       (loop :with ,first = t
	 :for ,line
	 :in (with-terminal-output-to-fat-strings (:start ,start :end ,end
						   :trim ,trim)
	       ,@body)
	 :do
	 (if (not ,first)
	     (write-char #\newline ,string)
	     (setf ,first nil))
	 (princ ,line ,string)))))

;; End
