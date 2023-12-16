;;;
;;; string-expand.lisp - Expand variables in strings.
;;;

(defpackage :string-expand
  (:documentation "Expand variables in strings.")
  (:use :cl :dlib)
  (:export
   #:expand-variables
   ))
(in-package :string-expand)

;; @@@ maybe add delimeter-chars e.g "{}" or "%%" for windows.
(defun expand-variables (s &key (value-funcs `(,#'nos:environment-variable))
			        (indicator-char #\$)
			        (escape-char #\\)
			        (delimiters "{}")
			        allowed)
  "Return a string based on the string ‘s’ with variables expanded.
 ‘value-funcs’      A list of functions that given a string name, return
                    a value, or NIL if the variable is not defined.
                    It defaults to nos:environment-variable.
 ‘indicator-char’   A character that starts a variable name. Defaults to #\\$.
 ‘escape-char’      A character that if it prefaces an ‘indicator-char’, it
                    loses it's meaning. Defaults to backlash #\\\\.
 ‘allowed’          A boolean function of a single character or a string
                    specifiying what characters are allowed in the variable.
                    Defaults to alphanumeric and underscore #\_."
  (when (and (null indicator-char) (null delimiters))
    (error "One of indicator-char or delimiters must be provided."))
  (flet ((char-allowed (c)
	   (etypecase allowed
	     (string
	      (position c allowed))
	     (null
	      (or (alphanumericp c) (char= #\_ c)))
	     ((or symbol function)
	      (funcall allowed c)))))
  (let ((start 0) (last-start 0) (len (length s)))
    (with-output-to-string (str)
      (loop
	 :while (and (< last-start len)
		     (setf start
			   (position-if (_ (or (char= indicator-char _)
					       (char= escape-char _)))
					s :start last-start)))
	 :do
	 (cond
	   ;; backslash
	   ((char= escape-char (char s start))
	    (incf start)		; skip over backslash
	    (when (< start len)		; if there is one,
	      (incf start))		; skip over the next char
	    ;; output the first part
	    (when (not (zerop (- start last-start)))
	      (princ (subseq s last-start start) str)))
	   ;; dollar
	   ((char= indicator-char (char s start))
	    (when (not (zerop (- start last-start)))
	      (princ (subseq s last-start start) str))
	    (let ((end (or (position-if (_ (not (char-allowed _)))
					s :start (1+ start))
			   (length s))))
	      (incf start)
	      (cond
		((not (zerop (- end start)))
		 (princ (loop :with value
			  :for f :in value-funcs
			  :until (setf value (funcall f (subseq s start end)))
			  :finally (return (or value "")))
			str))
		(t
		 (princ "$" str)))
	      (setf start end)))
	   (t
	    (error "Variable parsing messed up somehow.")))
	 (setf last-start start))
      (when (not (zerop (- (or start len) last-start)))
	(princ (subseq s last-start (or start len)) str))))))

;; End
