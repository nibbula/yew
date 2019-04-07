;;
;; ms/errors.lisp - Error handling for Windows
;;

(in-package :opsys-ms)

(defcfun ("GetLastError" get-last-error) DWORD)

(defconstant +FORMAT-MESSAGE-ALLOCATE-BUFFER+ #x00000100)
(defconstant +FORMAT-MESSAGE-ARGUMENT-ARRAY+  #x00002000)
(defconstant +FORMAT-MESSAGE-FROM-HMODULE+    #x00000800)
(defconstant +FORMAT-MESSAGE-FROM-STRING+     #x00000400)
(defconstant +FORMAT-MESSAGE-FROM-SYSTEM+     #x00001000)
(defconstant +FORMAT-MESSAGE-IGNORE-INSERTS+  #x00000200)
(defconstant +FORMAT-MESSAGE-MAX-WIDTH-MASK+  #x000000FF)

(defcfun ("FormatMessageW" %format-message :convention :stdcall)
    DWORD
  (flags	DWORD)
  (source	LPCVOID)
  (message-id	DWORD)
  (language-id	DWORD)
  (buffer	LPTSTR)
  (size		DWORD)
  (va-list      PVOID) ; ??? _In_opt_ va_list *Arguments
  )

(defun error-message (&optional (error-code (get-last-error)))
  "Return a string describing the error code."
  (let (result)
    (with-foreign-object (message '(:pointer LPTSTR))
      (unwind-protect
	   (let ((bytes-stored
		  (%format-message
		   (logior +FORMAT-MESSAGE-FROM-SYSTEM+
			   +FORMAT-MESSAGE-ALLOCATE-BUFFER+
			   +FORMAT-MESSAGE-IGNORE-INSERTS+)
		   (null-pointer)
		   error-code
		   (MAKELANGID LANG_NEUTRAL SUBLANG_DEFAULT)
		   message
		   0
		   (null-pointer))))
	     (when (zerop bytes-stored)
	       (error "FormatMessage failed: ~s ~s ~s"
		      bytes-stored  error-code (get-last-error)))
	     (setf result
		   (wide-string-to-lisp (mem-ref message 'LPTSTR))))
	(when (not (null-pointer-p message))
	  (dork-free (mem-ref message 'LPTSTR)))))
    result))

(define-condition windows-error (opsys-error)
  ()
  (:documentation "An error from calling a Windows function."))

(defun error-check (c &optional fmt &rest args)
  "Check if a BOOL function fails (returns FALSE i.e. zero) and signal an
appropriate error."
  (if (zerop c)
      (error 'windows-error :error-code (get-last-error)
	     :format-control fmt :format-arguments args)
      c))

(defmacro syscall ((func &rest args))
  "Call a system function that returns BOOL false on failure and signal a
windows-error with an appropriate error message if it fails."
  `(error-check (,func ,@args)
		,(concatenate 'string (string-downcase func) ":")))

;; End
