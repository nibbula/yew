;;;
;;; ms.lisp - Interface to Microsoft systems.
;;;

(in-package :opsys-ms)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
;(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))

#|
(define-foreign-library (kernel32 :stdcall) (t "kernel32"))
(use-foreign-library kernel32)

(define-foreign-library (user32 :stdcall) (t "user32"))
(use-foreign-library user32)
|#

;; (define-foreign-library kernel32 (t "kernel32.dll"))
;; (define-foreign-library user32 (t "user32.dll"))
;; (use-foreign-library kernel32)
;; (use-foreign-library user32)

(use-foreign-library "kernel32.dll")
(use-foreign-library "user32.dll")
;;(use-foreign-library "advapi32.dll")
(use-foreign-library "secur32.dll")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants widely used

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +TRUE+ 1)
  (defconstant +FALSE+ 0)
  (defconstant +MAX-PATH+ 260)

  (defconstant +ERROR-FILE-NOT-FOUND+           2)
  (defconstant +ERROR-PATH-NOT-FOUND+           3)
  (defconstant +ERROR-NOT-READY+                21)
  (defconstant +ERROR-SHARING-VIOLATION+	32)
  (defconstant +ERROR-ENVVAR-NOT-FOUND+		203)

  ;; a.k.a: ((HANDLE)~(ULONG_PTR)0) or the maximum pointer value.
  (defconstant +INVALID-HANDLE-VALUE+
    ;;#+ms-win64 #.(1- (expt 2 64))
    ;;#-ms-win64 #.(1- (expt 2 32)))
    #+64-bit-target #.(1- (expt 2 64))
    #-64-bit-target #.(1- (expt 2 32)))

  (defconstant +INFINITE+ #xffffffff))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

;;(declaim (inline wchar-to-character))
(defun wchar-to-character (c)
  (if (and (> c #xd7ff) (< c #xe000))
      (error "I didn't write UTF-16 conversion yet! c=#x~x" c)
      (code-char c)))

(defun character-to-wchar (c)
  (let ((cc (char-code c)))
    (if (or (> cc #xffff)
	    (and (> cc #xd7ff) (< cc #xe000)))
	(error "I didn't write UTF-16 conversion yet! c=#x~x" cc)
	cc)))

;;(declaim (inline set-wchar))
(defun set-wchar (wchar-mem i character)
  (setf (mem-aref wchar-mem 'WCHAR i)
	(character-to-wchar character)))

(defun wide-string-to-lisp (wide-string &optional n)
  "Convert a Windows wide string (LPTSTR or LPWSTR) to a Lisp string.
If N isn't given, assume WIDE-STRING is terminated by a zero character."
  ;; @@@ XXX This is totally wrong. We need to do UTF-16 un-conversino.
  (if n
      (with-output-to-string (str)
	(loop :for i :from 0 :below n
	   :do (princ (code-char (mem-aref wide-string 'WCHAR i)) str)))
      (with-output-to-string (str)
	(loop :with c :and i = 0
	   :while (not (zerop (setf c (mem-aref wide-string 'WCHAR i))))
	   :do (princ (code-char c) str)
	   (incf i)))))

(defmacro with-wide-string ((var string) &body body)
  "Make a Windows wide string (LPTSTR or LPWSTR) out of a Lisp string."
  (with-unique-names (i)
    `(with-foreign-object (,var 'WCHAR (1+ (length ,string)))
       (let ((,i 0))
	 ;; @@@ XXX This is totally wrong. We need to do UTF-16 conversino.
	 (loop :while (< ,i (length ,string))
	    :do
	    (set-wchar ,var ,i (char ,string ,i))
	    (incf ,i))
	 (setf (cffi:mem-aref ,var 'WCHAR ,i) 0))
       ,@body)))

(defcfun ("LocalFree" local-free)
    HLOCAL
    (mem HLOCAL))

(defcfun ("HeapFree" heap-free)
    BOOL
  (heap HANDLE)
  (flags DWORD)
  (mem LPVOID))

(defcfun ("GetProcessHeap" get-process-heap) HANDLE)

(defun dork-free (ptr)
  #+t-os-old-windows
  (let ((result (local-free ptr)))
    (dbugf :ms "local-free ~s~%" result)
    (when (not (null-pointer-p result))
      (error "dork-free failed ~s" ptr))) ;; @@@ call (error-message)       
  #-t-os-old-windows
  (let* ((heap (get-process-heap))
	 (result (heap-free heap 0 ptr)))
    (dbugf :ms "heap-free ~s~%" result)
    (when (zerop result)
      (error "dork-free failed ~s" ptr)))) ;; @@@ call (error-message)

;; There's many more of these, but I suppose this suffices for the current?
(defconstant LANG_NEUTRAL #x00)
(defconstant SUBLANG_DEFAULT #x01)

;; This is a macro in C code.
(defun MAKELANGID (primary sublang)
  (logior (ash (logand #xffff sublang) 10)
	  (logand #xffff primary)))

;; EOF
