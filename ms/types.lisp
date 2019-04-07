;;
;; ms/types.lisp - Data types for the Windows interface
;;

(in-package :opsys-ms)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (config-feature :ms-unicode) ;; ?? pre-NT or what?
  ;; (not (equal (machine-type) "x86"))
  #+64-bit-target (config-feature :ms-win64)

  (defparameter *windows-major-version*
    (parse-integer (initial-span (software-version) ".")))

  (when (< *windows-major-version* 10)
    (config-feature :t-os-old-windows)))

;; (defctype WINAPI) __stdcall
;; presumably using :stdcall on the library will take care of this.
;; Also one could specify something like:
;;   (defcfun ("WinFoo" win-foo :cconv :stdcall) DWORD (LPSTSTR derp))

(defctype wchar-t :uint16)
(defctype VOID :void)
(defctype BOOL :int)
(defctype WINBOOL :int)
(defctype INT :int)
(defctype UINT :unsigned-int)
(defctype INT8  :char)
(defctype INT16 :short)
(defctype INT32 :int)
(defctype INT64 :int64)
(defctype LONG :long)
(defctype WORD :unsigned-short)
(defctype DWORD :unsigned-long)
(defctype PDWORD (:pointer :unsigned-long))
(defctype ULONG :unsigned-long)
(defctype PULONG (:pointer ULONG))
(defctype DWORD32 :unsigned-int)
(defctype DWORD64 :uint64)
(defctype DWORDLONG :uint64)
(defctype ULONGLONG :uint64)
(defctype ULONG64 :uint64)
(defctype PULONG64 (:pointer :uint64))
(defctype LARGE_INTEGER :int64)
(defctype PLARGE_INTEGER (:pointer LARGE_INTEGER))
(defctype ULARGE_INTEGER :uint64)
(defctype PULARGE_INTEGER (:pointer ULARGE_INTEGER))
(defctype MS-BYTE :unsigned-char)
(defctype MS-BOOLEAN MS-BYTE)
(defctype MS-FLOAT :float)
(defctype MS-CHAR :char)
(defctype MS-SHORT :short)
(defctype CCHAR :char)
(defctype WCHAR wchar-t)
(defctype LPWCH (:pointer WCHAR))
(defctype LPTCH LPWCH)
(defctype LPSTR (:pointer MS-CHAR))
(defctype LPCSTR (:pointer MS-CHAR))
(defctype LPWSTR (:pointer WCHAR))
(defctype LPCWSTR (:pointer WCHAR))
#+ms-unicode
(progn
  (defctype TCHAR WCHAR)
  (defctype LPTSTR LPWSTR)
  (defctype LPCTSTR LPCWSTR))
#-ms-unicode
(progn
  (defctype TCHAR MS-CHAR)
  (defctype LPTSTR LPSTR)
  (defctype LPCTSTR LPCSTR))
(defctype PVOID (:pointer :void))
(defctype LPVOID (:pointer :void))
(defctype LPCVOID (:pointer :void))
(defctype LPDWORD (:pointer DWORD))
#+ms-win64
(progn
  (defctype INT_PTR :int64)
  (defctype LONG_PTR :int64)
  (defctype ULONG_PTR :uint64))
#-ms-win64
(progn
  (defctype INT_PTR :int)
  (defctype LONG_PTR :long)
  (defctype ULONG_PTR :unsigned-long))
(defctype DWORD_PTR ULONG_PTR)
(defctype HANDLE PVOID)
(defctype HFILE :int)
(defctype HLOCAL HANDLE)
(defctype SSIZE_T LONG_PTR)
(defctype PSSIZE_T (:pointer LONG_PTR))
(defctype SIZE_T ULONG_PTR)
(defctype PSIZE_T (:pointer ULONG_PTR))

;; Widely used structs.

(defcstruct FILETIME
  "100-nanosecond intervals since January 1, 1601 (UTC)."
  (low-date-time DWORD)
  (high-date-time DWORD))
(defctype PFILETIME (:pointer (:struct FILETIME)))
(defctype LPFILETIME (:pointer (:struct FILETIME)))

(defcstruct foreign-offset
  (offset-low DWORD)
  (offset-high DWORD))

(defcunion foreign-offset-pointer
  (offset (:struct foreign-offset))
  (pointer PVOID))

(defcstruct OVERLAPPED
  (internal ULONG_PTR)
  (internal-high ULONG_PTR)
  (offset-pointer (:union foreign-offset-pointer))
  (event HANDLE))
(defctype LPOVERLAPPED (:pointer (:struct OVERLAPPED)))

(defcstruct SECURITY_ATTRIBUTES
  (length DWORD)
  (security-descriptor LPVOID)
  (inherit-handle BOOL))

(defctype PSECURITY_ATTRIBUTES (:pointer (:struct SECURITY_ATTRIBUTES)))
(defctype LPSECURITY_ATTRIBUTES (:pointer (:struct SECURITY_ATTRIBUTES)))

;; End
