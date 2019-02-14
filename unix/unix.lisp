;;
;; unix/unix.lisp - Miscellanous unix interface pieces
;;

(in-package :opsys-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thread-like

;; @@@ Just use bordeaux-threads!
;; locks (mutexes)
;; create thread
;; join thread

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System administration???
;; is this even a good idea

;; reboot
;; acct

;; Weird/simulation/emulation

(defcfun ("syscall" real-syscall) :long
  "Invoke a kernel system call with the NUMBER."
  (number :long) &rest)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C dynamic libraries.

(defconstant +RTLD-LAZY+     #x00001 "Lazy binding.")
(defconstant +RTLD-NOW+      #x00002 "Immediate binding.")
(defconstant +RTLD-NOLOAD+   #x00004 "Don't load the library.")
(defconstant +RTLD-DEEPBIND+ #x00008 "Bind ahead of global and to itself.")
(defconstant +RTLD-GLOBAL+   #x00100 "Make library and dependencies visisble.")
(defconstant +RTLD-LOCAL+    #x00000 "Don't make symbols visible to other libs.")
(defconstant +RTLD-NODELETE+ #x01000 "Don't delete when closed.")

(defconstant +RTLD-NEXT+ -11 "Get the symbol in the next object.")
(defconstant +RTLD-DEFAULT+ 0 "Get the symbol in the gloabl scope.")

(defconstant +LM-ID-BASE+  0 "First namespace.")
(defconstant +LM-ID-NEWLM+ -1 "A new namespace.")

(defparameter *dlinfo-requests* nil "Names for dl_info requests.")
(define-enum-list *dlinfo-requests*
    #(#(+RTLD-DI-LMID+        "Get namespace ID as a lmid-t.")
      #(+RTLD-DI-LINKMAP+     "Get link map as struct link_map.")
      #(+RTLD-DI-CONFIGADDR+  "") ;; solaris
      #(+RTLD-DI-SERINFO+     "Search info, as Dl_serinfo.")
      #(+RTLD-DI-SERINFOSIZE+ "Search info, size and count, as Dl_serinfo.")
      #(+RTLD-DI-ORIGIN+      "$ORIGIN directory")
      #(+RTLD-DI-PROFILENAME+ "") ;; solaris
      #(+RTLD-DI-PROFILEOUT+  "") ;; solaris
      #(+RTLD-DI-TLS-MODID+   "TLS module ID, as a size_t ")
      #(+RTLD-DI-TLS-DATA+    "TLS block of PT_TLS segment, as a void **"))
  :start 1)


(defcstruct dl-serpath
  (name :string)			; Name of the library.
  (flags :unsigned-int))		; Where the directory came from.

(defcstruct dl-serinfo
  (size size-t)				; size buffer.
  (count :unsigned-int)			; Number of elements in `dls_serpath'.
  (search-path (:pointer (:struct dl-serpath)))) ; Array of search paths.

(defctype lmid-t :long)

(defcfun dlopen :pointer
  "Open a shared library named by FILENAME and return a handle to it. If
FILENAME contains a slash take it as a path, otherwise look for it in some set
of places. If it is a null pointer, return a handle for the current image."
  (filename :string) (flags :int))

#+linux
(defcfun dlmopen :pointer
  "Open a shared library into the namespace given in LMID, named by FILENAME
and return a handle to it. If FILENAME contains a slash take it as a path,
otherwise look for it in some set of places. If it is a null pointer, return a
handle for the current image."
  (namspace lmid-t) (filename :string) (flags :int))
;; Ironically, C is becoming more like Lisp, being able to load libraries
;; into a hierarchy of namespaces.

(defcfun dlclose :int
  "Close a shared library. If all references to it are closed, it may be
unloaded from the image."
  (handle :pointer))

(defcfun dlsym :pointer
  "Return the address for symbol NAME in the library indicated by HANDLE,
or a null pointer if it's not found. Passing the +RTLD-DEFAULT+ as the handle
get the first symbol in the global namespace, +RTLD-NEXT+, gets the next
possible symbol."
  (handle :pointer) (name :string))

(defcfun dlinfo :int
  "Get information about a library."
  (handle :pointer) (request :int) (info :pointer))

(defcfun dlerror :string
  "Return a string describing the last error, once. Subsequent calls return a
null pointer."
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Character coding / localization

(defcfun wcwidth :int (wc wchar-t))

#+clisp (shadowing-import 'ext:char-width)
#+clisp (export 'char-width)
#-clisp
(defun char-width (char)
  "Return the column width of CHAR. If it's not working as expected, you ~
   probably have to call setlocale first."
  (if (graphic-char-p char)		; assume this is equivalent to iswprint
      (wcwidth (char-code char))
      (error "Can't determine the width of a non-graphic character: ~s" char)))

(defun language ()
  "Return a the system language."
  (let (lang)
    ;; This is lame. I'd like to turn these into something more readable
    ;; to humans, but that seems likely to bitrot and error.
    (cond
      ((setf lang (env "LC_MESSAGES"))
       lang)
      ((setf lang (env "LC_ALL"))
       lang)
      ((setf lang (env "LANG"))
       lang)
      ;; But this is really just for look messages.
      ;; On one hand, the user having more control over what language messages
      ;; are in is good. On the other hand, then we don't have a concept of
      ;; what the system default language is set to.
      ;; Anyway we just take the first one.
      ((setf lang (env "LANGUAGE"))
       (setf lang (initial-span lang ":")))
      ;; (t "Unknown") ;; Or should we just return NIL?
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

;; Go thru *features* and get rid of all our temporary configuration.
; (setf *features*
;       (delete-if #'(lambda (x)
; 		     (let ((s (string x)))
; 		       (string= s "OS-T-" :end1 (min 5 (length s)))))
; 		 *features*))

;; @@@ Should get rid of temporary features :os-t-*

;; End
