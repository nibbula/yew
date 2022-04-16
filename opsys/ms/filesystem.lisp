;;;
;;; ms/filesystem.lisp - Windows interface to files and filesystems
;;;

(in-package :opsys-ms)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files

(defun hidden-file-name-p (name)
  "Return true if the file NAME is normally hidden."
  (and name (> (length name) 0) (equal (char name 0) #\.)))

(defun superfluous-file-name-p (name)
  "Return true if the file NAME is considered redundant. On POSIX file
systems, this means \".\" and \"..\"."
  (and name (> (length name) 0)
       (or (and (= (length name) 1)
		(equal (char name 0) #\.))
	   (and (= (length name) 2)
		(equal (char name 0) #\.)
		(equal (char name 1) #\.)))))

(defparameter *legacy-dos-bullshit*
  `("NUL" "AUX" "CON" "PRN"
    ,(loop :for i :from 1 :to 9 :collect (s+ "COM" i))
    ,(loop :for i :from 1 :to 9 :collect (s+ "LPT" i))))

(defparameter *forbidden-chars*
  "<>:\"/\|?*"
  ;; @@@ also characters < ascii 32 (aka control characters and nul)
  "Characters that shouldn't appear in file name components. Of course some
of these can appear in path names.")

;; This assumes the Latin letters A-Z are code contiguous.
(defun device-letter-p (char)
  "Return true if the character could be a device letter."
  (let ((c (char-upcase char)))
    (and (>= (char-code c) (char-code #\A))
	 (<= (char-code c) (char-code #\Z)))))

#|
- A UNC name of any format, which always starts with two backslashs ("\\").
- A disk designator with a backslash, for example "C:\" or "d:\".
- A single backslash, for example, "\directory" or "\file.txt".
|#

(defun drive-prefix-p (path)
  (and (>= (length path) 3)
       (device-letter-p (char path 0))
       (char= (char path 1) #\:)
       (char= (char path 2) *directory-separator*)))

(defun %path-absolute-p (path)
  "Return true if the PATH is absolute."
  (let ((len (length path)))
    (and path (stringp path) (not (zerop len))
	 (or (char= *directory-separator* (char path 0))
	     (drive-prefix-p path)))))

#|

  <separator> := #\\
  <volume separator> := #\:
  <drive-letter> := [A-Za-z]
  <volume> := <drive-letter> <volume separator>

  ---------

  <tradtional-dos-path> := <volume>
                           [<separator>] { <directory> [<separator>] }...
                           <file-name>
  ---------

  <unc-path> := <separator> <separator> <unc-volume> <separator>
                { <directory> [<separator>] }... [<file-name>]
  <unc-volume> := <host-name> <separator> <share-name>
  <share-name> := [ <named-share> | <drive-share> ]
  <named-share> := { <share-name-char> ... }
  <drive-share> := #\$ <drive-letter>
  <host-name> := [ <netbios-name> | <ip-address> ]
  <ip-address> := [ <fqdn> | <ipv4-address> | <ipv6-address> ]

  ---------

  <time-low> := <hex-digit>*8
  <time-mid> := <hex-digit>*4
  <time-hi-and-version> := <hex-digit>*4
  <clock-seq-hi-and-res> := <hex-digit>*2
  <clock-seq-low> := <hex-digit>*2
  <node> := <hex-digit>*12
  <guid> := <time-low> #\- <time-mid> #\- <time-hi-and-version> #\- 
            <clock-seq-hi-and-res> <clock-seq-low> #\- <node>
  <guid-volume> := "Volume" #\{ <guid> #\}
  <unc> := "UNC"
  <partition-name-char> := ??? what ???
  <partition-name> := <partition-name-char>...
  <device-volume> := [ <guid-volume> | <volume> | <partition-name> | <unc> ]
  <device-path> := <separator> <separator> [ #\. | #\? ] <separator>
                   <device-volume>
  ---------

  <windows-path> := [ <tradtional-dos-path> | <unc-path> | <device-path> ]
|#

;; This just parses a <tradtional-dos-path> because fuck that noise.
;; @@@ although we probably should add simple share names
(defun parse-path (path)
  "Return an os-pathname for ‘path’."
  (let ((i 0) (start nil)
	(str (safe-namestring path))
	(result (make-os-pathname)))
    (labels ((is-sep () (char= #\\ (char str i)))
	     (note (x) (push x (os-pathname-path result)))
	     (start () (setf start i))
	     (end ()
	       (when start
		 (note (subseq str start i))
		 (setf start nil)
		 t))
	     (done ()
	       (end)
	       (setf (os-pathname-path result)
		     (nreverse (os-pathname-path result)))
	       (return-from parse-path result))
	     (next (&optional n)
	       (incf i n)
	       (when (>= i (length str))
		 (done))
	       t))
      (cond
	((drive-prefix-p str)
	 (setf (os-pathname-device result) (char str 0)
	       (os-pathname-absolute-p result) t)
	 (next 3))
	((is-sep)
	 (setf (os-pathname-absolute-p result) t)
	 (next)))
      (loop :while
	    (or
	     (and (is-sep) (next)
		  (loop :while (and (is-sep) (next))))
	     (progn
	       (start)
	       (loop :while (and (not (is-sep)) (next)))
	       (end)
	       t))))))

(defun os-pathname-namestring (os-path)
  "Return a namestring for an os-pathname."
  (check-type os-path os-pathname)
  (with-slots (path absolute-p device host) os-path
    (format nil "~:[~;~c:\\~]~:[~;\\~]~{~a~^\\~}"
	    device
	    (and absolute-p (not device))
	    (os-pathname-path os-path))))

(defmethod os-namestring ((path os-pathname))
  "Return a namestring for an os-pathname."
  (os-pathname-namestring path))

(defun os-pathname-pathname (os-path)
  "Return a pathname for an os-pathname."
  (check-type os-path os-pathname)
  (with-accessors ((path os-pathname-path)
		   (absolute-p os-pathname-absolute-p)
		   (device os-pathname-device)
		   (host os-pathname-host)) os-path
    (let* ((name (car (last path)))
	   (pos (position #\. name :from-end t))
	   (type nil))
      (when (setf pos (position #\. name :from-end t))
	(setf name (subseq name 0 pos)
	      type (subseq name (1+ pos))))
      (make-pathname :host host
		     :device device
		     :directory `(,(if absolute-p :absolute :relative)
				  ,@(butlast path))
		     :name name
		     :type type))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *file-attributes* nil "FILE_ATTRIBUTE_* constants")

  (define-to-list *file-attributes*
      #(#(+FILE-ATTRIBUTE-READONLY+              #x00000001 "")
	#(+FILE-ATTRIBUTE-HIDDEN+                #x00000002 "")
	#(+FILE-ATTRIBUTE-SYSTEM+                #x00000004 "")
	#(+FILE-ATTRIBUTE-DIRECTORY+             #x00000010 "")
	#(+FILE-ATTRIBUTE-ARCHIVE+               #x00000020 "")
	#(+FILE-ATTRIBUTE-DEVICE+                #x00000040 "")
	#(+FILE-ATTRIBUTE-NORMAL+                #x00000080 "")
	#(+FILE-ATTRIBUTE-TEMPORARY+             #x00000100 "")
	#(+FILE-ATTRIBUTE-SPARSE-FILE+           #x00000200 "")
	#(+FILE-ATTRIBUTE-REPARSE-POINT+         #x00000400 "symbolic link?")
	#(+FILE-ATTRIBUTE-COMPRESSED+            #x00000800 "")
	#(+FILE-ATTRIBUTE-OFFLINE+               #x00001000 "")
	#(+FILE-ATTRIBUTE-NOT-CONTENT-INDEXED+   #x00002000 "")
	#(+FILE-ATTRIBUTE-ENCRYPTED+             #x00004000 "")
	#(+FILE-ATTRIBUTE-INTEGRITY-STREAM+      #x00008000 "")
	#(+FILE-ATTRIBUTE-VIRTUAL+               #x00010000 "")
	#(+FILE-ATTRIBUTE-NO-SCRUB-DATA+         #x00020000 "")
	#(+FILE-ATTRIBUTE-RECALL-ON-OPEN+        #x00040000 "")
	#(+FILE-ATTRIBUTE-RECALL-ON-DATA-ACCESS+ #x00400000 ""))))

(defconstant GetFileExInfoStandard 0 "Indicates a WIN32_FILE_ATTRIBUTE_DATA.")
(defctype GET_FILEEX_INFO_LEVELS :int32) ; XXX whatever

(defcstruct WIN32_FILE_ATTRIBUTE_DATA
  (file-attributes DWORD)
  (creation-time (:struct FILETIME))
  (last-access-time (:struct FILETIME))
  (last-write-time (:struct FILETIME))
  (file-size-high DWORD)
  (file-size-low DWORD))

(defctype LPWIN32_FILE_ATTRIBUTE_DATA
    (:pointer (:struct WIN32_FILE_ATTRIBUTE_DATA)))

(defcfun ("GetFileAttributesExW" %get-file-attributes-ex)
    BOOL
  (file-name LPCTSTR)
  (info-level-id GET_FILEEX_INFO_LEVELS)
  (file-information LPVOID))

(defconstant +windows-to-universal-time+
  9435484800 ;; Calculated by comparing the actual times.
  ;; 9435456000 ;; Calculated by comparing the actual times. Off by TZ!?!
  ;; @@@ This can't be right? Leap years? etc.
  ;; (* (- 1900 1601) (* 60 60 24 (+ 365 1/4)))
  "Value to subtract from a 1601 based Windows time in seconds, to get a
Common Lisp 1900 based universal time.")

(defun filetime-to-universal-time (filetime)
  "Convert from a (:struct FILETIME) to a CL universal-time."
  ;; FILETIME is in 100-nanosecond intervals since January 1, 1601 (UTC).
  (let* ((low-date-time (getf filetime 'low-date-time))
	 (high-date-time (getf filetime 'high-date-time))
	 (100-nsec (logior (ash high-date-time 32) low-date-time))
	 (sec (truncate 100-nsec (expt 10 7))))
    (- sec +windows-to-universal-time+)))

(defun filetime-to-universal-time-and-nsec (filetime)
  "Convert from a (:struct FILETIME) to a CL universal-time and nanosecods."
  ;; FILETIME is in 100-nanosecond intervals since January 1, 1601 (UTC).
  (let* ((low-date-time (getf filetime 'low-date-time))
	 (high-date-time (getf filetime 'high-date-time))
	 (100-nsec (logior (ash high-date-time 32) low-date-time)))
    (multiple-value-bind (new-sec new-100nsec)
	(truncate 100-nsec (expt 10 7))
      (values (- new-sec +windows-to-universal-time+)
	      (* new-100nsec 100)))))

(defun filetime-to-os-time (filetime)
  (multiple-value-bind (sec nano)
      (filetime-to-universal-time-and-nsec filetime)
    (make-os-time :seconds sec :nanoseconds nano)))

(defun os-time-to-filetime-integer (os-time)
  "Convert an OS-TIME to an integer equivalent to the Windows FILETIME."
  (+ (* (+ +windows-to-universal-time+ (or (os-time-seconds os-time) 0))
	(expt 10 7))
     (truncate (or (os-time-nanoseconds os-time) 0) 100)))

(defun os-time-to-filetime (os-time)
  "Convert an OS-TIME structure to a CFFI (:struct FILETIME)."
  (let ((100-nsec (os-time-to-filetime-integer os-time)))
    (list 'high-date-time (logand (ash 100-nsec -32) #xffffffff)
	  'low-date-time  (logand 100-nsec #xffffffff))))

 ;; :immutable :compressed :hidden
(defun attr-to-flags (attr)
  (let (flags)
    (when (plusp (logand attr +FILE-ATTRIBUTE-READONLY+))
      (push :immutable flags))
    (when (plusp (logand attr +FILE-ATTRIBUTE-HIDDEN+))
      (push :hidden flags))
    (when (plusp (logand attr +FILE-ATTRIBUTE-COMPRESSED+))
      (push :compressed flags))
    ;; @@@ What about the others? Or are we just doing least common denominator?
    ;; These seem like the could be important or something.
    (when (plusp (logand attr +FILE-ATTRIBUTE-SYSTEM+))
      (push :system flags))
    (when (plusp (logand attr +FILE-ATTRIBUTE-ENCRYPTED+))
      (push :encrypted flags))
    (when (plusp (logand attr +FILE-ATTRIBUTE-ARCHIVE+))
      (push :archive flags))))

(defmethod file-info ((path string) &key (follow-links t))
  "Return information about the file described by PATH in a FILE-INFO
structure. If FOLLOW-LINKS is true (the default), then if PATH is a symbolic
link, return information about the file it's linked to, otherwise return
information about the link itself."
  (declare (ignore follow-links)) ;; @@@
  (with-wide-string (w-path path)
    (with-foreign-object (info '(:struct WIN32_FILE_ATTRIBUTE_DATA))
      (syscall (%get-file-attributes-ex w-path GetFileExInfoStandard info))
      (with-foreign-slots ((file-attributes creation-time last-access-time
			    last-write-time file-size-low file-size-high)
			   info (:struct WIN32_FILE_ATTRIBUTE_DATA))
	(make-file-info
	 :type (attr-to-dir-entry-type file-attributes)
	 :size (+ (ash file-size-high 32) file-size-low)
	 :flags (attr-to-flags file-attributes)
	 :creation-time (filetime-to-os-time creation-time)
	 :access-time (filetime-to-os-time last-access-time)
	 :modification-time (filetime-to-os-time last-write-time))))))

(defcfun ("GetFileAttributesW" %get-file-attributes)
    DWORD
  (file-name LPCTSTR))

(defconstant +INVALID-FILE-ATTRIBUTES+ #xffffffff)

(defmethod file-exists ((path string))
  "Check that a file with FILENAME exists at the moment. But it might not exist
for long."
  (with-wide-string (w-file (safe-namestring path))
    ;; I'm really just guessing with this whole thing. For example, are there
    ;; any other errors which would constitute being not found?
    (let ((result (%get-file-attributes w-file)))
      (if (= result +INVALID-FILE-ATTRIBUTES+)
	  (let ((err (get-last-error)))
	    (if (or (= err +ERROR-FILE-NOT-FOUND+)
		    (= err +ERROR-PATH-NOT-FOUND+))
		nil
		(error 'windows-error :error-code err
		       :format-control "file-exists failed.")))
	  t))))

;; Things for %get-final-path-name-by-handle flags:
(defconstant +FILE-NAME-NORMALIZED+ #x0 "Normalize it.")
(defconstant +FILE-NAME-OPENED+     #x8 "Give the name it was opened with.")
(defconstant +VOLUME-NAME-DOS+      #x0 "Use a DOS drive letter.")
(defconstant +VOLUME-NAME-GUID+     #x1 "Use a GUID volume.")
(defconstant +VOLUME-NAME-NONE+     #x4 "Don't even put a drive.")
(defconstant +VOLUME-NAME-NT+       #x2 "Add the device path.")

(defcfun ("GetFinalPathNameByHandleW" %get-final-path-name-by-handle)
  DWORD
  (file HANDLE)
  (file-path LPSTR)
  (file-path-size DWORD)
  (flags DWORD))

(defun get-handle-path (handle)
  (with-foreign-object (name 'WCHAR +MAX-PATH+)
    (let ((returned-chars
	    (%get-final-path-name-by-handle handle name +MAX-PATH+ 0)))
      (when (zerop returned-chars)
	(error 'windows-error
	       :error-code (get-last-error)
	       :format-control "get-handle-path failed."))
      (wide-string-to-lisp name))))

(defcfun ("DeleteFileW" %delete-file)
    BOOL
  (file-name LPCTSTR))

(defmethod os-delete-file ((path string))
  "Delete a file. Doesn't monkey with the name, which should be a string.
Doesn't operate on streams."
  (with-wide-string (w-path (safe-namestring path))
    (syscall (%delete-file w-path))))

(defmethod os-rename-file ((from string) (to string))
  "Rename the file ‘from’ to the file ‘to’. Doesn't monkey with the names,
which should be a strings. It doesn't operate on streams. CAUTION: If ‘to’
already exists, it will be replaced, effectively deleting it."
  ;; (syscall (posix-rename (safe-namestring from) (safe-namestring to))))
  ;; (syscall (posix-rename from to))
  (declare (ignore from to))
  (missing-implementation 'os-rename-file))

;; For sahre-mode
(defconstant +FILE-SHARE-READ+   #x00000001)
(defconstant +FILE-SHARE-WRITE+  #x00000002)
(defconstant +FILE-SHARE-DELETE+ #x00000004)

;; For desired-access?
(defconstant +DELETE+                   #x00010000)
(defconstant +READ-CONTROL+             #x00020000)
(defconstant +WRITE-DAC+                #x00040000)
(defconstant +WRITE-OWNER+              #x00080000)
(defconstant +SYNCHRONIZE+              #x00100000)
(defconstant +STANDARD-RIGHTS-REQUIRED+ #x000f0000)

(defconstant +STANDARD-RIGHTS-READ+     +READ-CONTROL+)
(defconstant +STANDARD-RIGHTS-WRITE+    +READ-CONTROL+)
(defconstant +STANDARD-RIGHTS-EXECUTE+  +READ-CONTROL+)
(defconstant +STANDARD-RIGHTS-ALL+      #x001f0000)
(defconstant +SPECIFIC-RIGHTS-ALL+      #x0000ffff)

(defconstant +GENERIC-READ+    #x80000000)
(defconstant +GENERIC-WRITE+   #x40000000)
(defconstant +GENERIC-EXECUTE+ #x20000000)
(defconstant +GENERIC-ALL+     #x10000000)

(defconstant +FILE-LIST-DIRECTORY+       #x001)
(defconstant +FILE-READ-DATA+            #x001)
(defconstant +FILE-ADD-FILE+             #x002)
(defconstant +FILE-WRITE-DATA+           #x002)
(defconstant +FILE-ADD-SUBDIRECTORY+     #x004)
(defconstant +FILE-APPEND-DATA+          #x004)
(defconstant +FILE-CREATE-PIPE-INSTANCE+ #x004)
(defconstant +FILE-READ-EA+              #x008)
(defconstant +FILE-WRITE-EA+             #x010)
(defconstant +FILE-EXECUTE+              #x020)
(defconstant +FILE-TRAVERSE+             #x020)
(defconstant +FILE-DELETE-CHILD+         #x040)
(defconstant +FILE-READ-ATTRIBUTES+      #x080)
(defconstant +FILE-WRITE-ATTRIBUTES+     #x100)
(defconstant +FILE-ALL-ACCESS+
  (logior +STANDARD-RIGHTS-REQUIRED+ +SYNCHRONIZE+ #x1ff))

;; For creation-disposition
(defconstant +CREATE-NEW+        1 "Creates a new file, if it does not exist.")
(defconstant +CREATE-ALWAYS+     2 "Creates a new file, always.")
(defconstant +OPEN-EXISTING+     3 "Opens a file or device, only if it exists.")
(defconstant +OPEN-ALWAYS+       4 "Opens a file, always.")
(defconstant +TRUNCATE-EXISTING+ 5 "Truncates it, if it exists.")

;; For flags-and-attributes:
;; Attributes:
(defconstant +FILE-ATTRIBUTE-READONLY+  #x0001)
(defconstant +FILE-ATTRIBUTE-HIDDEN+    #x0002)
(defconstant +FILE-ATTRIBUTE-SYSTEM+    #x0004)
(defconstant +FILE-ATTRIBUTE-ARCHIVE+   #x0020)
(defconstant +FILE-ATTRIBUTE-NORMAL+    #x0080)
(defconstant +FILE-ATTRIBUTE-TEMPORARY+ #x0100)
(defconstant +FILE-ATTRIBUTE-OFFLINE+   #x1000)
(defconstant +FILE-ATTRIBUTE-ENCRYPTED+ #x4000)

;; Flags: (are these really right?)
(defconstant +FILE-FLAG-FIRST-PIPE-INSTANCE+ #x00080000)
(defconstant +FILE-FLAG-OPEN-NO-RECALL+      #x00100000)
(defconstant +FILE-FLAG-OPEN-REPARSE-POINT+  #x00200000)
(defconstant +FILE-FLAG-SESSION-AWARE+       #x00800000)
(defconstant +FILE-FLAG-POSIX-SEMANTICS+     #x01000000)
(defconstant +FILE-FLAG-BACKUP-SEMANTICS+    #x02000000)
(defconstant +FILE-FLAG-DELETE-ON-CLOSE+     #x04000000)
(defconstant +FILE-FLAG-SEQUENTIAL-SCAN+     #x08000000)
(defconstant +FILE-FLAG-RANDOM-ACCESS+       #x10000000)
(defconstant +FILE-FLAG-NO-BUFFERING+        #x20000000)
(defconstant +FILE-FLAG-OVERLAPPED+          #x40000000)
(defconstant +FILE-FLAG-WRITE-THROUGH+       #x80000000)

;; Are these corrrect?
(defconstant +SECURITY-ANONYMOUS+          #.(ash 0 16))
(defconstant +SECURITY-IDENTIFICATION+     #.(ash 1 16))
(defconstant +SECURITY-IMPERSONATION+      #.(ash 2 16))
(defconstant +SECURITY-DELEGATION+         #.(ash 3 16))

(defconstant +SECURITY-CONTEXT-TRACKING+   #x00040000)
(defconstant +SECURITY-EFFECTIVE-ONLY+     #x00080000)

(defconstant +SECURITY-SQOS-PRESENT+       #x00100000)
(defconstant +SECURITY-VALID-SQOS-FLAGS+   #x001f0000)

(defcfun ("CreateFileW" %create-file)
    HANDLE
  (file-name 		 LPCTSTR)
  (desired-access 	 DWORD)
  (share-mode 		 DWORD)
  (security-attributes   LPSECURITY_ATTRIBUTES)
  (creation-disposition  DWORD)
  (flags-and-attributes  DWORD)
  (template-file 	 HANDLE))

(defconstant +FILE-BEGIN+   0)
(defconstant +FILE-CURRENT+ 1)
(defconstant +FILE-END+     2)

(defcfun ("SetFilePointerEx" %set-file-pointer-ex)
    BOOL
  (file             HANDLE)
  (distance-to-move LARGE_INTEGER)
  (new-file-pointer PLARGE_INTEGER)
  (move-method      DWORD))

(defcfun ("CloseHandle" %close-handle)
    BOOL
   (object HANDLE))

;; @@@ merge with with-os-file?
(defmacro with-windows-file ((var pathname access &key
				  share-mode
				  security
				  disposition
				  flags)
			     &body body)
  "Evaluate the body with the variable VAR bound to a Windows file handle
opened on PATHNAME with ACCESS. Arguments are as in the arguments to
%CREATE-FILE."
  (with-unique-names (w-path w-share-mode w-security w-disposition w-flags)
    `(let ((,w-share-mode (or ,share-mode
			      (logior +FILE-SHARE-READ+ +FILE-SHARE-WRITE+)))
	   (,w-security (or ,security (null-pointer)))
	   (,w-disposition (or ,disposition
			       (if (not (zerop (logand ,access +GENERIC-READ+)))
				   +OPEN-EXISTING+
				   +CREATE-NEW+)))
	   (,w-flags (or ,flags 0))
	   ,var)
       (unwind-protect
	    (progn
	      ;; (format t "access = ~x share-mode = ~s disposition = ~s~%"
	      ;; 	      ,access ,w-share-mode ,w-disposition)
	      (with-wide-string (,w-path ,pathname)
		(setf ,var (%create-file ,w-path
					 ,access ,w-share-mode ,w-security
					 ,w-disposition ,w-flags
					 (null-pointer))))
	      (when (= (pointer-address ,var) +INVALID-HANDLE-VALUE+)
		(error 'windows-error :error-code (get-last-error)
		       :format-control "Failed to open the file."))
	      ,@body)
	 (when (and ,var (not (= (pointer-address ,var)
				 +INVALID-HANDLE-VALUE+)))
	   (syscall (%close-handle ,var)))))))

;; @@@ the if-* handling isn't exactly right yet
(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro with-os-file ((var pathname &key
			     (direction :input)
			     if-exists
			     if-does-not-exist) &body body)
  "Evaluate the body with the variable VAR bound to a Windows file handle
opened on FILENAME. DIRECTION, IF-EXISTS, and IF-DOES-NOT-EXIST are simpler
versions of the keywords used in Lisp open.
  DIRECTION         - supports :INPUT, :OUTPUT, and :IO.
  IF-EXISTS         - supports :ERROR and :APPEND.
  IF-DOES-NOT-EXIST - supports :ERROR, and :CREATE.
"
  (with-unique-names (w-path access disposition)
    `(let ((,access (ecase ,direction
		      (:input +GENERIC-READ+)
		      (:output +GENERIC-WRITE+)
		      (:io (logior +GENERIC-READ+ +GENERIC-WRITE+))))
	   (,disposition (cond
			   ((eq ,if-exists :error) +CREATE-NEW+)
			   ((eq ,if-exists :append) +OPEN-EXISTING+)
			   ((eq ,if-does-not-exist :error) +OPEN-EXISTING+)
			   ((eq ,if-does-not-exist :create) +CREATE-ALWAYS+)
			   ((and (not (and ,if-exists ,if-does-not-exist))
				 (or (eq ,direction :input)
				     (eq ,direction :io)))
			    +OPEN-EXISTING+)
			   ((and (not (and ,if-exists ,if-does-not-exist))
				 (or (eq ,direction :output)))
			    +CREATE-NEW+)
			   (t
			    (error 'opsys-error
				   :format-control
				   "Unsupported values for if-exists and/or ~
                                    if-does-not-exist and direction."))))
	   ,var)
       (unwind-protect
	    (progn
	      (with-wide-string (,w-path ,pathname)
		(setf ,var (%create-file
			    ,w-path
			    ,access
			    0	 ; (logior +FILE-SHARE-READ+ +FILE-SHARE-WRITE+)
			    (null-pointer) ; SECURITY_ATTRIBUTES
			    ,disposition
			    0
			    (null-pointer))))
	      (when (= (pointer-address ,var) +INVALID-HANDLE-VALUE+)
		(error 'windows-error :error-code (get-last-error)
		       :format-control "Failed to open the file."))
	      (when (eq ,if-exists :append)
		(syscall (%set-file-pointer-ex ,var 0 (null-pointer)
					       +FILE-END+)))
	      ,@body)
	 (when (and ,var (not (= (pointer-address ,var) +INVALID-HANDLE-VALUE+)))
	   (syscall (%close-handle ,var))))))))

(defmacro as-32bit-unsigned (n) `(logand (1- (expt 2 32)) (lognot (1- (- ,n)))))
(defconstant +STD-INPUT-HANDLE+  (as-32bit-unsigned -10)) ; CONIN$
(defconstant +STD-OUTPUT-HANDLE+ (as-32bit-unsigned -11)) ; CONOUT$
(defconstant +STD-ERROR-HANDLE+  (as-32bit-unsigned -12)) ; CONOUT$

(defcfun ("GetStdHandle" %get-std-handle :convention :stdcall)
    HANDLE
   (nStdHandle DWORD)) ; in

(defcstruct BY_HANDLE_FILE_INFORMATION
  (file-attributes       DWORD)
  (creation-time         (:struct FILETIME))
  (last-access-time      (:struct FILETIME))
  (last-write-time       (:struct FILETIME))
  (volume-serial-number  DWORD)
  (file-size-high        DWORD)
  (file-size-low         DWORD)
  (number-of-links       DWORD)
  (file-index-high       DWORD)
  (file-index-low        DWORD))

(defctype PBY_HANDLE_FILE_INFORMATION
    (:pointer (:struct BY_HANDLE_FILE_INFORMATION)))
(defctype LPBY_HANDLE_FILE_INFORMATION
    (:pointer (:struct BY_HANDLE_FILE_INFORMATION)))

(defcfun ("GetFileInformationByHandle" %get-file-information-by-handle)
    BOOL
 (file             HANDLE)
 (file-information LPBY_HANDLE_FILE_INFORMATION))

;; Values of a FILE_INFO_BY_HANDLE_CLASS
(defconstant +file-basic-info+            0)
(defconstant +file-rename-info+           3)
(defconstant +file-disposition-info+      4)
(defconstant +file-allocation-info+       5)
(defconstant +file-end-of-file-info+      6)
(defconstant +file-io-priority-hint-info+ 12)

(defcstruct FILE_BASIC_INFO
  (creation-time    LARGE_INTEGER)
  (last-access-time LARGE_INTEGER)
  (last-write-time  LARGE_INTEGER)
  (change-time      LARGE_INTEGER)
  (file-attributes  DWORD))
(defctype PFILE_BASIC_INFO (:pointer (:struct FILE_BASIC_INFO)))

(defcfun ("SetFileInformationByHandle" %set-file-information-by-handle)
    BOOL
 (file                   HANDLE)
 ;;(file-information-class FILE_INFO_BY_HANDLE_CLASS)
 (file-information-class :unsigned-int)	; :(
 (file-information       LPVOID)
 (buffer-size            DWORD))

(defmethod set-file-time ((path string) &key access-time modification-time)
  "Set the given times on PATH. The times are OS-TIME structures. Either
time can be :NOW to use the current time."
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((now-ish (get-current-filetime-as-integer)))
    (flet ((flank (time)
	     (if time
		 (if (eq time :now)
		     now-ish
		     (os-time-to-filetime-integer time))
		 0)))
      (with-windows-file (handle path (logior +FILE-READ-ATTRIBUTES+
					      +FILE-WRITE-ATTRIBUTES+)
				 :share-mode +FILE-SHARE-READ+
				 :disposition +OPEN-EXISTING+
				 ;; BACKUP-SEMANTICS is necessary to open
				 ;; a directory!
				 :flags +FILE-FLAG-BACKUP-SEMANTICS+)
	(with-foreign-objects ((file-info '(:struct FILE_BASIC_INFO)))
	  (with-foreign-slots ((creation-time last-access-time last-write-time
				change-time file-attributes) file-info
			       (:struct FILE_BASIC_INFO))
	    ;; Apparently, even though it doesn't seem to mention it in the
	    ;; Microsoft documentation, you can set a time to zero to leave
	    ;; it unchanged.
	    (setf creation-time 0
		  last-access-time (flank access-time)
		  change-time 0
		  last-write-time (flank modification-time)
		  file-attributes 0)
	    (syscall (%set-file-information-by-handle
		      handle +file-basic-info+
		      file-info
		      (foreign-type-size '(:struct FILE_BASIC_INFO))))))))))


(defconstant +SYMBOLIC-LINK-FLAG-FILE+      #x0 "Link target is a file.")
(defconstant +SYMBOLIC-LINK-FLAG-DIRECTORY+ #x1 "Link target is a directory.")
(defconstant +SYMBOLIC-LINK-FLAG-ALLOW-UNPRIVILEGED-CREATE #x2
  "Allow creation of symbolic links when not privileged.
Developer Mode has to be enabled.")

(defcfun ("CreateSymbolicLinkW" %create-symbolic-link) MS-BOOLEAN
  ;; (lpSymlinkFileName LPCSTR)
  ;; (lpTargetFileName LPCSTR)
  (symlink-file-name :string)
  (target-file-name :string)
  (flags DWORD))

(defmethod make-symbolic-link ((from string) (to string))
  "Make a symbolic link from FROM to TO."
  (with-wide-string (w-from from)
    (with-wide-string (w-to to)
      (syscall (%create-symbolic-link w-from w-to 0)))))

(defmethod symbolic-link-target ((link-name string))
  "Return the target of the symbolic link."
  (with-os-file (handle link-name)
    (get-handle-path handle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directories

(defcfun ("SetCurrentDirectoryW" %set-current-directory)
    BOOL
  (path-name LPCTSTR))

;; @@@ This isn't OS specific, but implementation specific, so it should be
;; moved to base.lisp or opsys.lisp or something.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Set on implementations where we need to *default-pathname-defaults*
  ;; when we change the system current directory, so that load, open, etc.
  ;; will work as expected.
  #+(or sbcl excl abcl) (config-feature :t-os-cd-dpd))

(defun change-directory (&optional (path (user-homedir-pathname)))
  "Change the current directory to DIR. Defaults to (user-homedir-pathname) ~
if not given."
  ;; @@@ We should put a thread lock around this if we want it to be thread safe.
  (let ((our-path
	 (typecase path
	   (pathname (safe-namestring path))
	   (string path)
	   (null (safe-namestring (user-homedir-pathname))))))
    (when (char/= #\\ (aref our-path (1- (length our-path))))
      (setf our-path (s+ our-path "\\")))
    (with-wide-string (w-path our-path)
      (syscall (%set-current-directory w-path))
      #+t-os-cd-dpd
      (let ((tn (ignore-errors (truename our-path))))
	(when tn
	  (setf *default-pathname-defaults* tn))))))

(defcfun ("GetCurrentDirectoryW" %get-current-directory)
    DWORD
  (buffer-length DWORD)
  (buffer LPTSTR))

(defun current-directory ()
  "Return the full path of the current working directory as a string."
  (let ((len (%get-current-directory 0 (null-pointer)))
	result)
    (with-foreign-object (dir 'TCHAR len)
      (setf result (%get-current-directory len dir))
      (when (/= result (1- len))
	(error 'windows-error :error-code (get-last-error)
	       :format-control "Failed to get the current directory."))
      (wide-string-to-lisp dir))))

(defcfun ("CreateDirectoryW" %create-directory)
    BOOL
  (path-name LPCTSTR)
  (security-attributes LPSECURITY_ATTRIBUTES))

(defmethod make-directory ((path string) &key (mode #o755))
  "Make a directory."
  (declare (ignore mode))
  (with-wide-string (w-path path)
    (syscall (%create-directory w-path (null-pointer)))))

(defcfun ("RemoveDirectoryW" %remove-directory)
    BOOL
  (path-name LPCTSTR))

(defmethod delete-directory ((path string))
  "Delete a directory."
  (with-wide-string (w-path path)
    (syscall (%remove-directory w-path))))

(defmethod directory-p ((path string))
  "Return true if PATH is a directory."
  (typecase path
    ;; @@@ We should try to do better, at least for file-streams
    ;; or os-streams.
    ;; (stream (return-from directory-p nil))
    ((or string pathname)
     (with-wide-string (w-file path)
       (let (result)
	 (setf result (%get-file-attributes w-file))
	 (if (= result +INVALID-FILE-ATTRIBUTES+)
	     (let ((err (get-last-error)))
	       (if (or (= err +ERROR-FILE-NOT-FOUND+)
		       (= err +ERROR-PATH-NOT-FOUND+))
		   nil
		   (error 'windows-error :error-code err
			  :format-control "file-exists failed.")))
	     (if (plusp (logand result +FILE-ATTRIBUTE-DIRECTORY+))
		 t
		 nil)))))))

;; This has similar issues as file-exists.
(defmethod probe-directory ((dir string))
  "Something like probe-file but for directories."
  (with-wide-string (w-file dir)
    (let ((result (%get-file-attributes w-file)))
      (if (= result +INVALID-FILE-ATTRIBUTES+)
	  (let ((err (get-last-error)))
	    (if (or (= err +ERROR-FILE-NOT-FOUND+)
		    (= err +ERROR-PATH-NOT-FOUND+))
		nil
		(error 'windows-error :error-code err
		       :format-control "file-exists failed.")))
	  (if (plusp (logand result +FILE-ATTRIBUTE-DIRECTORY+))
	      t
	      nil)))))

(defcstruct WIN32_FIND_DATA
  (file-attributes DWORD)
  (creation-time (:struct FILETIME))
  (last-access-time (:struct FILETIME))
  (last-write-time (:struct FILETIME))
  (file-size-high DWORD)
  (file-size-low DWORD)
  (reserved0 DWORD)
  (reserved1 DWORD)
  (file-name TCHAR :count #.+MAX-PATH+)
  (alternate-file-name TCHAR :count 14))

(defctype PWIN32_FIND_DATA (:pointer (:struct WIN32_FIND_DATA)))
(defctype LPWIN32_FIND_DATA (:pointer (:struct WIN32_FIND_DATA)))

(defcfun ("FindFirstFileW" %find-first-file)
    HANDLE
  (file-name LPCTSTR)
  (find-file-data LPWIN32_FIND_DATA))

(defconstant +ERROR-NO-MORE-FILES+ 18)

(defcfun ("FindNextFileW" %find-next-file)
    BOOL
  (find-file HANDLE)
  (find-file-data LPWIN32_FIND_DATA))

(defcfun ("FindClose" %find-close)
    BOOL
  (find-file HANDLE))

;; @@@ If we wanted to be more complete we could open the file and call
;; GetFileType, but I imagine it would slow things quite a bit.
(defun attr-to-dir-entry-type (attr)
  "Return a dir-entry-type value given a file-attribute value."
  (cond
    ((plusp (logand attr +FILE-ATTRIBUTE-DIRECTORY+))     :directory)
    ((or (= attr +FILE-ATTRIBUTE-NORMAL+)
	 ;; It doesn't have any other flags than these:
	 (zerop (logand attr (lognot (logior +FILE-ATTRIBUTE-ARCHIVE+
					     +FILE-ATTRIBUTE-HIDDEN+
					     +FILE-ATTRIBUTE-READONLY+)))))
     :regular)
    ((plusp (logand attr +FILE-ATTRIBUTE-DEVICE+))        :device)
    ((plusp (logand attr +FILE-ATTRIBUTE-REPARSE-POINT+)) :link)
    (t :other)))

;; @@@ Perhaps we should use FindFirstFileExW on Windows 7 and above since it's
;; supposedly faster.

;; This is a highly anaphoric macro, since we just intend to use it internally.
;; The body has to be a valid LOOP clause.
;; ITEM is the directory item. DIR-LIST is the results of the loop.
;; IS-DIR is true if the item is a directory.
;; It gets DIR APPEND-TYPE FULL OMIT-HIDDEN from the lexical environment.
;;
;; @@@ This probably should be the system specific part and then read-directory
;; and map-directory could be in the generic code, but I'd have to work out
;; at least all the symbols mentioned above. see unix/filesystem.lisp

(defmacro %with-directory-entries ((&key result) &body body)
				     ;; dir append-type full omit-hidden
  "Implement directory iteration. See the documentation for read-directory or
map-directory for more information."
  `(block nil
     (let ((find-dir (s+ actual-dir "\\*")) ;; for find-first-file
	   handle dir-list item is-dir)
       (unwind-protect
	  (with-foreign-object (find-data '(:struct WIN32_FIND_DATA))
	    (with-wide-string (w-dir find-dir)
	      (setf handle (%find-first-file w-dir find-data))
	      (when (= (pointer-address handle) +INVALID-HANDLE-VALUE+)
		(when errorp
		  (cerror "Just go on."
			  'windows-error :error-code (get-last-error)
			  :format-control
			  "read-directory failed to read first file."))
		(return nil)))
	    (setf dir-list
		  (loop
		     :with real-name
		     :do
		     (setf item nil)
		     (with-foreign-slots ((file-name file-attributes) find-data
					  (:struct WIN32_FIND_DATA))
		       (setf real-name (wide-string-to-lisp file-name))
		       (when (or (not omit-hidden)
				 (and (zerop (logand file-attributes
						     +FILE-ATTRIBUTE-HIDDEN+))
				      (not (hidden-file-name-p real-name))))
			 (setf is-dir
			       (plusp (logand file-attributes
					      +FILE-ATTRIBUTE-DIRECTORY+))
			       item
			       (if full
				   (make-dir-entry
				    :name real-name
				    :type (attr-to-dir-entry-type
					   file-attributes)
				    :inode nil)
				   real-name))))
		     :when item
		     ;;:collect item
		     ,@body
		     :while (not (zerop (%find-next-file handle find-data)))))
	    (when (and (/= (get-last-error) +ERROR-NO-MORE-FILES+)
		       errorp)
	      (cerror "Just go on."
		      'windows-error :error-code (get-last-error)
		     :format-control
		     "read-directory failed to read next file.")))
	 (when (and handle (/= (pointer-address handle) +INVALID-HANDLE-VALUE+))
	   (syscall (%find-close handle))))
       (progn ,@result)))) ;; @@@ was dir-list

#|
(defun OLD-read-directory (&key dir append-type full omit-hidden)
  "Return a list of the file names in DIR as strings. DIR defaults to the ~
current directory. If APPEND-TYPE is true, append a character to the end of ~
the name indicating what type of file it is. Indicators are:
  / : directory
  @ : symbolic link
  | : FIFO (named pipe)
  = : Socket
  > : Doors
If FULL is true, return a list of dir-entry structures instead of file name ~
strings. Some dir-entry-type keywords are:
  :unknown :pipe :character-device :dir :block-device :regular :link :socket
  :whiteout :undefined
If OMIT-HIDDEN is true, do not include entries that start with ‘.’.
"
  (declare (ignore append-type))
  (when (not dir)
    (setf dir "."))			; XXX ???

  (setf dir (s+ dir "\\*"))
  (let (handle dir-list)
    (unwind-protect
      (with-foreign-object (find-data '(:struct WIN32_FIND_DATA))
	(with-wide-string (w-dir dir)
	  (setf handle (%find-first-file w-dir find-data))
	  (when (= (pointer-address handle) +INVALID-HANDLE-VALUE+)
	    (error 'windows-error :error-code (get-last-error)
		   :format-control
		   "read-directory failed to read first file.")))
	(setf dir-list
	      (loop
		 :with entry :and name
		 :do
		 (setf entry nil)
		 (with-foreign-slots ((file-name file-attributes) find-data
				      (:struct WIN32_FIND_DATA))
		   (setf name (wide-string-to-lisp file-name))
		   (when (or (not omit-hidden)
			     (and (zerop (logand file-attributes
						 +FILE-ATTRIBUTE-HIDDEN+))
				  (not (hidden-file-name-p name))))
		     (setf entry
			   (if full
			       (make-dir-entry
				:name name
				:type (attr-to-dir-entry-type file-attributes)
				:inode nil)
			       name))))
		 :when entry
		 :collect entry
		 :while (not (zerop (%find-next-file handle find-data)))))
	(when (/= (get-last-error) +ERROR-NO-MORE-FILES+)
	  (error 'windows-error :error-code (get-last-error)
		 :format-control "read-directory failed to read next file.")))
      (when (and handle (/= (pointer-address handle) +INVALID-HANDLE-VALUE+))
	(syscall (%find-close handle))))
  dir-list))
|#

(defun read-directory (&key dir append-type full omit-hidden (errorp t))
  "Return a list of the file names in DIR as strings. DIR defaults to the ~
current directory. If APPEND-TYPE is true, append a character to the end of ~
the name indicating what type of file it is. Indicators are:
  / : directory
  @ : symbolic link
  | : FIFO (named pipe)
  = : Socket
  > : Doors
If FULL is true, return a list of dir-entry structures instead of file name ~
strings. Some dir-entry-type keywords are:
  :unknown :pipe :character-device :directory :block-device :regular :link
  :socket :whiteout :undefined
Be aware that DIR-ENTRY-TYPE type can't really be relied on, since many
systems return :UNKNOWN or something, when the actual type can be determined
by FILE-INFO-TYPE.
If OMIT-HIDDEN is true, do not include entries that start with ‘.’.
"
  (declare (ignore append-type))
  (declare (type (or string null) dir) (type boolean full))
  (let ((actual-dir (or dir ".")))
    (%with-directory-entries (:result (dir-list))
      :collect item)))

;; @@@ I'm sure this could use some speeding up.
;; @@@ Try to keep this and read-directory exactly like the ones in
;; unix/filesytem.lisp so that we can get rid of this duplication.
(defun map-directory (function
		      &key dir append-type full omit-hidden collect recursive
			(errorp t) post-dir-function)
  "Call FUNCTION with the file name of each file in directory DIR. DIR defaults ~
to the current directory. If APPEND-TYPE is true, append a character to the end ~
of the name indicating what type of file it is. Indicators are:
  / : directory
  @ : symbolic link
  | : FIFO (named pipe)
  = : Socket
  > : Doors
If FULL is true, call FUNCTION with a list of dir-entry structures instead of ~
file name strings. Some dir-entry-type keywords are:
  :unknown :pipe :character-device :directory :block-device :regular :link
  :socket :whiteout :undefined
Be aware that DIR-ENTRY-TYPE type can't really be relied on, since many
systems return :UNKNOWN or something, when the actual type can be determined
by FILE-INFO-TYPE.
If OMIT-HIDDEN is true, do not include entries that start with ‘.’.
If COLLECT is true, return the results of calling FUNCTION as a list.
If RECURSIVE is true, descend breadth-first into sub-directories.
If ERRORP is true, signal correctable errors. The default is T.
POST-DIR-FUNCTION is called with the directory only when RECURSIVE is true and
all the files in directory have been enumerated.
"
  ;; (declare (ignore append-type))
  (declare (type (or string null) dir) (type boolean full collect)
	   (type function-designator post-dir-function))
  (let ((actual-dir (or dir ".")))
    (labels ((join-dir (dir name)
	       "Tack the directory on the front."
	       (concatenate 'string dir *directory-separator-string* name))
	     (thingy (item real-name)
	       "Return an appropriately fixed dir-entry or joined name."
	       (or (and dir
			(if full
			    (and (setf (dir-entry-name item)
				       (join-dir dir real-name))
				 item)
			    (join-dir dir real-name)))
		   item))
	     (recursive-call (dir)
	       (map-directory function :dir dir
			      :append-type append-type
			      :full full
			      :omit-hidden omit-hidden
			      :collect collect
			      :recursive t
			      :errorp errorp
			      :post-dir-function post-dir-function)))
      (if collect
	  ;; Collect results breadth first.
	  (let (sub-dirs files)
	    (setf files
		  (%with-directory-entries (:result (dir-list))
		    :collect (funcall function (thingy item real-name))
		    :when (and recursive is-dir
			       (not (superfluous-file-name-p real-name))
			       (not (and omit-hidden
					 (hidden-file-name-p real-name))))
		    :do
		    (push (or (and dir (join-dir dir real-name))
			      real-name) sub-dirs)))
	    (prog1
		(if sub-dirs
		    ;; @@@ Use of flatten here is probably inappropriate.
		    (flatten (nconc files (mapcar #'recursive-call sub-dirs)))
		    files)
	      (when post-dir-function
		(funcall post-dir-function actual-dir))))
	  ;; Don't collect, just funcall and count.
	  (let ((count 0) sub-dirs)
	    (%with-directory-entries (:result (count))
	      :do
	      (funcall function (thingy item real-name))
	      (incf count)
	      :when (and recursive is-dir
			 (not (superfluous-file-name-p real-name))
			 (not (and omit-hidden (hidden-file-name-p real-name))))
	      :do
	      (push (or (and dir (join-dir dir real-name))
			real-name) sub-dirs))
	    (prog1
		(+ count (reduce #'+ (mapcar #'recursive-call sub-dirs)))
	      (when post-dir-function
		(funcall post-dir-function actual-dir))))))))


(defmacro without-access-errors (&body body)
  "Evaluate the body while ignoring typical file access error from system
calls. Returns NIL when there is an error."
  `(ignore-errors ,@body))

;; Since this and the following are SO FAR the same as on POSIX, perhaps we
;; should move them to opsys.lisp?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file locking

#|
(with-foreign-object (o '(struct overlapped))
  (with-foreign-slots ((offset-pointer event) o (struct overlapped))
    (print event)
    (print offset-pointer)
    (print
     (foreign-slot-value offset-pointer '(union foreign-offset-pointer)
                         'offset))
    (with-foreign-slots ((offset-low offset-high) offset-pointer
                         (struct foreign-offset))
      (print offset-low)
      (print offset-high)
      (setf offset-low 0
            offset-high 0)
      (print
       (foreign-slot-value offset-pointer '(struct foreign-offset)
                           'offset-high))
      (print
       (foreign-slot-value offset-pointer '(struct foreign-offset)
                           'offset-low))))
  (values))
|#

(defun set-overlapped (overlapped offset-low-in offset-high-in event-handle)
  (with-foreign-slots ((offset-pointer event) overlapped (:struct OVERLAPPED))
    (with-foreign-slots ((offset-low offset-high)
			  (foreign-slot-pointer
			   offset-pointer
			   '(:union foreign-offset-pointer)
			   'offset)
			 (:struct foreign-offset))
      ;; (setf (foreign-slot-value offset-pointer
      ;; 				'(:struct foreign-offset) 'offset-low)
      ;; 	    offset-low-in
      ;; 	    (foreign-slot-value offset-pointer
      ;; 				'(:struct foreign-offset) 'offset-high)
      ;; 	    offset-high-in)
      (setf offset-low offset-low-in
	    offset-high offset-high-in)
      (setf event event-handle))))

(defconstant +LOCKFILE-EXCLUSIVE-LOCK+ #x00000002
  "Request an exclusive lock. Otherwise a shared lock is requested.")
(defconstant +LOCKFILE-FAIL-IMMEDIATELY+ #x00000001
  "The function returns immediately if it is unable to acquire the requested
lock. Otherwise, it waits.")

(defcfun ("LockFileEx" %lock-file-ex)
    BOOL
  (file HANDLE)
  (flags DWORD)
  (reserved DWORD)
  (number-of-bytes-to-lock-low DWORD)
  (number-of-bytes-to-lock-high DWORD)
  (overlapped LPOVERLAPPED))

(defcfun ("UnlockFileEx" %unlock-file-ex)
    BOOL
  (file HANDLE)
  (reserved DWORD)
  (number-of-bytes-to-unlock-low DWORD)
  (number-of-bytes-to-unlock-high DWORD)
  (overlapped LPOVERLAPPED))

(defmacro with-locked-file ((pathname &key (lock-type :write) (timeout 3)
				      (increment .1))
			    &body body)
  "Evaluate BODY with PATHNAME locked. Only wait for TIMEOUT seconds to get a
lock, checking at least every INCREMNT seconds."
  (declare (ignore timeout increment))
  (with-unique-names (locked flags handle overlapped the-lock-type w-path)
    `(let ((,locked nil) (,handle nil) (,the-lock-type ,lock-type))
       (with-foreign-object (,overlapped '(:struct OVERLAPPED))
	 (unwind-protect
	      (let ((,flags (logior (ecase ,the-lock-type
				      (:write 0)
				      (:read +LOCKFILE-EXCLUSIVE-LOCK+))
				    +LOCKFILE-FAIL-IMMEDIATELY+)))
		(with-wide-string (,w-path ,pathname)
		  (setf ,handle (%create-file
				 ,w-path
				 (logior +GENERIC-READ+ +GENERIC-WRITE+)
				 (if (eq ,the-lock-type :write)
				     +FILE-SHARE-READ+
				     0)
				 (null-pointer)	; SECURITY_ATTRIBUTES
				 +OPEN-EXISTING+ ; disposition
				 0
				 (null-pointer))))
		(set-overlapped ,overlapped 0 0 0)
		;; We lock the maximum number of bytes to effectively lock the
		;; whole file.
		(syscall (%lock-file-ex ,handle ,flags 0 #xffffffff #xffffffff
					,overlapped))
		(setf ,locked t)
		,@body)
	   (when ,locked
	     (syscall (%unlock-file-ex ,handle 0 #xffffffff #xffffffff
				       ,overlapped)))
	   (when ,handle
	     (syscall (%close-handle ,handle))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System Commands?

(defcfun ("GetBinaryTypeW" %get-binary-type)
    BOOL
  (application-name LPCTSTR)
  (binary-type LPDWORD))

(defconstant +SCS_32BIT_BINARY+ 0 "A 32-bit Windows application")
(defconstant +SCS-DOS-BINARY+   1 "An MS-DOS application.")
(defconstant +SCS-WOW-BINARY+   2 "A 16-bit Windows application.")
(defconstant +SCS-PIF-BINARY    3 "A PIF file that executes an MS-DOS application.")
(defconstant +SCS-POSIX-BINARY+ 4 "A POSIX application.")
(defconstant +SCS-OS216-BINARY  5 "A 16-bit OS/2 application")
(defconstant +SCS_64BIT_BINARY+ 6 "A 64-bit Windows application.")

;; This relys on the positional nature of the above constants.
(defparameter *binary-types*
  #(#(:32BIT "A 32-bit Windows application")
    #(:DOS   "An MS-DOS application.")
    #(:WOW   "A 16-bit Windows application.")
    #(:PIF   "A PIF file that executes an MS-DOS application.")
    #(:POSIX "A POSIX application.")
    #(:OS216 "A 16-bit OS/2 application")
    #(:64BIT "A 64-bit Windows application.")))

(defun get-binary-type (pathname)
  "Return the executable binary for PATHNAME. The return value is one of the
keywords from *BINARY-TYPE*."
  (with-wide-string (w-path pathname)
    (with-foreign-object (binary-type 'DWORD)
      (syscall (%get-binary-type w-path binary-type))
      (aref (aref *binary-types* (mem-ref binary-type 'DWORD)) 0))))

(defun binary-type-description (binary-type)
  "Return the description of BINARY-TYPE, as returned by GET-BINARY-TYPE."
  (aref (find binary-type *binary-types* :key (_ (aref _ 0))) 1))

(defparameter *executable-types* '(".EXE" ".COM" ".BAT")
  "Horrible. We leave the dot in for ease of comparison.")

(defun command-test (test path &optional path2)
  "Return true if the command passes the test. Do special platform specific
processing, like adding `.exe' on windows. If path2 is provided, test takes
two arguments."
  ;; Make equality tests case insensitive.
  (when (and (eq test #'equal) path2)
    (setf test #'string-equal))
  (if path2
      (or (funcall test path path2)
	  (some (lambda (suffix)
		  (funcall test (s+ path suffix) path2))
		*executable-types*))
      (or (funcall test path)
	  (some (lambda (suffix)
		  (funcall test (s+ path suffix)))
		*executable-types*))))

(defun is-executable (path &key user regular)
  "Return true if the PATH is executable by the USER. USER defaults to the
current effective user. If REGULAR is true also check if it's a regular file."
  (declare (ignore user regular))
  ;; @@@
  ;; I think checking the for the "Read & execute" permission is not really
  ;; what we mean here. But neither is get-binary-type, since that won't
  ;; capture things like *.bat files.
  ;; We really mean something that will ‘work’ with %create-process.

  ;; This is just a terrible solution.
  (let ((tail (subseq path (max 0 (- (length path) 4)))))
    (or (and (member tail *executable-types* :test #'equalp) t)
	(command-test #'file-exists path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application paths

;; App name isn't really optional.
(defvar *app-name* nil)
(defun app-name ()
  (or *app-name* (setf *app-name* "LispApps"))) ;; XXX @@@

(defun config-dir (&optional (app-name (app-name)))
  "Where user specific configuration files should be stored."
  (s+ (env "USERPROFILE") "\\AppData\\Local\\" app-name "\\"))

(defun data-dir (&optional (app-name (app-name)))
  "Where user specific data files should be stored."
  (s+ (env "USERPROFILE") "\\AppData\\Local\\" app-name "\\"))

(defun data-path (&optional app-name)
  "Search path for user specific data files."
  (declare (ignore app-name))
  nil)

(defun config-path (&optional (app-name (app-name)))
  "Search path for user specific configuration files."
  (list
   (s+ (env "USERPROFILE") "\\AppData\\Local\\" app-name "\\")
   (s+ (env "PROGRAMDATA") "\\" app-name "\\config\\")))

(defun cache-dir (&optional app-name)
  "Directory where user specific non-essential data files should be stored."
  (declare (ignore app-name))
  nil)

(defun runtime-dir (&optional app-name)
  "Directory where user-specific non-essential runtime files and other file
objects should be stored."
  (declare (ignore app-name))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File systems

(defcfun ("FindFirstVolumeW" %find-first-volume)
    HANDLE
  (volume-name LPTSTR)			; out
  (buffer-length DWORD))		; in

(defcfun ("FindNextVolumeW" %find-next-volume)
    BOOL
  (find-volume HANDLE)			; in
  (volume-name LPTSTR)			; out
  (buffer-length DWORD))		; int

(defcfun ("FindVolumeClose" %find-volume-close)
    BOOL
  (find-volume HANDLE)) ; in

(defcfun ("FindFirstVolumeMountPointW" %find-first-volume-mount-point)
    HANDLE
  (root-path-name LPTSTR)		; in
  (volume-mount-point LPTSTR)		; out
  (buffer-length DWORD))		; in

(defcfun ("FindNextVolumeMountPointW" %find-next-volume-mount-point)
    BOOL
  (handle HANDLE)			; in
  (volume-mount-point LPTSTR)		; out
  (buffer-length DWORD))		; in

(defcfun ("FindVolumeMountPointClose" %find-volume-mount-point-close)
    BOOL
  (handle HANDLE)) ; in

(defcfun ("GetVolumeNameForVolumeMountPointW"
	  %get-volume-name-for-volume-mount-point)
    BOOL
  (volume-mount-point LPCTSTR) 		; in
  (volume-name LPTSTR)  		; out
  (buffer-length DWORD))   		; in

(defcfun ("GetLogicalDrives" %get-logical-drives)
    DWORD)

(defcfun ("GetVolumeInformationW" %get-volume-information)
    BOOL
  (root-path-name           LPCTSTR)	; in opt
  (volume-name-buffer       LPTSTR)	; out opt
  (volume-name-size         DWORD)	; in
  (volume-serial-number     LPDWORD)	; out opt
  (maximum-component-length LPDWORD)	; out opt
  (file-system-flags        LPDWORD)	; out opt
  (file-system-name-buffer  LPTSTR)	; out opt
  (file-system-name-size    DWORD)	; in
  )

(defcfun ("GetDiskFreeSpaceExW" %get-disk-free-space-ex)
    BOOL
  (directory-name LPCTSTR)		        ; in opt
  (free-bytes-available PULARGE_INTEGER)        ; out opt
  (total-number-of-bytes PULARGE_INTEGER)       ; out opt
  (total-number-of-free-bytes PULARGE_INTEGER)) ; out opt

(defcfun ("QueryDosDeviceW" %query-dos-device)
    DWORD
  (device-name LPCTSTR)			; in opt
  (target-path LPTSTR)			; out
  (ch-max DWORD))			; in

(defcfun ("GetVolumePathNameW" %get-volume-path-name)
    BOOL
  (file-name LPCWSTR)
  (volume-path-name LPWSTR)
  (buffer-length DWORD))

(defstruct volume-info
  device-name
  name
  mount-point
  serial-number
  component-len
  flags
  type
  total-bytes
  bytes-free
  bytes-available)

(defun get-mount-points ()
  (with-wide-string (drive "X:\\")
    (with-foreign-object (volume-name 'WCHAR +MAX-PATH+)
      (let ((drives (%get-logical-drives)))
	(loop
	   :for i :from 0
	   :for c :from (char-code #\A) :to (char-code #\Z)
	   :if (plusp (logand drives (ash 1 i)))
	   :do
	   (set-wchar drive 0 (code-char c))
	   (%get-volume-name-for-volume-mount-point drive volume-name +MAX-PATH+)
	   :and
	   :collect (list (code-char c) (wide-string-to-lisp volume-name)))))))

(defun get-volume-mount-points (root-path)
  (let (handle result err)
    (with-foreign-object (mount-point 'WCHAR +MAX-PATH+)
      (setf handle (%find-first-volume-mount-point
		    root-path mount-point +MAX-PATH+))
      (when (= (pointer-address handle) +INVALID-HANDLE-VALUE+)
	(error 'windows-error :error-code (get-last-error)
	       :format-control "get-volume-mount-points" :format-arguments nil))
      (push (wide-string-to-lisp mount-point) result)
      (loop
	 :while (not (zerop (%find-next-volume-mount-point
			     handle mount-point +MAX-PATH+)))
	 :do
	 (push (wide-string-to-lisp mount-point) result))
      (setf err (get-last-error))
      (syscall (%find-volume-mount-point-close handle))
      (when (/= +ERROR-NO-MORE-FILES+ err)
	(error 'windows-error :error-code (get-last-error)
	       :format-control "get-volume-mount-points" :format-arguments nil)))
    (setf result (nreverse result))
    result))

(defun get-volume-info (root-path)
  (with-foreign-objects ((volume-name 'WCHAR +MAX-PATH+)
			 (serial-number 'DWORD)
			 (component-len 'DWORD)
			 (flags 'DWORD)
			 (system-name 'WCHAR +MAX-PATH+)
			 (total 'ULARGE_INTEGER)
			 (free 'ULARGE_INTEGER)
			 (avail 'ULARGE_INTEGER)
			 ;; (dos-dev 'WCHAR +MAX-PATH+)
			 )
    (syscall (%get-volume-information root-path
				      volume-name +MAX-PATH+
				      serial-number
				      component-len
				      flags
				      system-name +MAX-PATH+))
    (syscall (%get-disk-free-space-ex root-path avail total free))
    ;; (syscall (%query-dos-device root-path dos-dev +MAX-PATH+))
    (make-volume-info
     :device-name (wide-string-to-lisp root-path)
     :name (wide-string-to-lisp volume-name)
     ;; :mount-point (wide-string-to-lisp dos-dev)
     ;; :mount-point (get-volume-mount-points root-path)
     :serial-number (mem-ref serial-number 'DWORD)
     :component-len (mem-ref component-len 'DWORD)
     :flags (mem-ref flags 'DWORD)
     :type (wide-string-to-lisp system-name)
     :total-bytes (mem-ref total 'ULARGE_INTEGER)
     :bytes-free (mem-ref free 'ULARGE_INTEGER)
     :bytes-available (mem-ref avail 'ULARGE_INTEGER))))

(defun safer-get-volume-info (root-path)
  "Get the volume info or NIL if we can't."
  (let (result)
    (block nil
      (handler-case
	  (setf result (get-volume-info root-path))
	(windows-error (e)
	  (when (= (opsys-error-code e) +ERROR-NOT-READY+)
	    (return nil))))
      result)))

(defun %mounted-filesystems ()
  "Return a list of filesystem info."
  (let (handle result err vol-info)
    (with-foreign-object (volume-name 'WCHAR +MAX-PATH+)
      (loop
	 :with mount-points = (get-mount-points)
	 :and letter
	 :initially
	 (setf handle (%find-first-volume volume-name +MAX-PATH+))
	 (when (= (pointer-address handle) +INVALID-HANDLE-VALUE+)
	   (error 'windows-error :error-code (get-last-error)
		  :format-control "mounted-filesystems" :format-arguments nil))
	 :do
	 ;; (push (wide-string-to-lisp volume-name) result))
	 (when (setf vol-info (safer-get-volume-info volume-name))
	   (setf letter (find (volume-info-device-name vol-info) mount-points
			      :key #'cadr :test #'equal))
	   (setf (volume-info-mount-point vol-info)
		 (and letter (s+ (car letter) ":\\")))
	   (push vol-info result))
	 :while (not (zerop (%find-next-volume handle volume-name +MAX-PATH+))))
      (setf err (get-last-error))
      (syscall (%find-volume-close handle))
      (when (/= +ERROR-NO-MORE-FILES+ err)
	(error 'windows-error :error-code (get-last-error)
	       :format-control "mounted-filesystems" :format-arguments nil)))
    (setf result (nreverse result))
    result))

(defun mounted-filesystems ()
  "Return a list of filesystem info."
  (loop :for f :in (%mounted-filesystems)
     :collect
     (make-filesystem-info
      ;;:device-name     (volume-info-device-name	    f)
      :device-name     (volume-info-name	    f)
      :mount-point     (volume-info-mount-point	    f)
      :type	       (volume-info-type	    f)
      :total-bytes     (volume-info-total-bytes	    f)
      :bytes-free      (volume-info-bytes-free	    f)
      :bytes-available (volume-info-bytes-available f))))

(defun get-file-volume-path (file)
  "Return the path of the volume FILE is on."
  (with-wide-string (w-file (safe-namestring file))
    (with-foreign-object (w-volume-path 'TCHAR +MAX-PATH+)
      (syscall (%get-volume-path-name w-file w-volume-path +MAX-PATH+))
      (wide-string-to-lisp w-volume-path))))

(defun get-filesystem-info (file)
  "Return the filesystem-info of FILE."
  (let* ((vol-path (get-file-volume-path file))
	 (vol-info (safer-get-volume-info vol-path))
	 (mount-points (get-mount-points))
	 letter)
    (when vol-info
      (setf letter (find (volume-info-device-name vol-info) mount-points
			 :key #'cadr :test #'equal)
	    (volume-info-mount-point vol-info)
	    (and letter (s+ (car letter) ":\\")))
      (make-filesystem-info
       :device-name     (volume-info-name	     vol-info)
       :mount-point     (volume-info-mount-point     vol-info)
       :type	        (volume-info-type	     vol-info)
       :total-bytes     (volume-info-total-bytes     vol-info)
       :bytes-free      (volume-info-bytes-free	     vol-info)
       :bytes-available (volume-info-bytes-available vol-info)))))

(defun mount-point-of-file (file)
  "Return the mount of FILE."
  (get-file-volume-path file))

;; End
