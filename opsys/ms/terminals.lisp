;;;
;;; ms/terminals.lisp - Windows interface to consoles
;;;

(in-package :opsys-ms)

;; This is almost like a mini terminal-* driver, even though we have a whole
;; terminal-ms. Is there a better way to do it?

(defstruct ms-term
  "A dumb way to deal with it."
  in-handle
  out-handle
  mode
  width
  height
  buffer-width
  buffer-height
  not-console
  read-ahead)

(defcstruct (COORD :class foreign-coord)
  (x MS-SHORT)
  (y MS-SHORT))

;; Shouldn't the be the default??!?!!!
(defmethod translate-into-foreign-memory (object (type foreign-coord) pointer)
  (with-foreign-slots ((x y) pointer (:struct COORD))
    (setf x (getf object 'x)
	  y (getf object 'y))))

(defun set-coord (coord o1 o2)
  (with-foreign-slots ((x y) coord (:struct COORD))
    (setf x o1 y o2)))

;; Things to support the dreadful snarble-func hack on CCL.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; I know this is relatively pointless and only serves to demonstate how much
  ;; I dislike StudlyCaps.
  (defun snarbled-name (string)
    "'FooBar' -> %foo-bar"
    (let ((i 0) (len (length string)) c start result)
      (flet ((scan-over (func)
	       (loop :while (< i len)
		  :do (setf c (char string i))
		  :while (funcall func c)
		  :do (incf i))))
	(setf result
	      (with-output-to-string (str)
		(write-char #\% str)
		(loop :do
		     ;; (format t "~s~%" i)
		     (setf start i
			   c (char string i))
		     (scan-over #'(lambda (c)
				    (and (alpha-char-p c) (upper-case-p c))))
		     (scan-over #'(lambda (c)
				    (and (alpha-char-p c) (lower-case-p c))))
		     (write-string
		      (string-downcase (subseq string start i)) str)
		     (when (< i len)
		       (setf c (char string i))
		       (when (and (alpha-char-p c) (upper-case-p c))
			 (write-char #\- str))
		       ;;(incf i)
		       )
		   :while (and (< i len) (alpha-char-p c) (upper-case-p c)))
		(when (< i len)
		  (write-string
		   (string-downcase (subseq string (1- i))) str))))
	(symbolify result))))

  (defun ccl-typeify (name)
    (intern (s+ "<" name ">") :keyword))

  (defun structify-name (symbol)
    (symbolify (s+ "s-" symbol)))

  #+ccl 
  (defmacro ccl-get-func (name)
    "Dredge something like the function address out of CCLs weirdling machinery."
    `(%reference-external-entry-point (external ,name))))

;; This is a hack so that on CCL, which supports calling C with struct args by
;; value without using libffi, we make that semi-transparent, until we can
;; patch CFFI. This is mostly because setting up libffi, and by proxy
;; cffi-libffi on Windows is troublesome at best. Also I feel it's something
;; that really should in the implementation's FFI. If this works, we should
;; put something like it into CFFI, and then go about making it work with SBCL
;; and other implementations. I'm guessing the proprietary implemetations can
;; already support struct call by value?

(defmacro snarble-func ((func-name &rest garbage) &body body)
  ;; Turn the cffi defcfun call into
  ;; (EXTERNAL-CALL "SetConsoleCursorPosition" :<HANDLE> A1 :<COORD> A2 :<WINBOOL>)
  ;; but converting struct args properly.
  (declare (ignore garbage)) ;; because we know it's gonna be stdcall
  #+ccl
  (let* (arglist struct-list func-arglist struct-type struct-arg)
    ;; arglist      <- args to foreign function
    ;; struct-list  <- structure args and types to allocate
    ;; func-arglist <- args to lisp function
    (loop :for (arg type) :in (rest body) :do
      (cond
	((and (consp type) (equal (car type) :struct))
	 (setf struct-arg (structify-name arg)
	       struct-type (cadr type))
	 (push `(,arg ,struct-type) struct-list)
	 (push (ccl-typeify struct-type) arglist)
	 (push struct-arg arglist)
	 (push arg func-arglist))
	((and (consp type) (equal (car type) :pointer))
	 (push :address arglist)
	 (push arg arglist)
	 (push arg func-arglist))
	(t
	 (push (ccl-typeify type) arglist)
	 (push arg arglist)
	 (push arg func-arglist))))
    (setf arglist (nreverse arglist)
	  struct-list (nreverse struct-list)
	  func-arglist (nreverse func-arglist))
    ;; (format t "~a ~s~%~s~%" func-name arglist struct-list)
    (with-unique-names (val type param struct-vals result)
      `(defun ,(snarbled-name func-name) ,func-arglist
	 ;; This is horribly inefficent and if it wasn't just a temporary hack,
	 ;; we should use %stack-block and do the arduous conversion ourselves.
	 (let (,val ,param ,struct-vals ,result)
	   (unwind-protect
		;; allocate and convert structs
		(let (,@(mapcar (_ (structify-name (first _))) struct-list))
		  ,@(loop :for (s typ) in struct-list
		       :collect
			 `(multiple-value-setq (,val ,param)
			    (convert-to-foreign ,s (list :struct ',typ)))
		       :collect `(setf ,(structify-name s) ,val)
		       :collect `(push (list ,val (list :struct ',typ) ,param)
				       ,struct-vals))
		  (setf ,result
			(ccl::ff-call (ccl-get-func ,func-name)
	 			      ,@arglist
	 			      ,(ccl-typeify (first body))
	 			      )))
	     ;; make sure to get rid of the junk
	     (loop :for (,val ,type ,param) :in ,struct-vals
		:do (free-converted-object ,val ,type ,param)))
	   ,result))))
	 ;; (ccl:external-call
	 ;;  ,(cffi-sys::convert-external-name func-name)
	 ;;  ;;,@(cffi-sys::convert-foreign-funcall-types arglist)
	 ;;  ,@arglist
	 ;;  ))))
  #-ccl
  `(defcfun (,func-name ,(snarbled-name func-name))
       ,@body))

(defctype PCOORD (:pointer (:struct COORD)))

(defcunion foreign-uchar
  (unicode-char WCHAR)
  (ascii-char MS-CHAR))

(defparameter *keys* nil "List of Windows “Virtual” keys.")

(define-to-list *keys*
  #(#(+VK-LBUTTON+			#x01)
    #(+VK-RBUTTON+			#x02)
    #(+VK-CANCEL+			#x03)
    #(+VK-MBUTTON+			#x04)
    #(+VK-XBUTTON1+			#x05)
    #(+VK-XBUTTON2+			#x06)
    #(+VK-BACK+				#x08)
    #(+VK-TAB+				#x09)
    #(+VK-CLEAR+			#x0C)
    #(+VK-RETURN+			#x0D)
    #(+VK-SHIFT+			#x10)
    #(+VK-CONTROL+			#x11)
    #(+VK-MENU+				#x12)
    #(+VK-PAUSE+			#x13)
    #(+VK-CAPITAL+			#x14)
    #(+VK-KANA+				#x15)
    #(+VK-JUNJA+			#x17)
    #(+VK-FINAL+			#x18)
    #(+VK-HANJA+			#x19)
    #(+VK-ESCAPE+			#x1B)
    #(+VK-CONVERT+			#x1C)
    #(+VK-NONCONVERT+			#x1D)
    #(+VK-ACCEPT+			#x1E)
    #(+VK-MODECHANGE+			#x1F)
    #(+VK-SPACE+			#x20)
    #(+VK-PRIOR+			#x21)
    #(+VK-NEXT+				#x22)
    #(+VK-END+				#x23)
    #(+VK-HOME+				#x24)
    #(+VK-LEFT+				#x25)
    #(+VK-UP+				#x26)
    #(+VK-RIGHT+			#x27)
    #(+VK-DOWN+				#x28)
    #(+VK-SELECT+			#x29)
    #(+VK-PRINT+			#x2A)
    #(+VK-EXECUTE+			#x2B)
    #(+VK-SNAPSHOT+			#x2C)
    #(+VK-INSERT+			#x2D)
    #(+VK-DELETE+			#x2E)
    #(+VK-HELP+				#x2F)
    #(+VK-LWIN+				#x5B)
    #(+VK-RWIN+				#x5C)
    #(+VK-APPS+				#x5D)
    #(+VK-SLEEP+			#x5F)
    #(+VK-NUMPAD0+			#x60)
    #(+VK-NUMPAD1+			#x61)
    #(+VK-NUMPAD2+			#x62)
    #(+VK-NUMPAD3+			#x63)
    #(+VK-NUMPAD4+			#x64)
    #(+VK-NUMPAD5+			#x65)
    #(+VK-NUMPAD6+			#x66)
    #(+VK-NUMPAD7+			#x67)
    #(+VK-NUMPAD8+			#x68)
    #(+VK-NUMPAD9+			#x69)
    #(+VK-MULTIPLY+			#x6A)
    #(+VK-ADD+				#x6B)
    #(+VK-SEPARATOR+			#x6C)
    #(+VK-SUBTRACT+			#x6D)
    #(+VK-DECIMAL+			#x6E)
    #(+VK-DIVIDE+			#x6F)
    #(+VK-F1+				#x70)
    #(+VK-F2+				#x71)
    #(+VK-F3+				#x72)
    #(+VK-F4+				#x73)
    #(+VK-F5+				#x74)
    #(+VK-F6+				#x75)
    #(+VK-F7+				#x76)
    #(+VK-F8+				#x77)
    #(+VK-F9+				#x78)
    #(+VK-F10+				#x79)
    #(+VK-F11+				#x7A)
    #(+VK-F12+				#x7B)
    #(+VK-F13+				#x7C)
    #(+VK-F14+				#x7D)
    #(+VK-F15+				#x7E)
    #(+VK-F16+				#x7F)
    #(+VK-F17+				#x80)
    #(+VK-F18+				#x81)
    #(+VK-F19+				#x82)
    #(+VK-F20+				#x83)
    #(+VK-F21+				#x84)
    #(+VK-F22+				#x85)
    #(+VK-F23+				#x86)
    #(+VK-F24+				#x87)
    #(+VK-NUMLOCK+			#x90)
    #(+VK-SCROLL+			#x91)
    #(+VK-OEM-NEC-EQUAL+		#x92)
    #(+VK-OEM-FJ-JISHO+			#x92)
    #(+VK-OEM-FJ-MASSHOU+		#x93)
    #(+VK-OEM-FJ-TOUROKU+		#x94)
    #(+VK-OEM-FJ-LOYA+			#x95)
    #(+VK-OEM-FJ-ROYA+			#x96)
    #(+VK-LSHIFT+			#xA0)
    #(+VK-RSHIFT+			#xA1)
    #(+VK-LCONTROL+			#xA2)
    #(+VK-RCONTROL+			#xA3)
    #(+VK-LMENU+			#xA4)
    #(+VK-RMENU+			#xA5)
    #(+VK-BROWSER-BACK+			#xA6)
    #(+VK-BROWSER-FORWARD+		#xA7)
    #(+VK-BROWSER-REFRESH+		#xA8)
    #(+VK-BROWSER-STOP+			#xA9)
    #(+VK-BROWSER-SEARCH+		#xAA)
    #(+VK-BROWSER-FAVORITES+		#xAB)
    #(+VK-BROWSER-HOME+			#xAC)
    #(+VK-VOLUME-MUTE+			#xAD)
    #(+VK-VOLUME-DOWN+			#xAE)
    #(+VK-VOLUME-UP+			#xAF)
    #(+VK-MEDIA-NEXT-TRACK+		#xB0)
    #(+VK-MEDIA-PREV-TRACK+		#xB1)
    #(+VK-MEDIA-STOP+			#xB2)
    #(+VK-MEDIA-PLAY-PAUSE+		#xB3)
    #(+VK-LAUNCH-MAIL+			#xB4)
    #(+VK-LAUNCH-MEDIA-SELECT+		#xB5)
    #(+VK-LAUNCH-APP1+			#xB6)
    #(+VK-LAUNCH-APP2+			#xB7)
    #(+VK-OEM-1+			#xBA)
    #(+VK-OEM-PLUS+			#xBB)
    #(+VK-OEM-COMMA+			#xBC)
    #(+VK-OEM-MINUS+			#xBD)
    #(+VK-OEM-PERIOD+			#xBE)
    #(+VK-OEM-2+			#xBF)
    #(+VK-OEM-3+			#xC0)
    #(+VK-OEM-4+			#xDB)
    #(+VK-OEM-5+			#xDC)
    #(+VK-OEM-6+			#xDD)
    #(+VK-OEM-7+			#xDE)
    #(+VK-OEM-8+			#xDF)
    #(+VK-OEM-AX+			#xE1)
    #(+VK-OEM-102+			#xE2)
    #(+VK-ICO-HELP+			#xE3)
    #(+VK-ICO-00+			#xE4)
    #(+VK-PROCESSKEY+			#xE5)
    #(+VK-ICO-CLEAR+			#xE6)
    #(+VK-PACKET+			#xE7)
    #(+VK-OEM-RESET+			#xE9)
    #(+VK-OEM-JUMP+			#xEA)
    #(+VK-OEM-PA1+			#xEB)
    #(+VK-OEM-PA2+			#xEC)
    #(+VK-OEM-PA3+			#xED)
    #(+VK-OEM-WSCTRL+			#xEE)
    #(+VK-OEM-CUSEL+			#xEF)
    #(+VK-OEM-ATTN+			#xF0)
    #(+VK-OEM-FINISH+			#xF1)
    #(+VK-OEM-COPY+			#xF2)
    #(+VK-OEM-AUTO+			#xF3)
    #(+VK-OEM-ENLW+			#xF4)
    #(+VK-OEM-BACKTAB+			#xF5)
    #(+VK-ATTN+				#xF6)
    #(+VK-CRSEL+			#xF7)
    #(+VK-EXSEL+			#xF8)
    #(+VK-EREOF+			#xF9)
    #(+VK-PLAY+				#xFA)
    #(+VK-ZOOM+				#xFB)
    #(+VK-NONAME+			#xFC)
    #(+VK-PA1+				#xFD)
    #(+VK-OEM-CLEAR+			#xFE)))

;; Key aliases
(defconstant +VK-HANGEUL+ +VK-KANA+)
(defconstant +VK-HANGUL+  +VK-KANA+)
(defconstant +VK-KANJI+   +VK-HANJA+)

(defparameter *key-symbols* (make-hash-table))
(loop :for name :in *keys* :do
   (setf (gethash (symbol-value name) *key-symbols*) name))

(defun key-symbol (code)
  "Return the symbol name of a key given it's CODE."
  (gethash code *key-symbols*))

(defun key-name (key-symbol)
  "Return a string name of the key given by KEY-SYMBOL."
  (when key-symbol
    (let ((n (symbol-name key-symbol))) (subseq n 4 (1- (length n))))))

(defun compatible-key-symbol (code)
  "Return a more compatible seeming key symbol."
  (let ((sym (key-symbol code)))
    (when sym
      (keywordify (key-name sym)))))

(defcstruct foreign-key-event
  (key-down 	       BOOL)
  (repeat-count        WORD)
  (virtual-key-code    WORD)
  (virtual-scan-code   WORD)
  (uchar               (:union foreign-uchar))
  (control-key-state   DWORD))

(defconstant +MAPVK-VK-TO-VSC+    0 "Virtual key to scan code.")
(defconstant +MAPVK-VSC-TO-VK+    1 "Scan code to virtual key. Return left.")
(defconstant +MAPVK-VK-TO-CHAR+   2 "Virtual key to unshifted character.")
(defconstant +MAPVK-VSC-TO-VK-EX+ 3
  "Scan code to virtual key. Distinguish between left and right.")

(defcfun ("MapVirtualKeyW" %map-virtual-key)
  UINT
  (code UINT)				; In
  (map-type UINT))			; In

(defun scan-code-to-virtual-key (scan-code)
  "Return the virtual key code for a scan code."
  (%map-virtual-key scan-code +MAPVK-VSC-TO-VK+))

(defconstant +FROM-LEFT-1ST-BUTTON-PRESSED+ #x0001)
(defconstant +RIGHTMOST-BUTTON-PRESSED+     #x0002)
(defconstant +FROM-LEFT-2ND-BUTTON-PRESSED+ #x0004)
(defconstant +FROM-LEFT-3RD-BUTTON-PRESSED+ #x0008)
(defconstant +FROM-LEFT-4TH-BUTTON-PRESSED+ #x0010)

(defconstant +RIGHT-ALT-PRESSED+  #x0001)
(defconstant +LEFT-ALT-PRESSED+   #x0002)
(defconstant +RIGHT-CTRL-PRESSED+ #x0004)
(defconstant +LEFT-CTRL-PRESSED+  #x0008)
(defconstant +SHIFT-PRESSED+      #x0010)
(defconstant +NUMLOCK-ON+         #x0020)
(defconstant +SCROLLLOCK-ON+      #x0040)
(defconstant +CAPSLOCK-ON+        #x0080)
(defconstant +ENHANCED-KEY+       #x0100)

(defconstant +MOUSE-MOVED+ 	  #x0001)
(defconstant +DOUBLE-CLICK+ 	  #x0002)
(defconstant +MOUSE-WHEELED+ 	  #x0004)
(defconstant +MOUSE-HWHEELED+ 	  #x0008)

(defcstruct foreign-mouse-event
  (mouse-position    (:struct COORD))
  (button-state      DWORD)
  (control-key-state DWORD)
  (event-flags	     DWORD))

(defcstruct foreign-buffer-size-event
    (size (:struct COORD)))

(defcstruct foreign-menu-event
    (command-id UINT))

(defcstruct foreign-focus-event
    (set-focus BOOL))

(defconstant +KEY-EVENT+                #x0001)
(defconstant +MOUSE-EVENT+ 	        #x0002)
(defconstant +WINDOW-BUFFER-SIZE-EVENT+ #x0004)
(defconstant +MENU-EVENT+ 	        #x0008)
(defconstant +FOCUS-EVENT+              #x0010)

(defcunion foreign-event-union
  (key-event                 (:struct foreign-key-event))
  (mouse-event               (:struct foreign-mouse-event))
  (window-buffer-size-event  (:struct foreign-buffer-size-event))
  (menu-event                (:struct foreign-menu-event))
  (focus-event		     (:struct foreign-focus-event)))

(defcstruct foreign-input-record
  (event-type WORD)
  (event (:union foreign-event-union)))

(defctype PINPUT_RECORD (:pointer (:struct foreign-input-record)))

(defcfun ("ReadConsoleInputW" %read-console-input :convention :stdcall)
    BOOL
   (console-input HANDLE)    		; in
   (buffer PINPUT_RECORD)   		; out
   (length DWORD)	      		; in
   (number-of-events-read LPDWORD))	; out

(defcfun ("PeekConsoleInputW" %peek-console-input)
    BOOL
  (console-input HANDLE)		; in
  (buffer PINPUT_RECORD)		; out
  (length DWORD)			; in
  (number-of-events-read LPDWORD))	; out

(defcfun ("FlushConsoleInputBuffer" %flush-console-input-buffer)
    BOOL
  (console-input HANDLE))

(defconstant +ENABLE-PROCESSED-INPUT+        #x0001)
(defconstant +ENABLE-LINE-INPUT+             #x0002)
(defconstant +ENABLE-ECHO-INPUT+             #x0004)
(defconstant +ENABLE-WINDOW-INPUT+           #x0008)
(defconstant +ENABLE-MOUSE-INPUT+            #x0010)
(defconstant +ENABLE-INSERT-MODE+            #x0020)
(defconstant +ENABLE-QUICK-EDIT-MODE+        #x0040)
(defconstant +ENABLE-VIRTUAL-TERMINAL-INPUT+ #x0200)

(defconstant +NORMAL-INPUT-MODES+ (logior +ENABLE-PROCESSED-INPUT+
					  +ENABLE-LINE-INPUT+
					  +ENABLE-ECHO-INPUT+
					  +ENABLE-MOUSE-INPUT+
					  +ENABLE-INSERT-MODE+
					  +ENABLE-QUICK-EDIT-MODE+
					  ;; +ENABLE-VIRTUAL-TERMINAL-INPUT+
					  ))

(defconstant +ENABLE-PROCESSED-OUTPUT+		  #x0001)
(defconstant +ENABLE-WRAP-AT-EOL-OUTPUT+	  #x0002)
(defconstant +ENABLE-VIRTUAL-TERMINAL-PROCESSING+ #x0004)
(defconstant +DISABLE-NEWLINE-AUTO-RETURN+	  #x0008)
(defconstant +ENABLE-LVB-GRID-WORLDWIDE+	  #x0010)

(defconstant +NORMAL-OUTPUT-MODES+ (logior +ENABLE-PROCESSED-OUTPUT+
					   +ENABLE-WRAP-AT-EOL-OUTPUT+))

(defcfun ("GetConsoleMode" %get-console-mode :convention :stdcall)
    BOOL
  (console-handle HANDLE)		; in
  (mode LPDWORD))			; out

(defcfun ("SetConsoleMode" %set-console-mode :convention :stdcall)
    BOOL
  (console-handle HANDLE)		; in
  (mode DWORD))				; in

(defun file-handle-terminal-p (fd)
  "Return true if the system file descriptor FD is attached to a terminal."
  (with-foreign-object (ms-mode 'DWORD)
    (not (zerop (%get-console-mode
		 (if (numberp fd) (make-pointer fd) fd)
		 ms-mode)))))

(defun file-handle-terminal-name (fd)
  "Return the device name of the terminal attached to the system file
descriptor FD."
  (declare (ignore fd))
  ;;(GetFileInformationByHandleEx fd)
  nil)

(defvar *default-console-device-name* "CON" ;; @@@ or should it be CONIN$ ?
  "Name of the default console device.")

(defun open-real-console (direction)
  (flet ((open-it (name)
	   (with-wide-string (nn name)
	     (%create-file nn
			   (logior +GENERIC-READ+ +GENERIC-WRITE+)
			   (logior +FILE-SHARE-READ+ +FILE-SHARE-WRITE+)
			   (null-pointer) ;; @@@ maybe we should set inherit?
			   +OPEN-EXISTING+
			   0 ;; very unspecified flags?
			   (null-pointer)))))
    (let ((handle
	   (ecase direction
	     (:input  (open-it "CONIN$"))
	     (:output (open-it "CONOUT$")))))
      (when (= (pointer-address handle) +INVALID-HANDLE-VALUE+)
	(error 'windows-error :error-code (get-last-error)
	       :format-control "Failed to open the real console."))
      handle)))

(defun open-terminal (device-name direction)
  "Open a terminal. Return the system file handle."
  (declare (ignore device-name))
  (ecase direction
    (:input
     (let (in-h out-h tty)
       ;; Input handle
       (setf in-h (%get-std-handle +STD-INPUT-HANDLE+))
       (when (= (pointer-address in-h) +INVALID-HANDLE-VALUE+)
	 (error 'windows-error :error-code (get-last-error)
		:format-control "Failed to get terminal input handle."))
       (dbugf :ms "open-terminal input handle = #x~x~%" (pointer-address in-h))

       ;; Output handle
       (setf out-h (%get-std-handle +STD-OUTPUT-HANDLE+))
       (when (= (pointer-address out-h) +INVALID-HANDLE-VALUE+)
	 (error 'windows-error :error-code (get-last-error)
		:format-control "Failed to get terminal output handle."))
       (dbugf :ms "open-terminal output handle = #x~x~%" (pointer-address out-h))

       (setf tty (make-ms-term :in-handle in-h :out-handle out-h))

       ;; Test handles to try to see if they're usable consoles.
       (when (not (file-handle-terminal-p in-h))
	 (dbugf :ms "terminal handles aren't real~%")
	 ;; Actually we probably shouldn't do this, because we're likely
	 ;; running in a Cygwin terminal or something else that requires we
	 ;; do output through it's handles. We just won't be able to do many
	 ;; console-ish things.
	 ;; (setf (ms-term-in-handle tty) (open-real-console :input)
	 ;;       (ms-term-out-handle tty) (open-real-console :output))
	 (with-slots (not-console mode width height) tty
	   ;; Fake some stuff.
	   (setf not-console t
		 mode (make-terminal-mode)
		 width 80 height 24)))
       (dbugf :ms "ms-term = ~s~%" tty)
       tty))
    (:output *terminal-io*)))

(defun close-terminal (terminal-handle)
  "Close a terminal."
  (declare (ignore terminal-handle))
  ;; We don't really need to close a standard handle.
  ;; @@@ But perhaps if we were to open (or attach) to another terminal device
  ;; we might need to close that.
  nil)

;; @@@ The following with-X structure access macros are a workaround because I
;; don't understand when or why the event which we get from %read-console-input
;; is supposed to be a pointer or not. If I ever figure it out, these can
;; go away.

#|
(defmacro with-key-event ((&rest slots) event &body body)
  (with-unique-names (key-event)
    `(etypecase event
       (foreign-pointer
	(with-foreign-slots ((,@slots) ,event
			     (:struct foreign-key-event))
	  ,@body))
       (cons
	(let ((,key-event (getf ,event 'key-event)))
	  (let ,(loop :for s :in slots
		   :collect `(,s (getf ,key-event ',s)))
	    ,@body)))
       )))
|#
(defmacro with-key-event ((&rest slots) event &body body)
  `(with-foreign-slots ((,@slots) ,event
			(:struct foreign-key-event))
     ,@body))

;; @@@ see above with-key-event comment
(defmacro uchar-unicode (uchar)
  `(etypecase uchar
     (foreign-pointer
      (foreign-slot-value ,uchar '(:union foreign-uchar) 'unicode-char))
     (cons
      (getf ,uchar 'unicode-char))))

(defmacro uchar-ascii (uchar)
  `(etypecase uchar
     (foreign-pointer
      (foreign-slot-value ,uchar '(:union foreign-uchar) 'ascii-char))
     (cons
      (getf ,uchar 'ascii-char))))

(defparameter *modifier-keys*
    (vector +VK-SHIFT+ +VK-LSHIFT+ +VK-RSHIFT+
	    +VK-CONTROL+ +VK-LCONTROL+ +VK-RCONTROL+
	    +VK-LWIN+ +VK-RWIN+
	    +VK-NUMLOCK+
	    +VK-SCROLL+
	    +VK-LMENU+ +VK-RMENU+)
  "Keys that are considered modifiers.")

(defun modifier-key-p (vkey)
  "Return true if the virtual key VKEY is considered a modifier key."
  (and (position vkey *modifier-keys*) t))

(defun read-console-input (terminal)
  (let (result c)
    (with-slots (in-handle width height read-ahead) terminal
      (when read-ahead
	(return-from read-console-input (pop read-ahead)))
      (with-foreign-objects ((buf '(:struct foreign-input-record))
			     (events-read 'DWORD))
	(loop :do
	   (setf result nil)
	   (syscall (%read-console-input in-handle buf 1 events-read))
	   (dbugf :ms "read-console-input buf = ~s~%" buf)
	   (with-foreign-slots ((event-type event)
				buf
				(:struct foreign-input-record))
	     (dbugf :ms "event-type ~s~%" event-type)
	     (dbugf :ms "event ~s~%" event)
	     (cond
	       ((= event-type +KEY-EVENT+)
		(with-key-event (key-down uchar virtual-key-code
				 virtual-scan-code control-key-state) event
		  (dbugf :ms "key-down ~a uchar = ~a~%" key-down uchar)
		  (dbugf :ms "unicode-char ~a ascii-char = ~a~%"
			 (uchar-unicode uchar) (uchar-ascii uchar))
		  (dbugf :ms "control-key-state ~a virtual-key-code = ~a~%"
			 control-key-state virtual-key-code)
		  (dbugf :ms "virtual-scan-code ~a~%" virtual-scan-code)
		  (when (= 1 key-down)
		    (setf c (uchar-unicode uchar))
		    (dbugf :ms "c = ~s~%" c)
		    (cond
		      ;; Convert Alt-<char> into #\Escape <Char>
		      ((plusp (logand control-key-state
				      (logior +RIGHT-ALT-PRESSED+
					      +LEFT-ALT-PRESSED+)))
		       (dbugf :ms "--ALT--~%")
		       (when (not (zerop c))
			 (setf read-ahead (append read-ahead (list c))
			       result (char-code #\escape))))
		      ;; Control key
		      ((plusp (logand control-key-state
				      (logior +RIGHT-CTRL-PRESSED+
					      +LEFT-CTRL-PRESSED+)))
		       (dbugf :ms "--CTRL--~%")
		       (when (not (zerop c))
			 (setf result
			       ;; (1+ (- c (char-code #\A)))
			       c)))
		      ;; Control key
		      ((plusp (logand control-key-state +SHIFT-PRESSED+))
		       (dbugf :ms "--SHIFT--~%")
		       (when (not (zerop c))
			 (setf result
			       ;; (1+ (- c (char-code #\A)))
			       c)))
		      ;; Virtual key?
		      ((and (plusp virtual-key-code) (zerop c))
		       (dbugf :ms "--VIRT--~%")
		       (setf result (or (compatible-key-symbol
					 virtual-key-code)
					c))
		       (dbugf :ms "key code = ~s~%" virtual-key-code))
		      ((not (zerop c))
		       (dbugf :ms "--CHAR--~%")
		       (when (not (zerop c))
			 (setf result c)))
		      ((plusp virtual-scan-code)
		       (dbugf :ms "--SCAN--~%")
		       (let ((vkey (scan-code-to-virtual-key virtual-scan-code)))
			 (cond
			   ((zerop vkey)
			    ;; Failed to convert scan code to virtual key.
			    ;; I guess we can just ignore it.
			    (dbugf :ms "scan ~s to vkey FAILED!~%"
				   virtual-scan-code))
			   ((modifier-key-p vkey)
			    #| ignore |#
			    (dbugf :ms "ignore modifier ~s~%" vkey))
			   (t
			    (dbugf :ms "using vkey ~s key ~s~%" vkey
				   (compatible-key-symbol vkey))
			    (setf result (or (compatible-key-symbol vkey)
					     c)))))
		       (dbugf :ms "scan code = ~s~%" virtual-scan-code))))
		  (dbugf :ms "result = ~s~%" result)))
	       ((equal event-type +WINDOW-BUFFER-SIZE-EVENT+)
		(with-foreign-slots ((size) event
				     (:struct foreign-buffer-size-event))
		  (setf width (foreign-slot-value size '(:struct COORD) 'x)
			height (foreign-slot-value size '(:struct COORD) 'y))))
	       ((equal event-type +MOUSE-EVENT+) #| @@@ ignore |# )
	       ((equal event-type +MENU-EVENT+) #| @@@ ignore |# )
	       ((equal event-type +FOCUS-EVENT+) #| @@@ ignore |# )
	       (t
		(format t "Unknown event type from console #x~x~%" event-type)
		;;(error "Unknown event type from console."))
		)))
	   :while (not result))))
    result))

(defun console-event-type-matches-p (event-buf types)
  "More specific event matching. EVENT is a foreign-input-record. Types is a
list of keywords.
  :all           - matches any event
  :key           - any key
  :key-char-up   - any key that generates a character and is up
  :key-char-down - any key that generates a character and is down
  :key-char      - any key that generates a character
  :key-up        - any key up
  :key-down      - any key down
  :mouse
  :size
  :menu
  :focus
"
  (let ((type (foreign-slot-value
	       event-buf '(:struct foreign-input-record) 'event-type)))
    (cond
      ((member :all types) t)
      (t (cond
	   ((= type +KEY-EVENT+)
	    (or (member :key types)
		(with-key-event (key-down uchar
				 ;; virtual-key-code
				 ;; virtual-scan-code
				 control-key-state)
		     (foreign-slot-value event-buf
					 '(:struct foreign-input-record)
					 'event)
		  (let ((c (uchar-unicode uchar)))
		    (or
		     (and (and (not (zerop c)) (= key-down +TRUE+))
			  (member :key-char-down types))
		     (and (and (not (zerop c)) (= key-down +FALSE+))
			  (member :key-char-up types))
		     (and (not (zerop c))
			  (member :key-char types))
		     ;; (and (not (zerop control-key-state))
		     ;; 	(member :key-modifier types))
		     (and (= key-down +TRUE+)
			  (member :key-down types))
		     (and (= key-down +FALSE+)
			  (member :key-up types)))))))
	   (t (member
	       (case type
		 (+MOUSE-EVENT+              :mouse)
		 (+WINDOW-BUFFER-SIZE-EVENT+ :size)
		 (+MENU-EVENT+               :menu)
		 (+FOCUS-EVENT+              :focus))
		types)))))))

(defun flush-console-events (terminal &key types not-types)
  "Get rid of some events from the terminal input queue."
  (if not-types
      (typecase not-types
	(keyword (setf not-types (list not-types)))
	(t (check-type not-types cons)))
      (typecase types
	(null (setf types '(:all)))
	(keyword (setf types (list types)))
	(t (check-type types cons))))
  ;;(dbugf :zzz "~s ~s~%" types not-types)
  (with-slots (in-handle width height read-ahead) terminal
    (with-foreign-objects ((buf '(:struct foreign-input-record))
			   (events-read 'DWORD))
      (loop :with event-count
	 :do
	 (syscall (%peek-console-input in-handle buf 1 events-read))
	 (setf event-count (mem-aref events-read 'DWORD))
	 :while (and (not (zerop event-count))
		     (if not-types
			 (not (console-event-type-matches-p buf not-types))
			 (console-event-type-matches-p buf types)))
	 :do
	 (syscall (%read-console-input in-handle buf 1 events-read))
	 (with-foreign-slots ((event-type event) buf
			      (:struct foreign-input-record))
	   (when (equal event-type +WINDOW-BUFFER-SIZE-EVENT+)
	     (with-foreign-slots ((size) event
				  (:struct foreign-buffer-size-event))
	       (setf width (foreign-slot-value size '(:struct COORD) 'x)
		     height (foreign-slot-value size '(:struct COORD) 'y)))))))))

(defconstant +oversize-peek-buf+ 200 "This is horrible.")
(defvar *peek-buf* nil "Buffer for peeking.")

(defun event-pending (terminal &key types not-types)
  "Return true if the events are pending in the terminal input queue."
  (if not-types
      (typecase not-types
	(keyword (setf not-types (list not-types)))
	(t (check-type not-types cons)))
      (typecase types
	(null (setf types '(:all)))
	(keyword (setf types (list types)))
	(t (check-type types cons))))

  (when (not *peek-buf*)
    (setf *peek-buf*
	  (foreign-alloc '(:struct foreign-input-record)
			 :count +oversize-peek-buf+)))
  (with-slots (in-handle width height read-ahead) terminal
    (with-foreign-objects ((events-read 'DWORD))
      (syscall (%peek-console-input in-handle *peek-buf* +oversize-peek-buf+
				    events-read))
      (dbugf :zzz "pending ~s~%" (mem-aref events-read 'DWORD))
      (loop :with e
	 :for i :from 0 :below (mem-aref events-read 'DWORD)
	 :do
	 (setf e (mem-aptr *peek-buf*
			   '(:struct foreign-input-record) i))
	 :when (if not-types
		   (not (console-event-type-matches-p e not-types))
		   (console-event-type-matches-p e types))
	 :return t))))

(defcfun ("ReadFile" %read-file)
    BOOL
   (file HANDLE)
   (buffer LPVOID)
   (number-of-bytes-to-read DWORD)
   (number-of-bytes-read LPDWORD)
   (overlapped LPOVERLAPPED))

;; Supposedly there's no way to tell if a handle was created with
;; FILE_FLAG_OVERLAPPED, but if you don't supply the OVERLAPPED to ReadFile,
;; it can mess up, generally by terminating prematurely if there is asynchronous
;; IO. This seems like a deep design problem. OVERLAPPED is like non-blocking
;; on Unix, which also suffers from design problems.
;; ....
;; but...
;; try a zero byte ReadFile with a NULL lpOverlapped. If it fails with
;; ERROR_INVALID_PARAMETER assume it was opened with FILE_FLAG_OVERLAPPED.
;; ... O_o O rly??
;; [I haven't tested this out.]

(defun read-handle-input (handle)
  (with-foreign-objects ((buf :unsigned-char 1)
			 (bytes-read 'DWORD))
    (let ((result (%read-file handle buf 1 bytes-read (null-pointer))))
      (format *debug-io* "%read-file result = ~s bytes-read = ~s~%" result
	      (mem-ref bytes-read 'DWORD))
      (format *debug-io* "wchar = #x~x~%" (mem-aref buf :unsigned-char 0))
      (finish-output *debug-io*))
    (when (/= 1 (mem-ref bytes-read 'DWORD))
      (error 'windows-error :format-control "Fail to read read 1 byte."))
    (mem-aref buf :unsigned-char 0)))

(defun read-terminal-char (terminal &key timeout)
  "Return a character read from the terminal TERMINAL-HANDLE.
If there's a problem, it will signal a READ-CHAR-ERROR. If the terminal is
resized it will signal an OPSYS-RESIZED. If the program is continued from
being suspended, it will signal an OPSYS-RESUMED. Usually this means the
caller should handle these possibilites. Returns the character read or NIL if it
the timeout is hit."
  (declare (ignore timeout))
  (with-slots (in-handle not-console) terminal
    (let ((c (if not-console
		 (read-handle-input in-handle)
		 (read-console-input terminal))))
      (typecase c
	(integer (wchar-to-character c))
	;; @@@ Maybe if it's not a character, we shouldn't return until we get
	;; a character?
	(t c)))))

(defun read-terminal-byte (terminal &key timeout)
  "Return an unsigned byte read from the terminal TERMINAL-HANDLE.
If there's a problem, it will signal a READ-CHAR-ERROR. If the terminal is
resized it will signal an OPSYS-RESIZED. If the program is continued from
being suspended, it will signal an OPSYS-RESUMED. Usually this means the
caller should handle these possibilites. Returns the byte read or NIL if it
the timeout is hit."
  (declare (ignore timeout))
  (with-slots (in-handle not-console) terminal
    (if not-console
	(read-handle-input in-handle)
	(read-console-input terminal))))

(define-condition terminal-read-timeout (opsys-error)
  ()
  (:documentation "The terminal timed out when reading."))

(defun read-until (tty stop-char &key timeout octets-p)
  "Read until STOP-CHAR is read. Return a string of the results.
TTY is a file descriptor."
  (declare (ignore tty stop-char timeout octets-p))
  ;; @@@ taking the lazy slow way out
  ;; (loop :with c = (read-terminal-char tty)
  ;;    :while (char/= c stop-char))
     )

(defun listen-for-terminal (seconds terminal)
  "Wait for ‘seconds’ or any input available on ‘terminal’. Return true if
there is input available."
  (with-foreign-objects ((f-start-time 'ULARGE_INTEGER)
			 (time 'ULARGE_INTEGER))
    (%get-system-time-as-file-time f-start-time)
    (with-slots (in-handle) terminal
      (flush-console-events terminal)
      (let ((duration (seconds-to-100ns seconds))
	    (start-time (mem-ref f-start-time 'ULARGE_INTEGER))
	    expired key-pending)
	(loop
	   :do
	   (listen-for seconds (ms-term-in-handle terminal))
	   (%get-system-time-as-file-time time)
	   (when (>= (- (mem-ref time 'ULARGE_INTEGER) start-time) duration)
	     (setf expired t))
	   (when (and (not expired)
		      (event-pending terminal :types '(:key-char-down)))
	     (setf key-pending t))
	   :while (and (not expired) (not key-pending))
	   :do
	   (flush-console-events terminal :not-types '(:key-char-down)))
	(and (not expired) key-pending)))))

(defcfun ("WriteConsoleW" %write-console)
    BOOL
  (console-output HANDLE)		; in
  (buffer (:pointer VOID))		; in
  (number-of-chars-to-write DWORD)	; in
  (number-of-chars-written LPDWORD)	; out
  (reserved LPVOID))			; reserved

(defcfun ("WriteFile" %write-file)
    BOOL
  (file HANDLE)				; in
  (buffer LPCVOID)			; in
  (number-of-bytes-to-write DWORD)	; in
  (number-of-bytes-written LPDWORD)	; out opt
  (overlapped LPOVERLAPPED))		; in/out opt

(defun write-terminal-string (tty string)
  "Write STRING to the terminal designated by TERMINAL-HANDLE."
  (cond
    ((ms-term-p tty)
     (with-slots (out-handle not-console) tty
       (with-wide-string (str string)
	 (with-foreign-object (written 'DWORD)
	   (if not-console
	       (syscall (%write-file out-handle str (length string)
				     written (null-pointer)))
	       (syscall (%write-console out-handle str (length string)
					written (null-pointer))))
	   ;; @@@ Should we complain if written != length ?
	   (mem-ref written 'DWORD)))))
    ((output-stream-p tty)
     (write-string string tty))))

(defun write-terminal-char (terminal char)
  "Write CHAR to the terminal designated by TERMINAL."
  (cond
    ((output-stream-p terminal)
     (write-char char terminal))
    ((ms-term-p terminal)
     (write-terminal-string terminal (string char)))))

(defun slurp-terminal (tty &key timeout)
  "Read until EOF. Return a string of the results. TTY is a file descriptor."
  (declare (ignore tty timeout))
  ;; @@@ XXX not done?
  "")

(defun set-terminal-mode (tty &key
				(echo    nil echo-supplied)
				(line    nil line-supplied)
				(raw     nil raw-supplied)
				(timeout nil timeout-supplied)
				(mode    nil mode-supplied))
  "Set the terminal mode. Arguments are:
  ECHO makes input automatically output back, so you can see what you typed.
  LINE makes input wait for a newline until returning.
  RAW ingores normal processing, like interrupt keys.
  TIMEOUT is the time in milliseconds to wait before returning with no input.
  MODE is a TERMINAL-MODE structure to take settings from.
The individual settings override the settings in MODE."
  (with-slots (not-console in-handle (our-mode mode)) tty
    (when mode-supplied
      ;; Copy modes from the given mode
      (setf (terminal-mode-echo our-mode) (terminal-mode-echo mode)
	    (terminal-mode-line our-mode) (terminal-mode-line mode)
	    (terminal-mode-raw our-mode) (terminal-mode-raw mode)
	    (terminal-mode-timeout our-mode) (terminal-mode-timeout mode)))
    (when echo-supplied    (setf (terminal-mode-echo our-mode) echo))
    (when line-supplied    (setf (terminal-mode-line our-mode) line))
    (when raw-supplied     (setf (terminal-mode-raw our-mode) raw))
    (when timeout-supplied (setf (terminal-mode-timeout our-mode) timeout))
    (when not-console
      (return-from set-terminal-mode tty))
    (with-foreign-object (ms-mode 'DWORD)
      (when (zerop (%get-console-mode in-handle ms-mode))
	(error 'windows-error :error-code (get-last-error)
	       :format-control "Can't get the console mode."))
      (let ((m (mem-ref ms-mode 'DWORD)))
	(dbugf :ms "console mode was ~s~%" m)
	(when (terminal-mode-echo our-mode)
	  (setf m (logior m +ENABLE-ECHO-INPUT+)))
	(when (terminal-mode-line our-mode)
	  (setf m (logior m +ENABLE-LINE-INPUT+)))
	(when (terminal-mode-raw our-mode)
	  (setf m (logand m (lognot +ENABLE-PROCESSED-INPUT+))))
	;; @@@ set timeout??
	(dbugf :ms "setting console mode ~s ~s~%" in-handle m)
	(when (zerop (%set-console-mode in-handle m))
	  (error 'windows-error :error-code (get-last-error)
		 :format-control "Can't set the console mode.")))))
  tty)

(defun get-terminal-mode (tty)
  "Return a TERMINAL-MODE structure with the current terminal settings."
  (with-slots (not-console in-handle mode) tty
    (when not-console
      (return-from get-terminal-mode mode))
    (with-foreign-object (ms-mode 'DWORD)
      (let ((result (%get-console-mode in-handle ms-mode)))
	(dbugf :ms "get-console-mode = ~s mode = #x~x in-handle = ~s~%"
	       result (mem-ref ms-mode 'DWORD) in-handle)
	(when (zerop result)
	  (error 'windows-error :error-code (get-last-error)
		 :format-control "Can't get the console mode."))
	(let ((m (mem-ref ms-mode 'DWORD)))
	  (setf mode (make-terminal-mode
		      :echo (plusp (logand m +ENABLE-ECHO-INPUT+))
		      :line (plusp (logand m +ENABLE-LINE-INPUT+))
		      :raw (zerop (logand m +ENABLE-PROCESSED-INPUT+))
		      :timeout nil)))))))

(defun reset-terminal-modes (&key file-descriptor device)
  "Set the terminal modes to a normal starting state."
  (declare (ignore device)) ;; @@@
  (if (not file-descriptor)
      (let ((in-h (%get-std-handle +STD-INPUT-HANDLE+)))
	(dbugf :ms "resetting terminal modes to ~s~%" +NORMAL-INPUT-MODES+)
	(when (file-handle-terminal-p in-h)
	  (when (zerop (%set-console-mode in-h +NORMAL-INPUT-MODES+))
	    (error 'windows-error :error-code (get-last-error)
		   :format-control "Can't set console mode.")
	    ;; @@@ but we don't reset the saved ms-term modes!!
	    )))
      (with-slots (not-console in-handle mode) file-descriptor
	(setf mode (make-terminal-mode :echo t :line t :raw nil :timeout nil))
	(when not-console
	  (return-from reset-terminal-modes (values)))
	(dbugf :ms "resetting terminal modes to ~s~%" +NORMAL-INPUT-MODES+)
	(when (zerop (%set-console-mode in-handle +NORMAL-INPUT-MODES+))
	  (error 'windows-error :error-code (get-last-error)
		 :format-control "Can't set console mode."))))
  (values))

(defcstruct SMALL_RECT
  (left   MS-SHORT)
  (top 	  MS-SHORT)
  (right  MS-SHORT)
  (bottom MS-SHORT))

;; DAMNIT THIDS IS STUPDI!!!!
(defun set-rect (rect o1 o2 o3 o4)
  (with-foreign-slots ((left top right bottom)
		       rect (:struct SMALL_RECT))
    (setf left o1
	  top o2 
	  right o3
	  bottom o4)))

(defcstruct CONSOLE_SCREEN_BUFFER_INFO
  (size                (:struct COORD))
  (cursor-position     (:struct COORD))
  (attributes          WORD)
  (window              (:struct SMALL_RECT))
  (maximum-window-size (:struct COORD)))

(defctype PCONSOLE_SCREEN_BUFFER_INFO
    (:pointer (:struct CONSOLE_SCREEN_BUFFER_INFO)))

(defcfun ("GetConsoleScreenBufferInfo" %get-console-screen-buffer-info)
    BOOL
  (console-output HANDLE)				    ; in 
  (console-screen-buffer-info PCONSOLE_SCREEN_BUFFER_INFO)) ; out

(defun get-console-info (tty)
  "Return the values: X Y width height attributes top. TOP is the Y offset of
the first line on visible in the window. Attributes is an integer with bits set
for different text attributes."
  (dbugf :ms "get-window-info tty = ~s~%" tty)
  (with-slots (out-handle width height buffer-width buffer-height) tty
    (let (x y attr)
      (with-foreign-object (buf '(:struct CONSOLE_SCREEN_BUFFER_INFO))
	(when (zerop (%get-console-screen-buffer-info out-handle buf))
	  (error 'windows-error :error-code (get-last-error)
		 :format-control "Can't get console screen size."))
	(with-foreign-slots ((size window cursor-position attributes) buf
			     (:struct CONSOLE_SCREEN_BUFFER_INFO))
	  (dbugf :ms "window = ~s~%curs-pos = ~s ~%" window
		 cursor-position)
	  (setf width (1+ (- (getf window 'right) (getf window 'left)))
		height (1+ (- (getf window 'bottom) (getf window 'top)))
		x (getf cursor-position 'x)
		y (getf cursor-position 'y)
		attr attributes
		buffer-width (getf size 'x)
		buffer-height (getf size 'y))
	  (values x y width height attr (getf window 'top)))))))

(defun get-window-size (tty)
  "Get the window size. The first value is columns, second value is rows."
  (dbugf :ms "get-window-size tty = ~s~%" tty)
  (with-slots (not-console width height buffer-width buffer-height) tty
    (when (not not-console)
      (multiple-value-bind (x y new-width new-height attrs
			      new-buf-width new-buf-height)
	  (get-console-info tty)
	(declare (ignore x y attrs))
	(setf width new-width
	      height new-height
	      buffer-width new-buf-width
	      buffer-height new-buf-height
	      )))
    (values width height)))

(defun get-cursor-position (tty)
  "Get the cursor position. Return as two values, Y and X position."
  (multiple-value-bind (x y) (get-console-info tty)
    (values x y)))

(defun get-attributes (tty)
  "Get the current attributes as an integer."
  (multiple-value-bind (x y width height attr) (get-console-info tty)
    (declare (ignore x y width height))
    (values attr)))

(defctype COLORREF DWORD)

(defcstruct CONSOLE_SCREEN_BUFFER_INFOEX
  (size-in-bytes	ULONG)
  (size			(:struct COORD))
  (cursor-position	(:struct COORD))
  (attributes		WORD)
  (window		(:struct SMALL_RECT))
  (maximum-window-size	(:struct COORD))
  (popup-attributes	WORD)
  (fullscreen-supported	BOOL)
  (color-table		COLORREF :count 16))

(defctype PCONSOLE_SCREEN_BUFFER_INFOEX
    (:pointer (:struct CONSOLE_SCREEN_BUFFER_INFOEX)))

(defcfun ("GetConsoleScreenBufferInfoEx" %get-console-screen-buffer-info-ex)
    BOOL
  (console-output HANDLE)					 ; in
  (console-screen-buffer-info-ex PCONSOLE_SCREEN_BUFFER_INFOEX)) ; out

(defstruct console-extended-info
  size-in-bytes		; ULONG
  size			; (:struct COORD)
  cursor-position	; (:struct COORD)
  attributes		; WORD
  window		; (:struct SMALL_RECT)
  maximum-window-size	; (:struct COORD)
  popup-attributes	; WORD
  fullscreen-supported	; BOOL
  color-table		; COLORREF :count 16
  )

(defun get-console-extended-info (tty)
  "Get the extended attributes for console TTY, returned as the values:
 - an integer representing the text attributes to draw pop-ups with.
 - a boolean indicating whether fullscreen mode is supported
 - an array of 16 color integers of the format 0x00bbggrr, representing
   the color values of the console text colors."
  (with-slots (out-handle) tty
    (with-foreign-object (buf '(:struct CONSOLE_SCREEN_BUFFER_INFOEX))
      (with-foreign-slots ((size-in-bytes
			    popup-attributes fullscreen-supported color-table)
			   buf (:struct CONSOLE_SCREEN_BUFFER_INFOEX))
	(setf size-in-bytes
	      (foreign-type-size '(:struct CONSOLE_SCREEN_BUFFER_INFOEX)))
	(when (zerop (%get-console-screen-buffer-info-ex out-handle buf))
	  (error 'windows-error :error-code (get-last-error)
		 :format-control "Can't get console extended info."))
	(values popup-attributes fullscreen-supported
		(let ((a (make-array 16 :element-type '(unsigned-byte 32))))
		  (loop :for i :from 0 :below 16 :do
		     (setf (aref a i) (mem-aref color-table 'COLORREF i)))
		  a))))))

(defun get-console-all-extended-info (tty)
  "Get the extended attributes for console TTY, returned as a
console-extended-info structure."
  (with-slots (out-handle) tty
    (with-foreign-object (buf '(:struct CONSOLE_SCREEN_BUFFER_INFOEX))
      (with-foreign-slots ((size-in-bytes
			    size
			    cursor-position
			    attributes
			    window
			    maximum-window-size
			    popup-attributes fullscreen-supported color-table)
			   buf (:struct CONSOLE_SCREEN_BUFFER_INFOEX))
	(setf size-in-bytes
	      (foreign-type-size '(:struct CONSOLE_SCREEN_BUFFER_INFOEX)))
	(when (zerop (%get-console-screen-buffer-info-ex out-handle buf))
	  (error 'windows-error :error-code (get-last-error)
		 :format-control "Can't get console extended info."))
	(make-console-extended-info
          :size-in-bytes size-in-bytes	     ; ULONG
          :size
	  (cons (getf size 'x)
		(getf size 'y))
          :cursor-position
	  (cons (getf cursor-position 'x)
		(getf cursor-position 'y))
          :attributes attributes
          :window
	  (list (cons (getf window 'left) (getf window 'top))
		(cons (getf window 'right) (getf window 'bottom)))
          :maximum-window-size
	  (list (getf maximum-window-size 'x)
		(getf maximum-window-size 'y))
          :popup-attributes popup-attributes
          :fullscreen-supported fullscreen-supported
          :color-table
	  (let ((a (make-array 16 :element-type '(unsigned-byte 32))))
	    (loop :for i :from 0 :below 16 :do
	       (setf (aref a i) (mem-aref color-table 'COLORREF i)))
	    a))))))

(defun set-console-all-extended-info (tty info)
  "Set the console extended info from the console-extended-info structure."
  (with-slots (out-handle) tty
    (with-foreign-object (buf '(:struct CONSOLE_SCREEN_BUFFER_INFOEX))
      (with-foreign-slots ((size-in-bytes
			    size
			    cursor-position
			    attributes
			    window
			    maximum-window-size
			    popup-attributes fullscreen-supported color-table)
			   buf (:struct CONSOLE_SCREEN_BUFFER_INFOEX))
	(setf size-in-bytes (console-extended-info-size-in-bytes info))
	(with-foreign-slots ((x y) size (:struct COORD))
	  (setf x (car (console-extended-info-size info))
		y (cdr (console-extended-info-size info))))
	(with-foreign-slots ((x y) cursor-position (:struct COORD))
	  (setf x (car (console-extended-info-cursor-position info))
		y (cdr (console-extended-info-cursor-position info))))
	(setf attributes (console-extended-info-attributes info))
	(with-foreign-slots ((left top right bottom) window (:struct SMALL_RECT))
	  (setf left   (car (first (console-extended-info-window info)))
	        top    (cdr (first (console-extended-info-window info)))
		right  (car (second (console-extended-info-window info)))
	        bottom (cdr (second (console-extended-info-window info)))))
	(with-foreign-slots ((x y) maximum-window-size (:struct COORD))
	  (setf x (car (console-extended-info-maximum-window-size info))
		y (cdr (console-extended-info-maximum-window-size info))))
	(setf popup-attributes
	      (console-extended-info-popup-attributes info)
	      fullscreen-supported
	      (console-extended-info-fullscreen-supported info))
	(loop :for i :from 0 :below 16 :do
	   (setf (mem-aref color-table 'COLORREF i)
		 (aref (console-extended-info-color-table info) i)))
	(when (zerop (%set-console-screen-buffer-info-ex out-handle buf))
	  (error 'windows-error :error-code (get-last-error)
		 :format-control
		 "Can't set console extended info for colors."))))))

(defun get-console-colors (tty)
  "Return an array of 16 color integers of the format 0x00bbggrr, representing
the color values of the console text colors."
  (multiple-value-bind (popup fs-p colors) (get-console-extended-info tty)
    (declare (ignore popup fs-p))
    colors))

(defcfun ("SetConsoleScreenBufferInfoEx" %set-console-screen-buffer-info-ex)
    BOOL
  (console-output HANDLE)					     ; in
  (console-screen-buffer-info-ex PCONSOLE_SCREEN_BUFFER_INFOEX))     ; in

;; @@@ This has a bug, at least on windows 7, that it changes the window size.
(defun set-console-colors (tty colors)
  "Set the console colors from COLORS, which should be an array of 16 color
integers of the format 0x00bbggrr, representing the color values of the console
text colors."
  (with-slots (out-handle) tty
    (with-foreign-object (buf '(:struct CONSOLE_SCREEN_BUFFER_INFOEX))
      (with-foreign-slots ((size-in-bytes color-table) buf
			   (:struct CONSOLE_SCREEN_BUFFER_INFOEX))
	(setf size-in-bytes
	      (foreign-type-size '(:struct CONSOLE_SCREEN_BUFFER_INFOEX)))
	(when (zerop (%get-console-screen-buffer-info-ex out-handle buf))
	  (error 'windows-error :error-code (get-last-error)
		 :format-control "Can't get console extended info for colors."))
	(loop :for i :from 0 :below 16 :do
	   (setf (mem-aref color-table 'COLORREF i) (aref colors i)))
	(when (zerop (%set-console-screen-buffer-info-ex out-handle buf))
	  (error 'windows-error :error-code (get-last-error)
		 :format-control
		 "Can't set console extended info for colors."))))))

(defcstruct CONSOLE_CURSOR_INFO
  (size DWORD)
  (visible BOOL))
(defctype PCONSOLE_CURSOR_INFO (:pointer (:struct CONSOLE_CURSOR_INFO)))

(defcfun ("GetConsoleCursorInfo" %get-console-cursor-info)
    BOOL
  (console-output HANDLE)
  (console-cursor-info PCONSOLE_CURSOR_INFO))

(defun get-cursor-info (tty)
  "Get the cursor info. Returns a size between 1 and 100 inclusive, and a
boolean indicating visibility."
  (with-slots (out-handle) tty
    (with-foreign-object (info '(:struct CONSOLE_CURSOR_INFO))
      (syscall (%get-console-cursor-info out-handle info))
      (values
       (foreign-slot-value info '(:struct CONSOLE_CURSOR_INFO) 'size)
       (plusp (foreign-slot-value info
				  '(:struct CONSOLE_CURSOR_INFO) 'visible))))))

(defcfun ("SetConsoleCursorInfo" %set-console-cursor-info)
    BOOL
  (console-output HANDLE)
  (console-cursor-info PCONSOLE_CURSOR_INFO))

(defun set-cursor-state (tty &key size (visible nil visible-provided-p))
  (with-slots (out-handle) tty
    (when (or (not size) (not visible-provided-p))
      (multiple-value-bind (old-size old-visible)
	  (get-cursor-info tty)
	(when (not size)
	  (setf size old-size))
	(when (not visible-provided-p)
	  (setf visible old-visible))))
    (when (not (and (integerp visible) (or (= visible 0) (= visible 1))))
      (setf visible (if visible 1 0)))
    ;; (when (not (and (integerp size) (>= size 0) (<= size 100)))
    ;;   (setf size 20))
    (with-foreign-object (info '(:struct CONSOLE_CURSOR_INFO))
      (setf (foreign-slot-value info '(:struct CONSOLE_CURSOR_INFO) 'size)
	    size
	    (foreign-slot-value info '(:struct CONSOLE_CURSOR_INFO) 'visible)
	    visible)
      (syscall (%set-console-cursor-info out-handle info)))))

#|
#-ccl
(defcfun ("SetConsoleCursorPosition" %set-console-cursor-position
				     :convention :stdcall)
    BOOL
  (console-output HANDLE)
  (cursor-position (:struct COORD))
  )
#+ccl
(defun %set-console-cursor-position (handle coord)
  ;; (%stack-block ((p 4))
  ;; (getf coord 'x)
  ;; (getf coord 'y)
  (with-foreign-objects ((cc '(:struct coord)))
    (with-foreign-slots ((x y) cc (:struct coord))
      (setf x (getf coord 'x)
	    y (getf coord 'y))
      (#_SetConsoleCursorPosition handle cc))))
(snarble-func ("SetConsoleCursorPosition")
	      WINBOOL
	      (console-output HANDLE)
	      (cursor-position (:struct COORD)))
|#

(defcfun ("SetConsoleCursorPosition" %set-console-cursor-position
				     :convention :stdcall)
    BOOL
  (console-output HANDLE)
  (cursor-position (:struct COORD))
  )

;; (macroexpand-1 '(cffi:defcfun ("SetConsoleCursorPosition" %set-console-cursor-position :convention :stdcall) ms::BOOL (console-output ms::HANDLE) (cursor-position (:struct ms::COORD))))

(defun set-cursor-position (tty row col)
  (with-dbug :ms "set-cursor-position ~s ~s ~%" row col)
  (with-slots (out-handle buffer-width buffer-height) tty
    (when (>= row buffer-height)
      (setf row (1- buffer-height)))
    (when (>= col buffer-width)
      (setf col (1- buffer-width)))
    (let ((rere (%set-console-cursor-position out-handle `(x ,col y ,row))))
      ;; (format t "result = ~s~%" rere)
      (error-check rere "set-cursor-position :"))))

;; (l :terminal-ms)
;; (use-package :terminal)
;; (setf *terminal* (make-instance 'terminal-ms:terminal-ms))
;; (terminal-start *terminal*)
;; (defvar hh)
;; (setf hh (ms::ms-term-out-handle (terminal-file-descriptor *terminal*)))
;; (ms::%zccp hh '(x 0 y 0))
;; (cffi:convert-to-foreign '(x 0 y 0) '(:struct ms::coord))
;; (rletz ((cc :<coord>)) (#_SetConsoleCursorPosition hh cc))
;; (rletz ((cc :<COORD>))
;;   (setf (pref cc :<COORD>.<X>) 0
;;         (pref cc :<COORD>.<X>) 0) (#_SetConsoleCursorPosition hh cc))
;; CRASHES:
;; (ff-call (external "SetConsoleCursorPosition")
;;          :signed-fullword 7
;;          :address 0
;;          :signed-fullword)

#|
(defun fuk ()
  (ccl:%stack-block ((handle 8) (coord 8))
		    (setf (ccl::%get-signed-doubleword handle) 7
			  (ccl::%get-signed-long coord) 0
			  (ccl::%get-signed-long coord 2) 0)
		    (ccl::ff-call (ccl::external "SetConsoleCursorPosition")
				  :address handle
				  4 coord
				  :signed-fullword)))

(defun %zccp (console-output cursor-position)
  (let (val param struct-vals result)
    (unwind-protect
	 (let (s-cursor-position)
	   (multiple-value-setq (val param)
	     (cffi:convert-to-foreign cursor-position
				      (list :struct 'coord)))
	   (setf s-cursor-position val)
	   (push (list val '(:struct coord) param) struct-vals)
	   (setf result (ccl:external-call "SetConsoleCursorPosition"
					   :<HANDLE> console-output
					   :<COORD> s-cursor-position
					   :<WINBOOL>)))
      (loop :for (val type param) :in struct-vals
	 :do (cffi:free-converted-object val type param)))
    result))
|#

(defcstruct CHAR_INFO
  (uchar (:union foreign-uchar))
  (attributes WORD))
(defctype PCHAR_INFO (:pointer (:struct CHAR_INFO)))

;; (defcfun ("ScrollConsoleScreenBufferW" %scroll-console-screen-buffer)
;;     BOOL
;;   (console-output HANDLE)			     ; in
;;   (scroll-rectangle (:pointer (:struct SMALL_RECT))) ; in
;;   (clip-rectangle (:pointer (:struct SMALL_RECT)))   ; in optional
;;   (destination-origin (:struct COORD))		     ; in
;;   (fill (:pointer (:struct CHAR_INFO))))	     ; in
(snarble-func ("ScrollConsoleScreenBufferW")
    WINBOOL
  (console-output HANDLE)			     ; in
  (scroll-rectangle (:pointer (:struct SMALL_RECT))) ; in
  (clip-rectangle (:pointer (:struct SMALL_RECT)))   ; in optional
  (destination-origin (:struct COORD))		     ; in
  (fill (:pointer (:struct CHAR_INFO))))	     ; in

(defun scroll-console (tty &key (left 0) (top 0) right bottom x y)
  (with-slots (out-handle) tty
    (with-foreign-objects ((scroll-rect '(:struct SMALL_RECT))
			   ;;(clip-rect '(:struct SMALL_RECT))
			   (fill-char '(:struct CHAR_INFO))
			   (stupid-uchar '(:union foreign-uchar))
			   (dest '(:struct COORD)))
      (set-wchar stupid-uchar 0 #\space)
      (with-foreign-slots ((uchar attributes)
			   fill-char (:struct CHAR_INFO))
	(setf attributes 0
	      uchar stupid-uchar))
      (set-rect scroll-rect left top right bottom)
      (set-coord dest x y)

#|
      (setf (mem-ref fill-char '(:struct CHAR_INFO))
	    (convert-to-foreign `(char ,uchar attributes 0)
				'(:struct CHAR_INFO))
	    (mem-ref scroll-rect '(:struct SMALL_RECT))
	    (convert-to-foreign `(left ,left top ,top
				       :right ,right :bottom, bottom)
				'(:struct CHAR_INFO))
	    ;; (mem-ref clip-rect '(:struct SMALL_RECT)) ;
	    ;; (convert-to-foreign `(left ,left top ,top ;
	    ;;                       right ,right bottom ,bottom) ;
	    ;; 			'(:struct CHAR_INFO))) ;
	    (mem-ref scroll-rect '(:struct SMALL_RECT))
	    (convert-to-foreign `(left ,left top ,top
				       right ,right bottom, bottom)
				'(:struct SMALL_RECT))
|#

      (syscall (%scroll-console-screen-buffer-w
		out-handle
		scroll-rect (null-pointer)
		;;(mem-ref dest '(:struct COORD))
		`(x ,x y ,y)
		fill-char)))))

;; (defcfun ("FillConsoleOutputCharacterW" %fill-console-output-character-w
;; 					:convention :stdcall)
;;     BOOL
;;   (console-output HANDLE)			  ; in
;;   (character TCHAR)				  ; in
;;   (length DWORD)				  ; in
;;   (write-coord (:struct COORD))			  ; in
;;   (number-of-chars-written LPDWORD))		  ; out
(snarble-func ("FillConsoleOutputCharacterW")
    WINBOOL
  (console-output HANDLE)			  ; in
  (character TCHAR)				  ; in
  (length DWORD)				  ; in
  (write-coord (:struct COORD))			  ; in
  (number-of-chars-written LPDWORD))		  ; out

(defun fill-console-char (tty &key (char #\space) (x 0) (y 0) length)
  (when (not length)
    (multiple-value-bind (x y width height) (get-console-info tty)
      (declare (ignore x y))
      (setf length (* width height))))
  (with-slots (out-handle) tty
    (with-foreign-objects ((chars-written 'DWORD)
			   ;;(tchar 'TCHAR)
			   ;;(write-at '(:struct COORD))
			   )
      ;(set-wchar tchar 0 char)
      ;;(set-coord write-at x y)
      (syscall (%fill-console-output-character-w
		out-handle
		(character-to-wchar char)
		length
		;;(convert-to-foreign `(x ,x y ,y) '(:struct COORD))
		;;(mem-ref write-at '(:struct COORD))
		;;write-at
		`(x ,x y ,y)
		chars-written))
      (mem-ref chars-written 'DWORD))))

(defcfun ("SetConsoleTextAttribute" %set-console-text-attribute)
    BOOL
  (console-output HANDLE)
  (attributes WORD))

(defconstant +FOREGROUND-BLUE+      #x0001)
(defconstant +FOREGROUND-GREEN+     #x0002)
(defconstant +FOREGROUND-RED+       #x0004)
(defconstant +FOREGROUND-INTENSITY+ #x0008)
(defconstant +BACKGROUND-BLUE+      #x0010)
(defconstant +BACKGROUND-GREEN+     #x0020)
(defconstant +BACKGROUND-RED+       #x0040)
(defconstant +BACKGROUND-INTENSITY+ #x0080)

(defconstant +COMMON-LVB-REVERSE-VIDEO+ #x4000)
(defconstant +COMMON-LVB-UNDERSCORE+    #x8000)

(defun set-console-attribute (tty attribute)
  (with-slots (out-handle) tty
    (%set-console-text-attribute out-handle attribute)))

;; (defcfun ("FillConsoleOutputAttribute" %fill-console-output-attribute)
;;     BOOL
;;   (console-output HANDLE)  			; in
;;   (attribute WORD)    			  	; in
;;   (length DWORD)   				; in
;;   (write-coord (:struct COORD))   		; in
;;   (number-of-attrs-written LPDWORD)) 		; out
(snarble-func ("FillConsoleOutputAttribute")
    WINBOOL
  (console-output HANDLE)  			; in
  (attribute WORD)    			  	; in
  (length DWORD)   				; in
  (write-coord (:struct COORD))   		; in
  (number-of-attrs-written LPDWORD)) 		; out

(defun fill-console-attribute (tty &key (attribute 0) (x 0) (y 0) length)
  (when (not length)
    (multiple-value-bind (x y width height) (get-console-info tty)
      (declare (ignore x y))
      (setf length (* width height))))
  (with-slots (out-handle) tty
    (with-foreign-objects ((chars-written 'DWORD))
      (syscall (%fill-console-output-attribute
		out-handle
		attribute
		length
		`(x ,x y ,y)
		chars-written))
      (mem-ref chars-written 'DWORD))))

(defmacro with-nonblocking-io ((fd) &body body)
  (declare (ignore fd))
  ;; @@@
  `(progn ,@body)
  )

(defun terminal-time (fd)
  "Return the last modification time for the terminal device or NIL if we can't
get it."
  ;; @@@ maybe figure this out?
  (declare (ignore fd))
  nil)

;; (defun terminal-query (query &key max tty)
;;   "Output the string to the terminal and wait for a response. Read up to MAX
;; characters. If we don't get anything after a while, just return what we got."
;;   (declare (ignore query max tty))
;;   ;; @@@ XXX
;;   "")
(defun query-terminal (fd query end-tag &key buffer-size (timeout 2.5))
  ;; (declare (ignore buffer-size))	; @@@
  ;; (write-terminal-string fd query)
  ;; (if (listen-for-terminal timeout fd)
  ;;     (read-until fd end-tag :timeout (or timeout 2.5))
  ;;     ""))
  ;; @@@ keep faking it for now
  (declare (ignore fd query end-tag buffer-size timeout))
  "")

(defmacro with-terminal-signals (() &body body)
  "Evaluate the BODY with signal handlers set appropriately for reading from
a terminal."
  `(progn
     ;; What, if anything, should we do here?
     ,@body))

(defcfun ("GetConsoleTitleW" %get-console-title-w)
    DWORD
  (console-title LPTSTR) ; out
  (size DWORD))		 ; in

(defun get-console-title ()
  ;; Actually the maximum is supposedly 64k, but example code uses MAX-PATH.
  (let ((title-size (min (* +MAX-PATH+ 4) (ash 1 14))))
    (with-foreign-object (title 'WCHAR title-size)
      (%get-console-title-w title title-size)
      (wide-string-to-lisp title))))

(defcfun ("SetConsoleTitleW" %set-console-title-w)
    BOOL
  (console-title LPCTSTR)) ; In

(defun set-console-title (title)
  (let ((real-title (if (>= (length title) (ash 1 14))
			(subseq title 0 (1- (ash 1 14)))
			title)))
    (with-wide-string (str real-title)
      (%set-console-title-w str))))

;; End
