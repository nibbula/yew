;;;
;;; termios.lisp - Interface to POSIX terminal driver
;;;

;;; TODO:
;;;   * Consider putting some kind of earmuffs around constants and variables.
;;;   * More platforms: cygwin, *BSD, ...
;;;   * Esoteric stuff like STI, modem control, console, packet data, etc,
;;;     stuff in "man 4 tty" (not strictly termios)

(declaim (optimize (debug 3)))

(defpackage "TERMIOS"
  (:use :cl :cffi :dlib :opsys-base :os-unix)
  (:documentation
   "Interface to POSIX (and some non-POSIX) terminal driver.")
  (:export
   ;; tty chars
   #:+VEOF+ #:+VEOL+ #:+VEOL2+ #:+VERASE+ #:+VWERASE+ #:+VKILL+ #:+VREPRINT+
   #:+VINTR+ #:+VQUIT+ #:+VSUSP+ #:+VDSUSP+ #:+VSTART+ #:+VSTOP+ #:+VLNEXT+
   #:+VDISCARD+ #:+VMIN+ #:+VTIME+
   #:+VSTATUS+ #:+NCCS+ #:*cchars*

   ;; input modes
   #:+IGNBRK+ #:+BRKINT+ #:+IGNPAR+ #:+PARMRK+ #:+INPCK+ #:+ISTRIP+ #:+INLCR+
   #:+IGNCR+ #:+ICRNL+ #:+IXON+ #:+IXOFF+ #:+IXANY+ #:+IMAXBEL+ #:+IUCLC+
   #:+IUTF8+ #:+DOSMODE+ #:*iflags*

   ;; output modes
   #:+OPOST+ #:+ONLCR+ #:+OXTABS+ #:+ONOEOT+ #:*oflags*

   ;; control flags
   #:+CIGNORE+ #:+CSIZE+ #:+CS5+ #:+CS6+ #:+CS7+ #:+CS8+ #:+CSTOPB+ #:+CREAD+
   #:+PARENB+ #:+PARODD+ #:+HUPCL+ #:+CLOCAL+ #:+CCTS+ #:+CRTS+ #:+CRTSCTS+
   #:+CDTR+ #:+CDSR+ #:+CCAR+ #:+MDMBUF+ #:*cflags*

   ;; other "local" flags
   #:+ECHOKE+ #:+ECHOE+ #:+ECHOK+ #:+ECHO+ #:+ECHONL+ #:+ECHOPRT+ #:+ECHOCTL+
   #:+ISIG+ #:+ICANON+ #:+ALTWERASE+ #:+IEXTEN+ #:+EXTPROC+ #:+TOSTOP+
   #:+FLUSHO+ #:+NOKERNINFO+ #:+PENDIN+ #:+NOFLSH+ #:*lflags*

   ;; actions
   #:+TCSANOW+ #:+TCSADRAIN+ #:+TCSAFLUSH+ #:+TCSASOFT+
   #:+TCIFLUSH+ #:+TCOFLUSH+ #:+TCIOFLUSH+ #:+TCOOFF+ #:+TCOON+ #:+TCIOFF+
   #:+TCION+

   ;; speeds
   #:+B0+ #:+B50+ #:+B75+ #:+B110+ #:+B134+ #:+B150+ #:+B200+ #:+B300+ #:+B600+
   #:+B1200+ #:+B1800+ #:+B2400+ #:+B4800+ #:+B9600+ #:+B19200+ #:+B38400+
   #:+B7200+ #:+B14400+ #:+B28800+ #:+B57600+ #:+B76800+ #:+B115200+
   #:+B230400+ #:+EXTA+ #:+EXTB+

   ;; types
   #:tcflag-t
   #:cc-t
   #:speed-t
   #:termios
   ;; slot names
   #:c_iflag #:c_oflag #:c_cflag #:c_lflag #:c_cc #:c_ispeed #:c_ospeed

   ;; Posix-y Functions
   #:cfgetispeed
   #:cfgetospeed
   #:cfsetispeed
   #:cfsetospeed
   #:tcgetattr
   #:tcsetattr
   #:tcdrain
   #:tcflow
   #:tcflush
   #:tcsendbreak
   #:cfmakeraw
   #:cfsetspeed

   ;; Additional functions
   #:sane
   #:terminal-query
   #:call-with-raw
   #:describe-tty
   #:set-tty
   #:getch

   ;; old fashioned tty ioctls
   #:winsize
   #:ws_row
   #:ws_col
   #:ws_xpixel
   #:ws_ypixel
   #:+TIOCSTI+
   #:+TIOCGWINSZ+
   #:+TIOCSWINSZ+

   ;; Portable interafce
   ;; #:set-terminal-mode
   ;; #:get-terminal-mode
   ;; #:get-window-size
   ;; #:slurp-terminal

   ;; tests
   #:test
   #:test-echo
   #:test-input
  )
)
(in-package "TERMIOS")

#-(or darwin sunos linux) (error "Your platform is not really supported yet.")

; @@@ do we need this?
;(define-foreign-library libc
;    (t (:default "libc")))
;(use-foreign-library libc)

#+(and darwin 64-bit-target) (defctype tcflag-t :unsigned-long)
#-(and darwin 64-bit-target) (defctype tcflag-t :unsigned-int)
#+(and darwin 64-bit-target) (defctype speed-t :unsigned-long)
#-(and darwin 64-bit-target) (defctype speed-t :unsigned-int)
(defctype cc-t :unsigned-char)

;; control chars (c_cc)
(defconstant +VEOF+	#+darwin 0   #+sunos 4   #+linux 4)
(defconstant +VEOL+	#+darwin 1   #+sunos 5   #+linux 11)
(defconstant +VEOL2+	#+darwin 2   #+sunos 6   #+linux 16)
(defconstant +VERASE+	#+darwin 3   #+sunos 2   #+linux 2)
(defconstant +VWERASE+	#+darwin 4   #+sunos 14  #+linux 14)
(defconstant +VKILL+	#+darwin 5   #+sunos 3   #+linux 3)
(defconstant +VREPRINT+	#+darwin 6   #+sunos 12  #+linux 12)
(defconstant +VINTR+	#+darwin 8   #+sunos 0   #+linux 0)
(defconstant +VQUIT+	#+darwin 9   #+sunos 1   #+linux 1)
(defconstant +VSUSP+	#+darwin 10  #+sunos 10  #+linux 10)
(defconstant +VDSUSP+	#+darwin 11  #+sunos 11  #+linux nil)
(defconstant +VSWTCH+	#+darwin nil #+sunos 7   #+linux nil)
(defconstant +VSTART+	#+darwin 12  #+sunos 8   #+linux 8)
(defconstant +VSTOP+	#+darwin 13  #+sunos 9   #+linux 9)
(defconstant +VLNEXT+	#+darwin 14  #+sunos 15  #+linux 15)
(defconstant +VDISCARD+	#+darwin 15  #+sunos 13  #+linux 13)
(defconstant +VMIN+	#+darwin 16  #+sunos 4   #+linux 6)
(defconstant +VTIME+	#+darwin 17  #+sunos 5   #+linux 5)
(defconstant +VSTATUS+	#+darwin 18  #+sunos nil #+linux nil)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +NCCS+	#+darwin 20  #+sunos 19  #+linux 32))

(defparameter *cchars*
  '(+VDISCARD+ #-linux +VDSUSP+ +VEOF+ +VEOL+ +VEOL2+ +VERASE+ +VINTR+ +VKILL+
    +VLNEXT+ +VMIN+ +VQUIT+ +VREPRINT+ +VSTART+ #+darwin +VSTATUS+ +VSTOP+
    +VSUSP+ +VTIME+ +VWERASE+)
  "A list of all the control character symbols.")

(defcstruct termios
    "POSIX terminal driver interface structure"
  (c_iflag  tcflag-t)			; input flags
  (c_oflag  tcflag-t)			; output flags
  (c_cflag  tcflag-t)			; control flags
  (c_lflag  tcflag-t)			; local flags
  #+linux (c_line cc-t)			; line discipline
  (c_cc     cc-t :count #.+NCCS+)	; control chars
  ;;XXX count Should be NCCS!
  (c_ispeed speed-t)			; input speed
  (c_ospeed speed-t))			; output speed

;; input flags (c_iflag)
(defconstant +IGNBRK+	#+darwin #x00000001 #+sunos #o0000001 #+linux #o0000001)	; ignore BREAK condition
(defconstant +BRKINT+	#+darwin #x00000002 #+sunos #o0000002 #+linux #o0000002)	; map BREAK to SIGINTR
(defconstant +IGNPAR+	#+darwin #x00000004 #+sunos #o0000004 #+linux #o0000004)	; ignore (discard) parity errors
(defconstant +PARMRK+	#+darwin #x00000008 #+sunos #o0000010 #+linux #o0000010)	; mark parity and framing errors
(defconstant +INPCK+	#+darwin #x00000010 #+sunos #o0000020 #+linux #o0000020)	; enable checking of parity errors
(defconstant +ISTRIP+	#+darwin #x00000020 #+sunos #o0000040 #+linux #o0000040)	; strip 8th bit off chars
(defconstant +INLCR+	#+darwin #x00000040 #+sunos #o0000100 #+linux #o0000100)	; map NL into CR
(defconstant +IGNCR+	#+darwin #x00000080 #+sunos #o0000200 #+linux #o0000200)	; ignore CR
(defconstant +ICRNL+	#+darwin #x00000100 #+sunos #o0000400 #+linux #o0000400)	; map CR to NL (ala CRMOD)
(defconstant +IXON+	#+darwin #x00000200 #+sunos #o0002000 #+linux #o0002000)	; enable output flow control
(defconstant +IXOFF+	#+darwin #x00000400 #+sunos #o0010000 #+linux #o0010000)	; enable input flow control
(defconstant +IXANY+	#+darwin #x00000800 #+sunos #o0004000 #+linux #o0004000)	; any char will restart after stop
(defconstant +IMAXBEL+	#+darwin #x00002000 #+sunos #o0020000 #+linux #o0020000)	; ring bell on input queue full
(defconstant +IUCLC+	#+darwin nil        #+sunos #o0001000 #+linux #o0001000)	; @@@ add
(defconstant +IUTF8+	#+darwin nil        #+sunos nil       #+linux #o0040000)	; @@@ add
(defconstant +DOSMODE+	#+darwin nil        #+sunos #o0100000 #+linux nil)		; @@@ add

(defparameter *iflags*
  '(+IGNBRK+ +BRKINT+ +IGNPAR+ +PARMRK+ +INPCK+ +ISTRIP+ +INLCR+ +IGNCR+ +ICRNL+
    +IXON+ +IXOFF+ +IXANY+ +IMAXBEL+
    #+(or linux sunos) +IUCLC+
    #+linux +IUTF8+
    #+sunos +DOSMODE+)
  "List of the input flag symbols.")

;; output flags (c_oflag)
(defconstant +OPOST+	#+darwin #x00000001 #+sunos #o0000001 #+linux #o0000001)	; enable following output processing
(defconstant +ONLCR+	#+darwin #x00000002 #+sunos #o0000004 #+linux #o0000004)	; map NL to CR-NL (ala CRMOD)
(defconstant +OXTABS+	#+darwin #x00000004 #+sunos #o0014000 #+linux #o0014000)	; expand tabs to spaces
(defconstant +ONOEOT+	#+darwin #x00000008 #+sunos nil       #+linux nil)	; discard EOT's (^D) on output)
(defconstant +OLCUC+	#+darwin nil        #+sunos #o0000002 #+linux #o0000002)	; @@@ add
(defconstant +OCRNL+	#+darwin nil        #+sunos #o0000010 #+linux #o0000010)	; @@@ add
(defconstant +ONOCR+	#+darwin #x00000020 #+sunos #o0000020 #+linux #o0000020)	; @@@ add
(defconstant +ONLRET+	#+darwin #x00000040 #+sunos #o0000040 #+linux #o0000040)	; @@@ add
(defconstant +OFILL+	#+darwin nil        #+sunos #o0000100 #+linux #o0000100)	; @@@ add
(defconstant +OFDEL+	#+darwin nil        #+sunos #o0000200 #+linux #o0000200)	; @@@ add

(defconstant  +NLDLY+   #+darwin nil        #+sunos #o0000400 #+linux #o0000400)
(defconstant    +NL0+   #+darwin nil        #+sunos #o0000000 #+linux #o0000000)
(defconstant    +NL1+   #+darwin nil        #+sunos #o0000400 #+linux #o0000400)
(defconstant  +CRDLY+   #+darwin nil        #+sunos #o0003000 #+linux #o0003000)
(defconstant    +CR0+   #+darwin nil        #+sunos #o0000000 #+linux #o0000000)
(defconstant    +CR1+   #+darwin nil        #+sunos #o0001000 #+linux #o0001000)
(defconstant    +CR2+   #+darwin nil        #+sunos #o0002000 #+linux #o0002000)
(defconstant    +CR3+   #+darwin nil        #+sunos #o0003000 #+linux #o0003000)
(defconstant +TABDLY+   #+darwin nil        #+sunos #o0014000 #+linux #o0014000)
(defconstant   +TAB0+   #+darwin nil        #+sunos #o0000000 #+linux #o0000000)
(defconstant   +TAB1+   #+darwin nil        #+sunos #o0004000 #+linux #o0004000)
(defconstant   +TAB2+   #+darwin nil        #+sunos #o0010000 #+linux #o0010000)
(defconstant   +TAB3+   #+darwin nil        #+sunos #o0014000 #+linux #o0014000)
(defconstant  +BSDLY+   #+darwin nil        #+sunos #o0020000 #+linux #o0020000)
(defconstant    +BS0+   #+darwin nil        #+sunos #o0000000 #+linux #o0000000)
(defconstant    +BS1+   #+darwin nil        #+sunos #o0020000 #+linux #o0020000)
(defconstant  +FFDLY+   #+darwin nil        #+sunos #o0100000 #+linux #o0100000)
(defconstant    +FF0+   #+darwin nil        #+sunos #o0000000 #+linux #o0000000)
(defconstant    +FF1+   #+darwin nil        #+sunos #o0100000 #+linux #o0100000)
(defconstant  +VTDLY+   #+darwin nil        #+sunos #o0040000 #+linux #o0040000)
(defconstant    +VT0+   #+darwin nil        #+sunos #o0000000 #+linux #o0000000)
(defconstant    +VT1+   #+darwin nil        #+sunos #o0040000 #+linux #o0040000)
(defconstant  +XTABS+   #+darwin nil        #+sunos nil       #+linux #o0014000)
(defconstant +PAGEOUT+  #+darwin nil        #+sunos #o0200000 #+linux nil)
(defconstant +WRAP+     #+darwin nil        #+sunos #o0400000 #+linux nil)

(defparameter *oflags*
  '(
    +OPOST+
    +ONLCR+
    +OXTABS+
    #+darwin +ONOEOT+
    #+(or sunos linux) +OLCUC+
    #+(or sunos linux) +OCRNL+
    +ONOCR+
    +ONLRET+
    #+(or sunos linux) +OFILL+
    #+(or sunos linux) +OFDEL+
    #+(or sunos linux) +NLDLY+
    #+(or sunos linux)   +NL0+
    #+(or sunos linux)   +NL1+
    #+(or sunos linux) +CRDLY+
    #+(or sunos linux)   +CR0+
    #+(or sunos linux)   +CR1+
    #+(or sunos linux)   +CR2+
    #+(or sunos linux)   +CR3+
    #+(or sunos linux) +TABDLY+
    #+(or sunos linux)   +TAB0+
    #+(or sunos linux)   +TAB1+
    #+(or sunos linux)   +TAB2+
    #+(or sunos linux)   +TAB3+
    #+(or sunos linux) +BSDLY+
    #+(or sunos linux)   +BS0+
    #+(or sunos linux)   +BS1+
    #+(or sunos linux) +FFDLY+
    #+(or sunos linux)   +FF0+
    #+(or sunos linux)   +FF1+
    #+(or sunos linux) +VTDLY+
    #+(or sunos linux)   +VT0+
    #+(or sunos linux)   +VT1+
    #+sunos +PAGEOUT+
    #+sunos +WRAP+
    )
  "List of oflags symbols.")

;; Control flags - hardware control of terminal (c_cflag)
(defconstant +CIGNORE+	  #+darwin #x00000001 #+sunos nil       #+linux nil) ; ignore control flags
(defconstant +CSIZE+	  #+darwin #x00000300 #+sunos #o0000060 #+linux #o0000060) ; character size mask
(defconstant +CS5+	  #+darwin #x00000000 #+sunos #o0000000 #+linux #o0000000) ; 5 bits (pseudo)
(defconstant +CS6+	  #+darwin #x00000100 #+sunos #o0000020 #+linux #o0000020) ; 6 bits
(defconstant +CS7+	  #+darwin #x00000200 #+sunos #o0000040 #+linux #o0000040) ; 7 bits
(defconstant +CS8+	  #+darwin #x00000300 #+sunos #o0000060 #+linux #o0000060) ; 8 bits
(defconstant +CSTOPB+	  #+darwin #x00000400 #+sunos #o0000100 #+linux #o0000100) ; send 2 stop bits
(defconstant +CREAD+	  #+darwin #x00000800 #+sunos #o0000200 #+linux #o0000200) ; enable receiver
(defconstant +PARENB+	  #+darwin #x00001000 #+sunos #o0000400 #+linux #o0000400) ; parity enable
(defconstant +PARODD+	  #+darwin #x00002000 #+sunos #o0001000 #+linux #o0001000) ; odd parity, else even
(defconstant +HUPCL+	  #+darwin #x00004000 #+sunos #o0002000 #+linux #o0002000) ; hang up on last close
(defconstant +CLOCAL+	  #+darwin #x00008000 #+sunos #o0004000 #+linux #o0004000) ; ignore modem status lines
(defconstant +CCTS_OFLOW+ #+darwin #x00010000 #+sunos #o010000000000 #+linux nil) ; CTS flow control of output
(defconstant +CRTS_IFLOW+ #+darwin #x00020000 #+sunos #o020000000000 #+linux nil) ; RTS flow control of input
(defconstant +CRTSCTS+	  #+darwin (logior +CCTS_OFLOW+ +CRTS_IFLOW+) #+sunos #o020000000000 #+linux #o020000000000)
(defconstant +CDTR_IFLOW+ #+darwin #x00040000 #+sunos nil       #+linux nil) ; DTR flow control of input
(defconstant +CDSR_OFLOW+ #+darwin #x00080000 #+sunos nil       #+linux nil) ; DSR flow control of output
(defconstant +CCAR_OFLOW+ #+darwin #x00100000 #+sunos nil       #+linux nil) ; DCD flow control of output
(defconstant +MDMBUF+	  #+darwin #x00100000 #+sunos nil       #+linux nil) ; old name for CCAR_OFLOW
(defconstant +CBAUDEX+	  #+darwin nil        #+sunos nil       #+linux #o0010000)

(defparameter *cflags*
  '(#+darwin +CIGNORE+
    +CSIZE+ +CS5+ +CS6+ +CS7+ +CS8+ +CSTOPB+ +CREAD+ +PARENB+ +PARODD+
    +HUPCL+ +CLOCAL+ +CCTS_OFLOW+ +CRTS_IFLOW+ +CRTSCTS+
    #+darwin +CDTR_IFLOW+
    #+darwin +CDSR_OFLOW+
    #+darwin +CCAR_OFLOW+
    #+darwin +MDMBUF+
    #+linux +CBAUDEX+)
  "List of the control flag symbols.")

;; local flags (c_lflag)
(defconstant +ECHOKE+	  #+darwin #x00000001 #+sunos #o0004000 #+linux #o0004000)	; visual erase for line kill
(defconstant +ECHOE+	  #+darwin #x00000002 #+sunos #o0000010 #+linux #o0000020)	; visually erase chars 
(defconstant +ECHOK+	  #+darwin #x00000004 #+sunos #o0000040 #+linux #o0000040)	; echo NL after line kill 
(defconstant +ECHO+	  #+darwin #x00000008 #+sunos #o0000010 #+linux #o0000010)	; enable echoing 
(defconstant +ECHONL+	  #+darwin #x00000010 #+sunos #o0000100 #+linux #o0000100)	; echo NL even if ECHO is off 
(defconstant +ECHOPRT+	  #+darwin #x00000020 #+sunos #o0002000 #+linux #o0002000)	; visual erase mode for hardcopy 
(defconstant +ECHOCTL+    #+darwin #x00000040 #+sunos #o0001000 #+linux #o0001000)	; echo control chars as ^(Char) 
(defconstant +ISIG+	  #+darwin #x00000080 #+sunos #o0000001 #+linux #o0000001)	; enable signals INTR, QUIT, [D]SUSP 
(defconstant +ICANON+	  #+darwin #x00000100 #+sunos #o0000002 #+linux #o0000002)	; canonicalize input lines 
(defconstant +ALTWERASE+  #+darwin #x00000200 #+sunos nil       #+linux nil)		; use alternate WERASE algorithm 
(defconstant +IEXTEN+	  #+darwin #x00000400 #+sunos #o0100000 #+linux #o0100000)	; enable DISCARD and LNEXT 
(defconstant +EXTPROC+	  #+darwin #x00000800 #+sunos nil       #+linux nil)		; external processing
(defconstant +TOSTOP+	  #+darwin #x00400000 #+sunos #o0000400 #+linux #o0000400)	; stop background jobs from output 
(defconstant +FLUSHO+	  #+darwin #x00800000 #+sunos #o0020000 #+linux #o0010000)	; output being flushed (state) 
(defconstant +NOKERNINFO+ #+darwin #x02000000 #+sunos nil       #+linux nil)		; no kernel output from VSTATUS 
(defconstant +PENDIN+	  #+darwin #x20000000 #+sunos #o0040000 #+linux #o0040000)	; XXX retype pending input (state) 
(defconstant +NOFLSH+	  #+darwin #x80000000 #+sunos #o0000200 #+linux #o0000200)	; don't flush after interrupt

(defparameter *lflags*
  '(+ECHOKE+ +ECHOE+ +ECHOK+ +ECHO+ +ECHONL+ +ECHOPRT+ +ECHOCTL+ +ISIG+
    +ICANON+ #+darwin
    +ALTWERASE+ +IEXTEN+
    #+darwin
    +EXTPROC+ +TOSTOP+ +FLUSHO+
    #+darwin +NOKERNINFO+
    +PENDIN+ +NOFLSH+)
  "List of lflag symbols.")

#+sunos (defconstant +TIOC+ (ash 84 8))

; Commands passed to tcsetattr() for setting the termios structure.
(defconstant +TCSANOW+	 #+darwin 0 #+sunos (logior +TIOC+ 14) #+linux 0)		; make change immediate 
(defconstant +TCSADRAIN+ #+darwin 1 #+sunos (logior +TIOC+ 15) #+linux 1)		; drain output, then change 
(defconstant +TCSAFLUSH+ #+darwin 2 #+sunos (logior +TIOC+ 16) #+linux 2)		; drain output, flush input 
#+darwin (defconstant +TCSASOFT+	#x10)		; flag - don't alter h.w. state 

; Standard speeds
(defconstant +B0+	#+darwin 0      #+sunos 0   #+linux #o0000000)
(defconstant +B50+	#+darwin 50     #+sunos 1   #+linux #o0000001)
(defconstant +B75+	#+darwin 75     #+sunos 2   #+linux #o0000002)
(defconstant +B110+	#+darwin 110    #+sunos 3   #+linux #o0000003)
(defconstant +B134+	#+darwin 134    #+sunos 4   #+linux #o0000004)
(defconstant +B150+	#+darwin 150    #+sunos 5   #+linux #o0000005)
(defconstant +B200+	#+darwin 200    #+sunos 6   #+linux #o0000006)
(defconstant +B300+	#+darwin 300    #+sunos 7   #+linux #o0000007)
(defconstant +B600+	#+darwin 600    #+sunos 8   #+linux #o0000010)
(defconstant +B1200+	#+darwin 1200   #+sunos 9   #+linux #o0000011)
(defconstant +B1800+	#+darwin 1800   #+sunos 10  #+linux #o0000012)
(defconstant +B2400+	#+darwin 2400   #+sunos 11  #+linux #o0000013)
(defconstant +B4800+	#+darwin 4800   #+sunos 12  #+linux #o0000014)
(defconstant +B9600+	#+darwin 9600   #+sunos 13  #+linux #o0000015)
(defconstant +B19200+	#+darwin 19200  #+sunos 14  #+linux #o0000016)
(defconstant +B38400+	#+darwin 38400  #+sunos 15  #+linux #o0000017)
(defconstant +B7200+	#+darwin 7200   #+sunos nil #+linux nil)
(defconstant +B14400+	#+darwin 14400  #+sunos nil #+linux nil)
(defconstant +B28800+	#+darwin 28800  #+sunos nil #+linux nil)
(defconstant +B57600+	#+darwin 57600  #+sunos 16  #+linux nil)
(defconstant +B76800+	#+darwin 76800  #+sunos 17  #+linux nil)
(defconstant +B115200+	#+darwin 115200 #+sunos 18  #+linux nil)
(defconstant +B230400+	#+darwin 230400 #+sunos 20  #+linux nil)
(defconstant +EXTA+	#+darwin 19200  #+sunos nil #+linux #o0000016)
(defconstant +EXTB+	#+darwin 38400  #+sunos nil #+linux #o0000017)

(defconstant +TCIFLUSH+	 #+darwin 1 #+sunos 0 #+linux 0)
(defconstant +TCOFLUSH+	 #+darwin 2 #+sunos 1 #+linux 1)
(defconstant +TCIOFLUSH+ #+darwin 3 #+sunos 2 #+linux 2)
(defconstant +TCOOFF+	 #+darwin 1 #+sunos 0 #+linux 0)
(defconstant +TCOON+	 #+darwin 2 #+sunos 1 #+linux 1)
(defconstant +TCIOFF+	 #+darwin 3 #+sunos 2 #+linux 2)
(defconstant +TCION+	 #+darwin 4 #+sunos 3 #+linux 3)

(defcfun cfgetispeed speed-t (tio :pointer))
(defcfun cfgetospeed speed-t (tio :pointer))
(defcfun cfsetispeed :int (tio :pointer) (speed speed-t))
(defcfun cfsetospeed :int (tio :pointer) (speed speed-t))
(defcfun tcgetattr :int (fd :int) (tio :pointer))
(defcfun tcsetattr :int (fd :int) (action :int) (tio :pointer))
(defcfun tcdrain :int (fd :int))
(defcfun tcflow :int (fd :int) (action :int))
(defcfun tcflush :int (fd :int) (action :int))
(defcfun tcsendbreak :int (fd :int) (len :int))

#-sunos (defcfun cfmakeraw :void (tio :pointer))
#-sunos (defcfun cfsetspeed :int (tio :pointer) (speed speed-t))

(defconstant +IOCPARM_MASK+ #x1fff
  "parameter length, at most 13 bits")
(defmacro IOCPARM_LEN (x) `(logand (ash ,x -16) +IOCPARM_MASK+))
(defmacro IOCBASECMD (x) `(logand ,x (lognot (ash +IOCPARM_MASK+ -16))))
(defmacro IOGROUP (x) `(logand (ash ,x -8) #xff))
(defconstant +IOCPARM_MAX+ (1+ +IOCPARM_MASK+)
  "max size of ioctl args")
(defconstant +IOC_VOID+	   #x20000000 "no parameters")
(defconstant +IOC_OUT+	   #x40000000 "copy parameters out")
(defconstant +IOC_IN+	   #x80000000 "copy parameters in")
(defconstant +IOC_INOUT+   (logior +IOC_IN+ +IOC_OUT+) "copy paramters in and out")
(defconstant +IOC_DIRMASK+ #xe0000000 "mask for IN/OUT/VOID")

(defmacro _IOC (inout group num len)
  (let ((grp (etypecase group
	       (integer group)
	       (character (char-int group)))))
    `(logior ,inout (ash (logand ,len +IOCPARM_MASK+) 16)
      (ash ,grp 8) ,num)))

(defmacro _IO (g n)	 `(_IOC +IOC_VOID+  ,g ,n 0))
(defmacro _IOR (g n ty)	 `(_IOC +IOC_OUT+   ,g ,n ,(foreign-type-size ty)))
(defmacro _IOW (g n ty)	 `(_IOC +IOC_IN+    ,g ,n ,(foreign-type-size ty)))
(defmacro _IOWR (g n ty) `(_IOC +IOC_INOUT+ ,g ,n ,(foreign-type-size ty)))

;; "Window sizes. Not used by the kernel. Just for applications."
(defcstruct winsize
  (ws_row :unsigned-short)		; rows, in characters
  (ws_col :unsigned-short)		; columns, in characters
  (ws_xpixel :unsigned-short)		; horizontal size, pixels
  (ws_ypixel :unsigned-short))		; vertical size, pixels

;; @@@ Hold off on these
#| 
(defconstant TIOCMODG (_IOR 't'  3 :int) "get modem control state")
(defconstant TIOCMODS (_IOW 't'  4 :int) "set modem control state")

(defconstant +TIOCM_LE+	   0001		"line enable")
(defconstant +TIOCM_DTR+   0002		"data terminal ready")
(defconstant +TIOCM_RTS+   0004		"request to send")
(defconstant +TIOCM_ST+	   0010		"secondary transmit")
(defconstant +TIOCM_SR+	   0020		"secondary receive")
(defconstant +TIOCM_CTS+   0040		"clear to send")
(defconstant +TIOCM_CAR+   0100		"carrier detect")
(defconstant +TIOCM_CD+	   +TIOCM_CAR+)
(defconstant +TIOCM_RNG+   0200		"ring")
(defconstant +TIOCM_RI+	   +TIOCM_RNG+)
(defconstant +TIOCM_DSR+   0400		"data set ready")
					    ; 8-10 compat 
(defconstatn +TIOCEXCL+	 (_IO #\t 13)	    "set exclusive use of tty")
(defconstatn +TIOCNXCL+	 (_IO #\t 14)	    "reset exclusive use of tty")
					    ;; 15 unused
(defconstatn +TIOCFLUSH+ (_IOW #\t 16 :int) "flush buffers")
					    ;; 17-18 compat
(defconstatn +TIOCGETA+	 (_IOR #\t 19 (:struct termios)) "get termios struct")
(defconstatn +TIOCSETA+	 (_IOW #\t 20 (:struct termios)) "set termios struct")
(defconstatn +TIOCSETAW+ (_IOW #\t 21 (:struct termios)) "drain output, set")
(defconstatn +TIOCSETAF+ (_IOW #\t 22 (:struct termios)) "drn out, fls in, set")
(defconstatn +TIOCGETD+	 (_IOR #\t 26 int)   "get line discipline")
(defconstatn +TIOCSETD+	 (_IOW #\t 27 int)   "set line discipline")
					     ;; 127-124 compat
(defconstatn +TIOCSBRK+	 (_IO #\t 123)	     "set break bit")
(defconstatn +TIOCCBRK+	 (_IO #\t 122)	     "clear break bit")
(defconstatn +TIOCSDTR+	 (_IO #\t 121)	     "set data terminal ready")
(defconstatn +TIOCCDTR+	 (_IO #\t 120)	     "clear data terminal ready")
(defconstatn +TIOCGPGRP+ (_IOR #\t 119 :int) "get pgrp of tty")
(defconstatn +TIOCSPGRP+ (_IOW #\t 118 :int) "set pgrp of tty")
					     ;; 117-116 compat
(defconstatn +TIOCOUTQ+	 (_IOR #\t 115 :int) "output queue size")
 |#

;; SunOS TIOC and tIOC constants

;;#+sunos (defconstant little-tioc #.(ash 116 8)) ; 166 = (char-int #\t)
;;#+sunos (defconstant big-tioc    #.(ash 84  8)) ;  84 = (char-int #\T)
;;#+sunos (defconstant little-tioc (ash (char-int #\t) 8)) ; 166 << 8 = 42496
;;#+sunos (defconstant big-tioc    (ash (char-int #\T) 8)) ;  84 << 8 = 21504
;;#+sunos (defmacro l-tioc (n) (logior n little-tioc))
;;#+sunos (defmacro b-tioc (n) (logior n big-tioc))
#+sunos (defmacro l-tioc (n) (logior n (ash (char-int #\t) 8)))
#+sunos (defmacro b-tioc (n) (logior n (ash (char-int #\T) 8)))

;; I personally love this one.
;; @@@ I don't understand why it doesn't work on Lispworks.
#+darwin (defconstant TIOCSTI #-lispworks (_IOW #\t 114 :char) "simulate terminal input")
#+sunos  (defconstant TIOCSTI (l-tioc 23)          "simulate terminal input")
#+linux  (defconstant TIOCSTI #x5412               "simulate terminal input")

#|
(defconstant +TIOCNOTTY+         (_IO #\t 113)	     "void tty association")
(defconstant +TIOCPKT+	         (_IOW #\t 112 :int) "pty: set/clear packet mode")
(defconstant +TIOCPKT_DATA+       #x00 "data packet")
(defconstant +TIOCPKT_FLUSHREAD+  #x01 "flush packet")
(defconstant +TIOCPKT_FLUSHWRITE+ #x02 "flush packet")
(defconstant +TIOCPKT_STOP+       #x04 "stop output")
(defconstant +TIOCPKT_START+      #x08 "start output")
(defconstant +TIOCPKT_NOSTOP+     #x10 "no more ^S, ^Q")
(defconstant +TIOCPKT_DOSTOP+     #x20 "now do ^S ^Q")
(defconstant +TIOCPKT_IOCTL+      #x40 "state change of pty driver")
(defconstant +TIOCSTOP+	          (_IO #\t 111)	     "stop output, like ^S")
(defconstant +TIOCSTART+          (_IO #\t 110)	     "start output, like ^Q")
(defconstant +TIOCMSET+	          (_IOW #\t 109 :int) "set all modem bits")
(defconstant +TIOCMBIS+	          (_IOW #\t 108 :int) "bis modem bits")
(defconstant +TIOCMBIC+	          (_IOW #\t 107 :int) "bic modem bits")
(defconstant +TIOCMGET+	          (_IOR #\t 106 :int) "get all modem bits")
(defconstant +TIOCREMOTE+         (_IOW #\t 105 :int) "remote input editing")
|#

;; These are actually useful
#+darwin (defconstant +TIOCGWINSZ+
	   #-lispworks (_IOR #\t 104  (:struct winsize))
	   #+lispworks #x40087468
	   "get window size")
#+darwin (defconstant +TIOCSWINSZ+
	   #-lispworks (_IOW #\t 103  (:struct winsize))
	   #+lispworks #x80087467
	   "set window size")

#+sunos (defconstant +TIOCGWINSZ+ (b-tioc 104) "get window size")
#+sunos (defconstant +TIOCSWINSZ+ (b-tioc 103) "set window size")

#+linux (defconstant +TIOCGWINSZ+ #x5413 "get window size")
#+linux (defconstant +TIOCSWINSZ+ #x5414 "set window size")

#|
(defun UIOCCMD (n) "User control OP 'n'" (_IO #\u n))
(defconstant +TIOCUCNTL+       (_IOW #\t 102 :int) "pty: set/clr usr cntl mode")
(defconstant +TIOCSTAT+	       (_IO #\t 101) "simulate ^T status message")
(defconstant +TIOCSCONS+       (_IO #\t 99))
(defconstant +TIOCCONS+	       (_IOW #\t 98 :int) "become virtual console")
(defconstant +TIOCSCTTY+       (_IO #\t 97) "become controlling tty")
(defconstant +TIOCEXT+	       (_IOW #\t 96 :int) "pty: external processing")
(defconstant +TIOCSIG+	       (_IO #\t 95)       "pty: generate signal")
(defconstant +TIOCDRAIN+       (_IO #\t 94)       "wait till output drained")
(defconstant +TIOCMSDTRWAIT+   (_IOW #\t 91 :int) "modem: set wait on close")
(defconstant +TIOCMGDTRWAIT+   (_IOR #\t 90 :int) "modem: get wait on close")
(defconstant +TIOCTIMESTAMP+   (_IOR #\t 89 (:struct timeval)) "enable/get timestamp of last input event")
(defconstant +TIOCDCDTIMESTAMP+ (_IOR #\t 88, struct timeval) "enable/get timestamp of last DCd rise")
(defconstant +TIOCSDRAINWAIT+  (_IOW #\t 87, int) "set ttywait timeout")
(defconstant +TIOCGDRAINWAIT+  (_IOR #\t 86, int) "get ttywait timeout")
(defconstant +TIOCDSIMICROCODE+ (_IO #\t 85) "download microcode to DSI Softmodem")

(defconstant +TTYDISC+	0 "termios tty line discipline")
(defconstant +TABLDISC+	3 "tablet discipline")
(defconstant +SLIPDISC+	4 "serial IP discipline")
(defconstant +PPPDISC+	5 "PPP discipline")

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getch (tty &optional (debug nil))
  "Read a character from the tty."
  (with-foreign-object (c :char)
    (let ((status (posix-read tty c 1)))
      (cond
	((< status 0)
	 (error "Read error ~d~%" status))
	((= status 0)
	 nil)
	((= status 1)
	 (format debug "read ~d~%" (mem-ref c :char))
	 (mem-ref c :unsigned-char))))))

(defun pushcat-file (tty filename &optional (debug nil))
  "Show a file by characters or lines, controlled by the user."
  (handler-case
      (with-open-file (d filename)
	(handler-case
	    (let ((quit-flag nil))
	      (loop do
		    (case (code-char (getch tty debug))
		      ((#\q #\Q nil)
		       (setf quit-flag t))
		      (#\space
		       (write-char (read-char d))
		       (finish-output))
		      ((#\newline #\return)
		       (write-line (read-line d))))
		    while (not quit-flag)))
	  (end-of-file ())))
    (file-error (c)
	(format t "~a" c))))
;; Check out the graceful swoop!

(defun raw-echo (tty)
  "Output each character input immediately without processing. This is like of
like 'local' mode in real old fashioned terminals. This is useful for doing
experiments directly with the terminal emulator."
  (format t "You're talking directly to the terminal now.~c~c"
	  #\return #\linefeed)
  (format t "Press <ESC> four times in a row to stop.~c~c" #\return #\linefeed)
  (let ((escape 0) c)
    (loop :while (< escape 4)
       :do
       (setf c (code-char (getch tty)))
       (princ c) (finish-output)
       (if (eql c #\escape)
	   (incf escape)
	   (setf escape 0)))))

(defun input-test (tty)
  (format t "Testing input characters.~c~c" #\return #\linefeed)
  (format t "Press <ESC> four times in a row to stop.~c~c" #\return #\linefeed)
  (let ((escape 0) c)
    (loop :while (< escape 4)
       :do
       (setf c (getch tty))
       (if c
	   (progn
	     (format t "~d #x~x #o~o ~s~c~c" c c c (code-char c)
		     #\return #\linefeed)
	     (finish-output))
	   (format t "no character read~c~c" #\return #\linefeed))
       (if (eql (code-char c) #\escape)
	   (incf escape)
	   (setf escape 0)))))

(defun call-with-raw (tty func &key very-raw timeout)
  "Call the FUNC with the TTY set for raw input. FUNC is called with the TTY
as it's argument. It sets up the file descriptor for raw input, and cleans up
afterwards. If VERY-RAW is true, set the terminal to the most raw state, which
doesn't even process interrupts. If TIMEOUT is true, it's a number of
deciseconds, (by setting VTIME) to wait before a read returns nothing."
  (let (raw cooked raw-check result)
    (unwind-protect
	 (progn
	   (setf cooked    (foreign-alloc '(:struct termios))
		 raw       (foreign-alloc '(:struct termios))
		 raw-check (foreign-alloc '(:struct termios)))
	   
	   (syscall (tcgetattr tty cooked))
	   (syscall (tcgetattr tty raw))
	   (with-foreign-slots ((c_lflag c_iflag c_oflag c_cc) raw
				(:struct termios))
	     (if timeout
		 ;; Read returns immediately only after one character read,
		 ;; or times out after TIMEOUT deciseconds.
		 (progn
		   (setf (mem-aref c_cc :unsigned-char +VMIN+) 0)
		   (setf (mem-aref c_cc :unsigned-char +VTIME+) timeout))
		 ;; Read returns immediately only after one character read
		 (progn
		   (setf (mem-aref c_cc :unsigned-char +VMIN+) 1)
		   (setf (mem-aref c_cc :unsigned-char +VTIME+) 0)))
	     ;; Turn off any input processing
	     (setf c_iflag (logand c_iflag (lognot (logior +ISTRIP+ +INLCR+
							   +IGNCR+ +ICRNL+ +IXON+
							   +IXOFF+))))
	     ;; Turn off output processing
	     (when very-raw
	       (setf c_oflag (logand c_oflag (lognot (logior +OPOST+)))))
	     ;; Turn off canonical input and echo and signals
	     (if very-raw
		 (setf c_lflag (logand c_lflag (lognot (logior +ICANON+ +ECHO+
							       +ISIG+))))
		 (setf c_lflag (logand c_lflag (lognot (logior +ICANON+ +ECHO+)))))
	     ;; Actually set it
	     (syscall (tcsetattr tty +TCSANOW+ raw)))

	   ;; Do something
	   (setf result (funcall func tty)))
      (progn
	;; Clean up
	(when (and tty (>= tty 0) cooked)
	  (syscall (tcsetattr tty +TCSANOW+ cooked)))	; reset terminal modes
	(when cooked (foreign-free cooked))
	(when raw (foreign-free raw))))
;;    (format t "result = ~w~%" (map 'list #'identity result))
    result))

(defun raw-test (func &key debug very-raw timeout)
  "Test the termio package, by doing something with raw input.
FUNC is the thing to do with raw input. It's called with the POSIX file
descriptor as it's only argument. It sets up the file descriptor for raw
input, and cleans up afterwards. If DEBUG is true, print debugging details.
If VERY-RAW is true, set the terminal to the most raw state, which doesn't
even process interrupts. If TIMEOUT is true, it's a number of deciseconds,
(by setting VTIME) to wait before returning nothing."
  (let (tty raw cooked raw-check result)
    (unwind-protect
	 (progn
	   ;; perhaps we should just use 0, aka /dev/stdin aka /dev/fd/0
	   (setf tty (posix-open "/dev/tty" O_RDWR 0)
		 cooked    (foreign-alloc '(:struct termios))
		 raw       (foreign-alloc '(:struct termios))
		 raw-check (foreign-alloc '(:struct termios)))
	   (format debug "tty = ~a~%" tty)
	   (when (< tty 0)
	     (error "Error opening /dev/tty ~d~%" tty))
	   
	   (when (= -1 (tcgetattr tty cooked))
	     (error "Can't get cooked mode. errno = ~d" *errno*))
	   (format debug "cooked = ~x~%" cooked)
	   (when (= -1 (tcgetattr tty raw))
	     (error "Can't get raw mode. errno = ~d" *errno*))
	   (format debug "raw    = ~x~%" raw)
	   (with-foreign-slots ((c_lflag c_iflag c_oflag c_cc) raw
				(:struct termios))
	     (format debug "cooked c_lflag = ~x~%" c_lflag)
	     (if timeout
		 ;; Read returns immediately only after one character read,
		 ;; or times out after TIMEOUT deciseconds.
		 (progn
		   (setf (mem-aref c_cc :unsigned-char +VMIN+) 0)
		   (setf (mem-aref c_cc :unsigned-char +VTIME+) timeout))
		 ;; Read returns immediately only after one character read
		 (progn
		   (setf (mem-aref c_cc :unsigned-char +VMIN+) 1)
		   (setf (mem-aref c_cc :unsigned-char +VTIME+) 0)))
	     ;; Turn off any input processing
	     (setf c_iflag (logand c_iflag (lognot (logior +ISTRIP+ +INLCR+ +IGNCR+
							   +ICRNL+ +IXON+ +IXOFF+))))
	     ;; Turn off output processing
	     (when very-raw
	       (setf c_oflag (logand c_oflag (lognot (logior +OPOST+)))))
	     ;; Turn off canonical input and echo and signals
	     (if very-raw
		 (setf c_lflag (logand c_lflag (lognot (logior +ICANON+ +ECHO+
							       +ISIG+))))
		 (setf c_lflag (logand c_lflag (lognot (logior +ICANON+ +ECHO+)))))
	     ;; Actually set it
	     (when (= -1 (tcsetattr tty +TCSANOW+ raw))
	       (error "Can't set raw mode. errno = ~d" *errno*)))

	   ;; Check that things were set right 
	   (when (= -1 (tcgetattr tty raw-check))
	     (error "Can't get raw-check mode. errno = ~d" *errno*))

	   (with-foreign-slots ((c_lflag c_cc) raw-check (:struct termios))
	     (format debug "c_cc[~a] = ~a~%" +VMIN+
		     (mem-aref c_cc :unsigned-char +VMIN+))
	     (format debug "c_cc[~a] = ~a~%" +VTIME+
		     (mem-aref c_cc :unsigned-char +VTIME+))

	     (format debug "c_lflag = ~x~%" c_lflag)
	     (format debug "c_lflag = ~a ICANON  ~a ECHO~%"
		     (if (= 0 (logand c_lflag +ICANON+)) "NOT" "")
		     (if (= 0 (logand c_lflag +ECHO+))   "NOT" "")))

	   ;; Do something
	   (setf result (funcall func tty)))
      (progn
	;; Clean upp
	(when (and tty (>= tty 0) cooked)
	  (tcsetattr tty +TCSANOW+ cooked))	; reset terminal modes
	(when (and tty (>= tty 0))
	  (posix-close tty))			; close the terminal
	(when cooked (foreign-free cooked))
	(when raw (foreign-free raw))))
    result))

(defun test (&key (filename "termios.lisp") (debug nil))
  "This is pushcat."
  (raw-test #'(lambda (tty) (pushcat-file tty filename debug)) :debug debug))

(defun test-echo ()
  (raw-test #'(lambda (tty) (raw-echo tty)) :very-raw t))

(defun test-input ()
  (raw-test #'(lambda (tty) (input-test tty)) :very-raw t))

(defmacro with-raw-input (&body body)
  `(raw-test #'(lambda (tty) ,@body)))

(defmacro with-very-raw-input (&body body)
  `(raw-test #'(lambda (tty) ,body) :very-raw t))

(defun terminal-query (query &key max)
  "Output the string to the terminal and wait for a response. Read up to MAX
characters. If we don't get anything after a while, just return what we got."
  (raw-test
   #'(lambda (tty)
       (with-foreign-string (q query)
	 (posix-write tty q (length query)))
       (let ((str (with-output-to-string (stream)
		    (if max
			(loop :with c
			   :for i :from 0 :below max
			   :while (setf c (getch tty))
			   :do (princ (code-char c) stream))
			(loop :with c
			   :while (setf c (getch tty))
			   :do (princ (code-char c) stream))))))
	 (make-array (length str) :initial-contents str)))
   :very-raw t
   :timeout 1))

;; This is just what I consider sane. You might not. You can change them if
;; you want to.
;;
;; This is output from "stty -g" on Darwin 9.8.0:
;; cflag=5b00:iflag=300:lflag=200005cf:oflag=7:discard=ff:dsusp=ff:eof=4:eol=ff:eol2=0:erase=7f:intr=3:kill=15:lnext=11:min=1:quit=1c:reprint=ff:start=ff:status=14:stop=ff:susp=1a:time=0:werase=ff:ispeed=38400:ospeed=38400
;;
;; Of course this has to be customized for every OS.
;; @@@ But we could make a default base POSIX one, which might not be perfect
;; but would hopefully do the job?

#+darwin (progn
(defparameter *sane-iflag*   #x300)	 ; ICRNL | IXON
(defparameter *sane-oflag*   #x7)	 ; OPOST | ONLCR | OXTABS
(defparameter *sane-cflag*   #x5b00)	 ; PARENB | HUPCL | CS6 | CS7 | CREAD
(defparameter *sane-lflag*   #x200005cf) ; ECHOKE | ECHOE | ECHOK | ECHO |
					 ; ECHOCTL | ISIG | ICANON | IEXTEN |
					 ; PENDIN
(declaim (type (unsigned-byte 8) *sane-discard*))
(defparameter *sane-discard* #xff)	 ; undef
(defparameter *sane-dsusp*   #xff)	 ; undef
(defparameter *sane-eof*     #x4)	 ; ^D
(defparameter *sane-eol*     #xff)	 ; undef
(defparameter +sane-eol2+    #x0)	 ; ^@
(defparameter *sane-erase*   #x7f)	 ; Delete
(defparameter *sane-intr*    #x3)	 ; ^C
(defparameter *sane-kill*    #x15)	 ; ^U
(defparameter *sane-lnext*   #x11)	 ; ^Q
(defparameter *sane-min*     #x1)	 ; ^A
(defparameter *sane-quit*    #x1c)	 ; ^\
(defparameter *sane-reprint* #xff)	 ; undef
(defparameter *sane-start*   #xff)	 ; undef
(defparameter *sane-status*  #x14)	 ; ^T
(defparameter *sane-stop*    #xff)	 ; undef
(defparameter *sane-susp*    #x1a)	 ; ^Z
(defparameter *sane-time*    #x0)	 ; ^@
(defparameter *sane-werase*  #xff)	 ; undef
(defparameter *sane-ispeed*  38400)
(defparameter *sane-ospeed*  38400)

(defparameter *sanity*
  `((,*sane-iflag* ,*sane-oflag* ,*sane-cflag* ,*sane-lflag*
     ,*sane-ispeed* ,*sane-ospeed*)
    #(,*sane-discard* ,*sane-dsusp* ,*sane-eof* ,*sane-eol* ,+sane-eol2+
      ,*sane-erase* ,*sane-intr* ,*sane-kill* ,*sane-lnext* ,*sane-min*
      ,*sane-quit* ,*sane-reprint* ,*sane-start* ,*sane-status* ,*sane-stop*
      ,*sane-susp* ,*sane-time* ,*sane-werase*)))
)

#+sunos (progn
; @@@ not verified, from ttywtf
;cflag 0xf01bf iflag 0x102 lflag 0x8a3b oflag 0x1805
(defparameter *sane-iflag*   #x00102)	;
(defparameter *sane-oflag*   #x01805)	;
(defparameter *sane-cflag*   #xf01bf)	; 
(defparameter *sane-lflag*   #x08a3b) ;
(declaim (type (unsigned-byte 8) *sane-discard*))
(defparameter *sane-discard* #xff)	; undef
(defparameter *sane-dsusp*   #xff)	; undef
(defparameter *sane-eof*     #x4)	; ^D
(defparameter *sane-eol*     #xff)	; undef
(defparameter +sane-eol2+    #x0)	; ^@
(defparameter *sane-erase*   #x7f)	; Delete
(defparameter *sane-intr*    #x3)	; ^C
(defparameter *sane-kill*    #x15)	; ^U
(defparameter *sane-lnext*   #x11)	; ^Q
(defparameter *sane-min*     #x1)	; ^A
(defparameter *sane-quit*    #x1c)	; ^\
(defparameter *sane-reprint* #xff)	; undef
(defparameter *sane-start*   #xff)	; undef
(defparameter *sane-status*  #x14)	; ^T
(defparameter *sane-stop*    #xff)	; undef
(defparameter *sane-susp*    #x1a)	; ^Z
(defparameter *sane-time*    #x0)	; ^@
(defparameter *sane-werase*  #xff)	; undef
(defparameter *sane-ispeed*  38400)
(defparameter *sane-ospeed*  38400)

(defparameter *sanity*
  `((,*sane-iflag* ,*sane-oflag* ,*sane-cflag* ,*sane-lflag*
     ,*sane-ispeed* ,*sane-ospeed*)
    #(,*sane-discard* ,*sane-dsusp* ,*sane-eof* ,*sane-eol* ,+sane-eol2+
      ,*sane-erase* ,*sane-intr* ,*sane-kill* ,*sane-lnext* ,*sane-min*
      ,*sane-quit* ,*sane-reprint* ,*sane-start* ,*sane-status* ,*sane-stop*
      ,*sane-susp* ,*sane-time* ,*sane-werase*)))
)

#+linux (progn
; @@@ not verified, from ttywtf
;cflag 0xbf iflag 0x500 lflag 0x8a3b oflag 0x5
(defparameter *sane-iflag*   #x0500)	;
(defparameter *sane-oflag*   #x1805)	;
(defparameter *sane-cflag*   #x00b5)	; was #x0005
(defparameter *sane-lflag*   #x8a3b) ;
;cflag 0xb5 iflag 0x500 lflag 0x8a3b oflag 0x1805
(declaim (type (unsigned-byte 8) *sane-discard*))
(defparameter *sane-discard* #xff)	; undef
(defparameter *sane-dsusp*   #xff)	; undef
(defparameter *sane-eof*     #x4)	; ^D
(defparameter *sane-eol*     #xff)	; undef
(defparameter +sane-eol2+    #x0)	; ^@
(defparameter *sane-erase*   #x7f)	; Delete
(defparameter *sane-intr*    #x3)	; ^C
(defparameter *sane-kill*    #x15)	; ^U
(defparameter *sane-lnext*   #x11)	; ^Q
(defparameter *sane-min*     #x1)	; ^A
(defparameter *sane-quit*    #x1c)	; ^\
(defparameter *sane-reprint* #xff)	; undef
(defparameter *sane-start*   #xff)	; undef
(defparameter *sane-status*  #x14)	; ^T
(defparameter *sane-stop*    #xff)	; undef
(defparameter *sane-susp*    #x1a)	; ^Z
(defparameter *sane-time*    #x0)	; ^@
(defparameter *sane-werase*  #xff)	; undef
(defparameter *sane-ispeed*  38400)
(defparameter *sane-ospeed*  38400)

(defparameter *sanity*
  `((,*sane-iflag* ,*sane-oflag* ,*sane-cflag* ,*sane-lflag*
     ,*sane-ispeed* ,*sane-ospeed*)
    #(,*sane-discard* ,*sane-dsusp* ,*sane-eof* ,*sane-eol* ,+sane-eol2+
      ,*sane-erase* ,*sane-intr* ,*sane-kill* ,*sane-lnext* ,*sane-min*
      ,*sane-quit* ,*sane-reprint* ,*sane-start* ,*sane-status* ,*sane-stop*
      ,*sane-susp* ,*sane-time* ,*sane-werase*)))
)

(defun sane (&optional (device "/dev/tty"))
  "Reset standard input to sane modes."
  (when (not (boundp '*sanity*))
    (error "Sanity undefined."))
  (let (tty sane)
    (unwind-protect
      (progn
	(setf tty (posix-open device O_RDWR 0)
	      sane (foreign-alloc '(:struct termios)))
	(when (< tty 0)
	  (error "Error opening ~a ~d~%" device tty))

	(with-foreign-slots ((c_iflag c_oflag c_cflag c_lflag c_cc
			      c_ispeed c_ospeed)
			     sane (:struct termios))
	  (setf c_iflag *sane-iflag*)
	  (setf c_oflag *sane-oflag*)
	  (setf c_cflag *sane-cflag*)
	  (setf c_lflag *sane-lflag*)
	  (setf (mem-aref c_cc :unsigned-char +VDISCARD+) *sane-discard*)
#-linux	  (setf (mem-aref c_cc :unsigned-char +VDSUSP+)   *sane-dsusp*)
	  (setf (mem-aref c_cc :unsigned-char +VEOF+)     *sane-eof*)
	  (setf (mem-aref c_cc :unsigned-char +VEOL+)     *sane-eol*)
	  (setf (mem-aref c_cc :unsigned-char +VEOL2+)    +sane-eol2+)
	  (setf (mem-aref c_cc :unsigned-char +VERASE+)   *sane-erase*)
	  (setf (mem-aref c_cc :unsigned-char +VINTR+)    *sane-intr*)
	  (setf (mem-aref c_cc :unsigned-char +VKILL+)    *sane-kill*)
	  (setf (mem-aref c_cc :unsigned-char +VLNEXT+)   *sane-lnext*)
	  (setf (mem-aref c_cc :unsigned-char +VMIN+)     *sane-min*)
	  (setf (mem-aref c_cc :unsigned-char +VQUIT+)    *sane-quit*)
	  (setf (mem-aref c_cc :unsigned-char +VREPRINT+) *sane-reprint*)
	  (setf (mem-aref c_cc :unsigned-char +VSTART+)   *sane-start*)
#+darwin  (setf (mem-aref c_cc :unsigned-char +VSTATUS+)  *sane-status*)
	  (setf (mem-aref c_cc :unsigned-char +VSTOP+)    *sane-stop*)
	  (setf (mem-aref c_cc :unsigned-char +VSUSP+)    *sane-susp*)
	  (setf (mem-aref c_cc :unsigned-char +VTIME+)    *sane-time*)
	  (setf (mem-aref c_cc :unsigned-char +VWERASE+)  *sane-werase*)
	  (setf c_ispeed *sane-ispeed*)
	  (setf c_ospeed *sane-ospeed*)

	  (when (= -1 (tcsetattr tty +TCSANOW+ sane))
	    (error "Can't set sane mode. errno = ~d" *errno*))))
      ;; close the terminal
      (when (and tty (>= tty 0))
	(posix-close tty))
      ;; free C memory
      (when sane (foreign-free sane)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The portable interface:

;; I'm not really sure if this is a good interface. ☹
(defun os-unix:set-terminal-mode (tty &key (echo    nil echo-supplied)
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
  (when (not (or echo-supplied line-supplied raw-supplied timeout-supplied
		 mode-supplied))
    (return-from set-terminal-mode))
  (let (new-mode)
    (unwind-protect
      (progn
	(setf new-mode (foreign-alloc '(:struct termios)))
	(syscall (tcgetattr tty new-mode))
	(with-foreign-slots ((c_lflag c_iflag c_oflag c_cc) new-mode
			     (:struct termios))
	  (labels
	      ((char-mode (state)
		 (if state
		     (progn
		       ;; We assume with character mode you want relatively
		       ;; raw characters.
		       (setf c_iflag
			     (logand c_iflag
				     (lognot (logior +ISTRIP+ +INLCR+
						     +IGNCR+ +ICRNL+ +IXON+
						     +IXOFF+))))
		       ;; Turn off "cannonical" input.
		       ;; Stuck in input modes of the ancients?
		       ;; We are the new canon!
		       (setf c_lflag
			     (logand c_lflag
				     (lognot (logior +ICANON+ +IEXTEN+))))
		       ;; These are the default values, but we set them since
		       ;; otherwise a read might not immediately return a
		       ;; char.
		       (setf (mem-aref c_cc :char +VMIN+) 1)
		       (setf (mem-aref c_cc :char +VTIME+) 0))
		     (progn
		       (setf c_iflag (logior c_iflag +ICRNL+ +IXON+))
		       (setf c_lflag (logior c_lflag +ICANON+ +IEXTEN+)))))
	       (echo-mode (state)
		 (setf c_lflag
		       (if state
			   (logior c_lflag +ECHO+)
			   (logand c_lflag (lognot +ECHO+)))))
	       (raw-mode (state)
		 ;; Raw means the same as character mode AND don't process
		 ;; signals or do output processing.
		 (if state
		     (progn
		       (setf c_lflag (logand c_lflag (lognot +ISIG+)))
		       (setf c_oflag (logand c_oflag (lognot +OPOST+))))
		     (progn
		       (setf c_lflag (logior c_lflag +ISIG+))
		       (setf c_oflag (logior c_oflag +OPOST+)))))
	       (set-timeout (timeout)
		 (if timeout
		     (progn
		       (char-mode t) ;; char mode must be on
		       (setf (mem-aref c_cc :char +VMIN+) 0)
		       (setf (mem-aref c_cc :char +VTIME+) timeout))
		     (progn
		       (setf (mem-aref c_cc :char +VMIN+) 1)
		       (setf (mem-aref c_cc :char +VTIME+) 0)))))
	    (when mode-supplied
	      ;; Order is important here.
	      (echo-mode (terminal-mode-echo mode))
	      (char-mode (not (terminal-mode-line mode)))
	      (raw-mode (terminal-mode-raw mode))
	      (set-timeout (terminal-mode-timeout mode)))
	    ;; Order is important here, too.
	    (when echo-supplied (echo-mode echo))
	    (when line-supplied (char-mode (not line)))
	    (when raw-supplied (raw-mode raw))
	    (when timeout-supplied (set-timeout timeout))))
	;; Now actually set it.
	(when (< (tcsetattr tty +TCSANOW+ new-mode) 0)
	  (error "Can't set terminal mode. ~d ~d" *errno* tty)))
      (foreign-free new-mode))))

(defun os-unix:get-terminal-mode (tty)
  "Return a TERMINAL-MODE structure with the current terminal settings."
  (let ((new-mode (foreign-alloc '(:struct termios)))
	(echo nil) (line nil) (raw nil) (timeout nil))
    (syscall (tcgetattr tty new-mode))
    (with-foreign-slots ((c_lflag c_iflag c_oflag c_cc) new-mode
			 (:struct termios))
      ;; Don't check in detail, just assume the full setting.
      (setf echo (not (zerop (logand c_lflag +ECHO+)))
	    line (not (zerop (logand c_lflag +ICANON+)))
	    raw  (and (zerop (logand c_lflag +ISIG+))
		      (zerop (logand c_lflag +ICANON+)))
	    timeout (if (zerop (mem-aref c_cc :char +VMIN+))
			(mem-aref c_cc :char +VTIME+)
			nil)))
    (make-terminal-mode :echo echo :line line :raw raw :timeout timeout)))

(defun os-unix:get-window-size (tty-fd)
  "Get the window size. First value is columns, second value is rows."
  (with-foreign-object (ws '(:struct winsize))
    (when (< (posix-ioctl tty-fd +TIOCGWINSZ+ ws) 0)
      (error "Can't get the tty window size."))
    (values
     (foreign-slot-value ws '(:struct winsize) 'ws_col)
     (foreign-slot-value ws '(:struct winsize) 'ws_row))))

(defun os-unix:slurp-terminal (tty &key timeout)
  "Read until EOF. Return a string of the results. TTY is a file descriptor."
  (let* ((size (memory-page-size))
	 (result (make-array size
			     :element-type 'base-char
			     :fill-pointer 0 :adjustable t))
	 status)
    (when timeout
      (set-terminal-mode tty :timeout timeout))
    (with-output-to-string (str result)
      (with-foreign-object (buf :char size)
	(loop
	   :do (setf status (posix-read tty buf size))
	   :while (= status size)
	   :do (princ (cffi:foreign-string-to-lisp buf) str))
	(cond
	  ((> status size)
	   (error "Read returned too many characters? ~a" status))
	  ((< status 0)
	   (error "Read error ~d~%" status))
	  ((= status 0)
	   (or (and (length result) result) nil))
	  (t
	   (princ (cffi:foreign-string-to-lisp buf :count status) str)
	   result))))))

(defun crap (&optional (device "/dev/tty"))
  "An horrible test."
  (let (tty)
    (unwind-protect
      (progn
	(setf tty (posix-open device O_RDWR 0))
	(when (< tty 0)
	  (error "Error opening ~a ~d~%" device tty))

	(format t "No echo: ") (finish-output)
	(set-terminal-mode tty :echo nil)
	(format t "read: ~s~%" (read-line))

	(format t "Yes echo: ") (finish-output)
	(set-terminal-mode tty :echo t)
	(format t "read: ~s~%" (read-line))

	(format t "One char: ") (finish-output)
	(set-terminal-mode tty :line nil)
	(format t "read: ~s~%" (read-char))
	
	(format t "Whole line: ") (finish-output)
	(set-terminal-mode tty :line t)
	(format t "read: ~s~%" (read-line))

	(format t "Raw char: ") (finish-output)
	(set-terminal-mode tty :raw t)
	(format t "read: ~s~%" (read-char))

	(format t "Non-raw line: ") (finish-output)
	(set-terminal-mode tty :raw nil)
	(format t "read: ~s~%" (read-line))

	(format t "Time out (2 sec): ") (finish-output)
	(set-terminal-mode tty :timeout 20)
	(format t "read: ~s~%" (getch tty))

	(format t "No time out: ") (finish-output)
	(set-terminal-mode tty :timeout nil)
	(format t "read: ~s~%" (code-char (getch tty)))
	(set-terminal-mode tty :line t)
	)
      ;; close the terminal
      (when (and tty (>= tty 0))
	(posix-close tty)))))

;; EOF
