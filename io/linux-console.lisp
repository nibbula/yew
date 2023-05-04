;;;
;;; linux-console.lisp - Linux console
;;;

(defpackage :linux-console
  (:documentation "Interface to Linux console devices.")
  (:use :cl :dlib :opsys :opsys-unix :cffi :dlib-misc :dcolor :terminal
	:collections :fatchar)
  (:export
   #:console-p
   #:vt-mode
   #:vt-mode-switching
   #:vt-mode-wait
   #:vt-mode-release-signal
   #:vt-mode-acquire-signal
   #:mode
   #:set-mode
   #:vt-state
   #:vt-state-active-vt
   #:vt-state-signal
   #:vt-state-in-use
   #:*leds*
   #:led-mask
   #:leds
   #:set-leds
   #:reset-leds
   #:*lock-keys*
   #:lock-key-mask
   #:lock-keys
   #:set-lock-keys
   #:+text-modes+
   #:text-mode
   #:set-text-mode
   #:meta-key
   #:set-meta-key
   #:+keyboard-modes+
   #:keyboard-mode
   #:set-keyboard-mode
   #:tone
   #:sound
   #:sound-off
   #:*key-names*
   #:*keycodes*
   #:find-keycode
   #:keycode-name
   #:*key-table*
   #:*key-name-table*
   #:show-keys
   #:default-color-index
   #:italic-color-index
   #:underline-color-index
   #:cur-default-color-index
   #:global-cursor-default-color-index
   #:get-colors
   #:set-colors
   #:show-default-colors
   #:describe-console
   ))
(in-package :linux-console)

;; This is separate from the ‘opsys’ package since most programs don't need
;; to use console special features.

;; Interesting sources to consult are:
;;   linux-<version>/drivers/tty/vt/vt.c
;;   linux-<version>/include/linux/console_struct.h

(defparameter *console-paths*
  `("/proc/self/fd/0"
    "/dev/tty"
    "/dev/tty0"
    "/dev/vc/0"
    "/dev/systty"
    "/dev/console")
  "Files to try opening to get the current console.")

(defconstant +KDGKBTYPE+ #x4B33
  "ioctl code to get the keyboard type")

(defconstant +KB-84+    1) ;; OG keyboard with 10 function keys on the left
(defconstant +KB-101+   2) ;; Linus says it's always this 🖕
(defconstant +KB-OTHER+ 3) ;; Like maybe a Sun keyboard.

(define-constant +keyboard-types+ #(+KB-84+ +KB-101+ +KB-OTHER+)
  "Keyboard type codes returned by KDGKBTYPE." #'equalp)

(defun consolep (fd)
  "Return true if ‘fd’ is file descriptor open on a console."
  (and (integerp fd)
       (uos:isatty fd)
       (with-foreign-object (type :char)
	 (if (= -1 (posix-ioctl fd +KDGKBTYPE+ type))
	     (if (or (= uos:*errno* +EINVAL+)
		     (= uos:*errno* +ENOTTY+))
		 nil
		 (uos:error-check -1 "checking consolep for ~s" fd))
	     (let ((itype (mem-ref type :char)))
	       (or (= itype +KB-101+) (= itype +KB-84+)))))))

(defun open-console ()
  "Open the current console or signal an error."
  (let ((fd -1))
    (loop :for f :in *console-paths* :do
      (when (and (plusp (setf fd (posix-open f +O_RDWR+ 0)))
		 (consolep fd))
	(return-from open-console fd)))
    (error "Can't open a console.")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-console ((fd) &body body)
    "Evaluate the body with ‘fd’ set to the current console, or signal an
error if we can't open a console. If ‘fd’ is not nil, assume it's an open
console file descriptor."
    (with-names (we-opened)
      `(let ((,we-opened))
	 (unwind-protect
	      (progn
		(when (not ,fd)
		  (setf ,fd (open-console)
			,we-opened t))
		,@body)
	   (when ,we-opened
	     (syscall (posix-close ,fd))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VT modes

(defstruct vt-mode
  switching
  wait
  release-signal
  acquire-signal)

(defcstruct vt_mode
  (mode :char)				; vt mode
  (waitv :char)				; if set, hang on writes if not active
  (relsig :short)			; signal to raise on release req
  (acqsig :short)			; signal to raise on acquisition
  (frsig :short))			; unused (set to 0)

(defconstant +VT-GETMODE+ #x5601 "Get the modes of a VT.")
(defconstant +VT-SETMODE+ #x5602 "Set the modes of a VT.")

(defparameter *vt-switching-modes* nil)
(define-to-list *vt-switching-modes*
  #(#(+VT-SWITCHING-MODE-AUTOMATIC+   #x0 "Automatic VT switching.")
    #(+VT-SWITCHING-MODE-PROCESS+     #x1 "Process controled switching.")
    #(+VT-SWITCHING-MODE-ACKNOWLEDGE+ #x2 "Acknowledge switch.")))

(defun convert-mode (mode)
  (with-foreign-slots ((mode waitv relsig acqsig) mode (:struct vt_mode))
    (make-vt-mode :switching (find mode *vt-switching-modes*
				   :key #'symbol-value)
		  :wait (not (zerop waitv))
		  :release-signal relsig
		  :acquire-signal acqsig)))

(defun mode (&optional fd)
  "Return the modes of virtual terminal ‘fd’."
  (with-console (fd)
    (with-foreign-object (mode '(:struct vt_mode))
      (syscall (posix-ioctl fd +VT-GETMODE+ mode))
      (convert-mode mode))))

(defun set-mode (fd vt-mode)
  "Set the modes of virtual terminal ‘fd’ to ‘vt-mode’, which should be
a ‘vt-mode’ structure."
  (with-console (fd)
    (with-foreign-object (mode '(:struct vt_mode))
      (with-foreign-slots ((mode waitv relsig acqsig frsig) mode
			   (:struct vt_mode))
	(setf mode   (symbol-value (vt-mode-switching vt-mode))
	      waitv  (if (vt-mode-wait vt-mode) 1 0)
	      relsig (vt-mode-release-signal vt-mode)
	      acqsig (vt-mode-acquire-signal vt-mode)
	      frsig  0)
	(syscall (posix-ioctl fd +VT-SETMODE+ mode))))))

(defsetf mode set-mode
  "Set the modes of virtual terminal ‘fd’.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VT state

(defstruct vt-state
  active-vt		; The active VT
  signal		; The signal to send [when? for what?]
  in-use)		; VT "in use" bitmask, only first 16 [obsolete?]

(defcstruct vt_state
  (v_active :unsigned-short)
  (v_signal :unsigned-short)
  (v_state :unsigned-short))

;; @@@ we could just use the raw one?
(defun convert-state (state)
  (with-foreign-slots ((v_active v_signal v_state) state (:struct vt_state))
    (make-vt-state :active-vt v_active
		   :signal v_signal
		   :in-use v_state)))

(defconstant +VT-GETSTATE+ #x5603 "Get global VT state.")

(defun state (&optional fd)
  "Return the state of virtual terminal ‘fd’."
  (with-console (fd)
    (with-foreign-object (state '(:struct vt_state))
      (syscall (posix-ioctl fd +VT-GETSTATE+ state))
      (convert-state state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LEDs

(defconstant +KDGETLED+	#x4B31 "Get current LED state.")
(defconstant +KDSETLED+	#x4B32 "Set LED light state.")

(defparameter *leds* nil)
(define-to-list *leds*
  #(#(+LED-SCROLL-LOCK+ #x01 "Scroll Lock LED")
    #(+LED-NUM-LOCK+    #x02 "Num Lock LED")
    #(+LED-CAPS-LOCK+   #x04 "Caps Lock LED")))

(defun led-mask (fd)
  "Return a bitmask of the LEDs that are lit on the keyboard ‘fd’."
  (with-foreign-object (leds :char)
    (syscall (posix-ioctl fd +KDGETLED+ leds))
    (mem-ref leds :char)))

(defun leds (&optional fd)
  "Return the LEDs that are lit on the keyboard ‘fd’, as a list of
+LED-*+ symbols."
  (with-console (fd)
    (let ((result)
	  (leds (led-mask fd)))
      (when (not (zerop (logand +LED-SCROLL-LOCK+ leds)))
	(push '+LED-SCROLL-LOCK+ result))
      (when (not (zerop (logand +LED-NUM-LOCK+ leds)))
	(push '+LED-NUM-LOCK+ result))
      (when (not (zerop (logand +LED-CAPS-LOCK+ leds)))
	(push '+LED-CAPS-LOCK+ result))
      result)))

(defun set-leds (fd leds)
  "Set the LEDs that are lit on the keyboard ‘fd’ to ‘leds’, which is a list
of +LED-*+ symbols or an integer bitmask of the +LED-*+ values."
  (with-console (fd)
    (let ((led-set 0))
      (etypecase leds
	(list
	 (loop :for l :in leds
	       :do (setf led-set (logior led-set (symbol-value l)))))
	(integer
	 (setf led-set leds)))
      (syscall (posix-ioctl fd +KDSETLED+ (make-pointer led-set)))
      leds)))

(defsetf leds set-leds
  "Set the keyboard LEDs for ‘fd’.")

(defun reset-leds (fd)
  "Reset the LEDs to reflect the state of the lock keys."
  (set-leds fd #xf0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lock key state

(defconstant +KDGKBLED+	#x4B64	"Get current lock key state.")
(defconstant +KDSKBLED+	#x4B65	"Set lock key state.")

(defparameter *lock-keys* nil)
(define-to-list *lock-keys*
  #(#(+KEY-SCROLL-LOCK+ #x01 "Scroll Lock Key")
    #(+KEY-NUM-LOCK+    #x02 "Num Lock Key")
    #(+KEY-CAPS-LOCK+   #x04 "Caps Lock Key")))

(defun lock-key-mask (&optional fd)
  "Return a bitmask of the lock keys that are set on the keyboard ‘fd’."
  (with-console (fd)
    (with-foreign-object (keys :char)
      (syscall (posix-ioctl fd +KDGKBLED+ keys))
      (mem-ref keys :char))
    ;; (with-foreign-object (keys :long)
    ;;   (syscall (posix-ioctl fd +KDGKBLED+ keys))
    ;;   (mem-ref keys :long))
    ))

(defun lock-keys (&optional fd)
  "Return the lock keys that are set on the keyboard ‘fd’, as a list of
+KEY-*+ symbols."
  (with-console (fd)
    (let ((result)
	  (keys (lock-key-mask fd)))
      (when (not (zerop (logand +KEY-SCROLL-LOCK+ keys)))
	(push '+KEY-SCROLL-LOCK+ result))
      (when (not (zerop (logand +KEY-NUM-LOCK+ keys)))
	(push '+KEY-NUM-LOCK+ result))
      (when (not (zerop (logand +KEY-CAPS-LOCK+ keys)))
	(push '+KEY-CAPS-LOCK+ result))
      result)))

(defun set-lock-keys (fd keys)
  "Set the lock keys state on the keyboard ‘fd’ to ‘keys’, which is a list
of +KEY-*+ symbols or an integer bitmask of the +KEY-*+ values."
  (let ((key-set 0))
    (etypecase keys
      (list
       (loop :for k :in keys
	     :do (setf key-set (logior key-set (symbol-value k)))))
      (integer
       (setf key-set keys)))
    (syscall (posix-ioctl fd +KDGETLED+ (make-pointer key-set)))
    keys))

(defsetf lock-keys set-lock-keys
  "Set the keyboard lock key state for ‘fd’.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text/Graphics mode

(defconstant +KDSETMODE+ #x4B3A	"Set text/graphics mode.")
(defconstant +KDGETMODE+ #x4B3B	"Get current text mode.")

(define-constant +text-modes+
  #(:text :graphics :text0 :text1 :transparent)
  "Possible values for console text-mode." #'equalp)

(defconstant +KD-TEXT+         #x00)
(defconstant +KD-GRAPHICS+     #x01)
(defconstant +KD-TEXT0+        #x02)	; obsolete
(defconstant +KD-TEXT1+        #x03)	; obsolete
(defconstant +KD-TRANSPARENT+  #x04)

(defun check-text-mode (mode)
  (and (>= mode 0) (< mode (length +text-modes+))))

(defun text-mode (&optional fd)
  "Return the text mode for ‘fd’ as one of ‘+text-modes+’."
  (with-console (fd)
    (with-foreign-object (mode :int)
      (syscall (posix-ioctl fd +KDGETMODE+ mode))
      (let ((m (mem-ref mode :int)))
	(if (check-text-mode m)
	    (svref +text-modes+ m)
	    (error "Bad text mode value ~s" m))))))

(defun set-text-mode (fd mode)
  (unless (check-text-mode mode)
    (error "Bad text mode value ~s" mode))
  (syscall (posix-ioctl fd +KDSETMODE+
			(make-pointer (position mode +text-modes+)))))

(defsetf text-mode set-text-mode
  "Set the text mode for ‘fd’.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Meta key

(defconstant +KDGKBMETA+ #x4B62	"Get meta key handling mode.")
(defconstant +KDSKBMETA+ #x4B63	"Set meta key handling mode.")

(defconstant +K-METABIT+   #x03 "Set high order bit.")
(defconstant +K-ESCPREFIX+ #x04 "Prefix with escape.")

(defun meta-key (&optional fd)
  "Return :meta if ‘fd’ sets the high bit for the meta key, :escape if it
instead sends #\escape as a prefix."
  (with-console (fd)
    (with-foreign-object (meta :long)
      (syscall (posix-ioctl fd +KDGKBMETA+ meta))
      (if (= (mem-ref meta :long) +K-METABIT+)
	  :meta
	  :escape))))

(defun set-meta-key (fd meta)
  "If ‘meta’ is :meta, set the terminal on ‘fd’ to make the meta key set the
high character bit, otherwise if meta is :escape, send #\escape as a prefix."
  (syscall (posix-ioctl fd +KDSKBMETA+
			(make-pointer
			 (if (eq meta :meta) +K-METABIT+ +K-ESCPREFIX+))))
  meta)

(defsetf meta-key set-meta-key
  "Set the the meta key for ‘fd’.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard mode

(defconstant +KDGKBMODE+ #x4B44 "Get keyboard mode.")
(defconstant +KDSKBMODE+ #x4B45 "Set keyboard mode.")

(define-constant +keyboard-modes+
  #(:raw :translate :medium-raw :unicode :off)
  "Possible values for console keyboard mode."
  #'equalp)

(defconstant +K-RAW+       #x00)
(defconstant +K-XLATE+     #x01)
(defconstant +K-MEDIUMRAW+ #x02)
(defconstant +K-UNICODE+   #x03)
(defconstant +K-OFF+       #x04)

(defun check-keyboard-mode (mode)
  (and (>= mode 0) (< mode (length +keyboard-modes+))))

(defun keyboard-mode (&optional fd)
  "Return the keyboard mode for ‘fd’ as one of ‘+keyboard-modes+’."
  (with-console (fd)
    (with-foreign-object (mode-ptr :long)
      (syscall (posix-ioctl fd +KDGKBMODE+ mode-ptr))
      (let ((mode (mem-ref mode-ptr :long)))
	(if (check-keyboard-mode mode)
	    (svref +keyboard-modes+ mode)
	    (error "Bad keyboard mode value ~s" mode))))))

(defun set-keyboard-mode (fd mode)
  (let ((mode-number (if (numberp mode) mode
			 (position mode +keyboard-modes+))))
    (unless (check-keyboard-mode mode-number)
      (error "Bad keyboard mode value ~s" mode))
    (syscall (posix-ioctl fd +KDSKBMODE+ (make-pointer mode-number)))
    mode))

(defsetf keyboard-mode set-keyboard-mode
  "Set the keyboard mode for ‘fd’.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sounds

(defconstant +KDMKTONE+  #x4B30 "Generate a tone.")
(defconstant +KIOCSOUND+ #x4B2F	"Start sound generation, or 0 for off.")

(defun tone (frequency duration &optional fd)
  (with-console (fd)
    (with-foreign-object (blarg :unsigned-long)
      (setf blarg
	    (logior (logand #xffff duration) ; period in clock cycles
		    (ash (logand #xffff frequency) 16))) ; duration in msec
      (syscall (posix-ioctl fd +KDMKTONE+ blarg)))))

#|
Generate tone of specified length. The lower 16 bits of the unsigned long
integer in argp specify the period in clock cycles, and the upper 16 bits give
the duration in msec. If the duration is zero, the sound is turned off.
Control returns immediately. For example, argp = (125<<16) + 0x637 would
specify the beep normally associated with a ctrl-G.
|#

(defun sound (frequency duration &optional fd)
  (with-console (fd)
    (with-foreign-object (blarg :unsigned-long)
      (setf blarg
	    (logior (logand #xffff duration) ; period in clock cycles
		    (ash (logand #xffff frequency) 16))) ; duration in msec
      (syscall (posix-ioctl fd +KIOCSOUND+ blarg)))))

#|
Start or stop sound generation. The lower 16 bits of argp specify the period
in clock cycles (that is, argp = 1193180/frequency). argp = 0 turns sound
off. In either case, control returns immediately.
|#

(defun sound-off (&optional (fd 0))
  "Turn all sounds off."
  ;; Don't error, just "silently" fail.
  (posix-ioctl fd +KDMKTONE+ 0)
  (posix-ioctl fd +KIOCSOUND+ 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
  GIO_CMAP        Get colormap
  PIO_CMAP        Put colormap
  GIO_FONT        Get font
  GIO_FONTX       Get font and character info
  PIO_FONT        Set font
  PIO_FONTX       Set font and character info
  PIO_FONTRESET   Reset the font to boot default
  GIO_SCRNMAP     Get the screen mapping
  GIO_UNISCRNMAP  Get the unicode screen mapping
  PIO_SCRNMAP     Put the user definable screen mapping
  PIO_UNISCRNMAP  Put the user definable unicode screen mapping
  GIO_UNIMAP      Get the unicode font mapping
  PIO_UNIMAP      Put the unicode font mapping
  PIO_UNIMAPCLR   Clear the unicode mapping and hash advise
  KDGKBENT        Get a key action
  KDSKBENT        Set a key action
  KDGKBSENT       Get a key string
  KDSKBSENT       Set a key string
  KDGKBDIACR      Get the accent table
  KDGETKEYCODE    Get the keycode table
  KDSETKEYCODE    Set the keycode table
  KDSIGACCEPT     Accept console spawn signals
  VT_OPENQRY      Get next available console
  VT_RELDISP      Release a display.
  VT_ACTIVATE     Switch to a VT
  VT_WAITACTIVE   Wait until a VT has been activated.
  VT_DISALLOCATE  Deallocate memory associated with a VT
  VT_RESIZE       Set the screen character cell size.
  VT_RESIZEX      Set the screen character cell and pixel size.
  TIOCLINUX, 2    Set selection.
  TIOCLINUX, 3    Paste selection.
  TIOCLINUX, 4    Unblank the screen.
  TIOCLINUX, 5    Sets selection word characters
  TIOCLINUX, 6    Set shift state
  TIOCLINUX, 7    Mouse report
  TIOCLINUX, 8    Dump screen (obsolete, use /dev/vcsa*)
  TIOCLINUX, 9    Restore screen (obsolete, use /dev/vcsa*)
  TIOCLINUX, 10   Set VESA screen blanking
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun console-p (&optional fd)
  "Return true if ‘fd’ is a Linux VT/Console. ‘fd’ defaults to the
controlling terminal."
  (when (not fd)
    #|
@@@@ how the fuck?

ctermid just returns "/dev/tty" in glibc and musl, very DUH

Where is it in the kernel???

(task_struct *)current->(signal_struct *)->signal->(tty_struct *)tty;

but ‘current’ actually is
  current_thread_info()->task

but current_thread_info() is:
  (struct thread_info *)current

WHAT?

... anyway

tty_struct has:

   char name[64];
   struct device *dev

Also there's an EXPORT_SYMBOL'd:

  const char * tty_name((tty_struct *)tty)

which returns it.
But is that even what we want?
It should just return the first file descriptor which MUST be open.
    |#
    (setf fd 0))
  (with-foreign-object (mode :int)
    (if (= -1 (posix-ioctl fd +KDGETMODE+ mode))
	(if (or (= uos:*errno* +EINVAL+)
		(= uos:*errno* +ENOTTY+))
	    nil
	    (uos:error-check -1 "checking console-p for ~s" fd))
	t)))

(defstruct vt-info
  "Details about a virtual terminal."
  fd
  mode
  state
  size
  console-size)

(defstruct kb-info
  "Details about a keyboard."
  font-description			; struct consolefontdesc
  unicode-map				; struct unimapdesc
  font					; struct console_font
  keyboard-mode				;
  text-mode				;
  leds					;
  lock-flags				;
  meta					;
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys
;;
;; All this is very confusing. There are too many numbers.
;;
;; Scan code
;;   A number from the kernel driver for the key, presumably from the hardware.
;;
;; Key code
;;   A hardware independent number for the key. There are scan code to key code
;;   translation tables in the kernel, which can be set and queried.
;;   Has a string name.
;;
;; Action code
;;   An action that a key performs, which is really just another naming and
;;   numbering system for keys.
;;
;; Key tables
;;   Mapping of key code to action code for each modifier bit.
;;
;; A vague and possibly wrong outline of how I think processing a key press
;; works, starting from the hardware:
;;
;;  hardware keyboard
;;  keyboard kernel driver
;;  translate scan code to key code
;;  look up key code + modifier in key table to get action code
;;  depending on the keyboard mode
;;     raw
;;       send raw scan codes
;;     medium raw
;;       send key codes & modifiers???
;;     unicode or text
;;       possibly do compose keys processing
;;       possibly translate to function key string
;;       depending on the action
;;         a thing that generates a character
;;           translate the key to an character encoding
;;           send the key encoded character to the terminal fd
;;         a key that does something in the kernel
;;           do that thing (e.g. switch console, sysrq processing)

(defconstant +NR-KEYS+ 256 "Keycode")
(defconstant +NR-KEYMAPS+ 256) ; aka MAX_NR_KEYMAPS

;; Shift key indexes
(defparameter *shift-keys* nil "List of shift keys.")
(define-to-list *shift-keys*
    #(#(+KG-SHIFT+        0)
      #(+KG-ALTGR+        1)
      #(+KG-CTRL+         2)
      #(+KG-ALT+          3)
      #(+KG-SHIFTL+       4)
      #(+KG-KANASHIFT+    4)
      #(+KG-SHIFTR+       5)
      #(+KG-CTRLL+        6)
      #(+KG-CTRLR+        7)
      #(+KG-CAPSSHIFT+    8)))

;; (defconstant +NR-SHIFT+ 9 "Number of shift keys.")

(defparameter *modifier-bits* nil "List of modifier bits.")
(define-to-list *modifier-bits*
    #(#(+M-PLAIN+         0)
      #(+M-SHIFT+         #.(ash 1 +KG-SHIFT+))
      #(+M-CTRL+          #.(ash 1 +KG-CTRL+))
      #(+M-ALT+           #.(ash 1 +KG-ALT+))
      #(+M-ALTGR+         #.(ash 1 +KG-ALTGR+))
      #(+M-SHIFTL+        #.(ash 1 +KG-SHIFTL+))
      #(+M-SHIFTR+        #.(ash 1 +KG-SHIFTR+))
      #(+M-CTRLL+         #.(ash 1 +KG-CTRLL+))
      #(+M-CTRLR+         #.(ash 1 +KG-CTRLR+))
      #(+M-CAPSSHIFT+     #.(ash 1 +KG-CAPSSHIFT+))))

(defstruct modifier
  "Modifier keys."
  name
  bit)

(defparameter *modifiers*
  (loop :for (name bit) :in `(("shift"     ,+KG-SHIFT+)
			      ("altgr"     ,+KG-ALTGR+)
			      ("control"   ,+KG-CTRL+)
			      ("alt"       ,+KG-ALT+)
			      ("shiftl"    ,+KG-SHIFTL+)
			      ("shiftr"    ,+KG-SHIFTR+)
			      ("ctrll"     ,+KG-CTRLL+)
			      ("ctrlr"     ,+KG-CTRLR+)
			      ("capsshift" ,+KG-CAPSSHIFT+))
	:collect (make-modifier :name name :bit bit)))

(defun print-modifiers (n)
  "Print the modifiers set in the integer ‘n’."
  (format t "~{~a~^ ~}"
	  (loop :for m :in *modifiers*
		:when (plusp (logand (ash 1 (modifier-bit m)) n))
		:collect (modifier-name m))))

(defun modifier-symbol-equal (a b)
  (flet ((base (n) (subseq n 3 (1- (length n)))))
    (or (eq a b)
	(equal (symbol-name a) (symbol-name b))
	(and (> (length (symbol-name a)) 4)
	     (> (length (symbol-name b)) 4)
	     (equalp (base (symbol-name a)) (base (symbol-name b)))))))

(defun find-modifier-bit (modifier-name)
  "Return the modifier bit for ‘modifier-name’, which can be a string from
*modifiers* or a symbol from *modifier-bits*."
  (let (m result)
    (typecase modifier-name
      (symbol
       (when (setf m (find modifier-name *modifier-bits*
			   :test #'modifier-symbol-equal))
	 (setf result (symbol-value m))))
      (string
       (cond
	 ((setf m (find modifier-name *modifiers* :test #'equalp
						  :key #'modifier-name))
	  (setf result (ash 1 (modifier-bit m))))
	 ((equalp modifier-name "plain")
	  (setf result 0)))))
    (unless result
      (error "~s is not a known modifier." modifier-name))
    result))

(defun modifier-table-number (modifier-list)
  "Return the modifier table number for the list of modifiers."
  (loop :with result = 0
	:for m :in modifier-list
	:do (setf result (logior result (find-modifier-bit m)))
	:finally (return result)))

(defparameter *key-types* nil "List of key types.")
(define-to-list *key-types*
    #(#(+KT-LATIN+        0)
      #(+KT-FN+           1)
      #(+KT-SPEC+         2)
      #(+KT-PAD+          3)
      #(+KT-DEAD+         4)
      #(+KT-CONS+         5)
      #(+KT-CUR+          6)
      #(+KT-SHIFT+        7)
      #(+KT-META+         8)
      #(+KT-ASCII+        9)
      #(+KT-LOCK+         10)
      #(+KT-LETTER+       11)
      #(+KT-SLOCK+        12)
      #(+KT-DEAD2+        13)
      #(+KT-BRL+          14)))

(defparameter *key-type-names*
  #("Latin"
    "Function"
    "Special"
    "Keypad"
    "Dead"
    "Console"
    "Cursor"
    "Shift"
    "Meta"
    "ASCII"
    "Lock"
    "Letter"
    "StickyLock"
    "Dead2"
    "Braille"))

(defun key-type-name (type)
  (aref *key-type-names* type))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro keycode-number (type value)
    "The key number for ‘type’ and ‘value’."
    `(logior (ash ,type 8) ,value))

  (defmacro keycode-type (key)
    "The key type for ‘key’.’"
    `(ash ,key -8))

  (defmacro keycode-value (key)
    `(logand ,key #xff)))

;; This is so fucking stupid. Why can't they clean this shit up?
(defun action-code-to-keysym (code)
  (cond
    ((minusp code)
     nil)
    ((< code 4096) ;; non unicode
     (cond
       ((< code 128)
	(key-action-name (keycode-number +KT-LATIN+ code)))
       ;; ((= (key-type code) +KT-META+)
       ;; 	nil)
       ;; ((= (key-type code) +KT-LETTER+)
       ;; 	(key-action-name (keycode-number +KT-LATIN+ (key-value code))))
       (t ; (> (key-type code) +KT-LATIN+)
	(key-action-name code))))
    (t
     (setf code (logxor code #xf000))
     (cond
       ((< code 128)
	(key-action-name (keycode-number +KT-LATIN+ code)))
       (t
	;; Fuck all that noise, just pretend it's a unicode code point.
	(ignore-errors (char-name (code-char code))))))))

(defparameter *key-action-names* nil "List of key action names.")
(define-to-list *key-action-names*
  #(#(+K-F1+       #.(keycode-number +KT-FN+ 0))
    #(+K-F2+       #.(keycode-number +KT-FN+ 1))
    #(+K-F3+       #.(keycode-number +KT-FN+ 2))
    #(+K-F4+       #.(keycode-number +KT-FN+ 3))
    #(+K-F5+       #.(keycode-number +KT-FN+ 4))
    #(+K-F6+       #.(keycode-number +KT-FN+ 5))
    #(+K-F7+       #.(keycode-number +KT-FN+ 6))
    #(+K-F8+       #.(keycode-number +KT-FN+ 7))
    #(+K-F9+       #.(keycode-number +KT-FN+ 8))
    #(+K-F10+      #.(keycode-number +KT-FN+ 9))
    #(+K-F11+      #.(keycode-number +KT-FN+ 10))
    #(+K-F12+      #.(keycode-number +KT-FN+ 11))
    #(+K-F13+      #.(keycode-number +KT-FN+ 12))
    #(+K-F14+      #.(keycode-number +KT-FN+ 13))
    #(+K-F15+      #.(keycode-number +KT-FN+ 14))
    #(+K-F16+      #.(keycode-number +KT-FN+ 15))
    #(+K-F17+      #.(keycode-number +KT-FN+ 16))
    #(+K-F18+      #.(keycode-number +KT-FN+ 17))
    #(+K-F19+      #.(keycode-number +KT-FN+ 18))
    #(+K-F20+      #.(keycode-number +KT-FN+ 19))
    #(+K-FIND+     #.(keycode-number +KT-FN+ 20))
    #(+K-INSERT+   #.(keycode-number +KT-FN+ 21))
    #(+K-REMOVE+   #.(keycode-number +KT-FN+ 22))
    #(+K-SELECT+   #.(keycode-number +KT-FN+ 23))
    #(+K-PGUP+     #.(keycode-number +KT-FN+ 24))
    #(+K-PGDN+     #.(keycode-number +KT-FN+ 25))
    #(+K-MACRO+    #.(keycode-number +KT-FN+ 26))
    #(+K-HELP+     #.(keycode-number +KT-FN+ 27))
    #(+K-DO+       #.(keycode-number +KT-FN+ 28))
    #(+K-PAUSE+    #.(keycode-number +KT-FN+ 29))
    #(+K-F21+      #.(keycode-number +KT-FN+ 30))
    #(+K-F22+      #.(keycode-number +KT-FN+ 31))
    #(+K-F23+      #.(keycode-number +KT-FN+ 32))
    #(+K-F24+      #.(keycode-number +KT-FN+ 33))
    #(+K-F25+      #.(keycode-number +KT-FN+ 34))
    #(+K-F26+      #.(keycode-number +KT-FN+ 35))
    #(+K-F27+      #.(keycode-number +KT-FN+ 36))
    #(+K-F28+      #.(keycode-number +KT-FN+ 37))
    #(+K-F29+      #.(keycode-number +KT-FN+ 38))
    #(+K-F30+      #.(keycode-number +KT-FN+ 39))
    #(+K-F31+      #.(keycode-number +KT-FN+ 40))
    #(+K-F32+      #.(keycode-number +KT-FN+ 41))
    #(+K-F33+      #.(keycode-number +KT-FN+ 42))
    #(+K-F34+      #.(keycode-number +KT-FN+ 43))
    #(+K-F35+      #.(keycode-number +KT-FN+ 44))
    #(+K-F36+      #.(keycode-number +KT-FN+ 45))
    #(+K-F37+      #.(keycode-number +KT-FN+ 46))
    #(+K-F38+      #.(keycode-number +KT-FN+ 47))
    #(+K-F39+      #.(keycode-number +KT-FN+ 48))
    #(+K-F40+      #.(keycode-number +KT-FN+ 49))
    #(+K-F41+       #.(keycode-number +KT-FN+ 50))
    #(+K-F42+       #.(keycode-number +KT-FN+ 51))
    #(+K-F43+       #.(keycode-number +KT-FN+ 52))
    #(+K-F44+       #.(keycode-number +KT-FN+ 53))
    #(+K-F45+       #.(keycode-number +KT-FN+ 54))
    #(+K-F46+       #.(keycode-number +KT-FN+ 55))
    #(+K-F47+       #.(keycode-number +KT-FN+ 56))
    #(+K-F48+       #.(keycode-number +KT-FN+ 57))
    #(+K-F49+       #.(keycode-number +KT-FN+ 58))
    #(+K-F50+       #.(keycode-number +KT-FN+ 59))
    #(+K-F51+       #.(keycode-number +KT-FN+ 60))
    #(+K-F52+       #.(keycode-number +KT-FN+ 61))
    #(+K-F53+       #.(keycode-number +KT-FN+ 62))
    #(+K-F54+       #.(keycode-number +KT-FN+ 63))
    #(+K-F55+       #.(keycode-number +KT-FN+ 64))
    #(+K-F56+       #.(keycode-number +KT-FN+ 65))
    #(+K-F57+       #.(keycode-number +KT-FN+ 66))
    #(+K-F58+       #.(keycode-number +KT-FN+ 67))
    #(+K-F59+       #.(keycode-number +KT-FN+ 68))
    #(+K-F60+       #.(keycode-number +KT-FN+ 69))
    #(+K-F61+       #.(keycode-number +KT-FN+ 70))
    #(+K-F62+       #.(keycode-number +KT-FN+ 71))
    #(+K-F63+       #.(keycode-number +KT-FN+ 72))
    #(+K-F64+       #.(keycode-number +KT-FN+ 73))
    #(+K-F65+       #.(keycode-number +KT-FN+ 74))
    #(+K-F66+       #.(keycode-number +KT-FN+ 75))
    #(+K-F67+       #.(keycode-number +KT-FN+ 76))
    #(+K-F68+       #.(keycode-number +KT-FN+ 77))
    #(+K-F69+       #.(keycode-number +KT-FN+ 78))
    #(+K-F70+       #.(keycode-number +KT-FN+ 79))
    #(+K-F71+       #.(keycode-number +KT-FN+ 80))
    #(+K-F72+       #.(keycode-number +KT-FN+ 81))
    #(+K-F73+       #.(keycode-number +KT-FN+ 82))
    #(+K-F74+       #.(keycode-number +KT-FN+ 83))
    #(+K-F75+       #.(keycode-number +KT-FN+ 84))
    #(+K-F76+       #.(keycode-number +KT-FN+ 85))
    #(+K-F77+       #.(keycode-number +KT-FN+ 86))
    #(+K-F78+       #.(keycode-number +KT-FN+ 87))
    #(+K-F79+       #.(keycode-number +KT-FN+ 88))
    #(+K-F80+       #.(keycode-number +KT-FN+ 89))
    #(+K-F81+       #.(keycode-number +KT-FN+ 90))
    #(+K-F82+       #.(keycode-number +KT-FN+ 91))
    #(+K-F83+       #.(keycode-number +KT-FN+ 92))
    #(+K-F84+       #.(keycode-number +KT-FN+ 93))
    #(+K-F85+       #.(keycode-number +KT-FN+ 94))
    #(+K-F86+       #.(keycode-number +KT-FN+ 95))
    #(+K-F87+       #.(keycode-number +KT-FN+ 96))
    #(+K-F88+       #.(keycode-number +KT-FN+ 97))
    #(+K-F89+       #.(keycode-number +KT-FN+ 98))
    #(+K-F90+       #.(keycode-number +KT-FN+ 99))
    #(+K-F91+       #.(keycode-number +KT-FN+ 100))
    #(+K-F92+       #.(keycode-number +KT-FN+ 101))
    #(+K-F93+       #.(keycode-number +KT-FN+ 102))
    #(+K-F94+       #.(keycode-number +KT-FN+ 103))
    #(+K-F95+       #.(keycode-number +KT-FN+ 104))
    #(+K-F96+       #.(keycode-number +KT-FN+ 105))
    #(+K-F97+       #.(keycode-number +KT-FN+ 106))
    #(+K-F98+       #.(keycode-number +KT-FN+ 107))
    #(+K-F99+       #.(keycode-number +KT-FN+ 108))
    #(+K-F100+      #.(keycode-number +KT-FN+ 109))
    #(+K-F101+      #.(keycode-number +KT-FN+ 110))
    #(+K-F102+      #.(keycode-number +KT-FN+ 111))
    #(+K-F103+      #.(keycode-number +KT-FN+ 112))
    #(+K-F104+      #.(keycode-number +KT-FN+ 113))
    #(+K-F105+      #.(keycode-number +KT-FN+ 114))
    #(+K-F106+      #.(keycode-number +KT-FN+ 115))
    #(+K-F107+      #.(keycode-number +KT-FN+ 116))
    #(+K-F108+      #.(keycode-number +KT-FN+ 117))
    #(+K-F109+      #.(keycode-number +KT-FN+ 118))
    #(+K-F110+      #.(keycode-number +KT-FN+ 119))
    #(+K-F111+      #.(keycode-number +KT-FN+ 120))
    #(+K-F112+      #.(keycode-number +KT-FN+ 121))
    #(+K-F113+      #.(keycode-number +KT-FN+ 122))
    #(+K-F114+      #.(keycode-number +KT-FN+ 123))
    #(+K-F115+      #.(keycode-number +KT-FN+ 124))
    #(+K-F116+      #.(keycode-number +KT-FN+ 125))
    #(+K-F117+      #.(keycode-number +KT-FN+ 126))
    #(+K-F118+      #.(keycode-number +KT-FN+ 127))
    #(+K-F119+      #.(keycode-number +KT-FN+ 128))
    #(+K-F120+      #.(keycode-number +KT-FN+ 129))
    #(+K-F121+      #.(keycode-number +KT-FN+ 130))
    #(+K-F122+      #.(keycode-number +KT-FN+ 131))
    #(+K-F123+      #.(keycode-number +KT-FN+ 132))
    #(+K-F124+      #.(keycode-number +KT-FN+ 133))
    #(+K-F125+      #.(keycode-number +KT-FN+ 134))
    #(+K-F126+      #.(keycode-number +KT-FN+ 135))
    #(+K-F127+      #.(keycode-number +KT-FN+ 136))
    #(+K-F128+      #.(keycode-number +KT-FN+ 137))
    #(+K-F129+      #.(keycode-number +KT-FN+ 138))
    #(+K-F130+      #.(keycode-number +KT-FN+ 139))
    #(+K-F131+      #.(keycode-number +KT-FN+ 140))
    #(+K-F132+      #.(keycode-number +KT-FN+ 141))
    #(+K-F133+      #.(keycode-number +KT-FN+ 142))
    #(+K-F134+      #.(keycode-number +KT-FN+ 143))
    #(+K-F135+      #.(keycode-number +KT-FN+ 144))
    #(+K-F136+      #.(keycode-number +KT-FN+ 145))
    #(+K-F137+      #.(keycode-number +KT-FN+ 146))
    #(+K-F138+      #.(keycode-number +KT-FN+ 147))
    #(+K-F139+      #.(keycode-number +KT-FN+ 148))
    #(+K-F140+      #.(keycode-number +KT-FN+ 149))
    #(+K-F141+      #.(keycode-number +KT-FN+ 150))
    #(+K-F142+      #.(keycode-number +KT-FN+ 151))
    #(+K-F143+      #.(keycode-number +KT-FN+ 152))
    #(+K-F144+      #.(keycode-number +KT-FN+ 153))
    #(+K-F145+      #.(keycode-number +KT-FN+ 154))
    #(+K-F146+      #.(keycode-number +KT-FN+ 155))
    #(+K-F147+      #.(keycode-number +KT-FN+ 156))
    #(+K-F148+      #.(keycode-number +KT-FN+ 157))
    #(+K-F149+      #.(keycode-number +KT-FN+ 158))
    #(+K-F150+      #.(keycode-number +KT-FN+ 159))
    #(+K-F151+      #.(keycode-number +KT-FN+ 160))
    #(+K-F152+      #.(keycode-number +KT-FN+ 161))
    #(+K-F153+      #.(keycode-number +KT-FN+ 162))
    #(+K-F154+      #.(keycode-number +KT-FN+ 163))
    #(+K-F155+      #.(keycode-number +KT-FN+ 164))
    #(+K-F156+      #.(keycode-number +KT-FN+ 165))
    #(+K-F157+      #.(keycode-number +KT-FN+ 166))
    #(+K-F158+      #.(keycode-number +KT-FN+ 167))
    #(+K-F159+      #.(keycode-number +KT-FN+ 168))
    #(+K-F160+      #.(keycode-number +KT-FN+ 169))
    #(+K-F161+      #.(keycode-number +KT-FN+ 170))
    #(+K-F162+      #.(keycode-number +KT-FN+ 171))
    #(+K-F163+      #.(keycode-number +KT-FN+ 172))
    #(+K-F164+      #.(keycode-number +KT-FN+ 173))
    #(+K-F165+      #.(keycode-number +KT-FN+ 174))
    #(+K-F166+      #.(keycode-number +KT-FN+ 175))
    #(+K-F167+      #.(keycode-number +KT-FN+ 176))
    #(+K-F168+      #.(keycode-number +KT-FN+ 177))
    #(+K-F169+      #.(keycode-number +KT-FN+ 178))
    #(+K-F170+      #.(keycode-number +KT-FN+ 179))
    #(+K-F171+      #.(keycode-number +KT-FN+ 180))
    #(+K-F172+      #.(keycode-number +KT-FN+ 181))
    #(+K-F173+      #.(keycode-number +KT-FN+ 182))
    #(+K-F174+      #.(keycode-number +KT-FN+ 183))
    #(+K-F175+      #.(keycode-number +KT-FN+ 184))
    #(+K-F176+      #.(keycode-number +KT-FN+ 185))
    #(+K-F177+      #.(keycode-number +KT-FN+ 186))
    #(+K-F178+      #.(keycode-number +KT-FN+ 187))
    #(+K-F179+      #.(keycode-number +KT-FN+ 188))
    #(+K-F180+      #.(keycode-number +KT-FN+ 189))
    #(+K-F181+      #.(keycode-number +KT-FN+ 190))
    #(+K-F182+      #.(keycode-number +KT-FN+ 191))
    #(+K-F183+      #.(keycode-number +KT-FN+ 192))
    #(+K-F184+      #.(keycode-number +KT-FN+ 193))
    #(+K-F185+      #.(keycode-number +KT-FN+ 194))
    #(+K-F186+      #.(keycode-number +KT-FN+ 195))
    #(+K-F187+      #.(keycode-number +KT-FN+ 196))
    #(+K-F188+      #.(keycode-number +KT-FN+ 197))
    #(+K-F189+      #.(keycode-number +KT-FN+ 198))
    #(+K-F190+      #.(keycode-number +KT-FN+ 199))
    #(+K-F191+      #.(keycode-number +KT-FN+ 200))
    #(+K-F192+      #.(keycode-number +KT-FN+ 201))
    #(+K-F193+      #.(keycode-number +KT-FN+ 202))
    #(+K-F194+      #.(keycode-number +KT-FN+ 203))
    #(+K-F195+      #.(keycode-number +KT-FN+ 204))
    #(+K-F196+      #.(keycode-number +KT-FN+ 205))
    #(+K-F197+      #.(keycode-number +KT-FN+ 206))
    #(+K-F198+      #.(keycode-number +KT-FN+ 207))
    #(+K-F199+      #.(keycode-number +KT-FN+ 208))
    #(+K-F200+      #.(keycode-number +KT-FN+ 209))
    #(+K-F201+      #.(keycode-number +KT-FN+ 210))
    #(+K-F202+      #.(keycode-number +KT-FN+ 211))
    #(+K-F203+      #.(keycode-number +KT-FN+ 212))
    #(+K-F204+      #.(keycode-number +KT-FN+ 213))
    #(+K-F205+      #.(keycode-number +KT-FN+ 214))
    #(+K-F206+      #.(keycode-number +KT-FN+ 215))
    #(+K-F207+      #.(keycode-number +KT-FN+ 216))
    #(+K-F208+      #.(keycode-number +KT-FN+ 217))
    #(+K-F209+      #.(keycode-number +KT-FN+ 218))
    #(+K-F210+      #.(keycode-number +KT-FN+ 219))
    #(+K-F211+      #.(keycode-number +KT-FN+ 220))
    #(+K-F212+      #.(keycode-number +KT-FN+ 221))
    #(+K-F213+      #.(keycode-number +KT-FN+ 222))
    #(+K-F214+      #.(keycode-number +KT-FN+ 223))
    #(+K-F215+      #.(keycode-number +KT-FN+ 224))
    #(+K-F216+      #.(keycode-number +KT-FN+ 225))
    #(+K-F217+      #.(keycode-number +KT-FN+ 226))
    #(+K-F218+      #.(keycode-number +KT-FN+ 227))
    #(+K-F219+      #.(keycode-number +KT-FN+ 228))
    #(+K-F220+      #.(keycode-number +KT-FN+ 229))
    #(+K-F221+      #.(keycode-number +KT-FN+ 230))
    #(+K-F222+      #.(keycode-number +KT-FN+ 231))
    #(+K-F223+      #.(keycode-number +KT-FN+ 232))
    #(+K-F224+      #.(keycode-number +KT-FN+ 233))
    #(+K-F225+      #.(keycode-number +KT-FN+ 234))
    #(+K-F226+      #.(keycode-number +KT-FN+ 235))
    #(+K-F227+      #.(keycode-number +KT-FN+ 236))
    #(+K-F228+      #.(keycode-number +KT-FN+ 237))
    #(+K-F229+      #.(keycode-number +KT-FN+ 238))
    #(+K-F230+      #.(keycode-number +KT-FN+ 239))
    #(+K-F231+      #.(keycode-number +KT-FN+ 240))
    #(+K-F232+      #.(keycode-number +KT-FN+ 241))
    #(+K-F233+      #.(keycode-number +KT-FN+ 242))
    #(+K-F234+      #.(keycode-number +KT-FN+ 243))
    #(+K-F235+      #.(keycode-number +KT-FN+ 244))
    #(+K-F236+      #.(keycode-number +KT-FN+ 245))
    #(+K-F237+      #.(keycode-number +KT-FN+ 246))
    #(+K-F238+      #.(keycode-number +KT-FN+ 247))
    #(+K-F239+      #.(keycode-number +KT-FN+ 248))
    #(+K-F240+      #.(keycode-number +KT-FN+ 249))
    #(+K-F241+      #.(keycode-number +KT-FN+ 250))
    #(+K-F242+      #.(keycode-number +KT-FN+ 251))
    #(+K-F243+      #.(keycode-number +KT-FN+ 252))
    #(+K-F244+      #.(keycode-number +KT-FN+ 253))
    #(+K-F245+      #.(keycode-number +KT-FN+ 254))
    #(+K-UNDO+      #.(keycode-number +KT-FN+ 255))
    #(+K-HOLE+         #.(keycode-number +KT-SPEC+ 0))
    #(+K-ENTER+        #.(keycode-number +KT-SPEC+ 1))
    #(+K-SH-REGS+      #.(keycode-number +KT-SPEC+ 2))
    #(+K-SH-MEM+       #.(keycode-number +KT-SPEC+ 3))
    #(+K-SH-STAT+      #.(keycode-number +KT-SPEC+ 4))
    #(+K-BREAK+        #.(keycode-number +KT-SPEC+ 5))
    #(+K-CONS+         #.(keycode-number +KT-SPEC+ 6))
    #(+K-CAPS+         #.(keycode-number +KT-SPEC+ 7))
    #(+K-NUM+          #.(keycode-number +KT-SPEC+ 8))
    #(+K-HOLD+         #.(keycode-number +KT-SPEC+ 9))
    #(+K-SCROLLFORW+   #.(keycode-number +KT-SPEC+ 10))
    #(+K-SCROLLBACK+   #.(keycode-number +KT-SPEC+ 11))
    #(+K-BOOT+         #.(keycode-number +KT-SPEC+ 12))
    #(+K-CAPSON+       #.(keycode-number +KT-SPEC+ 13))
    #(+K-COMPOSE+      #.(keycode-number +KT-SPEC+ 14))
    #(+K-SAK+          #.(keycode-number +KT-SPEC+ 15))
    #(+K-DECRCONSOLE+  #.(keycode-number +KT-SPEC+ 16))
    #(+K-INCRCONSOLE+  #.(keycode-number +KT-SPEC+ 17))
    #(+K-SPAWNCONSOLE+ #.(keycode-number +KT-SPEC+ 18))
    #(+K-BARENUMLOCK+  #.(keycode-number +KT-SPEC+ 19))
    #(+K-ALLOCATED+    #.(keycode-number +KT-SPEC+ 126))
    #(+K-NOSUCHMAP+    #.(keycode-number +KT-SPEC+ 127))
    #(+K-P0+           #.(keycode-number +KT-PAD+ 0))
    #(+K-P1+           #.(keycode-number +KT-PAD+ 1))
    #(+K-P2+           #.(keycode-number +KT-PAD+ 2))
    #(+K-P3+           #.(keycode-number +KT-PAD+ 3))
    #(+K-P4+           #.(keycode-number +KT-PAD+ 4))
    #(+K-P5+           #.(keycode-number +KT-PAD+ 5))
    #(+K-P6+           #.(keycode-number +KT-PAD+ 6))
    #(+K-P7+           #.(keycode-number +KT-PAD+ 7))
    #(+K-P8+           #.(keycode-number +KT-PAD+ 8))
    #(+K-P9+           #.(keycode-number +KT-PAD+ 9))
    #(+K-PPLUS+        #.(keycode-number +KT-PAD+ 10))
    #(+K-PMINUS+       #.(keycode-number +KT-PAD+ 11))
    #(+K-PSTAR+        #.(keycode-number +KT-PAD+ 12))
    #(+K-PSLASH+       #.(keycode-number +KT-PAD+ 13))
    #(+K-PENTER+       #.(keycode-number +KT-PAD+ 14))
    #(+K-PCOMMA+       #.(keycode-number +KT-PAD+ 15))
    #(+K-PDOT+         #.(keycode-number +KT-PAD+ 16))
    #(+K-PPLUSMINUS+   #.(keycode-number +KT-PAD+ 17))
    #(+K-PPARENL+      #.(keycode-number +KT-PAD+ 18))
    #(+K-PPARENR+      #.(keycode-number +KT-PAD+ 19))
    #(+K-DGRAVE+       #.(keycode-number +KT-DEAD+ 0))
    #(+K-DACUTE+       #.(keycode-number +KT-DEAD+ 1))
    #(+K-DCIRCM+       #.(keycode-number +KT-DEAD+ 2))
    #(+K-DTILDE+       #.(keycode-number +KT-DEAD+ 3))
    #(+K-DDIERE+       #.(keycode-number +KT-DEAD+ 4))
    #(+K-DCEDIL+       #.(keycode-number +KT-DEAD+ 5))
    #(+K-DMACRON+      #.(keycode-number +KT-DEAD+ 6))
    #(+K-DBREVE+       #.(keycode-number +KT-DEAD+ 7))
    #(+K-DABDOT+       #.(keycode-number +KT-DEAD+ 8))
    #(+K-DABRING+      #.(keycode-number +KT-DEAD+ 9))
    #(+K-DDBACUTE+     #.(keycode-number +KT-DEAD+ 10))
    #(+K-DCARON+       #.(keycode-number +KT-DEAD+ 11))
    #(+K-DOGONEK+      #.(keycode-number +KT-DEAD+ 12))
    #(+K-DIOTA+        #.(keycode-number +KT-DEAD+ 13))
    #(+K-DVOICED+      #.(keycode-number +KT-DEAD+ 14))
    #(+K-DSEMVOICED+   #.(keycode-number +KT-DEAD+ 15))
    #(+K-DBEDOT+       #.(keycode-number +KT-DEAD+ 16))
    #(+K-DHOOK+        #.(keycode-number +KT-DEAD+ 17))
    #(+K-DHORN+        #.(keycode-number +KT-DEAD+ 18))
    #(+K-DSTROKE+      #.(keycode-number +KT-DEAD+ 19))
    #(+K-DABCOMMA+     #.(keycode-number +KT-DEAD+ 20))
    #(+K-DABREVCOMMA+  #.(keycode-number +KT-DEAD+ 21))
    #(+K-DDBGRAVE+     #.(keycode-number +KT-DEAD+ 22))
    #(+K-DINVBREVE+    #.(keycode-number +KT-DEAD+ 23))
    #(+K-DBECOMMA+     #.(keycode-number +KT-DEAD+ 24))
    #(+K-DCURRENCY+    #.(keycode-number +KT-DEAD+ 25))
    #(+K-DGREEK+       #.(keycode-number +KT-DEAD+ 26))
    #(+K-DOWN+         #.(keycode-number +KT-CUR+ 0))
    #(+K-LEFT+         #.(keycode-number +KT-CUR+ 1))
    #(+K-RIGHT+        #.(keycode-number +KT-CUR+ 2))
    #(+K-UP+           #.(keycode-number +KT-CUR+ 3))
    #(+K-SHIFT+        #.(keycode-number +KT-SHIFT+ +KG-SHIFT+))
    #(+K-CTRL+         #.(keycode-number +KT-SHIFT+ +KG-CTRL+))
    #(+K-ALT+          #.(keycode-number +KT-SHIFT+ +KG-ALT+))
    #(+K-ALTGR+        #.(keycode-number +KT-SHIFT+ +KG-ALTGR+))
    #(+K-SHIFTL+       #.(keycode-number +KT-SHIFT+ +KG-SHIFTL+))
    #(+K-SHIFTR+       #.(keycode-number +KT-SHIFT+ +KG-SHIFTR+))
    #(+K-CTRLL+        #.(keycode-number +KT-SHIFT+ +KG-CTRLL+))
    #(+K-CTRLR+        #.(keycode-number +KT-SHIFT+ +KG-CTRLR+))
    #(+K-CAPSSHIFT+    #.(keycode-number +KT-SHIFT+ +KG-CAPSSHIFT+))
    #(+K-ASC0+         #.(keycode-number +KT-ASCII+ 0))
    #(+K-ASC1+         #.(keycode-number +KT-ASCII+ 1))
    #(+K-ASC2+         #.(keycode-number +KT-ASCII+ 2))
    #(+K-ASC3+         #.(keycode-number +KT-ASCII+ 3))
    #(+K-ASC4+         #.(keycode-number +KT-ASCII+ 4))
    #(+K-ASC5+         #.(keycode-number +KT-ASCII+ 5))
    #(+K-ASC6+         #.(keycode-number +KT-ASCII+ 6))
    #(+K-ASC7+         #.(keycode-number +KT-ASCII+ 7))
    #(+K-ASC8+         #.(keycode-number +KT-ASCII+ 8))
    #(+K-ASC9+         #.(keycode-number +KT-ASCII+ 9))
    #(+K-HEX0+         #.(keycode-number +KT-ASCII+ 10))
    #(+K-HEX1+         #.(keycode-number +KT-ASCII+ 11))
    #(+K-HEX2+         #.(keycode-number +KT-ASCII+ 12))
    #(+K-HEX3+         #.(keycode-number +KT-ASCII+ 13))
    #(+K-HEX4+         #.(keycode-number +KT-ASCII+ 14))
    #(+K-HEX5+         #.(keycode-number +KT-ASCII+ 15))
    #(+K-HEX6+         #.(keycode-number +KT-ASCII+ 16))
    #(+K-HEX7+         #.(keycode-number +KT-ASCII+ 17))
    #(+K-HEX8+         #.(keycode-number +KT-ASCII+ 18))
    #(+K-HEX9+         #.(keycode-number +KT-ASCII+ 19))
    #(+K-HEXa+         #.(keycode-number +KT-ASCII+ 20))
    #(+K-HEXb+         #.(keycode-number +KT-ASCII+ 21))
    #(+K-HEXc+         #.(keycode-number +KT-ASCII+ 22))
    #(+K-HEXd+         #.(keycode-number +KT-ASCII+ 23))
    #(+K-HEXe+         #.(keycode-number +KT-ASCII+ 24))
    #(+K-HEXf+         #.(keycode-number +KT-ASCII+ 25))
    #(+K-SHIFTLOCK+       #.(keycode-number +KT-LOCK+ +KG-SHIFT+))
    #(+K-CTRLLOCK+        #.(keycode-number +KT-LOCK+ +KG-CTRL+))
    #(+K-ALTLOCK+         #.(keycode-number +KT-LOCK+ +KG-ALT+))
    #(+K-ALTGRLOCK+       #.(keycode-number +KT-LOCK+ +KG-ALTGR+))
    #(+K-SHIFTLLOCK+      #.(keycode-number +KT-LOCK+ +KG-SHIFTL+))
    #(+K-SHIFTRLOCK+      #.(keycode-number +KT-LOCK+ +KG-SHIFTR+))
    #(+K-CTRLLLOCK+       #.(keycode-number +KT-LOCK+ +KG-CTRLL+))
    #(+K-CTRLRLOCK+       #.(keycode-number +KT-LOCK+ +KG-CTRLR+))
    #(+K-CAPSSHIFTLOCK+   #.(keycode-number +KT-LOCK+ +KG-CAPSSHIFT+))
    #(+K-SHIFT-SLOCK+     #.(keycode-number +KT-SLOCK+ +KG-SHIFT+))
    #(+K-CTRL-SLOCK+      #.(keycode-number +KT-SLOCK+ +KG-CTRL+))
    #(+K-ALT-SLOCK+       #.(keycode-number +KT-SLOCK+ +KG-ALT+))
    #(+K-ALTGR-SLOCK+     #.(keycode-number +KT-SLOCK+ +KG-ALTGR+))
    #(+K-SHIFTL-SLOCK+    #.(keycode-number +KT-SLOCK+ +KG-SHIFTL+))
    #(+K-SHIFTR-SLOCK+    #.(keycode-number +KT-SLOCK+ +KG-SHIFTR+))
    #(+K-CTRLL-SLOCK+     #.(keycode-number +KT-SLOCK+ +KG-CTRLL+))
    #(+K-CTRLR-SLOCK+     #.(keycode-number +KT-SLOCK+ +KG-CTRLR+))
    #(+K-CAPSSHIFT-SLOCK+ #.(keycode-number +KT-SLOCK+ +KG-CAPSSHIFT+))

    #(+K-BRL-BLANK+       #.(keycode-number +KT-BRL+ 0))
    #(+K-BRL-DOT1+        #.(keycode-number +KT-BRL+ 1))
    #(+K-BRL-DOT2+        #.(keycode-number +KT-BRL+ 2))
    #(+K-BRL-DOT3+        #.(keycode-number +KT-BRL+ 3))
    #(+K-BRL-DOT4+        #.(keycode-number +KT-BRL+ 4))
    #(+K-BRL-DOT5+        #.(keycode-number +KT-BRL+ 5))
    #(+K-BRL-DOT6+        #.(keycode-number +KT-BRL+ 6))
    #(+K-BRL-DOT7+        #.(keycode-number +KT-BRL+ 7))
    #(+K-BRL-DOT8+        #.(keycode-number +KT-BRL+ 8))
    #(+K-BRL-DOT9+        #.(keycode-number +KT-BRL+ 9))
    #(+K-BRL-DOT10+       #.(keycode-number +KT-BRL+ 10))))

(defparameter *key-action-table*
  (load-time-value
   (let ((table (make-hash-table)))
     (loop :for k :in *key-action-names*
	   :do (setf (gethash (symbol-value k) table) k))
     table))
  "Map key action names to numbers.")

(defconstant +NR-PAD+     20)
(defconstant +NR-DEAD+    27)
(defconstant +NR-ASCII+   26)
(defconstant +NR-LOCK+    9)
(defconstant +NR-BRL+     11)
(defconstant +MAX-DIACR+  256)

;; These are defined by the kernel in input-event-codes.h
(defparameter *keycodes* nil "Key codes.")
(define-to-list *keycodes*
  #(#(+KEY-RESERVED+          0)
    #(+KEY-ESC+               1)
    #(+KEY-1+                 2)
    #(+KEY-2+                 3)
    #(+KEY-3+                 4)
    #(+KEY-4+                 5)
    #(+KEY-5+                 6)
    #(+KEY-6+                 7)
    #(+KEY-7+                 8)
    #(+KEY-8+                 9)
    #(+KEY-9+                 10)
    #(+KEY-0+                 11)
    #(+KEY-MINUS+             12)
    #(+KEY-EQUAL+             13)
    #(+KEY-BACKSPACE+         14)
    #(+KEY-TAB+               15)
    #(+KEY-Q+                 16)
    #(+KEY-W+                 17)
    #(+KEY-E+                 18)
    #(+KEY-R+                 19)
    #(+KEY-T+                 20)
    #(+KEY-Y+                 21)
    #(+KEY-U+                 22)
    #(+KEY-I+                 23)
    #(+KEY-O+                 24)
    #(+KEY-P+                 25)
    #(+KEY-LEFTBRACE+         26)
    #(+KEY-RIGHTBRACE+        27)
    #(+KEY-ENTER+             28)
    #(+KEY-LEFTCTRL+          29)
    #(+KEY-A+                 30)
    #(+KEY-S+                 31)
    #(+KEY-D+                 32)
    #(+KEY-F+                 33)
    #(+KEY-G+                 34)
    #(+KEY-H+                 35)
    #(+KEY-J+                 36)
    #(+KEY-K+                 37)
    #(+KEY-L+                 38)
    #(+KEY-SEMICOLON+         39)
    #(+KEY-APOSTROPHE+        40)
    #(+KEY-GRAVE+             41)
    #(+KEY-LEFTSHIFT+         42)
    #(+KEY-BACKSLASH+         43)
    #(+KEY-Z+                 44)
    #(+KEY-X+                 45)
    #(+KEY-C+                 46)
    #(+KEY-V+                 47)
    #(+KEY-B+                 48)
    #(+KEY-N+                 49)
    #(+KEY-M+                 50)
    #(+KEY-COMMA+             51)
    #(+KEY-DOT+               52)
    #(+KEY-SLASH+             53)
    #(+KEY-RIGHTSHIFT+        54)
    #(+KEY-KPASTERISK+        55)
    #(+KEY-LEFTALT+           56)
    #(+KEY-SPACE+             57)
    #(+KEY-CAPSLOCK+          58)
    #(+KEY-F1+                59)
    #(+KEY-F2+                60)
    #(+KEY-F3+                61)
    #(+KEY-F4+                62)
    #(+KEY-F5+                63)
    #(+KEY-F6+                64)
    #(+KEY-F7+                65)
    #(+KEY-F8+                66)
    #(+KEY-F9+                67)
    #(+KEY-F10+               68)
    #(+KEY-NUMLOCK+           69)
    #(+KEY-SCROLLLOCK+        70)
    #(+KEY-KP7+               71)
    #(+KEY-KP8+               72)
    #(+KEY-KP9+               73)
    #(+KEY-KPMINUS+           74)
    #(+KEY-KP4+               75)
    #(+KEY-KP5+               76)
    #(+KEY-KP6+               77)
    #(+KEY-KPPLUS+            78)
    #(+KEY-KP1+               79)
    #(+KEY-KP2+               80)
    #(+KEY-KP3+               81)
    #(+KEY-KP0+               82)
    #(+KEY-KPDOT+             83)
    #(+KEY-ZENKAKUHANKAKU+    85)
    #(+KEY-102ND+             86)
    #(+KEY-F11+               87)
    #(+KEY-F12+               88)
    #(+KEY-RO+                89)
    #(+KEY-KATAKANA+          90)
    #(+KEY-HIRAGANA+          91)
    #(+KEY-HENKAN+            92)
    #(+KEY-KATAKANAHIRAGANA+  93)
    #(+KEY-MUHENKAN+          94)
    #(+KEY-KPJPCOMMA+         95)
    #(+KEY-KPENTER+           96)
    #(+KEY-RIGHTCTRL+         97)
    #(+KEY-KPSLASH+           98)
    #(+KEY-SYSRQ+             99)
    #(+KEY-RIGHTALT+          100)
    #(+KEY-LINEFEED+          101)
    #(+KEY-HOME+              102)
    #(+KEY-UP+                103)
    #(+KEY-PAGEUP+            104)
    #(+KEY-LEFT+              105)
    #(+KEY-RIGHT+             106)
    #(+KEY-END+               107)
    #(+KEY-DOWN+              108)
    #(+KEY-PAGEDOWN+          109)
    #(+KEY-INSERT+            110)
    #(+KEY-DELETE+            111)
    #(+KEY-MACRO+             112)
    #(+KEY-MUTE+              113)
    #(+KEY-VOLUMEDOWN+        114)
    #(+KEY-VOLUMEUP+          115)
    #(+KEY-POWER+             116)
    #(+KEY-KPEQUAL+           117)
    #(+KEY-KPPLUSMINUS+       118)
    #(+KEY-PAUSE+             119)
    #(+KEY-SCALE+             120)
    #(+KEY-KPCOMMA+           121)
    #(+KEY-HANGEUL+           122)
    #(+KEY-HANGUEL+	      122)	; synonym
    #(+KEY-HANJA+             123)
    #(+KEY-YEN+               124)
    #(+KEY-LEFTMETA+          125)
    #(+KEY-RIGHTMETA+         126)
    #(+KEY-COMPOSE+           127)
    #(+KEY-STOP+              128)
    #(+KEY-AGAIN+             129)
    #(+KEY-PROPS+             130)
    #(+KEY-UNDO+              131)
    #(+KEY-FRONT+             132)
    #(+KEY-COPY+              133)
    #(+KEY-OPEN+              134)
    #(+KEY-PASTE+             135)
    #(+KEY-FIND+              136)
    #(+KEY-CUT+               137)
    #(+KEY-HELP+              138)
    #(+KEY-MENU+              139)
    #(+KEY-CALC+              140)
    #(+KEY-SETUP+             141)
    #(+KEY-SLEEP+             142)
    #(+KEY-WAKEUP+            143)
    #(+KEY-FILE+              144)
    #(+KEY-SENDFILE+          145)
    #(+KEY-DELETEFILE+        146)
    #(+KEY-XFER+              147)
    #(+KEY-PROG1+             148)
    #(+KEY-PROG2+             149)
    #(+KEY-WWW+               150)
    #(+KEY-MSDOS+             151)
    #(+KEY-COFFEE+            152)
    #(+KEY-SCREENLOCK+	      152) ;; synonym KEY-COFFEE
    #(+KEY-ROTATE-DISPLAY+    153)
    #(+KEY-DIRECTION+	      153) ;; synonym KEY-ROTATE-DISPLAY
    #(+KEY-CYCLEWINDOWS+      154)
    #(+KEY-MAIL+              155)
    #(+KEY-BOOKMARKS+         156)
    #(+KEY-COMPUTER+          157)
    #(+KEY-BACK+              158)
    #(+KEY-FORWARD+           159)
    #(+KEY-CLOSECD+           160)
    #(+KEY-EJECTCD+           161)
    #(+KEY-EJECTCLOSECD+      162)
    #(+KEY-NEXTSONG+          163)
    #(+KEY-PLAYPAUSE+         164)
    #(+KEY-PREVIOUSSONG+      165)
    #(+KEY-STOPCD+            166)
    #(+KEY-RECORD+            167)
    #(+KEY-REWIND+            168)
    #(+KEY-PHONE+             169)
    #(+KEY-ISO+               170)
    #(+KEY-CONFIG+            171)
    #(+KEY-HOMEPAGE+          172)
    #(+KEY-REFRESH+           173)
    #(+KEY-EXIT+              174)
    #(+KEY-MOVE+              175)
    #(+KEY-EDIT+              176)
    #(+KEY-SCROLLUP+          177)
    #(+KEY-SCROLLDOWN+        178)
    #(+KEY-KPLEFTPAREN+       179)
    #(+KEY-KPRIGHTPAREN+      180)
    #(+KEY-NEW+               181)
    #(+KEY-REDO+              182)
    #(+KEY-F13+               183)
    #(+KEY-F14+               184)
    #(+KEY-F15+               185)
    #(+KEY-F16+               186)
    #(+KEY-F17+               187)
    #(+KEY-F18+               188)
    #(+KEY-F19+               189)
    #(+KEY-F20+               190)
    #(+KEY-F21+               191)
    #(+KEY-F22+               192)
    #(+KEY-F23+               193)
    #(+KEY-F24+               194)
    #(+KEY-PLAYCD+            200)
    #(+KEY-PAUSECD+           201)
    #(+KEY-PROG3+             202)
    #(+KEY-PROG4+             203)
    #(+KEY-ALL-APPLICATIONS+  204)
    #(+KEY-DASHBOARD+	      204) ;; synonym KEY-ALL-APPLICATIONS
    #(+KEY-SUSPEND+           205)
    #(+KEY-CLOSE+             206)
    #(+KEY-PLAY+              207)
    #(+KEY-FASTFORWARD+       208)
    #(+KEY-BASSBOOST+         209)
    #(+KEY-PRINT+             210)
    #(+KEY-HP+                211)
    #(+KEY-CAMERA+            212)
    #(+KEY-SOUND+             213)
    #(+KEY-QUESTION+          214)
    #(+KEY-EMAIL+             215)
    #(+KEY-CHAT+              216)
    #(+KEY-SEARCH+            217)
    #(+KEY-CONNECT+           218)
    #(+KEY-FINANCE+           219)
    #(+KEY-SPORT+             220)
    #(+KEY-SHOP+              221)
    #(+KEY-ALTERASE+          222)
    #(+KEY-CANCEL+            223)
    #(+KEY-BRIGHTNESSDOWN+    224)
    #(+KEY-BRIGHTNESSUP+      225)
    #(+KEY-MEDIA+             226)
    #(+KEY-SWITCHVIDEOMODE+   227)
    #(+KEY-KBDILLUMTOGGLE+    228)
    #(+KEY-KBDILLUMDOWN+      229)
    #(+KEY-KBDILLUMUP+        230)
    #(+KEY-SEND+              231)
    #(+KEY-REPLY+             232)
    #(+KEY-FORWARDMAIL+       233)
    #(+KEY-SAVE+              234)
    #(+KEY-DOCUMENTS+         235)
    #(+KEY-BATTERY+           236)
    #(+KEY-BLUETOOTH+         237)
    #(+KEY-WLAN+              238)
    #(+KEY-UWB+               239)
    #(+KEY-UNKNOWN+           240)
    #(+KEY-VIDEO-NEXT+        241)
    #(+KEY-VIDEO-PREV+        242)
    #(+KEY-BRIGHTNESS-CYCLE+  243)
    #(+KEY-BRIGHTNESS-AUTO+   244)
    #(+KEY-BRIGHTNESS-ZERO+   244) ;; synonym KEY-BRIGHTNESS-AUTO
    #(+KEY-DISPLAY-OFF+       245)
    #(+KEY-WWAN+              246)
    #(+KEY-WIMAX+	      246) ;; synonum KEY-WWAN
    #(+KEY-RFKILL+            247)
    #(+KEY-MICMUTE+           248)
    #(+KEY-OK+                #x160)
    #(+KEY-SELECT+            #x161)
    #(+KEY-GOTO+              #x162)
    #(+KEY-CLEAR+             #x163)
    #(+KEY-POWER2+            #x164)
    #(+KEY-OPTION+            #x165)
    #(+KEY-INFO+              #x166)
    #(+KEY-TIME+              #x167)
    #(+KEY-VENDOR+            #x168)
    #(+KEY-ARCHIVE+           #x169)
    #(+KEY-PROGRAM+           #x16a)
    #(+KEY-CHANNEL+           #x16b)
    #(+KEY-FAVORITES+         #x16c)
    #(+KEY-EPG+               #x16d)
    #(+KEY-PVR+               #x16e)
    #(+KEY-MHP+               #x16f)
    #(+KEY-LANGUAGE+          #x170)
    #(+KEY-TITLE+             #x171)
    #(+KEY-SUBTITLE+          #x172)
    #(+KEY-ANGLE+             #x173)
    #(+KEY-FULL-SCREEN+       #x174)
    #(+KEY-ZOOM+	      #x174) ;; synonym KEY-FULL-SCREEN
    #(+KEY-MODE+              #x175)
    #(+KEY-KEYBOARD+          #x176)
    #(+KEY-ASPECT-RATIO+      #x177)
    #(+KEY-SCREEN+	      #x177) ;; synonym KEY-ASPECT-RATIO
    #(+KEY-PC+                #x178)
    #(+KEY-TV+                #x179)
    #(+KEY-TV2+               #x17a)
    #(+KEY-VCR+               #x17b)
    #(+KEY-VCR2+              #x17c)
    #(+KEY-SAT+               #x17d)
    #(+KEY-SAT2+              #x17e)
    #(+KEY-CD+                #x17f)
    #(+KEY-TAPE+              #x180)
    #(+KEY-RADIO+             #x181)
    #(+KEY-TUNER+             #x182)
    #(+KEY-PLAYER+            #x183)
    #(+KEY-TEXT+              #x184)
    #(+KEY-DVD+               #x185)
    #(+KEY-AUX+               #x186)
    #(+KEY-MP3+               #x187)
    #(+KEY-AUDIO+             #x188)
    #(+KEY-VIDEO+             #x189)
    #(+KEY-DIRECTORY+         #x18a)
    #(+KEY-LIST+              #x18b)
    #(+KEY-MEMO+              #x18c)
    #(+KEY-CALENDAR+          #x18d)
    #(+KEY-RED+               #x18e)
    #(+KEY-GREEN+             #x18f)
    #(+KEY-YELLOW+            #x190)
    #(+KEY-BLUE+              #x191)
    #(+KEY-CHANNELUP+         #x192)
    #(+KEY-CHANNELDOWN+       #x193)
    #(+KEY-FIRST+             #x194)
    #(+KEY-LAST+              #x195)
    #(+KEY-AB+                #x196)
    #(+KEY-NEXT+              #x197)
    #(+KEY-RESTART+           #x198)
    #(+KEY-SLOW+              #x199)
    #(+KEY-SHUFFLE+           #x19a)
    #(+KEY-BREAK+             #x19b)
    #(+KEY-PREVIOUS+          #x19c)
    #(+KEY-DIGITS+            #x19d)
    #(+KEY-TEEN+              #x19e)
    #(+KEY-TWEN+              #x19f)
    #(+KEY-VIDEOPHONE+        #x1a0)
    #(+KEY-GAMES+             #x1a1)
    #(+KEY-ZOOMIN+            #x1a2)
    #(+KEY-ZOOMOUT+           #x1a3)
    #(+KEY-ZOOMRESET+         #x1a4)
    #(+KEY-WORDPROCESSOR+     #x1a5)
    #(+KEY-EDITOR+            #x1a6)
    #(+KEY-SPREADSHEET+       #x1a7)
    #(+KEY-GRAPHICSEDITOR+    #x1a8)
    #(+KEY-PRESENTATION+      #x1a9)
    #(+KEY-DATABASE+          #x1aa)
    #(+KEY-NEWS+              #x1ab)
    #(+KEY-VOICEMAIL+         #x1ac)
    #(+KEY-ADDRESSBOOK+       #x1ad)
    #(+KEY-MESSENGER+         #x1ae)
    #(+KEY-DISPLAYTOGGLE+     #x1af)
    #(+KEY-BRIGHTNESS-TOGGLE+ #x1af) ;; synonym KEY-DISPLAYTOGGLE
    #(+KEY-SPELLCHECK+        #x1b0)
    #(+KEY-LOGOFF+            #x1b1)
    #(+KEY-DOLLAR+            #x1b2)
    #(+KEY-EURO+              #x1b3)
    #(+KEY-FRAMEBACK+         #x1b4)
    #(+KEY-FRAMEFORWARD+      #x1b5)
    #(+KEY-CONTEXT-MENU+      #x1b6)
    #(+KEY-MEDIA-REPEAT+      #x1b7)
    #(+KEY-10CHANNELSUP+      #x1b8)
    #(+KEY-10CHANNELSDOWN+    #x1b9)
    #(+KEY-IMAGES+            #x1ba)
    #(+KEY-NOTIFICATION-CENTER+	#x1bc)
    #(+KEY-PICKUP-PHONE+      #x1bd)
    #(+KEY-HANGUP-PHONE+      #x1be)
    #(+KEY-DEL-EOL+           #x1c0)
    #(+KEY-DEL-EOS+           #x1c1)
    #(+KEY-INS-LINE+          #x1c2)
    #(+KEY-DEL-LINE+          #x1c3)
    #(+KEY-FN+                #x1d0)
    #(+KEY-FN-ESC+            #x1d1)
    #(+KEY-FN-F1+             #x1d2)
    #(+KEY-FN-F2+             #x1d3)
    #(+KEY-FN-F3+             #x1d4)
    #(+KEY-FN-F4+             #x1d5)
    #(+KEY-FN-F5+             #x1d6)
    #(+KEY-FN-F6+             #x1d7)
    #(+KEY-FN-F7+             #x1d8)
    #(+KEY-FN-F8+             #x1d9)
    #(+KEY-FN-F9+             #x1da)
    #(+KEY-FN-F10+            #x1db)
    #(+KEY-FN-F11+            #x1dc)
    #(+KEY-FN-F12+            #x1dd)
    #(+KEY-FN-1+              #x1de)
    #(+KEY-FN-2+              #x1df)
    #(+KEY-FN-D+              #x1e0)
    #(+KEY-FN-E+              #x1e1)
    #(+KEY-FN-F+              #x1e2)
    #(+KEY-FN-S+              #x1e3)
    #(+KEY-FN-B+              #x1e4)
    #(+KEY-FN-RIGHT-SHIFT+    #x1e5)
    #(+KEY-BRL-DOT1+          #x1f1)
    #(+KEY-BRL-DOT2+          #x1f2)
    #(+KEY-BRL-DOT3+          #x1f3)
    #(+KEY-BRL-DOT4+          #x1f4)
    #(+KEY-BRL-DOT5+          #x1f5)
    #(+KEY-BRL-DOT6+          #x1f6)
    #(+KEY-BRL-DOT7+          #x1f7)
    #(+KEY-BRL-DOT8+          #x1f8)
    #(+KEY-BRL-DOT9+          #x1f9)
    #(+KEY-BRL-DOT10+         #x1fa)
    #(+KEY-NUMERIC-0+         #x200)
    #(+KEY-NUMERIC-1+         #x201)
    #(+KEY-NUMERIC-2+         #x202)
    #(+KEY-NUMERIC-3+         #x203)
    #(+KEY-NUMERIC-4+         #x204)
    #(+KEY-NUMERIC-5+         #x205)
    #(+KEY-NUMERIC-6+         #x206)
    #(+KEY-NUMERIC-7+         #x207)
    #(+KEY-NUMERIC-8+         #x208)
    #(+KEY-NUMERIC-9+         #x209)
    #(+KEY-NUMERIC-STAR+      #x20a)
    #(+KEY-NUMERIC-POUND+     #x20b)
    #(+KEY-NUMERIC-A+         #x20c)
    #(+KEY-NUMERIC-B+         #x20d)
    #(+KEY-NUMERIC-C+         #x20e)
    #(+KEY-NUMERIC-D+         #x20f)
    #(+KEY-CAMERA-FOCUS+      #x210)
    #(+KEY-WPS-BUTTON+        #x211)
    #(+KEY-TOUCHPAD-TOGGLE+   #x212)
    #(+KEY-TOUCHPAD-ON+       #x213)
    #(+KEY-TOUCHPAD-OFF+      #x214)
    #(+KEY-CAMERA-ZOOMIN+     #x215)
    #(+KEY-CAMERA-ZOOMOUT+    #x216)
    #(+KEY-CAMERA-UP+         #x217)
    #(+KEY-CAMERA-DOWN+       #x218)
    #(+KEY-CAMERA-LEFT+       #x219)
    #(+KEY-CAMERA-RIGHT+      #x21a)
    #(+KEY-ATTENDANT-ON+      #x21b)
    #(+KEY-ATTENDANT-OFF+     #x21c)
    #(+KEY-ATTENDANT-TOGGLE+  #x21d)
    #(+KEY-LIGHTS-TOGGLE+     #x21e)
    #(+KEY-ALS-TOGGLE+        #x230)
    #(+KEY-ROTATE-LOCK-TOGGLE+ #x231)
    #(+KEY-BUTTONCONFIG+      #x240)
    #(+KEY-TASKMANAGER+       #x241)
    #(+KEY-JOURNAL+           #x242)
    #(+KEY-CONTROLPANEL+      #x243)
    #(+KEY-APPSELECT+         #x244)
    #(+KEY-SCREENSAVER+       #x245)
    #(+KEY-VOICECOMMAND+      #x246)
    #(+KEY-ASSISTANT+         #x247)
    #(+KEY-KBD-LAYOUT-NEXT+   #x248)
    #(+KEY-EMOJI-PICKER+      #x249)
    #(+KEY-DICTATE+           #x24a)
    #(+KEY-BRIGHTNESS-MIN+              #x250)
    #(+KEY-BRIGHTNESS-MAX+              #x251)
    #(+KEY-KBDINPUTASSIST-PREV+         #x260)
    #(+KEY-KBDINPUTASSIST-NEXT+         #x261)
    #(+KEY-KBDINPUTASSIST-PREVGROUP+    #x262)
    #(+KEY-KBDINPUTASSIST-NEXTGROUP+    #x263)
    #(+KEY-KBDINPUTASSIST-ACCEPT+       #x264)
    #(+KEY-KBDINPUTASSIST-CANCEL+       #x265)
    #(+KEY-RIGHT-UP+                    #x266)
    #(+KEY-RIGHT-DOWN+                  #x267)
    #(+KEY-LEFT-UP+                     #x268)
    #(+KEY-LEFT-DOWN+                   #x269)
    #(+KEY-ROOT-MENU+                   #x26a)
    #(+KEY-MEDIA-TOP-MENU+              #x26b)
    #(+KEY-NUMERIC-11+                  #x26c)
    #(+KEY-NUMERIC-12+                  #x26d)
    #(+KEY-AUDIO-DESC+                  #x26e)
    #(+KEY-3D-MODE+                     #x26f)
    #(+KEY-NEXT-FAVORITE+               #x270)
    #(+KEY-STOP-RECORD+                 #x271)
    #(+KEY-PAUSE-RECORD+                #x272)
    #(+KEY-VOD+                         #x273)
    #(+KEY-UNMUTE+                      #x274)
    #(+KEY-FASTREVERSE+                 #x275)
    #(+KEY-SLOWREVERSE+                 #x276)
    #(+KEY-DATA+                        #x277)
    #(+KEY-ONSCREEN-KEYBOARD+           #x278)
    #(+KEY-PRIVACY-SCREEN-TOGGLE+       #x279)
    #(+KEY-SELECTIVE-SCREENSHOT+        #x27a)
    #(+KEY-MACRO1+                      #x290)
    #(+KEY-MACRO2+                      #x291)
    #(+KEY-MACRO3+                      #x292)
    #(+KEY-MACRO4+                      #x293)
    #(+KEY-MACRO5+                      #x294)
    #(+KEY-MACRO6+                      #x295)
    #(+KEY-MACRO7+                      #x296)
    #(+KEY-MACRO8+                      #x297)
    #(+KEY-MACRO9+                      #x298)
    #(+KEY-MACRO10+                     #x299)
    #(+KEY-MACRO11+                     #x29a)
    #(+KEY-MACRO12+                     #x29b)
    #(+KEY-MACRO13+                     #x29c)
    #(+KEY-MACRO14+                     #x29d)
    #(+KEY-MACRO15+                     #x29e)
    #(+KEY-MACRO16+                     #x29f)
    #(+KEY-MACRO17+                     #x2a0)
    #(+KEY-MACRO18+                     #x2a1)
    #(+KEY-MACRO19+                     #x2a2)
    #(+KEY-MACRO20+                     #x2a3)
    #(+KEY-MACRO21+                     #x2a4)
    #(+KEY-MACRO22+                     #x2a5)
    #(+KEY-MACRO23+                     #x2a6)
    #(+KEY-MACRO24+                     #x2a7)
    #(+KEY-MACRO25+                     #x2a8)
    #(+KEY-MACRO26+                     #x2a9)
    #(+KEY-MACRO27+                     #x2aa)
    #(+KEY-MACRO28+                     #x2ab)
    #(+KEY-MACRO29+                     #x2ac)
    #(+KEY-MACRO30+                     #x2ad)
    #(+KEY-MACRO-RECORD-START+          #x2b0)
    #(+KEY-MACRO-RECORD-STOP+           #x2b1)
    #(+KEY-MACRO-PRESET-CYCLE+          #x2b2)
    #(+KEY-MACRO-PRESET1+               #x2b3)
    #(+KEY-MACRO-PRESET2+               #x2b4)
    #(+KEY-MACRO-PRESET3+               #x2b5)
    #(+KEY-KBD-LCD-MENU1+               #x2b8)
    #(+KEY-KBD-LCD-MENU2+               #x2b9)
    #(+KEY-KBD-LCD-MENU3+               #x2ba)
    #(+KEY-KBD-LCD-MENU4+               #x2bb)
    #(+KEY-KBD-LCD-MENU5+               #x2bc)
    ;; #(+KEY-MIN-INTERESTING+	        +KEY-MUTE+)
    #(+KEY-MAX+			        #x2ff)
    #(+KEY-CNT+			        (1+ +KEY-MAX+))))

(defun keycode-name-from-symbol (s)
  "Return a name for the keycode given the keycode constant symbol ‘s’."
  (let ((n (symbol-name s)))
    (subseq n 5 (1- (length n)))))

(defparameter *keycode-name-table*
  (let ((table (make-hash-table :test #'equalp)))
    (loop :for k :in *keycodes* :do
      (setf (gethash (keycode-name-from-symbol k) table) (symbol-value k)))
    table)
  "Table of integer keycode values to keycode names.")

(defparameter *keycode-table*
  (let ((table (make-hash-table)))
    (loop :for k :in *keycodes* :do
      (setf (gethash (symbol-value k) table) (keycode-name-from-symbol k)))
    table)
  "Table of keycode key names to keycode values.")

(defun find-keycode (name)
  "Return the keycode number of the ‘name’."
  (gethash name *keycode-name-table*))

(defun keycode-name (keycode)
  "Return the key name for the ‘keycode’ number."
  (gethash keycode *keycode-table*))

(defstruct key
  type
  number
  name)

(defparameter *key-table* (make-hash-table))
(defparameter *key-name-table* (make-hash-table :test #'equal))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun define-keys (type array &key (start 0))
    (loop :with key
	  :for k :across array
	  :for i = start :then (1+ i)
	  :do
	  (setf key (make-key :type type :number i :name k)
		(gethash (keycode-number type i) *key-table*) key
		(gethash k *key-name-table*) key))))

(define-keys +KT-LATIN+ 
    #("nul"
      "Control_a"
      "Control_b"
      "Control_c"
      "Control_d"
      "Control_e"
      "Control_f"
      "Control_g"
      "BackSpace"
      "Tab"
      "Linefeed"
      "Control_k"
      "Control_l"
      "Control_m"
      "Control_n"
      "Control_o"
      "Control_p"
      "Control_q"
      "Control_r"
      "Control_s"
      "Control_t"
      "Control_u"
      "Control_v"
      "Control_w"
      "Control_x"
      "Control_y"
      "Control_z"
      "Escape"
      "Control_backslash"
      "Control_bracketright"
      "Control_asciicircum"
      "Control_underscore"
      "space"
      "exclam"
      "quotedbl"
      "numbersign"
      "dollar"
      "percent"
      "ampersand"
      "apostrophe"
      "parenleft"
      "parenright"
      "asterisk"
      "plus"
      "comma"
      "minus"
      "period"
      "slash"
      "zero"
      "one"
      "two"
      "three"
      "four"
      "five"
      "six"
      "seven"
      "eight"
      "nine"
      "colon"
      "semicolon"
      "less"
      "equal"
      "greater"
      "question"
      "at"
      "A"
      "B"
      "C"
      "D"
      "E"
      "F"
      "G"
      "H"
      "I"
      "J"
      "K"
      "L"
      "M"
      "N"
      "O"
      "P"
      "Q"
      "R"
      "S"
      "T"
      "U"
      "V"
      "W"
      "X"
      "Y"
      "Z"
      "bracketleft"
      "backslash"
      "bracketright"
      "asciicircum"
      "underscore"
      "grave"
      "a"
      "b"
      "c"
      "d"
      "e"
      "f"
      "g"
      "h"
      "i"
      "j"
      "k"
      "l"
      "m"
      "n"
      "o"
      "p"
      "q"
      "r"
      "s"
      "t"
      "u"
      "v"
      "w"
      "x"
      "y"
      "z"
      "braceleft"
      "bar"
      "braceright"
      "asciitilde"
      "Delete"
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      ""
      "nobreakspace"
      "exclamdown"
      "cent"
      "sterling"
      "currency"
      "yen"
      "brokenbar"
      "section"
      "diaeresis"
      "copyright"
      "ordfeminine"
      "guillemotleft"
      "notsign"
      "hyphen"
      "registered"
      "macron"
      "degree"
      "plusminus"
      "twosuperior"
      "threesuperior"
      "acute"
      "mu"
      "paragraph"
      "periodcentered"
      "cedilla"
      "onesuperior"
      "masculine"
      "guillemotright"
      "onequarter"
      "onehalf"
      "threequarters"
      "questiondown"
      "Agrave"
      "Aacute"
      "Acircumflex"
      "Atilde"
      "Adiaeresis"
      "Aring"
      "AE"
      "Ccedilla"
      "Egrave"
      "Eacute"
      "Ecircumflex"
      "Ediaeresis"
      "Igrave"
      "Iacute"
      "Icircumflex"
      "Idiaeresis"
      "ETH"
      "Ntilde"
      "Ograve"
      "Oacute"
      "Ocircumflex"
      "Otilde"
      "Odiaeresis"
      "multiply"
      "Ooblique"
      "Ugrave"
      "Uacute"
      "Ucircumflex"
      "Udiaeresis"
      "Yacute"
      "THORN"
      "ssharp"
      "agrave"
      "aacute"
      "acircumflex"
      "atilde"
      "adiaeresis"
      "aring"
      "ae"
      "ccedilla"
      "egrave"
      "eacute"
      "ecircumflex"
      "ediaeresis"
      "igrave"
      "iacute"
      "icircumflex"
      "idiaeresis"
      "eth"
      "ntilde"
      "ograve"
      "oacute"
      "ocircumflex"
      "otilde"
      "odiaeresis"
      "division"
      "oslash"
      "ugrave"
      "uacute"
      "ucircumflex"
      "udiaeresis"
      "yacute"
      "thorn"
      "ydiaeresis"))

(define-keys +KT-FN+
    #("F1" "F2" "F3" "F4" "F5"
      "F6" "F7" "F8" "F9" "F10"
      "F11" "F12" "F13" "F14" "F15"
      "F16" "F17" "F18" "F19" "F20"
      "Find"
      "Insert"
      "Remove"
      "Select"
      "Prior"
      "Next"
      "Macro"
      "Help"
      "Do"
      "Pause"
      "F21" "F22" "F23" "F24" "F25"
      "F26" "F27" "F28" "F29" "F30"
      "F31" "F32" "F33" "F34" "F35"
      "F36" "F37" "F38" "F39" "F40"
      "F41" "F42" "F43" "F44" "F45"
      "F46" "F47" "F48" "F49" "F50"
      "F51" "F52" "F53" "F54" "F55"
      "F56" "F57" "F58" "F59" "F60"
      "F61" "F62" "F63" "F64" "F65"
      "F66" "F67" "F68" "F69" "F70"
      "F71" "F72" "F73" "F74" "F75"
      "F76" "F77" "F78" "F79" "F80"
      "F81" "F82" "F83" "F84" "F85"
      "F86" "F87" "F88" "F89" "F90"
      "F91" "F92" "F93" "F94" "F95"
      "F96" "F97" "F98" "F99" "F100"
      "F101" "F102" "F103" "F104" "F105"
      "F106" "F107" "F108" "F109" "F110"
      "F111" "F112" "F113" "F114" "F115"
      "F116" "F117" "F118" "F119" "F120"
      "F121" "F122" "F123" "F124" "F125"
      "F126" "F127" "F128" "F129" "F130"
      "F131" "F132" "F133" "F134" "F135"
      "F136" "F137" "F138" "F139" "F140"
      "F141" "F142" "F143" "F144" "F145"
      "F146" "F147" "F148" "F149" "F150"
      "F151" "F152" "F153" "F154" "F155"
      "F156" "F157" "F158" "F159" "F160"
      "F161" "F162" "F163" "F164" "F165"
      "F166" "F167" "F168" "F169" "F170"
      "F171" "F172" "F173" "F174" "F175"
      "F176" "F177" "F178" "F179" "F180"
      "F181" "F182" "F183" "F184" "F185"
      "F186" "F187" "F188" "F189" "F190"
      "F191" "F192" "F193" "F194" "F195"
      "F196" "F197" "F198" "F199" "F200"
      "F201" "F202" "F203" "F204" "F205"
      "F206" "F207" "F208" "F209" "F210"
      "F211" "F212" "F213" "F214" "F215"
      "F216" "F217" "F218" "F219" "F220"
      "F221" "F222" "F223" "F224" "F225"
      "F226" "F227" "F228" "F229" "F230"
      "F231" "F232" "F233" "F234" "F235"
      "F236" "F237" "F238" "F239" "F240"
      "F241" "F242" "F243" "F244" "F245"
      "F246"))

(define-keys +KT-SPEC+
    #("VoidSymbol"
      "Return"
      "Show_Registers"
      "Show_Memory"
      "Show_State"
      "Break"
      "Last_Console"
      "Caps_Lock"
      "Num_Lock"
      "Scroll_Lock"
      "Scroll_Forward"
      "Scroll_Backward"
      "Boot"
      "Caps_On"
      "Compose"
      "SAK"
      "Decr_Console"
      "Incr_Console"
      "KeyboardSignal"
      "Bare_Num_Lock"))

(define-keys +KT-PAD+
    #("KP_0"
      "KP_1"
      "KP_2"
      "KP_3"
      "KP_4"
      "KP_5"
      "KP_6"
      "KP_7"
      "KP_8"
      "KP_9"
      "KP_Add"
      "KP_Subtract"
      "KP_Multiply"
      "KP_Divide"
      "KP_Enter"
      "KP_Comma"
      "KP_Period"
      "KP_MinPlus"))

(define-keys +KT-DEAD+
    #("dead_grave"
      "dead_acute"
      "dead_circumflex"
      "dead_tilde"
      "dead_diaeresis"
      "dead_cedilla"
      "dead_macron"
      "dead_kbreve"
      "dead_abovedot"
      "dead_abovering"
      "dead_kdoubleacute"
      "dead_kcaron"
      "dead_kogonek"
      "dead_iota"
      "dead_voiced_sound"
      "dead_semivoiced_sound"
      "dead_belowdot"
      "dead_hook"
      "dead_horn"
      "dead_stroke"
      "dead_abovecomma"
      "dead_abovereversedcomma"
      "dead_doublegrave"
      "dead_invertedbreve"
      "dead_belowcomma"
      "dead_currency"
      "dead_greek"))

(define-keys +KT-CONS+
    #("Console_1"
      "Console_2"
      "Console_3"
      "Console_4"
      "Console_5"
      "Console_6"
      "Console_7"
      "Console_8"
      "Console_9"
      "Console_10"
      "Console_11"
      "Console_12"
      "Console_13"
      "Console_14"
      "Console_15"
      "Console_16"
      "Console_17"
      "Console_18"
      "Console_19"
      "Console_20"
      "Console_21"
      "Console_22"
      "Console_23"
      "Console_24"
      "Console_25"
      "Console_26"
      "Console_27"
      "Console_28"
      "Console_29"
      "Console_30"
      "Console_31"
      "Console_32"
      "Console_33"
      "Console_34"
      "Console_35"
      "Console_36"
      "Console_37"
      "Console_38"
      "Console_39"
      "Console_40"
      "Console_41"
      "Console_42"
      "Console_43"
      "Console_44"
      "Console_45"
      "Console_46"
      "Console_47"
      "Console_48"
      "Console_49"
      "Console_50"
      "Console_51"
      "Console_52"
      "Console_53"
      "Console_54"
      "Console_55"
      "Console_56"
      "Console_57"
      "Console_58"
      "Console_59"
      "Console_60"
      "Console_61"
      "Console_62"
      "Console_63"))

(define-keys +KT-CUR+
    #("Down"
      "Left"
      "Right"
      "Up"))

(define-keys +KT-SHIFT+
    #("Shift"
      "AltGr"
      "Control"
      "Alt"
      "ShiftL"
      "ShiftR"
      "CtrlL"
      "CtrlR"
      "CapsShift"))

(define-keys +KT-ASCII+
    #("Ascii_0"
      "Ascii_1"
      "Ascii_2"
      "Ascii_3"
      "Ascii_4"
      "Ascii_5"
      "Ascii_6"
      "Ascii_7"
      "Ascii_8"
      "Ascii_9"
      "Hex_0"
      "Hex_1"
      "Hex_2"
      "Hex_3"
      "Hex_4"
      "Hex_5"
      "Hex_6"
      "Hex_7"
      "Hex_8"
      "Hex_9"
      "Hex_A"
      "Hex_B"
      "Hex_C"
      "Hex_D"
      "Hex_E"
      "Hex_F"))

(define-keys +KT-LOCK+
    #("Shift_Lock"
      "AltGr_Lock"
      "Control_Lock"
      "Alt_Lock"
      "ShiftL_Lock"
      "ShiftR_Lock"
      "CtrlL_Lock"
      "CtrlR_Lock"
      "CapsShift_Lock"))

(define-keys +KT-SLOCK+
    #("SShift"
      "SAltGr"
      "SControl"
      "SAlt"
      "SShiftL"
      "SShiftR"
      "SCtrlL"
      "SCtrlR"
      "SCapsShift"))

(define-keys +KT-BRL+
    #("Brl_blank"
      "Brl_dot1"
      "Brl_dot2"
      "Brl_dot3"
      "Brl_dot4"
      "Brl_dot5"
      "Brl_dot6"
      "Brl_dot7"
      "Brl_dot8"
      "Brl_dot9"
      "Brl_dot10"))

(defun key-action-name (keycode)
  "Return a key action name for a key action number ‘keycode’."
  (gethash keycode *key-table*))

(defun key-action-number (name)
  "Return a key action number for a key action name string ‘name’."
  (gethash name *key-name-table*))

(defun find-key-action (thing)
  "Return the key action for ‘thing’, which is a integer key code or string
key name."
  (typecase thing
    (integer (key-action-name thing))
    (string
      (let ((key (key-action-number thing)))
	(when key
	  (keycode-number (key-type key) (key-number key)))))))

(defcstruct kbkeycode
  "Scan code to key code translation."
  (scancode :unsigned-int)
  (keycode :unsigned-int))

(defconstant +KDGETKEYCODE+ #x4b4c "Get the keycode table entry.")
(defconstant +KDSETKEYCODE+ #x4b4d "Set the keycode table entry.")

#| @@@ This wasn't really the same thing for integer and string
(defun key-code (thing &optional fd)
  "Return the key code for ‘scan-code’."
  (typecase thing
    (integer
     (with-console (fd)
       (with-foreign-object (kk '(:struct kbkeycode))
	 (with-foreign-slots ((scancode keycode) kk (:struct kbkeycode))
	   (setf scancode thing
		 keycode 0)
	   (syscall (posix-ioctl fd +KDGETKEYCODE+ kk))
	   keycode))))
    (string
     (let ((k (gethash thing *key-name-table*)))
       (keycode-number (key-type k) (key-number k))))))
|#

(defun key-code (thing &optional fd)
  "Return the key code for ‘scan-code’."
  (with-console (fd)
    (with-foreign-object (kk '(:struct kbkeycode))
      (with-foreign-slots ((scancode keycode) kk (:struct kbkeycode))
	(setf scancode thing
	      keycode 0)
	(syscall (posix-ioctl fd +KDGETKEYCODE+ kk))
	keycode))))

(defun key-codes ()
  "Return a list of key codes for scan codes."
  (loop :for i :from 1 :below +NR-KEYS+
	:collect (vector i (key-code i))))

(defun set-key-code (scan-code fd key-code)
  (with-console (fd)
    (with-foreign-object (kk '(:struct kbkeycode))
      (with-foreign-slots ((scancode keycode) kk (:struct kbkeycode))
	(setf scancode scan-code
	      keycode key-code)
	(syscall (posix-ioctl fd +KDSETKEYCODE+ kk))
	keycode))))

(defsetf key-code set-key-code
  "Set the key code for the scan code for ‘fd’.")

(defparameter *default-timeout* 5
  "How long to wait before stopping in show-keys.")

(defparameter *timeout* *default-timeout*
  "How long to wait before stopping in show-keys.")

(define-constant +crlf+ #.(s+ #\return #\linefeed))

(defun print-scan-codes (fd)
  (format t "Showing scan codes of keys pressed.~a~
             Wait ~a seconds to stop.~a" +crlf+ *timeout* +crlf+)
  (finish-output)
  (with-foreign-object (key :char)
    (unwind-protect
	 (progn
	   (setf (keyboard-mode fd) +K-MEDIUMRAW+)
	   (loop :while (listen-for *timeout* fd)
		 :do
		 (syscall (posix-read fd key 1))
		 (format t "#x~2,'0x ~d~a"
			 (mem-ref key :char) (mem-ref key :char) +crlf+)
		 (finish-output)))
      (setf (keyboard-mode fd) +K-UNICODE+))
    (format t "~&Done.~a" +crlf+)))

(defun print-key-codes (fd)
  (format t "Showing key codes of keys pressed.~a~
             Wait ~a seconds to stop.~a" +crlf+ *timeout* +crlf+)
  (finish-output)
  (with-foreign-object (buf :unsigned-char 3)
    (unwind-protect
	 (let ((n 0) (k0 0) (k1 0) (k2 0) (code 0))
	   (setf (keyboard-mode fd) +K-RAW+)
	   (labels ((is-press (k) (zerop (logand k #x80)))
		    (is-blank (k) (zerop (logand k #x7f)))
		    (print-code (key code)
		      (format t "~3d ~a ~a~a" code
			      (keycode-name code)
			      (if (is-press key) "press" "release")
			      +crlf+)
		      (finish-output)))
	     (loop
	       :while (listen-for *timeout* fd)
	       :do 
		  (syscall (setf n (posix-read fd buf 3)))
		  (setf k0 (mem-aref buf :unsigned-char 0))
	          (format t "n = ~s ~s ~s ~s~a" n
			  (when (>= n 1) (mem-aref buf :unsigned-char 0))
			  (when (>= n 2) (mem-aref buf :unsigned-char 1))
			  (when (>= n 3) (mem-aref buf :unsigned-char 2))
			  +crlf+)
		  (cond
		    ((and (= n 3)
			  (is-blank k0)
			  (not (is-press
				(setf k1 (mem-aref buf :unsigned-char 1))))
			  (not (is-press
				(setf k2 (mem-aref buf :unsigned-char 2)))))
		     (setf code (logior (ash (logand k1 #x7f) 7)
					     (logand k2 #x7f)))
		     (print-code k0 code))
		    ((= n 2)
		     (print-code k0 (logand k0 #x7f))
		     (setf k1 (mem-aref buf :unsigned-char 1))
		     (print-code k1 (logand k1 #x7f)))
		    ((= n 1)
		     (print-code k0 (logand k0 #x7f)))
		    (t
		     (format t "Weird n = ~s k = ~s~a" n k0 +crlf+))))))
      (setf (keyboard-mode fd) +K-UNICODE+)))
  (format t "~&Done.~a" +crlf+))

(defun reset-keyboard ()
  "Reset the console keyboard to unicode mode."
  (let (fd)
    (ignore-errors
     (with-console (fd)
       (setf (keyboard-mode fd) :unicode)))))

(defmacro with-debugger-reset ((func) &body body)
  "Evaluate ‘body’ calling ‘func’ before entering the debugger."
  `(unwind-protect
	(progn
	  (when (find-package :deblarg)
	    (dlib-misc:add-hook (refer-to :deblarg :*deblargger-entry-hook*)
				,func))
	  ,@body)
     (when (find-package :deblarg)
       (dlib-misc:remove-hook (refer-to :deblarg :*deblargger-entry-hook*)
			      ,func))))

(defun show-keys (&key fd scan-codes normal (timeout *default-timeout*))
  "Show keys as they are pressed."
  (with-console (fd)
    (let ((*timeout* timeout))
      (with-debugger-reset (#'reset-keyboard)
	(cond
	  (normal
	   (uos:test-input))
	  (scan-codes
	   (uos:call-with-raw fd #'print-scan-codes :very-raw t))
	  (t
	   (uos:call-with-raw fd #'print-key-codes :very-raw t)))))))

(defconstant +KDGKBENT+	#x4b46 "Get an entry from the translation table.")
(defconstant +KDSKBENT+	#x4b47 "Set an entry in translation table.")

(defcstruct kbentry
  "Key mapping table entry."
  (kb_table :unsigned-char)
  (kb_index :unsigned-char)
  (kb_value :unsigned-short))

(defun get-key (table index &key fd)
  (with-console (fd)
    (with-foreign-object (ke '(:struct kbentry))
      (with-foreign-slots ((kb_table kb_index kb_value) ke (:struct kbentry))
	(setf kb_table table
	      kb_index index)
	(syscall (posix-ioctl fd +KDGKBENT+ ke))
	(values kb_table kb_index kb_value)))))

(defun get-keycode (key)
  (etypecase key
    (string (find-keycode key))
    (integer key)))

#|
keycode 58 = CtrlL_Lock
keycode 29 = Control

ioctl(fd, KDSKBENT, {kb_table=K_NORMTAB, kb_index=29, kb_value=0xa06 /* K_CTRLLLOCK */}) = -1 EPERM (operation not permitted)

(set-key "LEFTCTRL" '() "CtrlL_Lock")
(set-key "CAPSLOCK" '() "Control"))

|#

(defun set-key (key table value &key fd)
  (with-console (fd)
    (with-foreign-object (ke '(:struct kbentry))
      (let ((key-code
	      (if (integerp key) key (get-keycode key)))
	    (table-code
	      (if (integerp table) table (modifier-table-number table)))
	    (value-code
	      (if (integerp value) value (find-key-action value))))
	(with-foreign-slots ((kb_table kb_index kb_value) ke (:struct kbentry))
	  (setf kb_table table-code
		kb_index key-code
		kb_value value-code)
	  (syscall (posix-ioctl fd +KDSKBENT+ ke)))))))

(defun fuck-lock (&key fd)
  "Make CapsLock be Control like it always should be."
  (with-console (fd)
    (loop :for tab :from 0 :to 127 :do
      (set-key "CAPSLOCK" tab "Control")
      (set-key "LEFTCTRL" tab "CtrlL_Lock"))))

(defun show-key (key &key fd)
  "Output one key like the dumpkeys command."
  (with-console (fd)
    (loop
      :with action
      :for i :from 0 :below +NR-KEYMAPS+ :do
        (multiple-value-bind (type val code) (get-key i key :fd fd)
	  (declare (ignore type))
	  (setf action (key-action-name code))
	  (when (and (/= code +K-HOLE+) action)
	    (if (zerop i)
		(format t "keycode ~s = ~s~%" val (key-name action))
		(progn
		    (format t "~8t")
		    (print-modifiers i)
		    (format t " keycode ~a = ~a~%" val (key-name action)))))))))
	    
(defun fake-dump-keys (&optional fd)
  "Output something like the dumpkeys command."
  (with-console (fd)
    (loop :for key :from 1 :below +NR-KEYS+
	  :do (show-key key :fd fd))))

(defconstant +KDGKBSENT+ #x4B48	"Get a function key string entry.")
(defconstant +KDSKBSENT+ #x4B49	"Set a function key string entry.")

(defcstruct ksbentry
  "Keyboard strings"
  (kb_func :unsigned-char)
  (kb_string :unsigned-char :count 512))

(defun get-key-string (key &key fd)
  (with-console (fd)
    (with-foreign-object (ks '(:struct ksbentry))
      (let ((key-func key #|(find-key key)|#))
	(with-foreign-slots ((kb_func kb_string) ks (:struct ksbentry))
	  (setf kb_func key-func #|(key-number key) |#)
	  (syscall (posix-ioctl fd +KDGKBSENT+ ks))
	  (values kb_func (foreign-string-to-lisp kb_string)))))))

(defun describe-modifiers (&key fd)
  (with-console (fd)
    (loop :for m :in *modifiers* :do
      (format t "~15a ~a~%" (modifier-name m) (ash 1 (modifier-bit m))))))

#|
(defun describe-key-tables ()
  (let ((table
	  (make-table-from
	   `(("Keycode range"                        ,+NR-KEYS+)
	     ("Number of keymaps"                    ,+NR-KEYMAPS+)
	     ("Keymaps in use"	                      ,keymaps)
	     ("Allocated keymaps"                    ,keymaps-allocated)
	     ("Action code range"                    ,action-code-range)
	     ("Number of function keys"              ,+NR-FUNC+)
	     ("Number of compose definitions"        ,+MAX-DIACR+)
	     ("Number of compose definitions in use" ,compose-used))
	   :column-names '("Description" "Value"))))
    (table-print:print-table table :print-titles nil)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colors

(defvar *param-dir*
  "/sys/module/vt/parameters/"
  "Idiotic everthing is a file bullshit.")

(defun get-param (param-string)
  "Get the parameter in a stupid way."
  (parse-integer (slurp (s+ *param-dir* param-string))))

(defun default-color-index ()
  "Return the default color index."
  (get-param "color"))

(defun italic-color-index ()
  "Return the color index to use for fake italic."
  (get-param "italic"))

(defun underline-color-index ()
  "Return the color index to use for fake underline."
  (get-param "underline"))

;; @@@ What is it?
(defun cur-default-color-index ()
  "Return the color index to use for something."
  (get-param "cur_default"))

(defun global-cursor-default-color-index ()
  "Return the color index to use for the global cursor."
  (get-param "global_cursor_default"))

(defun get-colors ()
  "Return a list of dcolors for the console colors."
  (flet ((read-component (c)
           (mapcar #'parse-integer
		   (split-sequence
		    "," (slurp (s+ "/sys/module/vt/parameters/default_" c))))))
    (let* ((reds   (read-component "red"))
           (greens (read-component "grn"))
           (blues  (read-component "blu"))
           (colors
	     (loop :for r :in reds
                   :for g :in greens
                   :for b :in blues
	       :collect (dcolor:make-color :rgb8 :red r :green g :blue b))))
      colors)))

(defconstant +GIO-CMAP+ #x4b70 "Set the colormap.")
(defconstant +PIO-CMAP+ #x4b71 "Get the colormap.")

(defun set-colors (colors &optional fd)
  "Set the terminal colors from ‘colors’, which should should be an array
of sixteen dcolors. The first 8 are the normal colors. The second 8 are the bold
colors."
  (with-console (fd)
    (when (or (not (or (listp colors) (vectorp colors)))
	      (/= (length colors) 16)
	      (every #'dcolor:known-color-p colors))
      (error "Must be given a sequence of 16 colors."))
    (with-foreign-object (colormap :unsigned-char (* 3 16))
      (loop :with c
        :for i :from 0 :below 16 :do
          (setf c (convert-color-to (elt colors i) :rgb8)
		(mem-aref colormap (+ (* 3 i) 0)) (color-component :red   c)
		(mem-aref colormap (+ (* 3 i) 1)) (color-component :green c)
		(mem-aref colormap (+ (* 3 i) 2)) (color-component :blue  c)))
      (syscall (uos:posix-ioctl fd +PIO-CMAP+ colormap)))))

(defun show-default-colors (&key (format :short))
  "Show the console colors. Ironically, this probably doesn't work on the
console."
  (let* ((is-console (console-p))
	 (fake-colors `(:black :red :green :yellow :blue :magenta :cyan :white))
	 (colors (get-colors)))
    (flet ((show-short ()
	     (loop
	       :for c :in (if is-console fake-colors colors)
	       :for i = 0 :then (1+ i)
	       :do
		  (tt-color nil c)
		  (tt-write-string "  ")
		  (tt-color nil :default)
		  (when (= i 7)
		    (tt-newline))))
	   (show-long ()
	     (if is-console
		 (progn
		   (loop
		     :for fc :in fake-colors
		     :for c :in colors
		     :do
			(tt-color nil fc)
			(tt-write-string "  ")
			(tt-color nil :default)
			(tt-format "  ~2,'0x~2,'0x~2,'0x~%"
				   (color-component c :red)
				   (color-component c :green)
				   (color-component c :blue)))
		   (tt-bold t)
		   (loop
		     :for fc :in fake-colors
		     :for c :in (nthcdr 8 colors)
		     :do
			(tt-color nil fc)
			(tt-write-string "  ")
			(tt-color nil :default)
			(tt-format "  ~2,'0x~2,'0x~2,'0x~%"
				   (color-component c :red)
				   (color-component c :green)
				   (color-component c :blue)))
		   (tt-bold nil))
		 (loop
		   :for c :in colors
		   :do
		      (tt-color nil c)
		      (tt-write-string "  ")
		      (tt-color nil :default)
		      (tt-format "  ~2,'0x~2,'0x~2,'0x~%"
				 (color-component c :red)
				 (color-component c :green)
				 (color-component c :blue))))))
      (tt-newline)
      (case format
	(:short
	 (show-short)
	 (when is-console
	   (tt-bold t)
	   (show-short)
	   (tt-bold nil))
	 (tt-color :default :default)
	 (tt-newline))
	(:long
	 (show-long)
	 (tt-bold nil)
	 (tt-finish-output))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts

(defconstant +GIO-FONT+ #x4b60 "Get the font in expanded form.")
(defconstant +PIO-FONT+ #x4b61 "Set the font in expanded form.")

(defconstant +GIO-FONTX+ #x4b6b "Get the font using a struct.")
(defconstant +PIO-FONTX+ #x4b6c "Set the font using a struct.")

(defconstant +PIO-FONTRESET+ #x4b6d "Reset to the default font.")

(defcstruct consolefontdesc
  "Foreign console font"
  (charcount :unsigned-short)
  (charheight :unsigned-short)
  (chardata (:pointer :char)))

(defstruct font
  "Console font"
  count
  height
  width					; might be nil for the old interface
  data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Older interface

(defun old-convert-font-from-foreign (font)
  "Return a font structure from the given C consolefontdesc struct."
  (with-foreign-slots ((charcount charheight chardata) font
		       (:struct consolefontdesc))
    (make-font
     :count charcount :height charheight
     :data
     (let ((data (make-array charcount)))
       (loop :for i :from 0 :below charcount
         :do (setf (aref data i) (mem-aref chardata :char i)))))))

(defun old-convert-font-to-foreign (font foreign-font)
  "Fill in the C consolefontdesc struct ‘foreign-font’ from the given font
structure ‘font’."
  (with-foreign-slots ((charcount charheight chardata) foreign-font
		       (:struct consolefontdesc))
    (setf charcount (font-count font)
	  charheight (font-height font)
	  chardata (foreign-alloc :char :count (* (font-count font) 32)))
    (loop :for i :from 0 :below (font-count font)
      :do (setf (mem-aref chardata :char i) (aref (font-data font) i)))
    foreign-font))

(defun old-get-font (&optional fd)
  "Return the current console font as a structure."
  (with-console (fd)
    (with-foreign-object (font '(:struct consolefontdesc))
      (setf (foreign-slot-value font '(:struct consolefontdesc)
				'chardata) (null-pointer))
      (unwind-protect
	   (progn
	     (setf (foreign-slot-value font '(:struct consolefontdesc) 'chardata)
		   (foreign-alloc :char :count 16384))
	     (syscall (posix-ioctl fd +GIO-FONTX+ font))
	     (old-convert-font-from-foreign font))
	(when (not (equal (foreign-slot-value font
					      '(:struct consolefontdesc)
					      'chardata)
			  (null-pointer)))
	  (foreign-free (foreign-slot-value
			 font '(:struct consolefontdesc)
			 'chardata)))))))

(defun old-set-font (font &key fd)
  "Set the current console font from the font structure ‘font’."
  (with-console (fd)
    (with-foreign-object (foreign-font '(:struct consolefontdesc))
      (setf (foreign-slot-value
		       foreign-font '(:struct consolefontdesc)
		       'chardata) (null-pointer))
      (unwind-protect
	   (progn
	     (old-convert-font-to-foreign font foreign-font)
	     (syscall (posix-ioctl fd +GIO-FONTX+ foreign-font)))
	(when (not (equal (foreign-slot-value foreign-font
					      '(:struct consolefontdesc)
					      'chardata)
			  (null-pointer)))
	  (foreign-free (foreign-slot-value
			 foreign-font '(:struct consolefontdesc)
			 'chardata)))))))

(defun old-font-reset (&optional fd)
  "Reset the console to the default font."
  (with-console (fd)
    (syscall (posix-ioctl fd +PIO-FONTRESET+ 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Newer font interface

(defconstant +KFONTOP+ #x4b72 "Kernel font operation.")

(defconstant +FONT-OP-SET+         0)
(defconstant +FONT-OP-GET+         1)
(defconstant +FONT-OP-SET-DEFAULT+ 2)
(defconstant +FONT-OP-COPY+        3)

(defcstruct console-font-op
  "Console font operation."
  (op        :unsigned-int)
  (flags     :unsigned-int)
  (width     :unsigned-int)
  (height    :unsigned-int)
  (charcount :unsigned-int)
  (data      (:pointer :unsigned-char)))

(defun convert-font-from-foreign (font)
  "Return a font structure from the given C console-font-op struct."
  (with-foreign-slots ((width height charcount data) font
		       (:struct console-font-op))
    (make-font
     :count charcount :width width :height height
     :data
     (let ((new-data (make-array charcount)))
       (loop :for i :from 0 :below charcount
         :do (setf (aref new-data i) (mem-aref data :char i)))))))

(defun convert-font-to-foreign (font foreign-font)
  "Fill in the C console-font-op struct ‘foreign-font’ from the given font
structure ‘font’."
  (with-foreign-slots ((charcount width height data) foreign-font
		       (:struct console-font-op))
    (setf charcount (font-count font)
	  width (font-width font)
	  height (font-height font)
	  data (foreign-alloc :char :count (* (font-count font) 32)))
    (loop :for i :from 0 :below (font-count font)
      :do (setf (mem-aref data :char i) (aref (font-data font) i)))
    foreign-font))

(defun get-font (&optional fd)
  "Return the current console font as a structure."
  (with-console (fd)
    (with-foreign-object (font '(:struct console-font-op))
      (setf (foreign-slot-value font '(:struct console-font-op) 'data)
	    (null-pointer))
      (unwind-protect
	   (progn
	     (setf (foreign-slot-value font '(:struct console-font-op) 'data)
		   (foreign-alloc :char :count 16384)
		   (foreign-slot-value font '(:struct console-font-op) 'op)
		   +FONT-OP-GET+)
	     (syscall (posix-ioctl fd +KFONTOP+ font))
	     (convert-font-from-foreign font))
	(when (not (equal (foreign-slot-value font '(:struct console-font-op)
					      'data)
			  (null-pointer)))
	  (foreign-free (foreign-slot-value font '(:struct console-font-op)
					    'data)))))))

(defun set-font (font &key fd)
  "Set the current console font from the font structure ‘font’."
  (with-console (fd)
    (with-foreign-object (foreign-font '(:struct console-font-op))
      (setf (foreign-slot-value
	     foreign-font '(:struct console-font-op)
	     'data) (null-pointer)
	     (foreign-slot-value font '(:struct console-font-op) 'op)
	     +FONT-OP-SET+)
      (unwind-protect
	   (progn
	     (convert-font-to-foreign font foreign-font)
	     (syscall (posix-ioctl fd +GIO-FONTX+ foreign-font)))
	(when (not (equal (foreign-slot-value foreign-font
					      '(:struct console-font-op)
					      'data)
			  (null-pointer)))
	  (foreign-free (foreign-slot-value
			 foreign-font '(:struct console-font-op)
			 'data)))))))

(defun font-reset (&optional fd)
  "Reset the console to the default font."
  (with-console (fd)
    (syscall (posix-ioctl fd +PIO-FONTRESET+ 0))))

(defun read-font (file)
  "Read a console font from ‘file’ and return a font structure."
  (declare (ignore file))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text cursor

(defun tty-format (fd format-string &rest args)
  (let ((s (apply #'format nil format-string args)))
    (with-foreign-string (str s)
      ;; Just assume it's not multibyte.
      (syscall (posix-write fd str (length s)))
      (values))))

(defun set-cursor-blink (ms &key fd)
  "Set the text cursor blink interval to ‘ms’ milliseconds."
  (with-console (fd)
    (tty-format fd "~c[16;~d]" #\escape ms)))

;; This is also supposedly in /sys/module/vt/parameters/cur_default
(defparameter *cursor-style* nil
  "Cursor style constants.")
(define-to-list *cursor-style*
    #(#(+cursor-style-default+       0)
      #(+cursor-style-invisible+     1) ; a.k.a none
      #(+cursor-style-underscore+    2) ; a.k.a underline
      #(+cursor-style-lower-third+   3)
      #(+cursor-style-lower-half+    4)
      #(+cursor-style-two-thirds+    5)
      #(+cursor-style-block+         6)
      #(+cursor-style-software+      #x000010) ; non blinking?
      #(+cursor-style-always-bg+     #x000020)
      #(+cursor-style-invert-fg-bg+  #x000040)
      #(+cursor-style-cur-fg+        #x000700)
      #(+cursor-style-cur-bg+        #x007000)
      #(+cursor-style-change+        #x00ff00) ; mask
      #(+cursor-style-set+           #xff0000) ; mask
      ))

;; see linux-<version>/include/linux/console_struct.h

;; @@@ This isn't right
(defparameter *cursor-fg* nil
  "Cursor foreground constants.")
(define-to-list *cursor-fg*
    #(#(+cursor-fg-default+     0)
      #(+cursor-fg-cyan+        1)
      #(+cursor-fg-black+       2)
      #(+cursor-fg-grey+        3)
      #(+cursor-fg-lightyellow+ 4)
      #(+cursor-fg-white+       5)
      #(+cursor-fg-lightred+    6)
      #(+cursor-fg-magenta+     7)
      #(+cursor-fg-green+       8)
      #(+cursor-fg-darkgreen+   9)
      #(+cursor-fg-darkblue+    10)
      #(+cursor-fg-purple+      11)
      #(+cursor-fg-yellow+      12)
      #(+cursor-fg-darkwhite+   13)
      #(+cursor-fg-red+         14)
      #(+cursor-fg-pink+        15)
      ))

;; @@@ This isn't right
(defparameter *cursor-bg* nil
  "Cursor background constants.")
(define-to-list *cursor-bg*
    #(#(+cursor-bg-black+    0)
      #(+cursor-bg-blue      16)
      #(+cursor-bg-green+    32)
      #(+cursor-bg-cyan+     48)
      #(+cursor-bg-red+      64)
      #(+cursor-bg-magenta+  80)
      #(+cursor-bg-yellow+   96)
      #(+cursor-bg-white+    112)
      ))

;; see:
;;   linux-<version>/drivers/tty/vt/vt.c
;;   linux-<version>/include/linux/console_struct.h

;; Mayeb it would have been nice if this was documented anywhere!
;; like "man console_codes". Baka!
(defun set-cursor-style (&key fd (style +cursor-style-default+) fg bg)
  ;; fg is really change and bg is really set
  ;; it only works for software cursor style
  ;; @@@ figure out how to get a specific color?
  ;; see vt.c:add_softcursor
  ;; fg or set    is logior'd into the cell's next to last byte
  ;; bg or change is logxor'd into the cell's next to last byte
  (when (and (or fg bg)
	     (/= style +cursor-style-software+))
    (error "fg or bg given but style is not +cursor-style-software+"))
  (setf fg (or fg 0)
	bg (or bg 0))
  (with-console (fd)
    (tty-format fd "~c[?~a;~a;~a;c" #\escape style fg bg)))

;; This is pretty lame. I guess I can see why it's not documented,
;; because how are you really going to explain how to do this?
;; If linux was in Lisp, we could just specify a function, which could
;; do anything, like invert, set specific colors, etc. Or maybe it's
;; just simpler to do like in terminal-x11 where we just made it be a
;; specific fatchar style.
(defun design-a-cursor (&key fd)
  (with-console (fd)
    (let ((o1 0)
	  (o2 0)
	  (x1 0)
	  (x2 0)
	  (x 0)
	  (add 0) (change 0)
	  (example (span-to-fat-string
		    `((:red "X") (:green "X") (:blue "X") (:cyan "X")
		      (:magenta "X") (:yellow "X") (:white "X")))))
      (with-immediate ()
	(tt-clear)
	(loop :with key
	      :do (setf add     (logior (ash o1 4) o2)
			change  (logior (ash x1 4) x2))
		  (linux-console::set-cursor-style
		   :fd fd
		   :style linux-console::+cursor-style-software+
		   :fg add :bg change)
		  (tt-home) (tt-erase-below)
		  (tt-format "add    ~4b ~4b~%~
                              change ~4b ~4b~%" o1 o2 x1 x2)
		  (tt-write-span-at 10 10 example)
		  (tt-move-to 10 (+ 10 x))
		  (tt-finish-output)
	      :while (not (equal (setf key (tt-get-key)) #\q))
	      :do
		 (case key
		   (#\a (when (< o1 #b1111) (incf o1)))
		   (#\A (when (> o1 0)      (decf o1)))
		   (#\s (when (< o2 #b1111) (incf o2)))
		   (#\S (when (> o2 0)      (decf o2)))
		   (#\d (when (< x1 #b1111) (incf x1)))
		   (#\D (when (> x1 0)      (decf x1)))
		   (#\f (when (< x2 #b1111) (incf x2)))
		   (#\F (when (> x2 0)      (decf x2)))
		   (:left  (when (> x 0) (decf x)))
		   (:right (when (< x (olength example)) (incf x)))))
	(tt-format-at 11 0 "add = #x~x change = #x~x~%" add change)
	(tt-finish-output)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun describe-console (&key fd)
  (if (console-p fd)
      (progn
	(when (not fd)
	  (setf fd 0))
	;; BUG ALERT: I have no idea why, but keyboard-mode doesn't
	;; work, unless you put it first! WTF????
	(print-properties `("Keyboard mode"  ,(keyboard-mode fd)
			    "VT Modes"       ,(mode fd)
                            "VT State"       ,(state fd)
                            "LEDs"           ,(leds fd)
                            "Lock Keys"      ,(lock-keys fd)
                            "Text Mode"      ,(text-mode fd)
                            "Meta key"       ,(meta-key fd)
			    )))
      (format t "~:[~s is not a console.~;Not on a console.~]~%" (null fd) fd)))

;; End
