;;
;; curses.lisp - Interface to the curses library
;;

;; TODO:
;;   - fix bugs?
;;     - line drawing chars on 64 bit?
;;   - Add all the missing functions!
;;   - More tests, with better coverage
;;   - Finish adding docstrings

#+cl-ncurses
(eval-when (:load-toplevel :execute)
  (when (and (find-package :cl-ncurses)
	     (not (find-symbol "*THIS-IS-IT*" :cl-ncurses))
	     (find :cl-ncurses *features*))
    (error "You can't load this curses package with CL-NCURSES compatibility
and also have the original CL-NCURSES loaded. Either remove :cl-ncurses
from *features* or delete the CL-NCURSES package.")))

(defpackage :curses
  (:documentation
"Interface to the curses terminal screen management library.

This pretty much requires ncurses. I haven't tested it with non-ncurses
recently.

HOW TO USE:

Set the features that you want before loading:

:CURSES-USE-WIDE          Use the wide character version of the library

:CURSES-USE-NCURSES       Use ncurses explicitly, instead of whatever plain
			  curses is. **This is not currently used, since we
			  pretty much require ncurses**.

:CURSES-MOUSE-VERSION-1   Explicity pick which version of the mouse protocol to
:CURSES-MOUSE-VERSION-2   use. This should correspond to NCURSES_MOUSE_VERSION
                          in the curses library you're using. It picks version 2
			  by default, so we can get the expected scrolling
			  behavior.

:CL-NCURSES             Turn on CL-NCURSES compatibility. This shouldn't be on
			if CL-NCURSES is being used also. If fact this makes
			sure we don't clash with the actual CL-NCURSES.

This requires CFFI, which must be able to find a curses library. Generally
CFFI will find whatever is available on your system, but you can configure it
if need be, for example by setting cffi:*foreign-library-directories* before
loading this library.")
  (:use :cl :cffi)
;  #+sbcl (:shadowing-import-from :sb-ext "TIMEOUT")
  #+sbcl (:shadow "TIMEOUT")
  #+cl-ncurses (:nickname :cl-ncurses)
  (:export
   ;; constants
   #:+ERR+ #:+OK+
   ;; variables
   #:*cols* #:*lines* #:*colors* #:*color-pairs* #:*stdscr*
   ;; attrs
   #:+a-normal+ #:+a-attributes+ #:+a-chartext+ #:+a-color+ #:+a-standout+
   #:+a-underline+ #:+a-reverse+ #:+a-blink+ #:+a-dim+ #:+a-bold+
   #:+a-altcharset+ #:+a-invis+ #:+a-protect+ #:+a-horizontal+ #:+a-left+
   #:+a-low+ #:+a-right+ #:+a-top+ #:+a-vertical+
   ;; colors
   #:+color-black+ #:+color-red+ #:+color-green+ #:+color-yellow+
   #:+color-blue+ #:+color-magenta+ #:+color-cyan+ #:+color-white+
   ;; acs macros/functions
   #:acs-map #:acs-ulcorner #:acs-llcorner #:acs-urcorner #:acs-lrcorner
   #:acs-ltee #:acs-rtee #:acs-btee #:acs-ttee #:acs-hline #:acs-vline
   #:acs-plus #:acs-s1 #:acs-s9 #:acs-diamond #:acs-ckboard #:acs-degree
   #:acs-plminus #:acs-bullet #:acs-larrow #:acs-rarrow #:acs-darrow
   #:acs-uarrow #:acs-board #:acs-lantern #:acs-block #:acs-s3 #:acs-s7
   #:acs-lequal #:acs-gequal #:acs-pi #:acs-nequal #:acs-sterling
   ;; mouse masks
   #:+button1-released+
   #:+button1-pressed+
   #:+button1-clicked+
   #:+button1-double-clicked+
   #:+button1-triple-clicked+
   #:+button1-reserved-event+
   #:+button2-released+
   #:+button2-pressed+
   #:+button2-clicked+
   #:+button2-double-clicked+
   #:+button2-triple-clicked+
   #:+button2-reserved-event+
   #:+button3-released+
   #:+button3-pressed+
   #:+button3-clicked+
   #:+button3-double-clicked+
   #:+button3-triple-clicked+
   #:+button3-reserved-event+
   #:+button4-released+
   #:+button4-pressed+
   #:+button4-clicked+
   #:+button4-double-clicked+
   #:+button4-triple-clicked+
   #:+button4-reserved-event+
   #:+button-ctrl+
   #:+button-shift+
   #:+button-alt+
   #:+button-any+
   #:+all-mouse-events+
   #:+report-mouse-position+
   ;; mouse event tests
   #:button-release #:button-press #:button-click #:button-double-click
   #:button-triple-click #:button-reserved-event
   #:mouse-mask #:mouse-event-mask

   ;; functions
   #:initscr #:endwin #:newterm #:delscreen #:set-term
   #:newwin #:delwin #:mvwin
   #:refresh #:wrefresh
   #:wnoutrefresh #:doupdate
   #:redrawwin #:wredrawln
   #:move #:wmove
   #:clear #:wclear #:erase #:werase #:clrtobot #:wclrtobot
   #:clrtoeol #:wclrtoeol
   ;; input
   #:getch #:wgetch #:mvgetch #:mvwgetch #:ungetch
   #:getnstr #:wgetnstr #:mvgetnstr #:mvwgetnstr
   #:mmask-t #:mevent #:make-mevent #:mevent-id #:mevent-x #:mevent-y
   #:mevent-z #:mevent-bstate
   #:mousemask #:getmouse #:ungetmouse #:mouseinterval
   ;; output
   #:addch #:waddch #:mvaddch #:mvwaddch #:echochar #:wechochar
   #:addstr #:addnstr #:mvaddstr #:mvaddnstr #:waddstr
   #:waddnstr #:mvaddstr #:mvwaddstr #:mvaddnstr #:mvwaddnstr
   #:addnwstr #:waddnwstr
   #:printw #:wprintw #:mvprintw #:mvwprintw
   #:flash #:beep #:scroll #:scrl #:wscrl
   ;; inserting
   #:insch #:winsch #:mvinsch #:mvwinsch
   #:insstr #:insnstr #:winsstr #:winsnstr #:mvinsstr #:mvinsnstr #:mvwinsstr
   #:mvwinsnstr
   ;; deleting
   #:delch #:wdelch #:mvdelch #:mvwdelch
   #:deleteln #:wdeleteln #:insdelln #:winsdelln #:insertln #:winsertln
   ;; tty modes
   #:resetty #:savetty #:reset-shell-mode #:reset-prog-mode #:cbreak #:nocbreak
   #:echo #:noecho #:nonl #:nl
   #:raw #:noraw #:meta #:nodelay #:notimeout #:halfdelay #:intrflush
   #-sbcl #:timeout
   #:wtimeout #:typeahead
   #:clearok #:idlok #:ikcok #:immedok #:leaveok #:setscrreg #:wsetscrreg
   #:scrollok
   #:erasechar
   #:keypad
   #:function-key
   #:curs-set
   #:getcurx
   #:getcury
   #:getyx
   ;; color & attributes
   #:start-color #:has-colors #:can-change-color #:init-pair
   #:attron #:wattron #:attroff #:wattroff #:attrset #:wattrset #:color-set
   #:wcolor-set #:standend #:wstandend #:standout #:wstandout #:bkgd #:wbkgd
   #:bkgdset #:wbkgdset
   #:color-pair
   #:border #:wborder #:box
   #:hline #:whline #:vline #:wvline #:mvhline #:mvwhline #:mvvline #:mvwvline
   #:napms
   #:is-term-resized #:resize_term #:resizeterm
   ;; utilities?
   #:unctrl #:wunctrl
   #:keyname #:key_name
   #:filter #:nofilter
   #:use-env #:use-tioctl
   #:putwin #:getwin
   #:delay-output
   #:flushinp
   ;; terminfo
   #:tigetstr #:tigetflag #:tigetnum
   )
)
(in-package :curses)

(defvar *this-is-it* t "The only one.")

;; If :curses-use-wide is in *features*, then use the wide character version
;; of the library. If :curses-use-ncurses is in *features* then use ncurses
;; explicitly, instead of whatever plain curses says.
;;
;; The user should put :curses-use-wide and/or :curses-use-ncurses in *features*
;; before loading this library, and also load CFFI and set
;; cffi:*foreign-library-directories* if needed before loading this library.

;; Let's better hope this does it.

;; XXX WRONG: This depends on *features* at compile time XXX
;; (define-foreign-library libcurses
;;   (t #+(and curses-use-wide curses-use-ncurses)
;;      (:default "libncursesw")
;;      #+(and curses-use-wide (not curses-use-ncurses))
;;      (:or (:default "libcursesw")
;; 	  (:default "libncursesw"))
;;      #+(and curses-use-ncurses (not curses-use-wide))
;;      (:or (:default "libncurses")
;; 	  (:default "libcurses"))
;;      #+(and (not curses-use-ncurses) (not curses-use-wide))
;;      (:or (:default "libcurses")
;; 	  (:default "libncurses"))))

(define-foreign-library libcurses
  (:linux (:or "libncursesw.so" "libncursesw.so.5"
	       "libncurses.so" "libncurses.so.5"))

  ;; Why does this fail?:
  ;; ((:and :curses-use-wide :curses-use-ncurses)
  ;;  (:default "libncursesw"))
  ;; ((:and :curses-use-wide (:not :curses-use-ncurses))
  ;;  (:or (:default "libcursesw")
  ;; 	(:default "libncursesw")))
  ;; ((:and :curses-use-ncurses (:not :curses-use-wide))
  ;;  (:or (:default "libncurses")
  ;; 	(:default "libcurses")))

  ;; ((:and (:not :curses-use-ncurses) (:not :curses-use-wide))
  ;;  (:or (:default "libcurses")
  ;; 	(:default "libncurses")
  ;; 	(:default "libcursesw")
  ;; 	(:default "libncursesw")
  ;; 	(:default "ncursesw")
  ;; 	(:default "ncurses")
  ;; 	(:default "cursesw")
  ;; 	(:default "curses")))
  (t (:or (:default "libncursesw")
	  (:default "libcursesw")
	  (:default "ncursesw")
	  (:default "cursesw")
	  (:default "libncurses")
	  (:default "libcurses")
	  (:default "ncurses")
	  (:default "curses"))))

(use-foreign-library libcurses)

;; See if we got the wide version, and set the feature accordingly.
(when (search "cursesw"
	      (namestring (cffi:foreign-library-pathname
			   (cffi::get-foreign-library 'curses::libcurses))))
  (pushnew :curses-use-wide *features*))

#|
(define-foreign-library libcurses
     (t (:or
	 (:default "libncursesw")	; prefer ncurses wide
	 (:default "libncurses")	; then normal ncurses
	 (:default "libcurses"))))	; but we'll take something else
;     (t (:default "libncurses")))
;     (t (:default "libncursesw")))

;; OLD WAY:
;; (define-foreign-library libcurses
;;      (t (:default "libncursesw")))
;; (use-foreign-library libcurses)

(setf *features* (delete :curses-use-wchar *features*))
(setf *features* (delete :curses-use-ncurses *features*))

;; Apparently this whole thing is moot, since we don't get an error
;; when defining or using a library.

(define-foreign-library libncurses-wide (:default "libncursesw"))
(define-foreign-library libncurses      (:default "libncurses"))
(define-foreign-library libcurses       (:default "libcurses"))

(defun load-curses-library ()
  (macrolet ((blip (lib &body body)
	       `(handler-case
		    (progn
		      (use-foreign-library ,lib)
		      ,@body
		      (return-from load-curses-library))
		  (load-foreign-library-error (c)
		    (declare (ignore c))
		    (format t ";; fail ~a~%" ',lib)))))
    (blip libncurses-wide
	  (format t ";; Using ncurses wide")
	  (pushnew :curses-has-wchar *features*)
	  (pushnew :curses-ncurses *features*))
    (blip libncurses
	  (format t ";; Using ncurses")
	  (pushnew :curses-ncurses *features*))
    (blip libcurses
	  (format t ";; Using curses"))
    (error "Can't seem to load a curses library.")))

(eval-when (:load-toplevel :execute)
  (load-curses-library))

|#

;; Simple curses types:
(defctype chtype     :unsigned-long)
;;(defctype chtype     :unsigned-int)
;(defctype bool       :unsigned-int)
(defctype bool       :unsigned-char)
(defctype wchar-t    :int)
(defctype attr-t     :unsigned-long)	; same as chtype
(defctype window-ptr :pointer)		; (WINDOW *)
(defctype screen-ptr :pointer)		; (SCREEN *)
;; This really should agree with whatever the calling code does for FILE *
(defctype file-ptr   :pointer)		; (FILE *)
;; This is also defined in opsys, but we can't depend on that.
#+curses-use-wide (defctype wint-t :unsigned-int)

(defcstruct cchar-t
  (attr attr-t)
  (chars wchar-t :count 5))
(defctype cchar-t-ptr (:pointer (:struct cchar-t))) ; (cchar-t *)

(defconstant +ERR+ -1 "Return value usually indicating an error.")
(defconstant +OK+   0 "Return value usually indicating success.")

(defcvar ("COLS"	*cols*)		:int)
(defcvar ("LINES"	*lines*)	:int)
(defcvar ("COLORS"	*colors*)	:int)
(defcvar ("COLOR_PAIRS"	*color-pairs*)	:int)
(defcvar ("stdscr"	*stdscr*)	window-ptr)

#-(or cygwin win32) (defcvar ("acs_map"	private-acs-map) :pointer)
#+(or cygwin win32) (defcfun ("_nc_acs_map" acs-map-func) :pointer)

(defun acs-map (c)
  (declare (type character c))
  (logand #xff
	  #-(or cygwin win32)
	  (mem-aref (get-var-pointer 'private-acs-map) 'chtype (char-code c))
	  #+(or cygwin win32)
	  (mem-aref (acs-map-func) 'chtype (char-code c))
	  ))

(defmacro acs-ulcorner  () '(acs-map #\l))	;; upper left corner
(defmacro acs-llcorner	() '(acs-map #\m))	;; lower left corner
(defmacro acs-urcorner	() '(acs-map #\k))	;; upper right corner
(defmacro acs-lrcorner	() '(acs-map #\j))	;; lower right corner
(defmacro acs-ltee	() '(acs-map #\t))	;; tee pointing right
(defmacro acs-rtee	() '(acs-map #\u))	;; tee pointing left
(defmacro acs-btee	() '(acs-map #\v))	;; tee pointing up
(defmacro acs-ttee	() '(acs-map #\w))	;; tee pointing down
(defmacro acs-hline	() '(acs-map #\q))	;; horizontal line
(defmacro acs-vline	() '(acs-map #\x))	;; vertical line
(defmacro acs-plus	() '(acs-map #\n))	;; large plus or crossover
(defmacro acs-s1	() '(acs-map #\o))	;; scan line 1
(defmacro acs-s9	() '(acs-map #\s))	;; scan line 9
(defmacro acs-diamond	() '(acs-map #\`))	;; diamond
(defmacro acs-ckboard	() '(acs-map #\a))	;; checker board (stipple)
(defmacro acs-degree	() '(acs-map #\f))	;; degree symbol
(defmacro acs-plminus	() '(acs-map #\g))	;; plus/minus
(defmacro acs-bullet	() '(acs-map #\~))	;; bullet
(defmacro acs-larrow	() '(acs-map #\,))	;; arrow pointing left
(defmacro acs-rarrow	() '(acs-map #\+))	;; arrow pointing right
(defmacro acs-darrow	() '(acs-map #\.))	;; arrow pointing down
(defmacro acs-uarrow	() '(acs-map #\-))	;; arrow pointing up
(defmacro acs-board	() '(acs-map #\h))	;; board of squares
(defmacro acs-lantern	() '(acs-map #\i))	;; lantern symbol
(defmacro acs-block	() '(acs-map #\0))	;; solid square block
(defmacro acs-s3	() '(acs-map #\p))	;; scan line 3
(defmacro acs-s7	() '(acs-map #\r))	;; scan line 7
(defmacro acs-lequal	() '(acs-map #\y))	;; less/equal
(defmacro acs-gequal	() '(acs-map #\z))	;; greater/equal
(defmacro acs-pi	() '(acs-map #\{))	;; Pi
(defmacro acs-nequal	() '(acs-map #\|))	;; not equal
(defmacro acs-sterling	() '(acs-map #\}))	;; UK pound sign

(defconstant +A-NORMAL+		#x00000000)
(defconstant +A-ATTRIBUTES+	#xffffff00)
(defconstant +A-CHARTEXT+	#x000000ff)
(defconstant +A-COLOR+		#x0000ff00)
(defconstant +A-STANDOUT+	#x00010000)
(defconstant +A-UNDERLINE+	#x00020000)
(defconstant +A-REVERSE+	#x00040000)
(defconstant +A-BLINK+		#x00080000)
(defconstant +A-DIM+		#x00100000)
(defconstant +A-BOLD+		#x00200000)
(defconstant +A-ALTCHARSET+	#x00400000)
(defconstant +A-INVIS+		#x00800000)
(defconstant +A-PROTECT+	#x01000000)
(defconstant +A-HORIZONTAL+	#x02000000)
(defconstant +A-LEFT+		#x04000000)
(defconstant +A-LOW+		#x08000000)
(defconstant +A-RIGHT+		#x10000000)
(defconstant +A-TOP+		#x20000000)
(defconstant +A-VERTICAL+	#x40000000)

(defconstant +COLOR-BLACK+   0)
(defconstant +COLOR-RED+     1)
(defconstant +COLOR-GREEN+   2)
(defconstant +COLOR-YELLOW+  3)
(defconstant +COLOR-BLUE+    4)
(defconstant +COLOR-MAGENTA+ 5)
(defconstant +COLOR-CYAN+    6)
(defconstant +COLOR-WHITE+   7)

;; mouse events

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *mouse-version*
    #+curses-mouse-version-1 1
    #+curses-mouse-version-2 2
    #-(or curses-mouse-version-1 curses-mouse-version-2) 1
    "What version of mouse events to use.")

  (defconstant +BUTTON-RELEASED+       #o001)
  (defconstant +BUTTON-PRESSED+        #o002)
  (defconstant +BUTTON-CLICKED+        #o004)
  (defconstant +BUTTON-DOUBLE-CLICKED+ #o010)
  (defconstant +BUTTON-TRIPLE-CLICKED+ #o020)
  (defconstant +BUTTON-RESERVED-EVENT+ #o040)
  (defconstant +BUTTON-ANY+            #o077)

  (cond
    ((= *mouse-version* 1)
     (defmacro mouse-mask (button type) `(ash ,type (* 6 (- ,button 1)))))
    ((> *mouse-version* 1)
     (defmacro mouse-mask (button type) `(ash ,type (* 5 (- ,button 1)))))
    (t (error "Unknown *mouse-version* ~d" *mouse-version*))))

(defconstant +BUTTON1-RELEASED+        (mouse-mask 1 +BUTTON-RELEASED+))
(defconstant +BUTTON1-PRESSED+         (mouse-mask 1 +BUTTON-PRESSED+))
(defconstant +BUTTON1-CLICKED+         (mouse-mask 1 +BUTTON-CLICKED+))
(defconstant +BUTTON1-DOUBLE-CLICKED+  (mouse-mask 1 +BUTTON-DOUBLE-CLICKED+))
(defconstant +BUTTON1-TRIPLE-CLICKED+  (mouse-mask 1 +BUTTON-TRIPLE-CLICKED+))

(defconstant +BUTTON2-RELEASED+        (mouse-mask 2 +BUTTON-RELEASED+))
(defconstant +BUTTON2-PRESSED+         (mouse-mask 2 +BUTTON-PRESSED+))
(defconstant +BUTTON2-CLICKED+         (mouse-mask 2 +BUTTON-CLICKED+))
(defconstant +BUTTON2-DOUBLE-CLICKED+  (mouse-mask 2 +BUTTON-DOUBLE-CLICKED+))
(defconstant +BUTTON2-TRIPLE-CLICKED+  (mouse-mask 2 +BUTTON-TRIPLE-CLICKED+))

(defconstant +BUTTON3-RELEASED+        (mouse-mask 3 +BUTTON-RELEASED+))
(defconstant +BUTTON3-PRESSED+         (mouse-mask 3 +BUTTON-PRESSED+))
(defconstant +BUTTON3-CLICKED+         (mouse-mask 3 +BUTTON-CLICKED+))
(defconstant +BUTTON3-DOUBLE-CLICKED+  (mouse-mask 3 +BUTTON-DOUBLE-CLICKED+))
(defconstant +BUTTON3-TRIPLE-CLICKED+  (mouse-mask 3 +BUTTON-TRIPLE-CLICKED+))

(defconstant +BUTTON4-RELEASED+        (mouse-mask 4 +BUTTON-RELEASED+))
(defconstant +BUTTON4-PRESSED+         (mouse-mask 4 +BUTTON-PRESSED+))
(defconstant +BUTTON4-CLICKED+         (mouse-mask 4 +BUTTON-CLICKED+))
(defconstant +BUTTON4-DOUBLE-CLICKED+  (mouse-mask 4 +BUTTON-DOUBLE-CLICKED+))
(defconstant +BUTTON4-TRIPLE-CLICKED+  (mouse-mask 4 +BUTTON-TRIPLE-CLICKED+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cond
    ((= *mouse-version* 1)
     ;; Version 1 has reserved events.
     (defconstant +BUTTON1-RESERVED-EVENT+
       (mouse-mask 1 +BUTTON-RESERVED-EVENT+))
     (defconstant +BUTTON2-RESERVED-EVENT+
       (mouse-mask 2 +BUTTON-RESERVED-EVENT+))
     (defconstant +BUTTON3-RESERVED-EVENT+
       (mouse-mask 3 +BUTTON-RESERVED-EVENT+))
     (defconstant +BUTTON4-RESERVED-EVENT+
       (mouse-mask 4 +BUTTON-RESERVED-EVENT+))

     (defconstant +BUTTON-CTRL+            (mouse-mask 5 #o001))
     (defconstant +BUTTON-SHIFT+           (mouse-mask 5 #o002))
     (defconstant +BUTTON-ALT+             (mouse-mask 5 #o004))
     (defconstant +REPORT-MOUSE-POSITION+  (mouse-mask 5 #o010)))
    ((> *mouse-version* 1)
     ;; Version >1 has button 5 and no reserved events.
     (defconstant +BUTTON5-RELEASED+
       (mouse-mask 5 +BUTTON-RELEASED+))
     (defconstant +BUTTON5-PRESSED+
       (mouse-mask 5 +BUTTON-PRESSED+))
     (defconstant +BUTTON5-CLICKED+
       (mouse-mask 5 +BUTTON-CLICKED+))
     (defconstant +BUTTON5-DOUBLE-CLICKED+
       (mouse-mask 5 +BUTTON-DOUBLE-CLICKED+))
     (defconstant +BUTTON5-TRIPLE-CLICKED+
       (mouse-mask 5 +BUTTON-TRIPLE-CLICKED+))

     (defconstant +BUTTON-CTRL+            (mouse-mask 6 #o001))
     (defconstant +BUTTON-SHIFT+           (mouse-mask 6 #o002))
     (defconstant +BUTTON-ALT+             (mouse-mask 6 #o004))
     (defconstant +REPORT-MOUSE-POSITION+  (mouse-mask 6 #o010)))
    (t (error "Unknown *mouse-version* ~d" *mouse-version*))))

(defconstant +ALL-MOUSE-EVENTS+ (1- +REPORT-MOUSE-POSITION+))

(defmacro mouse-event-mask (event button event-type)
  `(not (= 0 (logand ,event (mouse-mask ,button ,event-type)))))

(defun BUTTON-RELEASE	     (e x) (mouse-event-mask e x +BUTTON-RELEASED+))
(defun BUTTON-PRESS	     (e x) (mouse-event-mask e x +BUTTON-PRESSED+))
(defun BUTTON-CLICK	     (e x) (mouse-event-mask e x +BUTTON-CLICKED+))
(defun BUTTON-DOUBLE-CLICK   (e x)
  (mouse-event-mask e x +BUTTON-DOUBLE-CLICKED+))
(defun BUTTON-TRIPLE-CLICK   (e x)
  (mouse-event-mask e x +BUTTON-TRIPLE-CLICKED+))
(defun BUTTON-RESERVED-EVENT (e x)
  (mouse-event-mask e x +BUTTON-RESERVED-EVENT+))

(defvar *funkeys* nil
  "Hash table mapping curses function key codes to our keywords.")
(defun init-keys ()
  (setf *funkeys* (make-hash-table :test 'equal))
  (setf (gethash #o401 *funkeys*) :break)     ; Break key (unreliable)
  (setf (gethash #o530 *funkeys*) :SRESET)    ; Soft(partial) reset (unreliable)
  (setf (gethash #o531 *funkeys*) :RESET)     ; Reset or hard reset (unreliable)
  (setf (gethash #o632 *funkeys*) :RESIZE)    ; Terminal resize event
  (setf (gethash #o402 *funkeys*) :DOWN)      ; down-arrow key
  (setf (gethash #o403 *funkeys*) :UP)	      ; up-arrow key
  (setf (gethash #o404 *funkeys*) :LEFT)      ; left-arrow key
  (setf (gethash #o405 *funkeys*) :RIGHT)     ; right-arrow key
  (setf (gethash #o406 *funkeys*) :HOME)      ; home key
  (setf (gethash #o407 *funkeys*) :BACKSPACE) ; backspace key
  (loop :for i :from 0 :to 64 :do	      ; 64 function keys :F<n>
	(setf (gethash (+ #o410 i) *funkeys*)
	      (intern (format nil "F~d" i) :keyword)))
  (setf (gethash #o510 *funkeys*) :DL)	      ; delete-line key
  (setf (gethash #o511 *funkeys*) :IL)	      ; insert-line key
  (setf (gethash #o512 *funkeys*) :DC)	      ; delete-character key
  (setf (gethash #o513 *funkeys*) :IC)	      ; insert-character key
  (setf (gethash #o514 *funkeys*) :EIC)	      ; rmir/smir in insert mode
  (setf (gethash #o515 *funkeys*) :CLEAR)     ; clear-screen or erase key
  (setf (gethash #o516 *funkeys*) :EOS)	      ; clear-to-end-of-screen key
  (setf (gethash #o517 *funkeys*) :EOL)	      ; clear-to-end-of-line key
  (setf (gethash #o520 *funkeys*) :SF)	      ; scroll-forward key
  (setf (gethash #o521 *funkeys*) :SR)	      ; scroll-backward key
  (setf (gethash #o522 *funkeys*) :NPAGE)     ; next-page key
  (setf (gethash #o523 *funkeys*) :PPAGE)     ; previous-page key
  (setf (gethash #o524 *funkeys*) :STAB)      ; set-tab key
  (setf (gethash #o525 *funkeys*) :CTAB)      ; clear-tab key
  (setf (gethash #o526 *funkeys*) :CATAB)     ; clear-all-tabs key
  (setf (gethash #o527 *funkeys*) :ENTER)     ; enter/send key
  (setf (gethash #o532 *funkeys*) :PRINT)     ; print key
  (setf (gethash #o533 *funkeys*) :LL)	      ; lower-left key (home down)
  (setf (gethash #o534 *funkeys*) :A1)	      ; upper left of keypad
  (setf (gethash #o535 *funkeys*) :A3)	      ; upper right of keypad
  (setf (gethash #o536 *funkeys*) :B2)	      ; center of keypad
  (setf (gethash #o537 *funkeys*) :C1)	      ; lower left of keypad
  (setf (gethash #o540 *funkeys*) :C3)	      ; lower right of keypad
  (setf (gethash #o541 *funkeys*) :BTAB)      ; back-tab key
  (setf (gethash #o542 *funkeys*) :BEG)	      ; begin key
  (setf (gethash #o543 *funkeys*) :CANCEL)    ; cancel key
  (setf (gethash #o544 *funkeys*) :CLOSE)     ; close key
  (setf (gethash #o545 *funkeys*) :COMMAND)   ; command key
  (setf (gethash #o546 *funkeys*) :COPY)      ; copy key
  (setf (gethash #o547 *funkeys*) :CREATE)    ; create key
  (setf (gethash #o550 *funkeys*) :END)	      ; end key
  (setf (gethash #o551 *funkeys*) :EXIT)      ; exit key
  (setf (gethash #o552 *funkeys*) :FIND)      ; find key
  (setf (gethash #o553 *funkeys*) :HELP)      ; help key
  (setf (gethash #o554 *funkeys*) :MARK)      ; mark key
  (setf (gethash #o555 *funkeys*) :MESSAGE)   ; message key
  (setf (gethash #o556 *funkeys*) :MOVE)      ; move key
  (setf (gethash #o557 *funkeys*) :NEXT)      ; next key
  (setf (gethash #o560 *funkeys*) :OPEN)      ; open key
  (setf (gethash #o561 *funkeys*) :OPTIONS)   ; options key
  (setf (gethash #o562 *funkeys*) :PREVIOUS)  ; previous key
  (setf (gethash #o563 *funkeys*) :REDO)      ; redo key
  (setf (gethash #o564 *funkeys*) :REFERENCE) ; reference key
  (setf (gethash #o565 *funkeys*) :REFRESH)   ; refresh key
  (setf (gethash #o566 *funkeys*) :REPLACE)   ; replace key
  (setf (gethash #o567 *funkeys*) :RESTART)   ; restart key
  (setf (gethash #o570 *funkeys*) :RESUME)    ; resume key
  (setf (gethash #o571 *funkeys*) :SAVE)      ; save key
  (setf (gethash #o572 *funkeys*) :SBEG)      ; shifted begin key
  (setf (gethash #o573 *funkeys*) :SCANCEL)   ; shifted cancel key
  (setf (gethash #o574 *funkeys*) :SCOMMAND)  ; shifted command key
  (setf (gethash #o575 *funkeys*) :SCOPY)     ; shifted copy key
  (setf (gethash #o576 *funkeys*) :SCREATE)   ; shifted create key
  (setf (gethash #o577 *funkeys*) :SDC)	      ; shifted delete-character key
  (setf (gethash #o600 *funkeys*) :SDL)	      ; shifted delete-line key
  (setf (gethash #o601 *funkeys*) :SELECT)    ; select key
  (setf (gethash #o602 *funkeys*) :SEND)      ; shifted end key
  (setf (gethash #o603 *funkeys*) :SEOL)      ; shifted clear-to-eol key
  (setf (gethash #o604 *funkeys*) :SEXIT)     ; shifted exit key
  (setf (gethash #o605 *funkeys*) :SFIND)     ; shifted find key
  (setf (gethash #o606 *funkeys*) :SHELP)     ; shifted help key
  (setf (gethash #o607 *funkeys*) :SHOME)     ; shifted home key
  (setf (gethash #o610 *funkeys*) :SIC)	      ; shifted insert-character key
  (setf (gethash #o611 *funkeys*) :SLEFT)     ; shifted left-arrow key
  (setf (gethash #o612 *funkeys*) :SMESSAGE)  ; shifted message key
  (setf (gethash #o613 *funkeys*) :SMOVE)     ; shifted move key
  (setf (gethash #o614 *funkeys*) :SNEXT)     ; shifted next key
  (setf (gethash #o615 *funkeys*) :SOPTIONS)  ; shifted options key
  (setf (gethash #o616 *funkeys*) :SPREVIOUS) ; shifted previous key
  (setf (gethash #o617 *funkeys*) :SPRINT)    ; shifted print key
  (setf (gethash #o620 *funkeys*) :SREDO)     ; shifted redo key
  (setf (gethash #o621 *funkeys*) :SREPLACE)  ; shifted replace key
  (setf (gethash #o622 *funkeys*) :SRIGHT)    ; shifted right-arrow key
  (setf (gethash #o623 *funkeys*) :SRSUME)    ; shifted resume key
  (setf (gethash #o624 *funkeys*) :SSAVE)     ; shifted save key
  (setf (gethash #o625 *funkeys*) :SSUSPEND)  ; shifted suspend key
  (setf (gethash #o626 *funkeys*) :SUNDO)     ; shifted undo key
  (setf (gethash #o627 *funkeys*) :SUSPEND)   ; suspend key
  (setf (gethash #o630 *funkeys*) :UNDO)      ; undo key
  (setf (gethash #o631 *funkeys*) :MOUSE)     ; Mouse event has occurred
)
(defmacro function-key (k) `(gethash ,k *funkeys*))

;; Initialization
(defcfun newterm screen-ptr
  "Initialize a terminal. Takes a terminal type and an input and output
file name. Returns a pointer to a SCREEN."
  (term-type :string) (output-file file-ptr) (input-file file-ptr))
(defcfun delscreen :void
  "Frees the resources associated with the SCREEN. Should be called when done
with the screen allocated by newterm."
  (screen screen-ptr))
(defcfun set-term screen-ptr
  "Set the current terminal to SCREEN-PTR. Returns the previous terminal."
  (new screen-ptr))
(defcfun initscr window-ptr
  "Initialize curses and return a window pointer.")
(defcfun endwin :int
  "Put the terminal state back to before calling initscr and stop using
curses. endwin should be called for each terminal before exiting a curses
application.")

;; Windows
(defcfun newwin window-ptr
  "Create a new window at row BEGIN-Y and column BEGIN-X of size NLINES x NCOLS,
and return a WINDOW pointer to it. NLINES defaults to *LINES* and NCOLS defaults
to *COLS*."
  (nlines :int) (ncols :int) (begin-y :int) (begin-x :int))
(defcfun delwin :int
  "Delete the the window WIN. Subwindows should be deleted before the main
window."
  (win window-ptr :in))
(defcfun mvwin :int
  "Move the window to X and Y. Windows can't be moved off screen. Moving
subwindows is allowed, but supposedly should be avoided."
  (win window-ptr :in) (y :int) (x :int))

;; Update
(defcfun refresh :int
  "Actually write the output to the terminal.")
(defcfun wrefresh :int
  "Actually output the window to the physical screen."
  (w window-ptr))
(defcfun wnoutrefresh :int
  "Copy the window to the virtual screen."
  (w window-ptr))
(defcfun doupdate :int
  "Output the virtual screen.")
(defcfun redrawwin :int
  "Indicate that the entire screen has been corrupted and needs to be redrawn."
  (w window-ptr))
(defcfun wredrawln :int
  "Indicate that the lines starting at BEG-LINE for NUM-LINES are corrupted, and
must be redrawn."
  (w window-ptr) (beg-line :int) (num-lines :int))

;; Movement
(defcfun move :int
  "Move the cursor to row Y and column X, relative to the upper left corner
at (0, 0)."
  (y :int) (x :int))
(defcfun wmove :int
  "Move the cursor to row Y and column X, in window W."
  (w window-ptr) (y :int) (x :int))

;; Clearing and eraseing
(defcfun clear :int
  "Completely clear and redraw the phyical screen. Useful if may be unknown
contents.")
(defcfun wclear :int
  "Completely clear and redraw the phyical window. Useful if may be unknown
contents."
  (w window-ptr :in))
(defcfun erase :int
  "Copy blanks to every position, clearing the screen.")
(defcfun werase :int
  "Copy blanks to every position in the window, clearing the screen."
  (w window-ptr :in))
(defcfun clrtobot :int)
(defcfun wclrtobot :int (w window-ptr :in))
(defcfun clrtoeol :int)
(defcfun wclrtoeol :int (w window-ptr :in))

;; Input
(defcfun getch :int)
(defcfun wgetch :int (win window-ptr))
(defcfun mvgetch :int (y :int) (x :int))
(defcfun mvwgetch :int (win window-ptr) (y :int) (x :int))
(defcfun ungetch :int (ch :int))
;(def-curses ("has_key" has-key) (:int) (ch :int)) ; ncurses only

#+curses-use-wide
(progn
  (defcfun ("get_wch" get-wch) :int (wch (:pointer wint-t)))
  (defcfun ("wget_wch" wget-wch) :int (win window-ptr) (wch (:pointer wint-t)))
  (defcfun ("mvget_wch" mvget-wch) :int
    (y :int) (x :int) (wch (:pointer wint-t)))
  (defcfun ("mvwget_wch" mvwget-wch) :int
    (win window-ptr) (y :int) (x :int) (wch (:pointer wint-t)))
  (defcfun ("unget_wch"	 unget-wch)  :int (wch wchar-t)))

;; I'm not even going to provide the raw non-N versions, like getstr, because
;; you should never use them. BUT, we could provide a safe lisp equivalents
;; which would use the N versions and return strings.

;(defcfun getnstr :int (str (c-ptr :string) :in-out) (n :int))
;(defcfun getnstr :int
;  #+clisp (str (ffi:c-ptr (ffi:c-array-max ffi:char 256)) :out)
;  #+sbcl (str (* (:array :char 256)))
;  (str (* (:array :char 256)))
;  (n :int))
(defcfun getnstr :int (str :pointer) (n :int))
(defcfun wgetnstr :int (win :pointer) (str :pointer) (n :int))
(defcfun mvgetnstr :int (y :int) (x :int) (str :pointer) (n :int))
(defcfun mvwgetnstr :int (win :pointer) (y :int) (x :int) (str :pointer)
	 (n :int))

;; Mouse input
(defctype mmask-t :unsigned-long)
(defcfun mousemask mmask-t (newmask mmask-t) (oldmask :pointer))
;(defcfun mousemask mmask-t (newmask mmask-t) (oldmask (* mmask-t)))
;(defcfun mousemask mmask-t (newmask mmask-t) (oldmask :pointer))
; (defcfun mousemask mmask-t
; ;  (newmask mmask-t) (oldmask (ffi:c-ptr mmask-t) :in-out))
;   (newmask mmask-t) (oldmask ffi::c-pointer))
;(defcfun getmouse :int (event (* mevent)))
; (ffi:def-call-out getmouse
;     (:library "/usr/lib/libcurses.dylib")
;     (:language :stdc)			; Does it matter?
;     (:name "getmouse")
;     (:arguments (m (ffi:c-ptr mevent) :in-out))
;     (:return-type ffi:int))
(defcstruct foreign-mevent
  (id :short)
  (x :int)
  (y :int)
  (z :int)
  (bstate mmask-t))

(defstruct mevent
  "A mouse event. The Lisp equivalent of the C mevent struct."
  id x y z bstate)

(defcfun ("getmouse" real-getmouse) :int (event :pointer))
(defun getmouse (&optional mouse-event foreign-mevent)
  "Get a mouse event. If MOUSE-EVENT is provided, fill it in. Otherwise return
a new one. If FOREIGN-MOUSE-EVENT is also provided, use that to stor the C
structure, otherwise use a temporary one. It is probably the most efficent case
if both are provided, but it's sometimes convenient not to."
  (let ((result (or mouse-event (make-mevent))))
    (labels ((get-it (fmev)
	       (real-getmouse fmev)
	       (with-foreign-slots ((id x y z bstate) fmev
				    (:struct foreign-mevent))
		 (setf (mevent-id result) id)
		 (setf (mevent-x result) x)
		 (setf (mevent-y result) y)
		 (setf (mevent-z result) z)
		 (setf (mevent-bstate result) bstate))))
      (if foreign-mevent
	  (get-it foreign-mevent)
	  (with-foreign-object (fmev '(:struct foreign-mevent))
	    (get-it fmev)))
      result)))

(defcfun ungetmouse :int
  "Pushes a mouse event back on the queue."
  (event :pointer))
(defcfun mouseinterval :int
  "Set the maximum time in milliseconds for a press and release to be recognized
as a click. Return the previous setting. -1 makes no change. 0 disables it."
  (milliseconds :int))

;;
;; Output
;;
(defcfun addch :int (ch chtype))
(defcfun waddch :int (w window-ptr :in) (ch chtype))
(defcfun mvaddch :int (y :int) (x :int) (ch chtype))
(defcfun mvwaddch :int (w window-ptr :in) (y :int) (x :int) (ch chtype))
(defcfun echochar :int (ch chtype))
(defcfun wechochar :int (w window-ptr :in) (ch chtype))

;; wide chars
#+curses-use-wide (defcfun add-wch :int (wch cchar-t-ptr))
#+curses-use-wide (defcfun wadd-wch :int (win window-ptr) (wch cchar-t-ptr))
#+curses-use-wide (defcfun mvadd-wch :int (y :int) (x :int) (wch cchar-t-ptr))
#+curses-use-wide (defcfun mvwadd-wch
		      :int (win window-ptr) (y :int) (x :int) (wch cchar-t-ptr))
#+curses-use-wide (defcfun echo-wchar :int (wch cchar-t-ptr))
#+curses-use-wide (defcfun wecho-wchar :int (win window-ptr) (wch cchar-t-ptr))

;; strings
(defcfun addstr :int (str :string :in))
(defcfun addnstr :int (str :string :in) (n :int))
(defcfun mvaddstr :int (y :int) (x :int) (str :string :in))
(defcfun mvaddnstr :int (y :int) (x :int) (str :string :in) (n :int))
(defcfun waddstr :int (w window-ptr :in) (str :string :in))
(defcfun waddnstr :int
  (w window-ptr :in) (str :string :in) (n :int))
(defcfun mvwaddstr :int
  (w window-ptr :in) (y :int) (x :int) (str :string :in))
(defcfun mvwaddnstr :int
  (w window-ptr :in) (y :int) (x :int) (str :string :in) (n :int))

;; You have to call these with types, like:
;; (printw "%c %d %s %f" :char 33 :int 23 :string "foo" :float 1.273)
(defcfun printw    :int (fmt :string) &rest)
(defcfun wprintw   :int (win window-ptr) (fmt :string) &rest)
(defcfun mvprintw  :int (y :int) (x :int) (fmt :string) &rest)
(defcfun mvwprintw :int (win window-ptr) (y :int) (x :int) (fmt :string) &rest)

;; wide strings
#+curses-use-wide (defcfun addnwstr :int (str :pointer) (n :int))
#+curses-use-wide (defcfun waddnwstr :int (win window-ptr) (str :pointer) (n :int))

;; misc output
(defcfun flash :int "Try to flash the screen.")
(defcfun beep :int "Try to make a noise or something.")
(defcfun scroll :int
  "Scroll the window up one line."
  (win window-ptr))
(defcfun scrl :int
  "Scroll by N. Positive N is up, negative N is down."
  (n :int))
(defcfun wscrl :int
  "Scroll the window by N. Positive N is up, negative N is down."
  (win window-ptr) (n :int))

;; Inserting
(defcfun insch :int
  "Insert the character CH before the character under the cursor. Characters to
the right are moved, and possibly lost."
  (ch chtype))
(defcfun winsch :int
  "Insert the character CH before the character under the cursor, in the window
WIN. Characters to the right are moved, and possibly lost."
  (win window-ptr) (ch chtype))
(defcfun mvinsch :int
  "Move to Y, X and insert the character CH before the character under the
cursor. Characters to the right are moved, and possibly lost."
  (y :int) (x :int) (ch chtype))
(defcfun mvwinsch :int
  "Move to Y, X and insert the character CH before the character under the
cursor, in the window WIN. Characters to the right are moved, and possibly
lost."
  (win window-ptr) (y :int) (x :int) (ch chtype))
(defcfun insstr :int
  "Insert a string."
  (str :string))
(defcfun insnstr :int
  "Insert a the first N characters of a string."
  (str :string) (n :int))
(defcfun winsstr :int
  "Insert a string in window WIN."
  (win window-ptr) (str :string))
(defcfun winsnstr :int
  "Insert the first N characters of a string, in window WIN."
  (win window-ptr) (str :string) (n :int))
(defcfun mvinsstr :int
  "Move and insert a string."
  (y :int) (x :int) (str :string))
(defcfun mvinsnstr :int
  "Move and insert the first N characters of a string."
  (y :int) (x :int) (str :string) (n :int))
(defcfun mvwinsstr :int
  "Move and insert a string, in window WIN."
  (win window-ptr) (y :int) (x :int) (str :string))
(defcfun mvwinsnstr :int
  "Move and insert a the first N characters of a string, in window WIN."
  (win window-ptr) (y :int) (x :int) (str :string) (n :int))

;; #:ins_wch #:wins_wch #:mvins_wch #:mvwins_wch
;; #:ins_wstr #:ins_nwstr #:wins_wstr #:wins_nwstr #:mvins_wstr #:mvins_nwstr
;; #:mvwins_wstr #:mvwins_nwstr

#+curses-use-wide (defcfun ins_wch :int
  "Insert a complex character."
  (wch cchar-t-ptr))
#+curses-use-wide (defcfun wins_wch :int
  "Insert a complex character in a window."
  (win window-ptr) (wch cchar-t-ptr))
#+curses-use-wide (defcfun mvins_wch :int
  "Move and insert insert a complex character."
  (y :int) (x :int) (wch cchar-t-ptr))
#+curses-use-wide (defcfun mvwins_wch :int
  "Move and insert insert a complex character in a window."
  (win window-ptr) (y :int) (x :int) (wch cchar-t-ptr))
#+curses-use-wide (defcfun ins_wstr :int
  "Insert a wide character string."
  (wstr (:pointer wchar-t)))
#+curses-use-wide (defcfun ins_nwstr :int
  "Insert the first N characters of a wide character string."
  (wstr (:pointer wchar-t)) (n :int))
#+curses-use-wide (defcfun wins_wstr :int
  "Insert a wide character string in a window."
  (win window-ptr) (wstr (:pointer wchar-t)))
#+curses-use-wide (defcfun wins_nwstr :int
  "Insert the first N characters of a wide character string in a window."
  (win window-ptr) (wstr (:pointer wchar-t)) (n :int))
#+curses-use-wide (defcfun mvins_wstr :int
  "Move and insert a wide character string."
  (y :int) (x :int) (wstr (:pointer wchar-t)))
#+curses-use-wide (defcfun mvins_nwstr :int
  "Move and insert the first N characters of a wide character string."
  (y :int) (x :int) (wstr (:pointer wchar-t)) (n :int))
#+curses-use-wide (defcfun mvwins_wstr :int
  "Move and insert a wide character string in a window."
  (win window-ptr) (y :int) (x :int) (wstr (:pointer wchar-t)))
#+curses-use-wide (defcfun mvwins_nwstr :int
  "Move and insert the first N characters of a wide character string in a
window."
  (win window-ptr) (y :int) (x :int) (wstr (:pointer wchar-t)) (n :int))

;; Deleting
(defcfun delch :int
  "Delete a character.")
(defcfun wdelch :int
  "Delete a character in the window."
  (win window-ptr))
(defcfun mvdelch :int
  "Move and delete a character."
  (y :int) (x :int))
(defcfun mvwdelch :int
  "Move and delete a character in the window."
  (win window-ptr) (y :int) (x :int))
(defcfun deleteln :int
  "Delete a line.")
(defcfun wdeleteln :int
  "Delete a line in the window."
  (win window-ptr))
(defcfun insdelln :int
  "Insert or delete N lines. Positive N inserts, and negative N deletes."
  (n :int))
(defcfun winsdelln :int
  "Insert or delete N lines, in the window. Positive N inserts, and negative
N deletes."
  (win window-ptr) (n :int))
(defcfun insertln :int
  "Insert a blank line above the cursor. The bottom line is lost.")
(defcfun winsertln :int
  "Insert a blank line, in the window, above the cursor. The bottom line
is lost."
  (win window-ptr))

;;
;; Terminal modes
;;
(defcfun resetty :int
  "Restore the terminal modes to the last call to savetty.")
(defcfun savetty :int
  "Save the terminal modes for restoring by resetty.")
(defcfun def-prog-mode :void
  "Save the current terminal modes as the 'program' mode, for use by
reset-prog-mode. Normally done automatically.")
(defcfun def-shell-mode :void
  "Save the current terminal modes as the 'shell' mode, for use by
reset-shell-mode. Normally done by initscr.")
(defcfun reset-shell-mode :int
  "Reset the terminal modes to the 'shell', or out of curses, mode. Normally
done by endwin.")
(defcfun reset-prog-mode :int
  "Reset the terminal modes to the 'program', or in curses, mode. Normally
done by doupdate after an endwin.")
(defcfun cbreak :int
  "Disable line buffering and erase and kill characters.")
(defcfun nocbreak :int
  "Put the terminal in cooked mode where lines are buffered and and erase and
kill characters are processed.")
(defcfun echo :int
  "Put the terminal in echo mode, where input characters are printed.")
(defcfun noecho :int
  "Put the terminal in noecho mode, where input characters are NOT printed.")
(defcfun raw :int
  "Put the terminal in raw mode, where normally interrupting characters are
passed through as input.")
(defcfun noraw :int
  "Put the terminal in NON-raw mode, where interrupt characters are processed
as normal.")
(defcfun qiflush :void
  "Flush queued input on interrupt.")
(defcfun noqiflush :void "Don't flushed queued input on interrupt.")
(defcfun meta :int
  "If BF is true input 8 bits, otherwise input 7 bits. Also send smm and rmm
terminfo capabilities, to ask the terminal to enabe or disable sending meta
bits."
  (win window-ptr) (bf bool))
(defcfun nodelay :int
  "If BF is true, causes getch to be non-blocking, so that if no input is read
it returns ERR. Otherwise getch waits for input."
  (win window-ptr) (bf bool))
(defcfun halfdelay :int
  "Put the terminal in half-delay mode which is like cbreak mode but after
TENTHS tenths of second, ERR is returned if nothing has been typed. nocbreak
will leave half-delay mode."
  (tenths :int))
(defcfun intrflush :int
  "If BF is true, when an interrupt key is pressed, all output in the tty
driver queue will be flushed, giving faster response to an interrupt, but
probably messing up the screen."
  (win window-ptr) (bf bool))
(defcfun notimeout :int
  "If BF is true, don't time out in escape sequences to differentiate between
special keys and user input."
  (win window-ptr) (bf bool))
(defcfun timeout :void
  "Set read timeout. Negative DELAY waits indefinitely. Zero DELAY is
non-blocking. Positive waits for DELAY milliseconds. ERR is returned if no input
is available."
  (delay :int))
(defcfun wtimeout :void
  "Set read timeout for a window. Negative DELAY waits indefinitely. Zero DELAY
is non-blocking. Positive waits for DELAY milliseconds. ERR is returned if no
input is available."
  (win window-ptr) (delay :int))
(defcfun typeahead :int
  "Use the file descriptor FD to check for typeahead. If FD is -1 no typeahead
checking is done."
  (fd :int))

(defcfun clearok :int (win window-ptr) (bf bool))
(defcfun idlok :int (win window-ptr) (bf bool))
(defcfun idcok :void (win window-ptr) (bf bool))
(defcfun immedok :void (win window-ptr) (bf bool))
(defcfun leaveok :int (win window-ptr) (bf bool))
(defcfun setscrreg :int (top :int) (bot :int))
(defcfun wsetscrreg :int (win window-ptr) (top :int) (bot :int))
(defcfun scrollok :int (win window-ptr) (bf bool))
(defcfun nonl :int)
(defcfun nl :int)

(defcfun baudrate :int)
(defcfun erasechar :char)
(defcfun killchar :char)
(defcfun has-ic bool)
(defcfun has-il bool)
(defcfun longname :string)
(defcfun termname :string)

;; This is hack so that we can provide a function key table
(defcfun ("keypad" real-keypad) :int
  "The unwrapped version of keypad."
  (win window-ptr) (bf bool))
(defun keypad (w b)
  "Enable special keys to be returned as a code."
  (when (and b (not (= b 0))
	     (not *funkeys*))
    (init-keys))
  (real-keypad w b))

;; Cursor
(defcfun curs-set :int 
  "Set the cursor state indicated by the integer VISIBILITY. 0 for invisible,
1 for normal, 2 for 'very' visible. The terminal might not support any
of these, in which case it returns ERR."
  (visibility :int))
(defcfun getcurx :int
  "Return the current X coordinate of the window WIN."
  (win window-ptr))
(defcfun getcury :int 
  "Return the current Y coordinate of the window WIN."
  (win window-ptr))
(defmacro getyx (win y x)
  "Place the cursor position of the window WIN into the variables Y and X."
  `(setf ,y (getcury ,win)
	 ,x (getcurx ,win)))
;; void getparyx(WINDOW *win, int y, int x);
;; "Place the cursor position of the window WIN into the variables Y and X.
;; If WIN is a subwindow, it gets the coordinates relative to the parent."
;; void getbegyx(WINDOW *win, int y, int x);
;; "If WIN is a subwindow, place it's starting coordinates, relative to the
;; parent, into Y and X."
;; void getmaxyx(WINDOW *win, int y, int x);
;; "If WIN is a subwindow, place it's size into Y and X."


;; Color and attributes
(defcfun start-color :int
  "Enable use of color and initialize color variables.")
(defcfun has-colors bool "True if the terminal has color capability.")
(defcfun can-change-color bool "True if colors can be changed.")
;(defcfun init-pair :int (pair short) (fg short) (bg short))
(defcfun init-pair :int 
  "Initialize a color pair to use foreground color FG and background color BG."
  (pair :int) (fg :int) (bg :int))
(defcfun color-content :int
  "Return the amount of RED, GREEN and BLUE in a color."
  (color :int)
  (red (:pointer :int)) (green (:pointer :int)) (blue (:pointer :int)))
(defcfun pair-content :int
  "Return the foreground FG and background BG for a color pair."
  (pair :int) (fg (:pointer :int)) (bg (:pointer :int)))
(defcfun ("COLOR_PAIR" color-pair) :int
  "Return a video attribute given an initialized color pair N."
  (n :int))
(defcfun attron :int
  "Turns on the given attributes without affecting any others."
  (attrs :int))
(defcfun wattron :int
  "Turns on the given attributes in the window without affecting any others."
  (win window-ptr :in) (attrs :int))
(defcfun attroff :int
  "Turns off the given attributes without affecting any others."
  (attrs :int))
(defcfun wattroff :int
  "Turns off the given attributes in the window without affecting any others."
  (win window-ptr :in) (attrs :int))
(defcfun attrset :int
  "Sets the current attributes of the window to ATTRS."
  (attrs :int))
(defcfun wattrset :int
  "Sets the current attributes of the given window to ATTRS."
  (win window-ptr :in) (attrs :int))
(defcfun color-set :int
  "Sets the current color of the given window to the foreground/background
combination given in COLOR-PAIR-NUMBER. OPT is reserved and should be null."
  (color-pair-number :int) (opt :pointer))
(defcfun wcolor-set :int
  "Sets the current color of the given window to the foreground/background
combination given in COLOR-PAIR-NUMBER. OPT is reserved and should be null."
  (win :pointer :in) (color-pair-number :int) (opt :pointer))

(defcfun standend :int
  "Same as (attron +a-normal+), or it turns off all attributes.")
(defcfun wstandend :int 
  "Same as (wattron +a-normal+), or it turns off all attributes for the window."
  (win window-ptr :in))
(defcfun standout :int
  "Same as (attron +a-standout+), or turns on the best highlighting mode of
the terminal.")
(defcfun wstandout :int
  "Same as (attron +a-standout+), or turn on the best highlighting mode of
the terminal in the window."
  (win window-ptr :in))
(defcfun bkgd :int
  "Set the background to CHTYPE, like bkgdset, and change every character of
the window."
  (ch chtype))
(defcfun wbkgd :int
  "Set the background to CHTYPE, like wbkgdset, and change every character of
the given window."
  (win window-ptr :in) (ch chtype))
(defcfun bkgdset :void
  "Set the window background to the attributes and character given in CHTYPE.
The attributes are or'd with all non-blank characters. The attributes and the
character are combined with blank characters."
  (ch chtype))
(defcfun wbkgdset :void
  "Set the given window background to the attributes and character given in
CHTYPE. The attributes are or'd with all non-blank characters. The attributes
and the character are combined with blank characters."
  (win window-ptr :in) (ch chtype))

;; Boxes and lines
(defcfun border :int
  "Draws a box around the current window, using the given characters for parts
of the box. If any of the characters are zero, use the appropriate ACS
character."
  (left-side chtype) (right-size chtype) (top-side chtype) (bottom-side chtype)
  (top-left chtype) (top-right chtype) (bottom-left chtype)
  (bottom-right chtype))
(defcfun wborder :int
  "Draws a box around the window WIN, using the given characters for parts of
the box. If any of the characters are zero, use the appropriate ACS character."
  (w window-ptr)
  (left-side chtype) (right-size chtype) (top-side chtype) (bottom-side chtype)
  (top-left chtype) (top-right chtype) (bottom-left chtype)
  (bottom-right chtype))
(defcfun box :int
  "Draws a box around the window WIN, with VERCH as the vertical line character,
and HORCH as the horizontal line character, and the other normal ACS characters
for the other parts of the box. If VERCH or HORCH are zero, use the appropriate
ACS character."
  (win window-ptr :in) (verch chtype) (horch chtype))
(defcfun hline :int
  "Draw a horizontal line with character CH of length N. Doesn't move the
cursor."
  (ch chtype) (n :int))
(defcfun whline :int
  "Draw a horizontal line with character CH of length N, in window WIN.
Doesn't move the cursor."
  (win window-ptr) (ch chtype) (n :int))
(defcfun vline :int 
  "Draw a vertical line with character CH of length N. Doesn't move the cursor."
  (ch chtype) (n :int))
(defcfun wvline :int
  "Draw a vertical line with character CH of length N, in window WIN. Doesn't
move the cursor."
  (win window-ptr) (ch chtype) (n :int))
(defcfun mvhline :int
  "Draw a horizontal line with character CH of length N at position [Y X]."
  (y :int) (x :int) (ch chtype) (n :int))
(defcfun mvwhline :int
  "Draw a horizontal line with character CH of length N at position [Y X] in
window WIN."
  (win window-ptr) (y :int) (x :int) (ch chtype) (n :int))
(defcfun mvvline :int
  "Draw a vertical line with character CH of length N at position [Y X]."
  (y :int) (x :int) (ch chtype) (n :int))
(defcfun mvwvline :int
  "Draw a vertical line with character CH of length N at position [Y X] in
window WIN."
  (win window-ptr) (y :int) (x :int) (ch chtype) (n :int))

;; misc
(defcfun napms :int "Sleep for MS milliseconds." (ms :int))

;; ncurses extension
(defcfun is-term-resized bool
  "Return true if resize_term would modify the window structures."
  (lines :int) (columns :int))
(defcfun resize_term :int
  "Resize the current and standard windows to LINES and COLUMNS. This version
differs from resizeterm in that it doesn't change *lines* and *cols* or do
things for the resize signal SIGWINCH handler."
  (lines :int) (columns :int))
(defcfun resizeterm :int
  "Resize the current and standard windows to LINES and COLUMNS."
  (lines :int) (columns :int))

;; utilities
(defcfun unctrl :string
  "Return a printable representation of C, ignoring attributes."
  (c chtype))
(defcfun wunctrl :string
  "Return a printable representation of the wide char C, ignoring attributes."
  (c cchar-t-ptr))
(defcfun keyname :string
  "Returns a string corresponding to the key C. Control characters are shown
as ^X. Characters above 128 are shown as M-X if (meta t) has been called. If
there is no key name, it returns NIL."
  (c :int))
(defcfun key_name :string
  "Like keyname but doesn't print meta chars or something."
  (c wchar-t))
(defcfun filter :void
  "I think this turns off all cursor motion and pretends the screen is one line
high. This is probably mostly useful if you want curses output to be
interspersed with normal program output, like if used as a filter in a command
line. Needs to be called before initscr or newterm.")
(defcfun nofilter :void
  "Reverse the effect of filter. One probably needs to call newterm and/or
initscr again after this.")
(defcfun use-env :void
  "If F is false, don't look at the LINES and COLUMNS environment variables.
Needs to be called before initscr or newterm. Interacts with use-tioctl."
  (f bool))
(defcfun use-tioctl :void
  "If F is true, use operating system calls to determine the terminal size.
Needs to be called before initscr or newterm. Interacts with use-env."
  (f bool))
(defcfun putwin :int
  "Writes out window data from WIN to the FILE, which must be a C stdio FILE *
open for writing."
  (win window-ptr) (file file-ptr))
(defcfun getwin window-ptr
  "Returns a new window initialized with the data from FILE, which was written
by a previous putwin. FILE should be a C stdio FILE * open for reading. If the
terminal or color setup is different, it might look different."
  (file file-ptr))
(defcfun delay-output :int
  "Pause output by MS milliseconds. You probably shouldn't use this since it
might use padding characters, which can eat CPU and I/O bandwidth. If there is
no padding character, it uses napms which is probably better."
  (ms :int))
(defcfun flushinp :int
  "Thow away any typeahead that has been typed by the user but has not yet been
read by the program.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; terminfo

(defcfun tigetflag :int (capname :string))
(defcfun tigetnum :int (capname :string))
;; We have to do some hackery to work around getting a (char *) == -1
(defcfun ("tigetstr" tigetstr-check) :pointer (capname :string))
(defcfun ("tigetstr" tigetstr-internal) :string (capname :string))
(defun tigetstr (cap)
  "Get a terminfo string capability. Return -1 if it's not valid.
   NIL if the terminal doesn't have the capability."
  (let ((check-addr (tigetstr-check cap)))
    (cond
      ((eql nil check-addr)		; null pointer
       nil)
      ;; Get the address as a number and compare it to a pointer sized
      ;; number with the high bit set. This could potentially be processor
      ;; architecture dependant, but should in practice work.
;       ((eql (- (ash 1 (* 8 (size-of-foreign-type :pointer))) 1)
; 	    (pointer-address check-addr))
      ((eql -1 (pointer-address check-addr))
       -1)
      ;; Otherwise it's should be safe to dereference the pointer.
      (t
       (tigetstr-internal cap)))))

;; EOF
