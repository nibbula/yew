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

HOW TO USE:

If :curses-use-wide is in *features*, then use the wide character version
of the library. If :curses-use-ncurses is in *features* then use ncurses
explicitly, instead of whatever plain curses is.

The user should put :curses-use-wide and/or :curses-use-ncurses in *features*
before loading this library, and also load CFFI and set
cffi:*foreign-library-directories* if needed before loading this library.

CL-NCURSES compatibility, turned on by setting :cl-ncurses in features:
Make sure we don't clash with the actual CL-NCURSES.
")
  (:use :cl :cffi)
;  #+sbcl (:shadowing-import-from :sb-ext "TIMEOUT")
  #+sbcl (:shadow "TIMEOUT")
  #+cl-ncurses (:nickname :cl-ncurses)
  (:export
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
   #:+button1-released+ #:+button1-pressed+ #:+button1-clicked+
   #:+button1-double-clicked+ #:+button1-triple-clicked+
   #:+button1-reserved-event+ #:+button2-released+ #:+button2-pressed+
   #:+button2-clicked+ #:+button2-double-clicked+ #:+button2-triple-clicked+
   #:+button2-reserved-event+ #:+button3-released+ #:+button3-pressed+
   #:+button3-clicked+ #:+button3-double-clicked+ #:+button3-triple-clicked+
   #:+button3-reserved-event+ #:+button4-released+ #:+button4-pressed+
   #:+button4-clicked+ #:+button4-double-clicked+ #:+button4-triple-clicked+
   #:+button4-reserved-event+ #:+button-ctrl+ #:+button-shift+ #:+button-alt+
   #:+all-mouse-events+ #:+report-mouse-position+
   ;; mouse event tests
   #:button-release #:button-press #:button-click #:button-double-click
   #:button-triple-click #:button-reserved-event

   ;; functions
   #:initscr #:endwin #:newterm #:delscreen #:set-term
   #:newwin #:delwin #:mvwin
   #:refresh #:wrefresh
   #:move
   #:clear #:wclear #:erase #:werase #:clrtobot #:wclrtobot
   #:clrtoeol #:wclrtoeol
   ;; input
   #:getch #:wgetch #:mvgetch #:mvwgetch #:ungetch
   #:getnstr #:wgetnstr #:mvgetnstr #:mvwgetnstr
   #:mmask-t #:mevent #:make-mevent #:mevent-id #:mevent-x #:mevent-y
   #:mevent-z #:mevent-bstate
   #:mousemask #:getmouse #:ungetmouse
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
   ;; color & attributes
   #:start-color #:has-colors #:can-change-color #:init-pair
   #:attron #:wattron #:attroff #:wattroff #:attrset #:wattrset #:color-set
   #:wcolor-set #:standend #:wstandend #:standout #:wstandout #:bkgd #:wbkgd
   #:bkgdset #:wbkgdset
   #:color-pair
   #:border #:wborder #:box
   #:napms
   #:is_term_resized #:resize_term #:resizeterm
   ;; terminfo
   #:tigetstr #:tigetflag #:tigetnum
   ;; test functions
   #:test #:test-wchar
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
;(defctype chtype     :unsigned-long)
(defctype chtype     :unsigned-int)
;(defctype bool       :unsigned-int)
(defctype bool       :unsigned-char)
(defctype wchar-t    :int)
(defctype attr-t     :unsigned-long)	; same as chtype
(defctype window-ptr :pointer)		; (WINDOW *)
(defctype screen-ptr :pointer)		; (SCREEN *)
;; This really should agree with whatever the calling code does for FILE *
(defctype file-ptr   :pointer)		; (FILE *)

(defcstruct cchar-t
  (attr attr-t)
  (chars wchar-t :count 5))
(defctype cchar-t-ptr (:pointer (:struct cchar-t))) ; (cchar-t *)

(defcvar ("COLS"	*cols*)		:int)
(defcvar ("LINES"	*lines*)	:int)
(defcvar ("COLORS"	*colors*)	:int)
(defcvar ("COLOR_PAIRS"	*color-pairs*)	:int)
#-(or cygwin win32) (defcvar ("acs_map"	private-acs-map) :pointer)
#+(or cygwin win32) (defcfun ("_nc_acs_map" acs-map-func) :pointer)
(defcvar ("stdscr"	*stdscr*)	window-ptr)

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

;; mouse event masks
(defconstant +BUTTON1-RELEASED+        #o000000000001)
(defconstant +BUTTON1-PRESSED+         #o000000000002)
(defconstant +BUTTON1-CLICKED+         #o000000000004)
(defconstant +BUTTON1-DOUBLE-CLICKED+  #o000000000010)
(defconstant +BUTTON1-TRIPLE-CLICKED+  #o000000000020)
(defconstant +BUTTON1-RESERVED-EVENT+  #o000000000040)
(defconstant +BUTTON2-RELEASED+        #o000000000100)
(defconstant +BUTTON2-PRESSED+         #o000000000200)
(defconstant +BUTTON2-CLICKED+         #o000000000400)
(defconstant +BUTTON2-DOUBLE-CLICKED+  #o000000001000)
(defconstant +BUTTON2-TRIPLE-CLICKED+  #o000000002000)
(defconstant +BUTTON2-RESERVED-EVENT+  #o000000004000)
(defconstant +BUTTON3-RELEASED+        #o000000010000)
(defconstant +BUTTON3-PRESSED+         #o000000020000)
(defconstant +BUTTON3-CLICKED+         #o000000040000)
(defconstant +BUTTON3-DOUBLE-CLICKED+  #o000000100000)
(defconstant +BUTTON3-TRIPLE-CLICKED+  #o000000200000)
(defconstant +BUTTON3-RESERVED-EVENT+  #o000000400000)
(defconstant +BUTTON4-RELEASED+        #o000001000000)
(defconstant +BUTTON4-PRESSED+         #o000002000000)
(defconstant +BUTTON4-CLICKED+         #o000004000000)
(defconstant +BUTTON4-DOUBLE-CLICKED+  #o000010000000)
(defconstant +BUTTON4-TRIPLE-CLICKED+  #o000020000000)
(defconstant +BUTTON4-RESERVED-EVENT+  #o000040000000)
(defconstant +BUTTON-CTRL+             #o000100000000)
(defconstant +BUTTON-SHIFT+            #o000200000000)
(defconstant +BUTTON-ALT+              #o000400000000)
(defconstant +ALL-MOUSE-EVENTS+        #o000777777777)
(defconstant +REPORT-MOUSE-POSITION+   #o001000000000)

(defmacro mouse-event-mask (e x c)
  `(not (= 0 (logand ,e (ash ,c (* 6 (- ,x 1)))))))
(defun BUTTON-RELEASE	     (e x) (mouse-event-mask e x #o001))
(defun BUTTON-PRESS	     (e x) (mouse-event-mask e x #o002))
(defun BUTTON-CLICK	     (e x) (mouse-event-mask e x #o004))
(defun BUTTON-DOUBLE-CLICK   (e x) (mouse-event-mask e x #o010))
(defun BUTTON-TRIPLE-CLICK   (e x) (mouse-event-mask e x #o020))
(defun BUTTON-RESERVED-EVENT (e x) (mouse-event-mask e x #o040))

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
  (nlines :int) (nclose :int) (begin-y :int) (begin-x :int))
(defcfun delwin :int
  (win window-ptr :in))
(defcfun mvwin :int
  (win window-ptr :in) (y :int) (x :int))

;; Update
(defcfun refresh :int)
(defcfun wrefresh :int (w window-ptr))

;; Movement
(defcfun move :int (y :int) (x :int))

;; Clearing and eraseing
(defcfun clear :int)
(defcfun wclear :int (w window-ptr :in))
(defcfun erase :int)
(defcfun werase :int (w window-ptr :in))
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
#-cffi (defcfun mousemask mmask-t (newmask mmask-t) (oldmask :pointer))
#+cffi (defcfun mousemask mmask-t (newmask mmask-t) (oldmask :pointer))
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
(defcstruct %mevent
  (id :short)
  (x :int) (y :int) (z :int)
  (bstate mmask-t))
(defstruct mevent id x y z bstate)
(defcfun ("getmouse" %getmouse) :int (event :pointer))
(defun getmouse (mev)
  (with-foreign-object (fmev '(:struct %mevent))
    (%getmouse fmev)
    (with-foreign-slots ((id x y z bstate) fmev (:struct %mevent))
      (setf (mevent-id mev) id)
      (setf (mevent-x mev) x)
      (setf (mevent-y mev) y)
      (setf (mevent-z mev) z)
      (setf (mevent-bstate mev) bstate))))

(defcfun ungetmouse :int (event :pointer))

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
  "Insert the character CH before the character under the cursor. Characters to the right are moved, and possibly lost."
  (ch chtype))
(defcfun winsch :int
  "Insert the character CH before the character under the cursor, in the window WIN. Characters to the right are moved, and possibly lost."
  (win window-ptr) (ch chtype))
(defcfun mvinsch :int
  "Move to Y, X and insert the character CH before the character under the cursor. Characters to the right are moved, and possibly lost."
  (y :int) (x :int) (ch chtype))
(defcfun mvwinsch :int
  "Move to Y, X and insert the character CH before the character under the cursor, in the window WIN. Characters to the right are moved, and possibly lost."
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
  "Move and insert the first N characters of a wide character string in a window."
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
  "Set read timeout. Negative DELAY waits indefinitely. Zero DELAY is non-blocking. Positive waits for DELAY milliseconds. ERR is returned if no input is available."
  (delay :int))
(defcfun wtimeout :void
  "Set read timeout for a window. Negative DELAY waits indefinitely. Zero DELAY is non-blocking. Positive waits for DELAY milliseconds. ERR is returned if no input is available."
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
;; "Place the cursor position of the window WIN into the variables Y and X. If WIN is a subwindow, it gets the coordinates relative to the parent."
;; void getbegyx(WINDOW *win, int y, int x);
;; "If WIN is a subwindow, place it's starting coordinates, relative to the parent, into Y and X."
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
(defcfun attron :int (attrs :int))
(defcfun wattron :int (win window-ptr :in) (attrs :int))
(defcfun attroff :int (attrs :int))
(defcfun wattroff :int (win window-ptr :in) (attrs :int))
(defcfun attrset :int (attrs :int))
(defcfun wattrset :int (win window-ptr :in) (attrs :int))
(defcfun color-set :int (color-pair-number :int) (opt :pointer))
(defcfun wcolor-set :int
  (win :pointer :in) (color-pair-number :int) (opt :pointer))

(defcfun standend :int)
(defcfun wstandend :int (win window-ptr :in))
(defcfun standout :int)
(defcfun wstandout :int (win window-ptr :in))
(defcfun bkgd :int (ch chtype))
(defcfun wbkgd :int (win window-ptr :in) (ch chtype))
(defcfun bkgdset :void (ch chtype))
(defcfun wbkgdset :void (win window-ptr :in) (ch chtype))

;; Boxes and lines
(defcfun border :int
  (ls chtype) (rs chtype) (ts chtype) (bs chtype)
  (tl chtype) (tr chtype) (lt chtype) (br chtype))
(defcfun wborder :int
  (w window-ptr)
  (ls chtype) (rs chtype) (ts chtype) (bs chtype)
  (tl chtype) (tr chtype) (lt chtype) (br chtype))
(defcfun box :int
  (win window-ptr :in) (verch chtype) (horch chtype))

;; misc
(defcfun napms :int "Sleep for MS milliseconds." (ms :int))

;; ncurses extension
(defcfun is_term_resized bool (lines :int) (columns :int))
(defcfun resize_term :int (lines :int) (columns :int))
(defcfun resizeterm :int (lines :int) (columns :int))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test functions

(defun test (&key device term-type)
  (declare (type (or string null) device term-type))
  (let (screen)
    (if device
	(progn
	  (let (fin fout)
;	    (if (not (setq fin (fopen device "r")))
	    (if (null-pointer-p (setf fin (nos:fopen device "r")))
		(error "Can't open curses input device"))
;	    (if (not (setq fout (fopen device "w")))
	    (if (null-pointer-p (setf fout (nos:fopen device "w")))
		(error "Can't open curses output device"))
	    (if (null-pointer-p (setf screen (newterm term-type fout fin)))
		(error "Can't initialize curses terminal"))
	    (set-term screen)))
	(progn
	  (initscr)))
    (start-color)
    (cbreak)
    (noecho)
    (nonl)
    (typeahead -1)
    (clear)

    (let ((NCOLORS 8)
	  (has-color (= (has-colors) 1))
	  (normal-color-pairs (min *COLOR-PAIRS* 64)))

      ;; Describe the color setup
      (if (not has-color)
	  (addstr "!"))
      (addstr (format nil "has_colors~%"))
      (if (= (can-change-color) 0)
	  (addstr "!"))
      (addstr (format nil "can_change_color~%"))
      (addstr (format nil "COLOR_PAIRS = ~d~%" *COLOR-PAIRS*))

      ;; Initialize all the normal color pairs
      (if has-color
	  (prog ((pair 0))
	     (loop :for fg :from (- NCOLORS 1) :downto 0 :by 1 :do
		(loop :for bg :from 0 :below NCOLORS :by 1 :do
		   (if (> pair 0) ;; Pair 0 defaults to WHITE on BLACK
		       (init-pair pair fg bg))
		   (setq pair (+ pair 1))))))

      ;; Show the color pairs
      (addstr (format nil "Color pairs~%"))
      (loop :for i :from 0 :below normal-color-pairs :by 1 :do
	    (if (= (mod i 8) 0)
		(addch (char-code #\newline)))
	    (attron (COLOR-PAIR i))
	    (addstr (format nil "~2d " i))
	    (attroff (COLOR-PAIR i)))

      (addch (char-code #\newline))
      (refresh)

      ;; Show the color pairs with bold on
      (addstr (format nil "Bold color pairs~%"))
      (loop :for i :from 0 :below normal-color-pairs :by 1 :do
	    (if (= (mod i 8) 0)
		(addch (char-code #\newline)))
	    (attron (COLOR-PAIR i))
	    (attron +a-bold+)
	    (addstr (format nil "~2d " i))
	    (attroff (COLOR-PAIR i)))
      (attroff +a-bold+)
      (refresh)

      ;; Show the color pairs with bold and reverse on
      (let ((y 3))
	(move y 30)
	(addstr (format nil "Bold and reverse color pairs~%"))
	(loop :for i :from 0 :below normal-color-pairs :by 1 :do
	      (when (= (mod i 8) 0)
		(move (incf y) 30))
	      (attron (COLOR-PAIR i))
	      (attron +a-bold+)
	      (attron +a-reverse+)
	      (addstr (format nil "~2d " i))
	      (attroff (COLOR-PAIR i))))
      (attroff +a-bold+)
      (attroff +a-reverse+)
      (refresh)
      (getch)

      (when (> *color-pairs* 64)
	)
      
      ;; Alternate character set
      (clear)
      (let ((alt-chars
	     `((ulcorner      . ,(acs-ulcorner))
	       (llcorner      . ,(acs-llcorner))
	       (urcorner      . ,(acs-urcorner))
	       (lrcorner      . ,(acs-lrcorner))
	       (ltee	      . ,(acs-ltee))
	       (rtee	      . ,(acs-rtee))
	       (btee	      . ,(acs-btee))
	       (ttee	      . ,(acs-ttee))
	       (hline	      . ,(acs-hline))
	       (vline	      . ,(acs-vline))
	       (plus	      . ,(acs-plus))
	       (s1	      . ,(acs-s1))
	       (s9	      . ,(acs-s9))
	       (diamond	      . ,(acs-diamond))
	       (ckboard	      . ,(acs-ckboard))
	       (degree	      . ,(acs-degree))
	       (plminus	      . ,(acs-plminus))
	       (bullet	      . ,(acs-bullet))
	       (larrow	      . ,(acs-larrow))
	       (rarrow	      . ,(acs-rarrow))
	       (darrow	      . ,(acs-darrow))
	       (uarrow	      . ,(acs-uarrow))
	       (board	      . ,(acs-board))
	       (lantern	      . ,(acs-lantern))
	       (block	      . ,(acs-block))
	       (s3	      . ,(acs-s3))
	       (s7	      . ,(acs-s7))
	       (lequal	      . ,(acs-lequal))
	       (gequal	      . ,(acs-gequal))
	       (pi	      . ,(acs-pi))
	       (nequal	      . ,(acs-nequal))
	       (sterling      . ,(acs-sterling)))))
	(loop :with y = 0 :and x = 0
	   :for (name . c) :in alt-chars :do
	   (mvaddstr y x (string-downcase (princ-to-string name)))
	   (attron +a-altcharset+)
	   (mvaddch y (+ x 11) c)
	   (attroff +a-altcharset+)
	   (incf y)
	   (when (>= y (1- *LINES*))
	     (setf y 0)
	     (incf x 20))))
      (attroff +a-altcharset+)
      (refresh)
      (getch)

;; Nobody does dim
      ;; Show the color pairs with dim
;       (let ((y 13))
; 	(move y 30)
; 	(addstr (format nil "Dim color pairs~%"))
; 	(loop :for i :from 0 :below *COLOR-PAIRS* :by 1 :do
; 	      (when (= (mod i 8) 0)
; 		(move (incf y) 30))
; 	      (attron (COLOR-PAIR i))
; 	      (attron A-DIM)
; 	      (addstr (format nil "~2d " i))
; 	      (attroff (COLOR-PAIR i))))
;       (attroff A-DIM)
;       (refresh)
;       (getch)

      ;; Do borders and windows
      (border 0 0 0 0 0 0 0 0)
      (refresh)

      (loop :for i :from 1 :below (/ *LINES* 2) :by 1 :do
	    (let ((w (newwin (- *LINES* (* 2 i))
			     (- *COLS* (* 2 i)) i i)))
	      (wborder w 0 0 0 0 0 0 0 0)
	      (wrefresh w)
	      (refresh)
;	      (when (= (getch) (char-code #\q))
;		(return))
	      (sleep .1)
	      (delwin w)))
      (refresh)

      ;; Clear stuff out
      (getch)
      (let ((w (newwin (- *LINES*  2) (- *COLS* 2) 1 1)))
	(wclear w)
	(wrefresh w))

      ;; Random color blast
      (let* ((y (+ 1 (random (- *LINES* 2))))
	     (x (+ 1 (random (- *COLS*  2))))
	     (c (if (= 0 (random 2)) #\* #\space))
	     (color (if has-color (random *COLOR-PAIRS*) 0)))
	(loop :for i :from 0 :to 4000 :by 1 :do
	      (setq y (+ 1 (random (- *LINES* 2))))
	      (setq x (+ 1 (random (- *COLS* 2))))
	      (setq c (if (= 0 (logand (random #x9000) #x1000)) #\* #\space))
	      (if has-color
		  (progn
		    (setq color (random *COLOR-PAIRS*))
		    (attron (COLOR-PAIR color))))
	      (mvaddch y x (char-code c))
	      (refresh)
	      (if has-color
		  (attroff (COLOR-PAIR color)))))
      (move (- *LINES* 1) 0)
      (getch)

      (reset-shell-mode)
      (endwin)
      (when screen
	(delscreen screen))))

  (princ "Done.")
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-colors ()
  ;; Initialize all the color pairs
  (start-color)
  (let ((ncolors 8))
    (if (= (has-colors) 1)
    	(prog ((pair 0))
	   (loop :for fg :from (- ncolors 1) :downto 0 :by 1 :do
	      (loop :for bg :from 0 :below ncolors :by 1 :do
		 (if (> pair 0) ;; Pair 0 defaults to WHITE on BLACK
		     (init-pair pair fg bg))
		 (setq pair (+ pair 1)))))))
  (bkgd (color-pair 0)))

#+curses-use-wide
(defun test-wchar-1 ()
  (clear)
  (let ((chars
	 #+(or sbcl clisp cmu)
	 `(#\black_left-pointing_triangle
	   #\black_smiling_face
	   #\black_right-pointing_triangle
	   ,(code-char 0))
	 #+(or ecl ccl)
	 `(#\u25C0
	   #\u263B
	   #\u25B6
	   ,(code-char 0))
	 ))
    (with-foreign-object (str :int (length chars))
      (loop :with i = 0 :for c :in chars :do
	 (setf (mem-aref str :int i) (char-code c))
	 (incf i))
      (setf (mem-aref str :int (1- (length chars))) 0)
      (move 10 10)
      (addnwstr str (1- (length chars)))))
  (refresh)
  (getch))

#+curses-use-wide
(defun wa (wide-string)
  (with-foreign-object (fstr :int (1+ (length wide-string)))
      (loop :with i = 0 :for c :across wide-string :do
	 (setf (mem-aref fstr :int i) (char-code c))
	 (incf i))
      (setf (mem-aref fstr :int (length wide-string)) 0)
      (addnwstr fstr (length wide-string))))

#+curses-use-wide
(defparameter *block*
  '("░░░░░░░░"
    "░▒▒▒▒▒▒░"
    "░▒▓▓▓▓▒░"
    "░▒▓██▓▒░"
    "░▒▓▓▓▓▒░"
    "░▒▒▒▒▒▒░"
    "░░░░░░░░"))

#+curses-use-wide
(defun draw-block (y x)
  (move y x)
  (loop :with i = y
     :for line :in *block* :do
     (wa line)
     (move (incf i) x))
  (refresh))

#+curses-use-wide
(defstruct blook
  color
  x y
  xinc yinc)

#+curses-use-wide
(defun show-blook (b)
  (attron (color-pair (blook-color b)))
  (draw-block (blook-y b) (blook-x b))
  (attroff (color-pair (blook-color b))))

#+curses-use-wide
(defun test-wchar-2 ()
  (clear)
  (let ((blocks
	 (list
	  (make-blook :color 40 :y 10 :x 20 :xinc  1 :yinc 1)
	  (make-blook :color 16 :y 10 :x 30 :xinc  1 :yinc 0)
	  (make-blook :color 24 :y 20 :x 20 :xinc -1 :yinc 0)
	  (make-blook :color 48 :y 20 :x 40 :xinc -1 :yinc -1)))
	(t-o 50) xi yi)
    (flet ((new-blook ()
	     (loop :do (setf xi (- (random 3) 1) yi (- (random 3) 1))
		:while (and (= xi 0) (= yi 0)))
	     (push (make-blook :color (* (random 7) 8)
			       :y (random *lines*) :xinc xi
			       :x (random *cols*)  :yinc yi) blocks)))
      (timeout t-o)
      (loop :for i :from 1 :to 2000 :do
	 (erase)
	 (move 0 0) (wa "Ｙｅｓ， ｙｏｕ ｈａｖｅ ＷＩＤＥ ｃｈａｒｓ！")
	 (loop :for b :in blocks :do
	    (show-blook b)
	    (incf (blook-x b) (blook-xinc b))
	    (incf (blook-y b) (blook-yinc b))
	    (when (<= (blook-x b) 0)		 (setf (blook-xinc b) 1))
	    (when (>= (blook-x b) (- *cols* 8))	 (setf (blook-xinc b) -1))
	    (when (<= (blook-y b) 0)	    	 (setf (blook-yinc b) 1))
	    (when (>= (blook-y b) (- *lines* 7)) (setf (blook-yinc b) -1)))
	 (let* ((c (getch)) (cc (if (>= c 0) (code-char c) nil)))
	   (case cc
	     (#\q (return))
	     (#\n (new-blook))
	     (#\- (timeout (decf t-o 1)))
	     (#\+ (timeout (incf t-o 1))))))))
  (timeout -1))

;; On some implementations, such as CCL and ECL you need to call setlocale
;; before this will work.
#+curses-use-wide
(defun test-wchar ()
  (initscr)
  (cbreak)
  (noecho)
  (nonl)
  (typeahead -1)
  (init-colors)
  (test-wchar-1)
  (test-wchar-2)
  (endwin))

;; EOF
