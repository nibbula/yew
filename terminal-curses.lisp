;;
;; terminal-curses.lisp - Curses terminal
;;

(defpackage :terminal-curses
  (:documentation "Curses terminal")
  (:use :cl :terminal :curses :fui)
  (:export
   #:terminal-curses-stream
   #:terminal-curses
   ))
(in-package :terminal-curses)

;; @@@ Does this even make sense?
(defclass terminal-curses-stream (terminal-stream)
  ()
  (:documentation
   "Terminal as purely a Lisp output stream. This can't do input or things that
require terminal driver support."))

(defclass terminal-curses (terminal)
  ((screen
    :initarg :screen :accessor screen :initform nil
    :documentation "The curses screen.")
   (device
    :initarg :device :accessor device :initform nil
    :documentation "The device for the terminal.")
   (term-type
    :initarg :term-type :accessor term-type :initform nil
    :documentation "The type name of the terminal.")
   (in-fp
    :initarg :in-fp :accessor in-fp :initform nil
    :documentation "The input FILE pointer for the terminal.")
   (out-fp
    :initarg :out-fp :accessor out-fp :initform nil
    :documentation "The output FILE pointer for the terminal."))
  (:default-initargs
  )
  (:documentation "A terminal using the curses library."))

(defmethod terminal-default-device-name ((type (eql 'terminal-curses)))
  "Return the default device name for a TERMINAL-CURSES."
  ;; This is silly.
  "stdscr")

(defmethod initialize-instance
    :after ((o terminal-curses) &rest initargs &key &allow-other-keys)
  "Initialize a terminal-curses."
  (declare (ignore initargs))
  (with-slots (device term-type in-fp out-fp screen) o
    (when (not term-type)
      (setf term-type (nos:environment-variable "TERM")))
    (when (and (slot-boundp o 'device) device)
      (when (cffi:null-pointer-p (setf in-fp (nos:fopen device "r")))
	(error "Can't open curses input device ~a" device))
      (when (cffi:null-pointer-p (setf out-fp (nos:fopen device "w")))
	(error "Can't open curses output device ~a" device))
      (when (cffi:null-pointer-p
	     (setf screen (newterm term-type out-fp in-fp)))
	(error "Can't initialize curses terminal ~a" term-type)))))

(defmethod terminal-get-size ((tty terminal-curses))
  "Get the window size from the kernel and store it in tty."
    (setf (terminal-window-rows tty) curses:*lines*
	  (terminal-window-columns tty) curses:*cols*))

;; This isn't really accurate if any output has been done not through curses,
;; so it's not as useful as the one in terminal-ansi.

(defmethod terminal-get-cursor-position ((tty terminal-curses))
  "Try to somehow get the row of the screen the cursor is on."
  (values (getcury (screen tty)) (getcurx (screen tty))))

;; Just for debugging
; (defun terminal-report-size ()
;   (let ((tty (line-editor-terminal *line-editor*)))
;     (terminal-get-size tty)
;     (with-slots (window-rows window-columns) tty
;       (format t "[~d x ~d]~%" window-columns window-rows))))

(defmethod terminal-start ((tty terminal-curses))
  "Set up the terminal for reading a character at a time without echoing."
  (when (not (device tty))		; already done
    (initscr)
    (setf (screen tty) *stdscr*))
  (noecho)
  (nonl)
  (cbreak)
  (meta curses:*stdscr* 1)
  (keypad curses:*stdscr* 1)
  (typeahead -1)
  (start-color)
  ;; additional resets that wouldn't need to be done on a fresh application
  (attrset 0)
  (bkgd 0)
  (idlok curses:*stdscr* 0)
  (leaveok curses:*stdscr* 0)
  (scrollok curses:*stdscr* 0)
  (curs-set 1)
  (init-colors)
  (terminal-get-size tty))

(defmethod terminal-end ((tty terminal-curses))
  "Put the terminal back to the way it was before we called terminal-start."
;;;  (format t "[terminal-end]~%")
  (endwin))

(defmethod terminal-done ((tty terminal-curses))
  "Forget about the whole terminal thing and stuff."
  (terminal-end tty)
  (with-slots (device screen in-fp out-fp) tty
    (when device
      (delscreen screen)
      (nos:fclose out-fp)
      (nos:fclose in-fp)))
  (values))

(defmethod terminal-format ((tty terminal-curses) fmt &rest args)
  "Output a formatted string to the terminal."
  (let ((string (apply #'format nil fmt args)))
    (addstr string)))

(defmethod terminal-write-string ((tty terminal-curses) str)
  "Output a string to the terminal."
  (addstr str))

(defmethod terminal-write-char ((tty terminal-curses) char)
  "Output a character to the terminal."
  (addch (char-code char)))

(defmethod terminal-move-to ((tty terminal-curses) row col)
  (move row col))

(defmethod terminal-move-to-col ((tty terminal-curses) col)
  (move (getcury (screen tty)) col))

(defmethod terminal-beginning-of-line ((tty terminal-curses))
  (terminal-move-to-col tty 0))

(defmethod terminal-del-char ((tty terminal-curses) n)
  (dotimes (i n)
    (delch)))

(defmethod terminal-ins-char ((tty terminal-curses) n)
  (dotimes (i n)
    (insch (char-code #\space))))

(defmethod terminal-backward ((tty terminal-curses) n)
  (move (getcury (screen tty)) (- (getcurx (screen tty)) n)))

(defmethod terminal-forward ((tty terminal-curses) n)
  (move (getcury (screen tty)) (+ (getcurx (screen tty)) n)))

(defmethod terminal-up ((tty terminal-curses) n)
  (move (- (getcury (screen tty)) n) (getcurx (screen tty))))

(defmethod terminal-down ((tty terminal-curses) n)
  (move (+ (getcury (screen tty)) n) (getcurx (screen tty))))

(defmethod terminal-scroll-down ((tty terminal-curses) n)
  (when (> n 0)
    (scrl n)))
  
(defmethod terminal-erase-to-eol ((tty terminal-curses))
  (clrtoeol))

(defmethod terminal-erase-line ((tty terminal-curses))
  (let ((x (getcurx (screen tty)))
	(y (getcury (screen tty))))
    (move y 0)
    (clrtoeol)
    (move y x)))

(defmethod terminal-erase-above ((tty terminal-curses))
  (let ((x (getcurx (screen tty)))
	(y (getcury (screen tty))))
    (loop :for i :from 0 :below y :do
       (move i 0)
       (clrtoeol))
    (when (> x 0)
      (mvaddstr y 0 (format nil "~va" x #\space)))
    (move y x)))

(defmethod terminal-erase-below ((tty terminal-curses))
  (clrtobot))

(defmethod terminal-clear ((tty terminal-curses))
  (clear))

(defmethod terminal-home ((tty terminal-curses))
  (move 0 0))

(defmethod terminal-cursor-off ((tty terminal-curses))
  (curs-set 0))

(defmethod terminal-cursor-on ((tty terminal-curses))
  (curs-set 1))

(defmethod terminal-standout ((tty terminal-curses) state)
  (if state
      (attron +a-standout+)
      (attroff +a-standout+)))

(defmethod terminal-normal ((tty terminal-curses))
  (attrset +a-normal+))

(defmethod terminal-underline ((tty terminal-curses) state)
  (if state
      (attron +a-underline+)
      (attroff +a-underline+)))

(defmethod terminal-bold ((tty terminal-curses) state)
  (if state
      (attron +a-bold+)
      (attroff +a-bold+)))

(defmethod terminal-inverse ((tty terminal-curses) state)
  (if state
      (attron +a-reverse+)
      (attroff +a-reverse+)))

(defmethod terminal-color ((tty terminal-curses) fg bg)
  ;; This defaulting is bullcrap. But so is curses defaulting.
  ;; See man default_colors.
  (when (eq fg :default)
    (setf fg :white))
  (when (eq bg :default)
    (setf bg :black))
  (when (not (color-number fg))
    (error "Forground ~a is not a known color." fg))
  (when (not (color-number bg))
    (error "Background ~a is not a known color." bg))
  (color-set (fui:color-index
	      (or (color-number fg) +color-white+)
	      (or (color-number bg) +color-black+))
	     (cffi:null-pointer)))

;; 256 color? ^[[ 38;5;color <-fg 48;5;color <- bg
;; set color tab = ^[] Ps ; Pt BEL
;;;  4; color-number ; #rrggbb ala XParseColor

(defmethod terminal-beep ((tty terminal-curses))
  (beep))

(defmethod terminal-set-scrolling-region ((tty terminal-curses) start end)
  (if (and (not start) (not end))
      ;; Is this sensible? Or should we just unset 'scrollok'?
      (setscrreg 0 (1- *lines*))
      (progn
	(scrollok *stdscr* 1)
	(setscrreg start end))))

(defmethod terminal-finish-output ((tty terminal-curses))
  (refresh))

; (defmethod terminal-get-row ((tty terminal-ansi))
;   (let ((string (format nil "~a[R" #\escape))
; 	(stream (terminal-output-stream tty)))
;     (write-string string stream)
;     (finish-output stream)
;   (with-foreign-object (c :unsigned-char)
;     (let ((status (posix-read (terminal-file-descriptor tty) c 1)))
;       (cond
; 	((< status 0)
; 	 (error "Read error ~d~%" status))
; 	((= status 0)
; 	 nil)
; 	((= status 1)
; 	 (code-char (mem-ref c :unsigned-char)))))))

(defmethod terminal-get-char ((tty terminal-curses))
  "Read a character from the terminal."
  (get-char))

(defmethod terminal-get-key ((tty terminal-curses))
  "Read a character from the terminal."
  (get-char))

(defmethod terminal-listen-for ((tty terminal-curses) seconds)
  (let (c)
    (unwind-protect
	 (progn
	   (curses::timeout (round (* seconds 1000)))
	   (setf c (getch))
	   (when (not (equal c +ERR+))
	     (ungetch c)))
      ;; This assumes timeout was already -1. Since there's no prescribed way to
      ;; get it, the caller has to reset it after this if they want it to be
      ;; different.
      (curses::timeout -1))
    c))

(defmethod terminal-reset ((tty terminal-curses))
  "Try to reset the terminal to a sane state, without being too disruptive."
  (reset-shell-mode)) ; or something..

(defvar *saved-positions* nil
  "List of conses of saved positions, e.g. (x . y).")

(defmethod terminal-save-cursor ((tty terminal-curses))
  "Save the cursor position."
  ;; @@@ some thread safe incantation
  (let ((spot (cons (getcury (screen tty)) (getcurx (screen tty)))))
    (push spot *saved-positions*)))

(defmethod terminal-restore-cursor ((tty terminal-curses))
  "Restore the cursor position, from the last saved postion."
  (let ((bunkle (pop *saved-positions*)))
    (move (cdr bunkle) (car bunkle))))

(register-terminal-type :curses 'terminal-curses)

;; EOF
