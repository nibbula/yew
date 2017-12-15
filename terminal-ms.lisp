;;
;; terminal-ms.lisp - Microsoft console as a terminal.
;;

(defpackage :terminal-ms
  (:documentation "Microsoft console as a terminal.")
  (:use :cl :cffi :dlib :dlib-misc :terminal :char-util :opsys :ms
	:trivial-gray-streams)
  (:export
   #:terminal-ms
   ))
(in-package :terminal-ms)

(defvar *default-device-name* *default-console-device-name*
  "The default device to create a terminal on.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass terminal-ms (terminal)
  ((saved-mode
    :initarg :saved-mode :accessor saved-mode
    :documentation "Saved terminal modes for restoring on exit.")
   (saved-cursor-size
    :initarg :saved-cursor-size
    :initform 100
    :documentation
    "Saved size of the cursor to restore when turning the cursor back on.")
   (saved-cursor-position
    :initarg :saved-cursor-position
    :documentation "Saved position of the cursor.")
   (saved-char :accessor saved-char
    :initarg :unread-char :initform nil :type (or null character)
    :documentation "To support unread-char.")
   (bold
    :initarg :bold :accessor bold :initform nil :type boolean
    :documentation "True to make colors bold.")
   (inverse
    :initarg :inverse :accessor inverse :initform nil :type boolean
    :documentation "True to make colors inverse."))
  (:default-initargs
    :file-descriptor		nil
    :device-name		*default-device-name*
    :output-stream		nil
  )
  (:documentation "What we need to know about terminal device."))

(defmethod terminal-default-device-name ((type (eql 'terminal-ms)))
  "Return the default device name for a TERMINAL-MS."
  *default-device-name*)

(defmethod terminal-get-size ((tty terminal-ms))
  "Get the window size from the kernel and store it in tty."
  (when (terminal-file-descriptor tty)
    (multiple-value-bind (cols rows)
	(get-window-size (terminal-file-descriptor tty))
      (setf (terminal-window-rows tty) rows
	    (terminal-window-columns tty) cols))))

(defmethod terminal-get-cursor-position ((tty terminal-ms))
  "Get the position of the cursor. Returns the two values ROW and COLUMN."
  (multiple-value-bind (x y)
      (get-cursor-position (terminal-file-descriptor tty))
    (values y x)))

(defmethod terminal-start ((tty terminal-ms))
  "Set up the terminal for reading a character at a time without echoing."
  (with-slots ((file-descriptor	   terminal::file-descriptor)
	       (device-name   	   terminal::device-name)
	       (output-stream 	   terminal::output-stream)
	       saved-mode) tty
    (when (not file-descriptor)
      ;; (format t "[terminal-open ~s]~%" device-name)
      (setf file-descriptor (open-terminal (or device-name
					       *default-device-name*)
					   :input)))
    ;; (dbug "terminal-ms open in~%")
    (setf saved-mode (get-terminal-mode file-descriptor))
    (dbugf 'terminal-ms "saving terminal modes ~s ~s~%" tty saved-mode)
    (when (or (terminal-mode-line saved-mode)
	      (terminal-mode-echo saved-mode))
      (set-terminal-mode file-descriptor :line nil :echo nil))
    ;; @@@ This isn't entirely applicable to us. Maybe remove it?
    (when (not output-stream)
      (setf output-stream (open-terminal
			   (or device-name *default-device-name*)
			   :output))
      ;; @@@ Why do we have to do this?
      #+ccl (setf (stream-external-format output-stream)
		  (ccl:make-external-format :character-encoding :utf-8
					    :domain :file))
      )
      ;; (dbug "terminal-ms open out~%"))
    (terminal-get-size tty)))

(defmethod terminal-end ((tty terminal-ms))
  "Put the terminal back to the way it was before we called terminal-start."
  ;;  (format t "[terminal-end]~%")
  ;; (set-terminal-mode (terminal-file-descriptor tty)
  ;; 		     :line t :echo t :raw nil :timeout nil)
  (when (saved-mode tty)
    (dbugf 'terminal-ms "restoring terminal modes ~s ~s~%"
	   tty (saved-mode tty))
    (set-terminal-mode (terminal-file-descriptor tty) :mode (saved-mode tty))))

(defmethod terminal-done ((tty terminal-ms))
  "Forget about the whole terminal thing and stuff."
  (terminal-end tty)
  (close-terminal (terminal-file-descriptor tty))
  ;; (dbug "terminal-ms close in~%")
  (when (terminal-output-stream tty)
    (close-terminal (terminal-output-stream tty)))
  ;; (dbug "terminal-ms close out~%")
  ;; (format t "[terminal-done]~%")
  ;; (setf *tty* nil)
  (values))

(defmethod terminal-format ((tty terminal-ms) fmt &rest args)
  "Output a formatted string to the terminal."
  (terminal-write-string tty (apply #'format tty fmt args)))

;; resumed -> (terminal-start tty) #| (redraw) |# (terminal-finish-output tty)
;; resized -> (terminal-get-size tt)

;; @@@ BROKEN! FIX!
;; (defmacro with-raw ((tty) &body body)
;;   (with-unique-names (mode)
;;     `(let ((,mode (get-terminal-mode ,tty)))
;;        (unwind-protect
;; 	    (progn
;; 	      (set-terminal-mode ,tty :raw t :echo nil)
;; 	      ,@body)
;; 	 (set-terminal-mode ,tty :mode ,mode)))))

(defmacro with-raw ((tty) &body body)
  `(unwind-protect
	(progn
	  (set-terminal-mode ,tty :raw t :echo nil)
	  ,@body)
     (set-terminal-mode ,tty :raw nil)))

(defmacro with-immediate ((tty) &body body)
  (with-unique-names (mode)
    `(let ((,mode (get-terminal-mode (terminal-file-descriptor ,tty))))
       (unwind-protect
	    (progn
	      (set-terminal-mode (terminal-file-descriptor ,tty)
				 :line nil :echo nil)
	      ,@body)
	 (set-terminal-mode (terminal-file-descriptor ,tty) :mode ,mode)))))

(defmethod terminal-write-string ((tty terminal-ms) str
				  &key start end)
  "Output a string to the terminal."
  (let* ((actual-string str)
	 (actual-start (or start 0))
	 (actual-end (or end (length str)))
	 (actual-len (- actual-end actual-start)))
    (when (or start end)
      (setf actual-string (make-array actual-len
				      :element-type (array-element-type str)
				      :displaced-to str
				      :displaced-index-offset actual-start)))
    (write-terminal-string (terminal-file-descriptor tty) actual-string)))

(defmethod terminal-write-char ((tty terminal-ms) char)
  "Output a character to the terminal."
  (write-terminal-string (terminal-file-descriptor tty) (string char)))

(defmethod terminal-move-to ((tty terminal-ms) row col)
  (set-cursor-position (terminal-file-descriptor tty) row col))

(defmethod terminal-move-to-col ((tty terminal-ms) col)
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (_ row) (get-cursor-position fd)
      (declare (ignore _))
      (set-cursor-position fd row col))))

(defmethod terminal-beginning-of-line ((tty terminal-ms))
  (terminal-move-to-col tty 0))

(defmethod terminal-del-char ((tty terminal-ms) n)
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (x y width height) (get-console-info fd)
      (declare (ignore height))
      (scroll-console fd
		      :left (+ x n) :top y
		      :right width :bottom y
		      :x (1+ x) :y y))))

(defmethod terminal-ins-char ((tty terminal-ms) n)
  "Insert N blanks."
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (x y width height) (get-console-info fd)
      (declare (ignore height))
      (scroll-console fd
		      :left (1+ x) :top y
		      :right (- width n) :bottom y
		      :x (+ x n) :y y))))

(defun move-offset (tty offset-x offset-y)
  "Move the cursor to the offset, clamped to the current screen."
  (dbugf :ms "move-offset tty = ~s ~s ~s~%" tty offset-x offset-y)
  (when (not (and (zerop offset-x) (zerop offset-y)))
    (with-slots ((fd terminal::file-descriptor)) tty
      (multiple-value-bind (x y width height) (get-console-info fd)
	(set-cursor-position fd
			     (max 0 (min (+ y offset-y) height))
			     (max 0 (min (+ x offset-x) width)))))))

(defmethod terminal-backward ((tty terminal-ms) n)
  (move-offset tty (- n) 0))

(defmethod terminal-forward ((tty terminal-ms) n)
  (move-offset tty n 0))

(defmethod terminal-up ((tty terminal-ms) n)
  (move-offset tty 0 (- n)))

(defmethod terminal-down ((tty terminal-ms) n)
  (move-offset tty 0 n))

(defmethod terminal-scroll-down ((tty terminal-ms) n)
  (with-slots ((fd terminal::file-descriptor)) tty
    (if (> n 0)
	(multiple-value-bind (col row width height attr top)
	    (get-console-info fd)
	  (declare (ignore attr))
	  (if (> (+ row n) (+ top height))
	      (scroll-console fd
			      :left 0 :top (+ top n) :right width
			      :bottom (- height n)
			      :x 0 :y top)
	      (set-cursor-position fd (+ row n) col))))))

(defmethod terminal-erase-to-eol ((tty terminal-ms))
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (x y width) (get-console-info fd)
      (fill-console-char fd :x x :y y :length (- width x)))))

(defmethod terminal-erase-line ((tty terminal-ms))
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (x y width) (get-console-info fd)
      (declare (ignore x))
      (fill-console-char fd :x 0 :y y :length width))))

(defmethod terminal-erase-above ((tty terminal-ms))
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (x y width) (get-console-info fd)
      (fill-console-char fd :x 0 :y 0 :length (+ (* (1- y) width) x)))))

(defmethod terminal-erase-below ((tty terminal-ms))
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (x y) (get-cursor-position fd)
      (fill-console-char fd :x x :y y))))

(defmethod terminal-clear ((tty terminal-ms))
  (fill-console-char (terminal-file-descriptor tty))
  (fill-console-attribute (terminal-file-descriptor tty)))

(defmethod terminal-home ((tty terminal-ms))
  (set-cursor-position (terminal-file-descriptor tty) 0 0))

(defmethod terminal-cursor-off ((tty terminal-ms))
  (with-slots ((fd terminal::file-descriptor) saved-cursor-size) tty
    (multiple-value-bind (size visible) (get-cursor-info fd)
      (when visible
	(setf saved-cursor-size size)
	(set-cursor-state fd :size size :visible nil)))))

(defmethod terminal-cursor-on ((tty terminal-ms))
  (with-slots ((fd terminal::file-descriptor) saved-cursor-size) tty
    (multiple-value-bind (size visible) (get-cursor-info fd)
      (declare (ignore size))
      (when (not visible)
	(set-cursor-state fd :size saved-cursor-size :visible t)))))

(defmethod terminal-standout ((tty terminal-ms) state)
  (declare (ignore tty state)))

(defmethod terminal-normal ((tty terminal-ms))
  (declare (ignore tty)))

(defmethod terminal-underline ((tty terminal-ms) state)
  (declare (ignore tty state)))

(defmethod terminal-bold ((tty terminal-ms) state)
  (with-slots (bold (fd terminal::file-descriptor)) tty
    (setf bold state)
    (let ((attr (get-attributes fd)))
      (set-console-attribute fd
			     (if state
				 (logior attr
					 +FOREGROUND-INTENSITY+
					 +BACKGROUND-INTENSITY+)
				 (logand
				  attr
				  (lognot
				   (logior +FOREGROUND-INTENSITY+
					   +BACKGROUND-INTENSITY+))))))))

(defmethod terminal-inverse ((tty terminal-ms) state)
  (declare (ignore tty state)))

(defparameter *fg-colors*
  `(:black	0
    :red	,+FOREGROUND-RED+
    :green	,+FOREGROUND-GREEN+
    :yellow	,(logior +FOREGROUND-GREEN+ +FOREGROUND-BLUE+)
    :blue	,+FOREGROUND-BLUE+
    :magenta	,(logior +FOREGROUND-RED+ +FOREGROUND-BLUE+)
    :cyan	,(logior +FOREGROUND-GREEN+ +FOREGROUND-BLUE+)
    :white	,(logior +FOREGROUND-RED+ +FOREGROUND-GREEN+ +FOREGROUND-BLUE+)
    nil		,(logior +FOREGROUND-RED+ +FOREGROUND-GREEN+ +FOREGROUND-BLUE+)
    :default    ,(logior +FOREGROUND-RED+ +FOREGROUND-GREEN+ +FOREGROUND-BLUE+)))

(defparameter *bg-colors*
  `(:black	0
    :red	,+BACKGROUND-RED+
    :green	,+BACKGROUND-GREEN+
    :yellow	,(logior +BACKGROUND-GREEN+ +BACKGROUND-BLUE+)
    :blue	,+BACKGROUND-BLUE+
    :magenta	,(logior +BACKGROUND-RED+ +BACKGROUND-BLUE+)
    :cyan	,(logior +BACKGROUND-GREEN+ +BACKGROUND-BLUE+)
    :white	,(logior +BACKGROUND-RED+ +BACKGROUND-GREEN+ +BACKGROUND-BLUE+)
    nil		,(logior +BACKGROUND-RED+ +BACKGROUND-GREEN+ +BACKGROUND-BLUE+)
    :default    0))

(defun color-attr (fg bg)
  (logior (getf *fg-colors* fg) (getf *bg-colors* bg)))

(defun rgb-color-p (x)
  (and (not (null x))
       (or (consp x) (arrayp x))
       (= (length x) 3)
       (every #'numberp x)))

(defun   color-red   (c) (elt c 0))
(defsetf color-red   (c) (val) `(setf (elt ,c 0) ,val))
(defun   color-green (c) (elt c 1))
(defsetf color-green (c) (val) `(setf (elt ,c 1) ,val))
(defun   color-blue  (c) (elt c 2))
(defsetf color-blue  (c) (val) `(setf (elt ,c 2) ,val))

(defun set-foreground-color (color)
  (declare (ignore color)))

(defun set-background-color (color)
  (declare (ignore color)))

(defmethod terminal-color ((tty terminal-ms) fg bg)
  (with-slots ((fd terminal::file-descriptor) bold) tty
    (let ((our-fg (getf *fg-colors* fg))
	  (our-bg (getf *bg-colors* bg))
	  (bold-mix (if bold +FOREGROUND-INTENSITY+ 0)))
      (when (and (keywordp fg) (not our-fg))
	(error "Forground ~a is not a known color." fg))
      (when (and (keywordp bg) (not our-fg))
	(error "Background ~a is not a known color." bg))
      (cond
	((or (rgb-color-p fg) (rgb-color-p bg))
	  #|
	  (when (rgb-color-p fg)
	    (let ((red   (elt fg 0))
		  (green (elt fg 1))
		  (blue  (elt fg 2)))
	      (error "Sorry, I can't do an R G B triple.")))
	  (when (rgb-color-p bg)
	    (let ((red   (elt bg 0))
		  (green (elt bg 1))
		  (blue  (elt bg 2)))
	      (error "Sorry, I can't do an R G B triple."))))
	  |#
	 (error "Sorry, I can't do an R G B triple."))
	((and our-fg our-bg)
	 (set-console-attribute fd (logior our-fg our-bg bold-mix)))
	(our-fg
	 (set-console-attribute fd (logior our-fg bold-mix)))
	(our-bg
	 (set-console-attribute fd (logior our-bg bold-mix)))
	(t
	 (set-console-attribute fd (getf *fg-colors* :white)))))))

(defmethod terminal-beep ((tty terminal-ms))
  (terminal-write-char tty #\bel)) ; Not #\bell!!

(defmethod terminal-set-scrolling-region ((tty terminal-ms) start end)
  ;; @@@@
  )

(defmethod terminal-finish-output ((tty terminal-ms))
  ;; (finish-output (terminal-output-stream tty))
  )

; (defmethod terminal-get-row ((tty terminal-ms))
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


(defmethod terminal-get-char ((tty terminal-ms))
  "Read a character from the terminal."
  (read-terminal-char (terminal-file-descriptor tty)))

(defmethod terminal-get-key ((tty terminal-ms))
  (read-terminal-char (terminal-file-descriptor tty)))

(defmethod terminal-listen-for ((tty terminal-ms) seconds)
  (listen-for seconds (terminal-file-descriptor tty)))

(defmethod terminal-set-input-mode ((tty terminal-ms) mode)
  (case mode
    (:line
     (set-terminal-mode (terminal-file-descriptor tty) :line t :echo t))
    (:char
     (set-terminal-mode (terminal-file-descriptor tty) :line nil :echo nil))
    (t (error "Unknown terminal input mode ~s" mode))))

(defmethod terminal-reset ((tty terminal-ms))
  "Try to reset the terminal to a sane state, without being too disruptive."
  ;; @@@
  )

(defmethod terminal-reset ((tty terminal-ms))
  ;; First reset the terminal driver to a sane state.
  (reset-terminal-modes (terminal-file-descriptor tty))
  (call-next-method)) ;; Do the terminal-stream version

(defmethod terminal-save-cursor ((tty terminal-ms))
  "Save the cursor position."
  (with-slots (saved-cursor-position) tty
    (multiple-value-bind (x y)
	(get-cursor-position (terminal-file-descriptor tty))
      (setf saved-cursor-position `(,x ,y)))))

(defmethod terminal-restore-cursor ((tty terminal-ms))
  "Restore the cursor position, from the last saved postion."
  (with-slots (saved-cursor-position) tty
    (set-cursor-position (terminal-file-descriptor tty)
			 (second saved-cursor-position)
			 (first saved-cursor-position))))
#|
(defun describe-terminal ()
  "Interrogate the terminal properties and report the results."
  (let (a props)
    ;; Cursor position
    (setf a (query-parameters "?6n"))
    (push `("Cursor position" ,(format nil "~a ~a" (first a) (second a)))
	  props)
    ;; Locator status
    (setf a (query-parameters "?55n"))
    (push `("Locator status"
	    ,(case (first a)
		   (53 "Available")
		   (50 "No locator")
		   (t "Unknown")))
	  props)
    ;; Locator type
    (setf a (query-parameters "?56n"))
    (push `("Locator type"
	    ,(case (second a)
		   (1 "Mouse")
		   (t "Unknown")))
	  props)
    ;; Window state
    (setf a (query-parameters "11t" :offset 2))
    (push `("Window state"
	    ,(if (zerop (length a))
		 "Unavailable"
		 (case (first a)
		   (1 "Open")
		   (2 "Iconified")
		   (t "Unknown"))))
	  props)
    ;; Window position
    (setf a (query-parameters "13t" :offset 2))
    (push `("Window position"
	    ,(if (zerop (length a))
		 "Unavailable"
		 (format nil "~a ~a" (second a) (third a))))
	  props)
    ;; Window size
    (setf a (query-parameters "14t" :offset 2))
    (push `("Window size"
	    ,(if (zerop (length a))
		 "Unavailable"
		 (format nil "~a ~a" (second a) (third a))))
	  props)
    ;; Text size
    (setf a (query-parameters "18t" :offset 2))
    (push `("Text size"
	    ,(if (zerop (length a))
		 "Unavailable"
		 (format nil "~a ~a" (second a) (third a))))
	  props)
    ;; Text screen size
    (setf a (query-parameters "19t" :offset 2))
    (push `("Text screen size"
	    ,(if (zerop (length a))
		 "Unavailable"
		 (format nil "~a ~a" (second a) (third a))))
	  props)
    ;; Icon label
    (setf a (query-string "20t"))
    (push `("Icon label"
	    ,(if (zerop (length a))
		 "Unavailable"
		 a))
	  props)
    ;; Title
    (setf a (query-string "21t"))
    (push `("Title"
	    ,(if (zerop (length a))
		 "Unavailable"
		 a))
	  props)
    ;;
    (setf props (nreverse props))
    (print-properties props)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stream methods

;; common methods

(defmethod-quiet close ((stream terminal-ms) &key abort)
  (declare (ignore abort))
  (terminal-done stream))

;; output stream methods

(defmethod stream-clear-output ((stream terminal-ms))
  (clear-output (terminal-output-stream stream)))

(defmethod stream-finish-output ((stream terminal-ms))
  (terminal-finish-output stream))

(defmethod stream-force-output ((stream terminal-ms))
  (terminal-finish-output stream)
  (force-output (terminal-output-stream stream)))

(defmethod stream-write-sequence ((stream terminal-ms) seq start end
				  &key &allow-other-keys)
  (etypecase seq
    (string
     (terminal-write-string stream seq :start start :end end))
    (list
     (terminal-write-string stream (coerce seq 'string) :start start :end end))))

;; character output stream methods

(defmethod stream-advance-to-column ((stream terminal-ms) column)
  (with-slots ((fd terminal::file-descriptor)) stream
    (multiple-value-bind (col) (get-cursor-position fd)
      (when (> column col)
	(fill-console-char fd :length (- column col)))))
  t)

;; This is a weird trick to presumably make it so we don't have to do our own
;; buffering and we can also be relatively quick?
(defvar *endless-spaces* '#1=(#\space . #1#)
  "The vast emptyness of space.")

(defmethod stream-line-column ((stream terminal-ms))
  (with-slots ((fd terminal::file-descriptor)) stream
    (multiple-value-bind (col _) (get-cursor-position fd)
      (declare (ignore _))
      col)))

(defmethod stream-start-line-p ((stream terminal-ms))
  (zerop (stream-line-column stream)))

(defmethod stream-advance-to-column ((stream terminal-ms) column)
  (write-sequence *endless-spaces*
		  (terminal-output-stream stream) :start 0
		  :end (- column (stream-line-column stream)))
  t)

;;(defmethod stream-fresh-line ((stream terminal-ms))

;; (defmethod stream-line-length ((stream terminal-ms))
;;   )

(defmethod stream-write-char ((stream terminal-ms) char
			     #| &optional start end |#)
  (terminal-write-char stream char))

(defmethod stream-write-string ((stream terminal-ms) string
			       &optional start end)
  (terminal-write-string stream string :start start :end end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stream methods for terminal-ms, which is also an input stream.

(defun possible-saved-char (stream)
  (when (saved-char stream)
    (prog1 (saved-char stream)
      (setf (saved-char stream) nil))))

(defmethod stream-clear-input ((stream terminal-ms))
  (setf (saved-char stream) nil)
  ;; @@@ Should clear the console input too
  )

(defmethod stream-read-sequence ((stream terminal-ms) seq start end
				 &key &allow-other-keys
					#| &optional (start 0) end |#)
  (declare (ignore stream seq start end))
  nil)

;;(defgeneric stream-peek-char ((stream terminal-ms))
  ;; This is used to implement ‘peek-char’; this corresponds to
  ;; ‘peek-type’ of ‘nil’.  It returns either a character or ‘:eof’.
  ;; The default method calls ‘stream-read-char’ and
  ;; ‘stream-unread-char’.
;; )

(defmethod stream-read-char-no-hang ((stream terminal-ms))
  ;; This is used to implement ‘read-char-no-hang’.  It returns either a
  ;; character, or ‘nil’ if no input is currently available, or ‘:eof’
  ;; if end-of-file is reached.  The default method provided by
  ;; ‘fundamental-character-input-stream’ simply calls
  ;; ‘stream-read-char’; this is sufficient for file streams, but
  ;; interactive streams should define their own method.

  ;;(get-char stream :timeout 0)
  ;; @@@@ Really implement!
  (or (possible-saved-char stream)
      (terminal-get-char stream)))

(defmethod stream-read-char ((stream terminal-ms))
  (or (possible-saved-char stream)
      (terminal-get-char stream)))

;; (defmethod stream-read-line ((stream terminal-ms))
;;   ;; This is used by ‘read-line’.  A string is returned as the first
;;   ;; value.  The second value is true if the string was terminated by
;;   ;; end-of-file instead of the end of a line.  The default method uses
;;   ;; repeated calls to ‘stream-read-char’.
;;   )

(defmethod stream-listen ((stream terminal-ms))
  ;; This is used by ‘listen’.  It returns true or false.  The default
  ;; method uses ‘stream-read-char-no-hang’ and ‘stream-unread-char’.
  ;; Most streams should define their own method since it will usually
  ;; be trivial and will always be more efficient than the default
  ;; method.
  (terminal-listen-for stream 0))

(defmethod stream-unread-char ((stream terminal-ms) character)
  ;; Undo the last call to ‘stream-read-char’, as in ‘unread-char’.
  ;; Return ‘nil’.  Every subclass of
  ;; ‘fundamental-character-input-stream’ must define a method for this
  ;; function.
  (setf (saved-char stream) character))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-terminal-type :ms 'terminal-ms)

;; EOF
