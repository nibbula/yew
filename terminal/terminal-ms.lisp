;;;
;;; terminal-ms.lisp - Microsoft console as a terminal.
;;;

(declaim #.`(optimize ,.(getf terminal-config::*config* :optimization-settings)))

;;(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
;;(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))

(defpackage :terminal-ms
  (:documentation "Microsoft console as a terminal.")
  (:use :cl :cffi :dlib :dlib-misc :terminal :char-util :opsys :os-ms
	:dgray :fatchar :dcolor :terminal-crunch)
  (:export
   #:terminal-ms
   ;; Extras:
   #:set-cursor-size
   #:set-default-bold
   #:set-standout-use-color
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
    :initarg :bold :accessor bold :initform t :type boolean
    :documentation "True to make colors bold.")
   ;; (underline
   ;;  :initarg :underline :accessor underline :initform nil :type boolean
   ;;  :documentation "True to make characters underlined.")
   (default-bold
    :initarg :default-bold :accessor default-bold :initform t :type boolean
    :documentation "True to use bright colors by default.")
   (inverse
    :initarg :inverse :accessor inverse :initform nil :type boolean
    :documentation "True to make colors inverse.")
   (standout
    :initarg :standout :accessor standout :initform nil :type boolean
    :documentation "True to make text be very noticeable.")
   (saved-attrs
    :initarg :saved-attrs :accessor saved-attrs :initform nil
    :documentation "Saved attributes for color standout.")
   (standout-use-color
    :initarg :standout-use-color :accessor standout-use-color
    :initform t :type boolean
    :documentation
    "True to use colors for standout mode, otherwise it's just inverse.")
   (delay-scroll
    :initarg :delay-scroll :accessor delay-scroll :initform nil :type boolean
    :documentation
    "True to delay scrolling until the next character is output."))
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
	    ;; @@@ FIXME!: this -1 is totally terrible hack until I can figure
	    ;; out how to work around the auto-newline at the last col thing!!
	    ;;(terminal-window-columns tty) (1- cols)))))
	    (terminal-window-columns tty) cols))))

(defmethod terminal-get-cursor-position ((tty terminal-ms))
  "Get the position of the cursor. Returns the two values ROW and COLUMN."
  (multiple-value-bind (x y width height attr top)
      (get-console-info (terminal-file-descriptor tty))
    (declare (ignore width height attr))
    (values (- y top) x)))

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

    ;; (set-terminal-mode file-descriptor :line nil :echo nil)
    #| @@@ trying to find the problem
    (wos::%set-console-mode (wos::ms-term-in-handle file-descriptor) 0)

    (cffi:with-foreign-object (ms-mode 'wos::DWORD)
      (let ((result (wos::%get-console-mode
		     (wos::ms-term-in-handle file-descriptor) ms-mode)))
        (format t "get-console-mode = ~s mode = #x~x in-handle = ~s~%"
	        result (cffi:mem-ref ms-mode 'wos::DWORD)
		(wos::ms-term-in-handle file-descriptor))))
    (format t "full starting terminal mode ~s~%"
	    (wos:get-terminal-mode file-descriptor))
    (finish-output)
    |#

    ;; @@@ This isn't entirely applicable to us. Maybe remove it?
    (when (not output-stream)
      (setf output-stream (open-terminal
			   (or device-name *default-device-name*)
			   :output))
      ;; @@@ Why do we have to do this?
      ;; #+ccl (setf (stream-external-format output-stream)
      ;; 		  (ccl:make-external-format :character-encoding :utf-8
      ;; 					    :domain :file))
      )
      ;; (dbug "terminal-ms open out~%"))
    (terminal-get-size tty)

    ;; @@@ Temporary until we can put it in, say, lishrc
    (set-cursor-size tty 100)
    saved-mode))

(defmethod terminal-end ((tty terminal-ms) &optional state)
  "Put the terminal back to the way it was before we called terminal-start."
  ;;  (format t "[terminal-end]~%")
  ;; (set-terminal-mode (terminal-file-descriptor tty)
  ;; 		     :line t :echo t :raw nil :timeout nil)
  (when (or state (saved-mode tty))
    (dbugf 'terminal-ms "restoring terminal modes ~s ~s~%"
	   tty (or state (saved-mode tty)))
    (set-terminal-mode (terminal-file-descriptor tty)
		       :mode (or state (saved-mode tty)))))

(defmethod terminal-done ((tty terminal-ms) &optional state)
  "Forget about the whole terminal thing and stuff."
  (terminal-end tty state)
  (close-terminal (terminal-file-descriptor tty))
  ;; (dbug "terminal-ms close in~%")
  (when (terminal-output-stream tty)
    (close-terminal (terminal-output-stream tty)))
  ;; (dbug "terminal-ms close out~%")
  ;; (format t "[terminal-done]~%")
  ;; (setf *tty* nil)
  (values))

(defmethod terminal-reinitialize ((tty terminal-ms))
  "Do any re-initialization necessary, and return the saved state."
  (with-slots ((file-descriptor terminal::file-descriptor)) tty
    (let ((current-mode (get-terminal-mode file-descriptor)))
      (when (or (terminal-mode-line current-mode)
		(terminal-mode-echo current-mode))
	(set-terminal-mode file-descriptor :line nil :echo nil))
      (terminal-get-size tty)
      ;; Return the terminal's saved state.
      ;; (saved-mode tty)
      ;; Return the current mode
      ;; @@@ should we just get rid of the saved mode in the terminal??
      current-mode)))

(defmethod terminal-device-time ((tty terminal-ms))
  "Return the last time the terminal device was modified, or NIL if we don't
know."
  ;; (opsys:terminal-time (terminal-file-descriptor tty))
  nil ;; @@@
  )

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

;; (defmacro with-immediate ((tty) &body body)
;;   (with-unique-names (mode)
;;     `(let ((,mode (get-terminal-mode (terminal-file-descriptor ,tty))))
;;        (unwind-protect
;; 	    (progn
;; 	      (set-terminal-mode (terminal-file-descriptor ,tty)
;; 				 :line nil :echo nil)
;; 	      ,@body)
;; 	 (set-terminal-mode (terminal-file-descriptor ,tty) :mode ,mode)))))

(defun bottom-right-p (tty &optional (x-offset 0))
  (multiple-value-bind (x y width height attr top)
      (get-console-info (terminal-file-descriptor tty))
    (let ((rx (+ x x-offset))
	  (ry (- y top)))
      (dbugf :msterm "bottom-right-p ~s ~s ~s ~s ~s~%"
	     ry height
	     rx width
	     (and (= ry (1- height))
		  (>= rx width)))
      (values
       (and (= ry (1- height))
	    (>= rx width)
	    t)
       x y attr))))

(defun delayed-scroll (tty)
  (when (delay-scroll tty)
    (dbugf :msterm "delay scroll triggered~%")
    (setf (delay-scroll tty) nil)
    (when (bottom-right-p tty)
      ;; (scroll-one-line)
      (write-terminal-string (terminal-file-descriptor tty)
			     #.(char-code #\newline)))))

(defun %write-terminal-string (tty string)
  (delayed-scroll tty)
  (let ((display-length (char-util:display-length string)))
    (multiple-value-bind (br-p x y attr)
	(bottom-right-p tty display-length)
      (if br-p
	  (progn
	    (dbugf :msterm "delay scroll string activated~%")
	    (setf (delay-scroll tty) t)
	    ;; @@@ should use graphemes not chars!
	    (when (> display-length 1)
	      (write-terminal-string (terminal-file-descriptor tty)
				     (subseq string 0 (1- (length string))))
	      (incf x (1- display-length)))
	    (dbugf :msterm "delay scroll char ~s @ ~s ~s~%"
		   (char string (1- (length string))) x y)
	    (fill-console-char (terminal-file-descriptor tty)
			       :char (char string (1- (length string)))
			       :x x :y y :length 1)
	    (fill-console-attribute (terminal-file-descriptor tty)
			       :attribute attr
			       :x x :y y :length 1))
	  (write-terminal-string (terminal-file-descriptor tty) string)))))

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
    (%write-terminal-string tty actual-string)))

(defmethod terminal-write-char ((tty terminal-ms) char)
  "Output a character to the terminal."
  (%write-terminal-string tty (string char)))

(defmethod terminal-write-line ((tty terminal-ms) str &key start end)
  "Output a string to the terminal, followed by a newline."
  (terminal-write-string tty str :start start :end end)
  (terminal-write-char tty #\newline))

(defparameter *line-table*
  `#(,#\space
     ,(code-char #x2577) ;; #\box_drawings_light_down)                     ╷
     ,(code-char #x2576) ;; #\box_drawings_light_right)                    ╶
     ,(code-char #x250c) ;; #\box_drawings_light_down_and_right)           ┌
     ,(code-char #x2575) ;; #\box_drawings_light_up)                       ╵
     ,(code-char #x2502) ;; #\box_drawings_light_vertical)                 │
     ,(code-char #x2514) ;; #\box_drawings_light_up_and_right)             └
     ,(code-char #x251c) ;; #\box_drawings_light_vertical_and_right)       ├
     ,(code-char #x2574) ;; #\box_drawings_light_left)                     ╴
     ,(code-char #x2510) ;; #\box_drawings_light_down_and_left)            ┐
     ,(code-char #x2500) ;; #\box_drawings_light_horizontal)               ─
     ,(code-char #x252c) ;; #\box_drawings_light_down_and_horizontal)      ┬
     ,(code-char #x2518) ;; #\box_drawings_light_up_and_left)              ┘
     ,(code-char #x2524) ;; #\box_drawings_light_vertical_and_left)        ┤
     ,(code-char #x2534) ;; #\box_drawings_light_up_and_horizontal)        ┴
     ,(code-char #x253c) ;; #\box_drawings_light_vertical_and_horizontal)  ┼
     )
  "Line drawing characters from Unicode.")

(defun line-char (line)
  "Convert line bits into line drawing characters."
  (aref *line-table* line))

(defun %terminal-write-char (tty char &key reset)
  "Output a fat character to the terminal."
  (with-slots ((cc fatchar::c)
	       (fg fatchar::fg)
	       (bg fatchar::bg)
	       (line fatchar::line)
	       (attrs fatchar::attrs)) char
    (let ((c cc))
      ;; (if (or fg bg attrs)
      ;; 	  (progn
      ;; 	    (when (or fg bg)
      ;; 	      (terminal-color tty (or fg :default) (or bg :default)))
      ;; 	    (when attrs
      ;; 	      (terminal-set-attributes tty attrs)))
      ;; 	  (when reset
      ;; 	    (terminal-normal tty)))
      (terminal-color tty (or fg :default) (or bg :default))
      (terminal-set-attributes tty attrs)
      (when (not (zerop line))
	(setf c (line-char line)))
      (%write-terminal-string tty (string c))
      (when reset
	(terminal-normal tty)))))

(defmethod terminal-write-char ((tty terminal-ms) (char fatchar))
  (%terminal-write-char tty char :reset t))

(defmethod terminal-write-string ((tty terminal-ms) (str fat-string)
				  &key start end)
  "Output a fat string to the terminal."
  (when (and (not (and (and start (zerop start)) (and end (zerop end))))
	     (fat-string-string str))
    (let ((fs (fat-string-string str)))
      (loop
	 :with i = (or start 0)
	 :and our-end = (or end (length fs))
	 :and c :and last-c
	 :while (< i our-end)
	 :do
	 (setf c (aref fs i))
	 (with-slots ((cc fatchar::c)
		      (line fatchar::line)) c
	   (if (and last-c (same-effects c last-c))
	       (if (zerop line)
		   (terminal-write-char tty cc)
	       	   (terminal-write-char tty (line-char line)))
	       (progn
		 (%terminal-write-char tty c :reset nil)))
	   (setf last-c c))
	 (incf i))
      (terminal-normal tty))))

(defmethod terminal-newline ((tty terminal-ms))
  (terminal-write-char tty #\newline))

(defmethod terminal-fresh-line ((tty terminal-ms))
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (col _) (get-cursor-position fd)
      (declare (ignore _))
      (when (not (zerop col))
	(terminal-write-char tty #\newline)
	(terminal-write-char tty #\return)
	t))))

(defmethod terminal-move-to ((tty terminal-ms) row col)
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (_col _row  width height attr top)
	(get-console-info fd)
      (declare (ignore _col _row attr))
      (set-cursor-position fd (+ top (min row height))
			   (min col width)))))

(defmethod terminal-move-to-col ((tty terminal-ms) col)
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (_ row) (get-cursor-position fd)
      (declare (ignore _))
      (set-cursor-position fd row col))))

(defmethod terminal-beginning-of-line ((tty terminal-ms))
  (terminal-move-to-col tty 0))

(defmethod terminal-delete-char ((tty terminal-ms) &optional (n 1))
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (x y width height) (get-console-info fd)
      (declare (ignore height))
      (scroll-console fd
		      :left (+ x n) :top y
		      :right width :bottom y
		      :x x :y y))))

(defmethod terminal-insert-char ((tty terminal-ms) &optional (n 1))
  "Insert N blanks."
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (x y width height) (get-console-info fd)
      (declare (ignore height))
      (scroll-console fd
		      :left (1+ x) :top y
		      ;;:right (- width n) :bottom y ;; @@@ maybe need (1+ y) ??
		      :right (- width n) :bottom (1+ y)
		      :x (+ x n) :y y))))

(defmethod terminal-delete-line ((tty terminal-ms) &optional (n 1))
  (with-slots ((fd terminal::file-descriptor)) tty
    (when (> n 0)
      (multiple-value-bind (col row width height attr top)
	  (get-console-info fd)
	(declare (ignore attr col top))
	;; @@@ Maybe if the n is bigger than rest of the screen we can just
	;; clear? also applies to insert-line below
	(scroll-console fd
			:left 0 :right width
			:top (min (+ row n) (1- height))
			:bottom (1- height)
			:x 0
			:y row)))))

(defmethod terminal-insert-line ((tty terminal-ms) &optional (n 1))
  (with-slots ((fd terminal::file-descriptor)) tty
    (when (> n 0)
      (multiple-value-bind (col row width height attr top)
	  (get-console-info fd)
	(declare (ignore attr col top))
	(scroll-console fd
			:left 0 :right width
			:top row
			:bottom (max (- (1- height) n) row)
			:x 0
			:y (min (+ row n) (1- height)))))))

(defun move-offset (tty offset-x offset-y)
  "Move the cursor to the offset, clamped to the current screen."
  ;; (dbugf :ms "move-offset tty = ~s ~s ~s~%" tty offset-x offset-y)
  (when (not (and (zerop offset-x) (zerop offset-y)))
    (with-slots ((fd terminal::file-descriptor)) tty
      (multiple-value-bind (x y width height attr top) (get-console-info fd)
	(declare (ignore attr))
	(set-cursor-position fd
			     ;; (max top (min (+ y offset-y) (+ top height)))
			     ;; (max 0 (min (+ x offset-x) width)))))))
			     (max top (min (+ y offset-y)
					   (+ top (1- height))))
			     (max 0 (min (+ x offset-x) (1- width))))))))

(defmethod terminal-backward ((tty terminal-ms) &optional (n 1))
  (move-offset tty (- n) 0))

(defmethod terminal-forward ((tty terminal-ms) &optional (n 1))
  (move-offset tty n 0))

(defmethod terminal-up ((tty terminal-ms) &optional (n 1))
  (move-offset tty 0 (- n)))

(defmethod terminal-down ((tty terminal-ms) &optional (n 1))
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
			      :bottom (- (+ top height) n)
			      :x 0 :y top)
	      (set-cursor-position fd (+ row n) col))))))

(defmethod terminal-scroll-up ((tty terminal-ms) n)
  (with-slots ((fd terminal::file-descriptor)) tty
    (if (> n 0)
	(multiple-value-bind (col row width height attr top)
	    (get-console-info fd)
	  (declare (ignore attr))
	  (if (< (- row n) 0)
	      ;; @@@@ This would be right, but it's wrong.
	      (scroll-console fd
			      :left 0 :top top :right width
			      :bottom (- (+ top height) n)
			      :x 0 :y (+ top n))
	      (set-cursor-position fd (- row n) col))))))

(defun erase (fd &key x y length)
  (fill-console-char fd :x x :y y :length length)
  (fill-console-attribute fd :x x :y y :length length))

(defmethod terminal-erase-to-eol ((tty terminal-ms))
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (x y width) (get-console-info fd)
      (erase fd :x x :y y :length (- width x)))))

(defmethod terminal-erase-line ((tty terminal-ms))
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (x y width) (get-console-info fd)
      (declare (ignore x))
      (erase fd :x 0 :y y :length width))))

(defmethod terminal-erase-above ((tty terminal-ms))
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (x y width attr top) (get-console-info fd)
      (declare (ignore attr))
      (erase fd :x 0 :y top :length (+ (* (- y top) width) x)))))

(defmethod terminal-erase-below ((tty terminal-ms))
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (x y) (get-cursor-position fd)
      (erase fd :x x :y y))))

(defmethod terminal-clear ((tty terminal-ms) &key saved-p)
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (col row width height attr top)
	(get-console-info fd)
      (declare (ignore col row width height attr))
      (erase fd :x 0 :y (if saved-p 0 top)))))

(defmethod terminal-home ((tty terminal-ms))
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (col row width height attr top)
	(get-console-info fd)
      (declare (ignore col row width height attr))
      (set-cursor-position fd top 0))))

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
  (with-slots ((fd terminal::file-descriptor) standout inverse
	       standout-use-color saved-attrs) tty
    (when (not (eq state standout))
      (if state
	  (progn
	    (if standout-use-color
		(progn
		  (setf saved-attrs (get-attributes fd))
		  (set-console-attribute
		   fd (logior +BACKGROUND-INTENSITY+
			      (color-attr :black :yellow))))
		(%terminal-inverse tty t)))
	  (progn
	    (if standout-use-color
		(progn
		  (set-console-attribute fd saved-attrs)
		  ;; If inverse was on and we're turning off standout,
		  ;; effectively turn on inverse.
		  (when inverse
		    (%terminal-inverse tty t))
		  (setf saved-attrs nil))
		(%terminal-inverse tty nil))))
      (setf standout state))))

(defmethod terminal-normal ((tty terminal-ms))
  (terminal-standout tty nil)
  (terminal-inverse tty nil)
  (terminal-bold tty (if (default-bold tty) t nil))
  (terminal-color tty :default :default))

(defmethod terminal-underline ((tty terminal-ms) state)
  (declare (ignore tty state))
  ;; (with-slots (underline (fd terminal::file-descriptor)) tty
  ;;   (setf underline state)
  ;;   (let ((attr (get-attributes fd)))
  ;;     (set-console-attribute fd
  ;; 			     (if state
  ;; 				 (logior attr #x8000)
  ;; 				 (logand
  ;; 				  attr
  ;; 				  (lognot
  ;; 				   (logior #x8000)))))))
  )

(defmethod terminal-bold ((tty terminal-ms) state)
  (with-slots (bold (fd terminal::file-descriptor)) tty
    (setf bold state)
    (let ((attr (get-attributes fd)))
      (set-console-attribute fd
			     (if state
				 (logior attr
					 +FOREGROUND-INTENSITY+
					 #| +BACKGROUND-INTENSITY+ |#)
				 (logand
				  attr
				  (lognot
				   (logior +FOREGROUND-INTENSITY+
					   #| +BACKGROUND-INTENSITY+ |#))))))))

(defun %terminal-inverse (tty state)
  (with-slots (inverse (fd terminal::file-descriptor)) tty
    (when (not (eq state inverse))
      (let ((attr (get-attributes fd))
	    (new-attr 0))

	;; We do it this stupid way to avoid making assumptions about the
	;; color constants.
	(when (plusp (logand attr +FOREGROUND-BLUE+))
	  (setf new-attr (logior new-attr +BACKGROUND-BLUE+)))
	(when (plusp (logand attr +FOREGROUND-GREEN+))
	  (setf new-attr (logior new-attr +BACKGROUND-GREEN+)))
	(when (plusp (logand attr +FOREGROUND-RED+))
	  (setf new-attr (logior new-attr +BACKGROUND-RED+)))
	(when (plusp (logand attr +FOREGROUND-INTENSITY+))
	  (setf new-attr (logior new-attr +BACKGROUND-INTENSITY+)))

	(when (plusp (logand attr +BACKGROUND-BLUE+))
	  (setf new-attr (logior new-attr +FOREGROUND-BLUE+)))
	(when (plusp (logand attr +BACKGROUND-GREEN+))
	  (setf new-attr (logior new-attr +FOREGROUND-GREEN+)))
	(when (plusp (logand attr +BACKGROUND-RED+))
	  (setf new-attr (logior new-attr +FOREGROUND-RED+)))
	(when (plusp (logand attr +BACKGROUND-INTENSITY+))
	  (setf new-attr (logior new-attr +FOREGROUND-INTENSITY+)))

	(set-console-attribute fd new-attr)))))

(defmethod terminal-inverse ((tty terminal-ms) state)
  (when (not (standout tty))		; standout overrides inverse
    (%terminal-inverse tty state))
  (setf (inverse tty) state))

(defparameter *fg-colors*
  `(:black	0
    :red	,+FOREGROUND-RED+
    :green	,+FOREGROUND-GREEN+
    :yellow	,(logior +FOREGROUND-RED+ +FOREGROUND-GREEN+)
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
    :yellow	,(logior +BACKGROUND-RED+ +BACKGROUND-GREEN+)
    :blue	,+BACKGROUND-BLUE+
    :magenta	,(logior +BACKGROUND-RED+ +BACKGROUND-BLUE+)
    :cyan	,(logior +BACKGROUND-GREEN+ +BACKGROUND-BLUE+)
    :white	,(logior +BACKGROUND-RED+ +BACKGROUND-GREEN+ +BACKGROUND-BLUE+)
    nil		0
    :default    0
    ))

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
  (with-slots ((fd terminal::file-descriptor) bold inverse standout
	       standout-use-color) tty
    (when (and standout standout-use-color)
      ;; standout using color overrides any color setting!
      (return-from terminal-color nil))
    (when inverse
      (rotatef fg bg))
    (let ((our-fg (getf *fg-colors* fg))
	  (our-bg (getf *bg-colors* bg))
	  (bold-mix (if bold
			(if inverse
			    +BACKGROUND-INTENSITY+
			    +FOREGROUND-INTENSITY+)
			0)))

      (when (and (keywordp fg) (not our-fg))
	(error "Forground ~a is not a known color." fg))
      (when (and (keywordp bg) (not our-bg))
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

(defmethod terminal-colors ((tty terminal-ms))
  (declare (ignore tty))
  ;; @@@ There might be more colors starting in Windows 10?
  16)

(defun foob (tty)
  (with-slots ((fd terminal::file-descriptor)) tty
    (get-console-extended-info fd)))

(defmethod terminal-window-foreground ((tty terminal-ms))
  (declare (ignore tty))
  )

(defmethod (setf terminal-window-foreground) (color (tty terminal-ms))
  (declare (ignore tty color)))

(defmethod terminal-window-background ((tty terminal-ms))
  (declare (ignore tty)))

(defmethod (setf terminal-window-background) (color (tty terminal-ms))
  (declare (ignore tty color)))

(defmethod terminal-beep ((tty terminal-ms))
  (terminal-write-char tty #\bel)) ; Not #\bell!!

(defmethod terminal-set-scrolling-region ((tty terminal-ms) &optional start end)
  (declare (ignore start end))
  ;; @@@ implement me
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

;; This is so small it can just be an alist for now.
(defparameter *ms-keys*
  `((:back	#\backspace)
    (:return	#\return)
    (:tab	#\tab)
    (:escape	#\escape)
    (:space	#\space)
    (:delete	#\rubout)
    (:prior     :page-up)
    (:next      :page-down))
  "Key normalization alist.")

(defun normalize-key (symbol)
  (let ((key (assoc symbol *ms-keys*)))
    (if key
	(second key)
	symbol)))
				       
(defmethod terminal-get-key ((tty terminal-ms))
  (let ((key (read-terminal-byte (terminal-file-descriptor tty))))
    (typecase key
      (integer
       (wos::wchar-to-character key))
      (symbol
       (normalize-key key)))))
 
(defmethod terminal-listen-for ((tty terminal-ms) seconds)
  (listen-for-terminal seconds (terminal-file-descriptor tty)))

(defmethod terminal-input-mode ((tty terminal-ms))
  (let ((mode (get-terminal-mode (terminal-file-descriptor tty))))
    ;; (format t "terminal-mode ~s~%" mode)
    ;; (terminal-mode-line mode)
    (if (terminal-mode-line mode) :line :char)
    ))

(defmethod (setf terminal-input-mode) (mode (tty terminal-ms))
  (case mode
    (:line
     (set-terminal-mode (terminal-file-descriptor tty) :line t :echo t))
    (:char
     (set-terminal-mode (terminal-file-descriptor tty) :line nil :echo nil))
    (t (error "Unknown terminal input mode ~s" mode))))

;; (defmethod terminal-reset ((tty terminal-ms))
;;   "Try to reset the terminal to a sane state, without being too disruptive."
;;   ;; @@@
;;   )

(defmethod terminal-reset ((tty terminal-ms))
  ;; First reset the terminal driver to a sane state.
  (reset-terminal-modes :file-descriptor (terminal-file-descriptor tty))
  ;; (call-next-method)
  ) ;; Do the terminal-stream version

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

(defmethod terminal-title ((tty terminal-ms))
  "Get the title of the console window."
  (get-console-title))

(defmethod (setf terminal-title) (title (tty terminal-ms))
  "Set the title of a console window."
  (set-console-title title))

(defmethod terminal-has-attribute ((tty terminal-ms) attribute)
  "Return true if the terminal can display the character attribute."
  (case attribute
    ((:standout :bold :inverse :color) t)
    (:underline nil)))

(defmethod terminal-has-autowrap-delay ((tty terminal-ms))
  "Return true if the terminal delays automatic wrapping at the end of a line."
  nil)

(defmethod terminal-set-attributes ((tty terminal-ms) attributes)
  "Set the attributes given in the list. If NIL turn off all attributes.
Attributes are usually keywords."
  (if attributes
      (loop :for a :in attributes :do
	 (case a
	   (:standout  (terminal-standout tty t))
	   (:normal    (terminal-normal tty))
	   (:underline (terminal-underline tty t)) ; If we ever get it
	   (:bold      (terminal-bold tty t))
	   (:inverse   (terminal-inverse tty t))
	   ;; Just ignore anything we don't support.
	   ))
      (progn
	(terminal-standout tty nil)
	(terminal-underline tty nil)
	(terminal-bold tty (if (default-bold tty) t nil))
	(terminal-inverse tty nil))))

(defmethod terminal-alternate-characters ((tty terminal-ms) state)
  (declare (ignore state))
  ;; Fine. Just write them. That was simple.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra things:

(defun set-cursor-size (tty size)
  (when (or (< size 0) (> size 100))
    (error "Tried to set cursor size outside the range 0-100 ~d." size))
  (with-slots ((fd terminal::file-descriptor)) tty
    (multiple-value-bind (old-size visible) (get-cursor-info fd)
      (declare (ignore old-size))
      (set-cursor-state fd :size size :visible visible))))

(defun set-default-bold (tty state)
  "True to make the default colors be bold."
  (setf (default-bold tty) state)
  (terminal-bold tty state))

(defun set-standout-use-color (tty state)
  "True to make the default colors be bold."
  (setf (standout-use-color tty) state))

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

;; @@@@ Which stream-advance-to-column is right???
;; (defmethod stream-advance-to-column ((stream terminal-ms) column)
;;   (write-sequence *endless-spaces*
;; 		  (terminal-output-stream stream) :start 0
;; 		  :end (- column (stream-line-column stream)))
;;   t)

;;(defmethod stream-fresh-line ((stream terminal-ms))

;; #+sbcl (defmethod sb-gray:stream-line-length ((stream terminal-ms))
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

;; @@@ I should do some timing tests. These are fake for now.

(defmethod output-cost ((tty terminal-ms) (op (eql :move-to)) &rest params)
  (declare (ignore params))
  2)

(defmethod output-cost ((tty terminal-ms) (op (eql :move-to-col)) &rest params)
  (declare (ignore params))
  1)

(defmethod output-cost ((tty terminal-ms) (op (eql :up)) &rest params)
  (declare (ignore params))
  10)

(defmethod output-cost ((tty terminal-ms) (op (eql :down)) &rest params)
  (declare (ignore params))
  10)

(defmethod output-cost ((tty terminal-ms) (op (eql :backward)) &rest params)
  (declare (ignore params))
  2)

(defmethod output-cost ((tty terminal-ms) (op (eql :forward)) &rest params)
  (declare (ignore params))
  2)

(defmethod output-cost ((tty terminal-ms) (op (eql :color)) &rest params)
  (declare (ignore params))
  10)

(defmethod output-cost ((tty terminal-ms) (op (eql :write-fatchar))
			&rest params)
  (declare (ignore params))
  10)

(defmethod output-cost ((tty terminal-ms) (op (eql :write-fatchar-string))
			&rest params)
  (* 5 (length (first params))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-terminal-type :ms 'terminal-ms)

;; EOF
