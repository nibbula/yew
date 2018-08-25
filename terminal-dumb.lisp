;;
;; terminal-dumb.lisp - Dumb terminal
;;

(defpackage :terminal-dumb
  (:documentation "Dumb terminal")
  (:use :cl :dlib :terminal :char-util :trivial-gray-streams :fatchar)
  (:export
   #:terminal-dumb-stream
   #:terminal-dumb
   ))
(in-package :terminal-dumb)

(defclass terminal-dumb-stream-mixin ()
  ((fake-column
   :initarg :fake-column :accessor terminal-dumb-stream-fake-column
   :initform 0 :type fixnum
   :documentation "Guess for the current column."))
  (:documentation
   "This is just so the inheritance for terminal-dumb doesn't get multiple
terminal-streams."))

;; @@@ Does this even make sense?
(defclass terminal-dumb-stream (terminal-stream terminal-dumb-stream-mixin)
  ()
  (:documentation
   "Terminal as purely a Lisp output stream. This can't do input or things that
require terminal driver support."))

(defclass terminal-dumb (terminal terminal-dumb-stream-mixin)
  ((input-stream
    :accessor terminal-input-stream
    :initarg :input-stream
    :documentation "Lisp stream for input."))
  (:default-initargs
   :output-stream *standard-output*
   :input-stream *standard-input*
   :window-rows 24
   :window-columns 80
   )
  (:documentation "A fake terminal just using standard Lisp streams."))

(defmethod terminal-default-device-name ((type (eql 'terminal-dumb)))
  "Return the default device name for a TERMINAL-DUMB."
  ;; This is silly.
  "dumb")

(defmethod initialize-instance
    :after ((o terminal-dumb) &rest initargs &key &allow-other-keys)
  "Initialize a terminal-dumb."
  (declare (ignore initargs)))

;; We have no idea what the size is.

(defmethod terminal-get-size ((tty terminal-dumb))
  "Get the window size from the kernel and store it in tty."
  (setf (terminal-window-rows tty) 24
	(terminal-window-columns tty) 80))

(defmethod terminal-get-cursor-position ((tty terminal-dumb))
  "Try to somehow get the row of the screen the cursor is on."
  ;;(values nil nil)
  (values (terminal-window-rows tty)
	  (terminal-window-columns tty)))

(defmethod terminal-start ((tty terminal-dumb))
  "Set up the terminal for reading a character at a time without echoing."
  )

(defmethod terminal-end ((tty terminal-dumb) &optional state)
  "Put the terminal back to the way it was before we called terminal-start."
  (declare (ignore state))
  )

(defmethod terminal-done ((tty terminal-dumb) &optional state)
  "Forget about the whole terminal thing and stuff."
  (declare (ignore state))
  (values))

(defmethod terminal-format ((tty terminal-dumb) fmt &rest args)
  "Output a formatted string to the terminal."
  (apply #'format (terminal-output-stream tty)
	 fmt args))

(defmethod terminal-write-string ((tty terminal-dumb) str &key start end)
  "Output a string to the terminal."
  (apply #'write-string `(,str ,(terminal-output-stream tty)
			       ,@(and start `(:start ,start))
			       ,@(and end `(:end ,end)))))

(defmethod terminal-write-line ((tty terminal-dumb) str &key start end)
  "Output a string to the terminal, followed by a newline."
  (apply #'write-line `(,str ,(terminal-output-stream tty)
			     ,@(and start `(:start ,start))
			     ,@(and end `(:end ,end)))))

(defmethod terminal-write-string ((tty terminal-dumb) (str fat-string)
				  &key start end)
  "Output a string to the terminal."
  (apply #'write-string `(,(fat-string-to-string str)
			   ,(terminal-output-stream tty)
			   ,@(and start `(:start ,start))
			   ,@(and end `(:end ,end)))))

(defmethod terminal-write-line ((tty terminal-dumb) (str fat-string)
				&key start end)
  "Output a string to the terminal, followed by a newline."
  (apply #'write-line `(,(fat-string-to-string str)
			 ,(terminal-output-stream tty)
			 ,@(and start `(:start ,start))
			 ,@(and end `(:end ,end)))))

(defmethod terminal-write-char ((tty terminal-dumb) char)
  "Output a character to the terminal."
  (write-char char (terminal-output-stream tty)))

(defmethod terminal-write-char ((tty terminal-dumb) (char fatchar))
  "Output a character to the terminal."
  (write-char (fatchar-c char) (terminal-output-stream tty)))

(defmethod terminal-move-to ((tty terminal-dumb) row col)
  (declare (ignore tty row col)))

(defmethod terminal-move-to-col ((tty terminal-dumb) col)
  (declare (ignore tty col)))

(defmethod terminal-beginning-of-line ((tty terminal-dumb))
  (declare (ignore tty)))

(defmethod terminal-delete-char ((tty terminal-dumb) n) (declare (ignore tty n)))
(defmethod terminal-insert-char ((tty terminal-dumb) n) (declare (ignore tty n)))
(defmethod terminal-backward ((tty terminal-dumb) n)    (declare (ignore tty n)))
(defmethod terminal-forward ((tty terminal-dumb) n)     (declare (ignore tty n)))
(defmethod terminal-up ((tty terminal-dumb) n)          (declare (ignore tty n)))
(defmethod terminal-down ((tty terminal-dumb) n)        (declare (ignore tty n)))
(defmethod terminal-scroll-down ((tty terminal-dumb) n) (declare (ignore tty n)))
(defmethod terminal-erase-to-eol ((tty terminal-dumb))  (declare (ignore tty)))
(defmethod terminal-erase-line ((tty terminal-dumb))	(declare (ignore tty)))
(defmethod terminal-erase-above ((tty terminal-dumb))	(declare (ignore tty)))
(defmethod terminal-erase-below ((tty terminal-dumb))	(declare (ignore tty)))
(defmethod terminal-clear ((tty terminal-dumb))		(declare (ignore tty)))
(defmethod terminal-home ((tty terminal-dumb))		(declare (ignore tty)))
(defmethod terminal-cursor-off ((tty terminal-dumb))	(declare (ignore tty)))
(defmethod terminal-cursor-on ((tty terminal-dumb))	(declare (ignore tty)))

(defmethod terminal-standout ((tty terminal-dumb) state)
  (declare (ignore tty state)))

(defmethod terminal-normal ((tty terminal-dumb))
  (declare (ignore tty)))

(defmethod terminal-underline ((tty terminal-dumb) state)
  (declare (ignore tty state)))

(defmethod terminal-bold ((tty terminal-dumb) state)
  (declare (ignore tty state)))

(defmethod terminal-inverse ((tty terminal-dumb) state)
  (declare (ignore tty state)))

(defmethod terminal-color ((tty terminal-dumb) fg bg)
  (declare (ignore tty fg bg)))

(defmethod terminal-beep ((tty terminal-dumb))
  (declare (ignore tty)))

(defmethod terminal-set-scrolling-region ((tty terminal-dumb) start end)
  (declare (ignore tty start end)))

(defmethod terminal-finish-output ((tty terminal-dumb))
  (finish-output (terminal-output-stream tty)))

(defmethod terminal-get-char ((tty terminal-dumb))
  "Read a character from the terminal."
  ;; This is the best we can do. It probably won't work.
  (let ((str (terminal-input-stream tty)))
    (unread-char (read-char str) str)
    (read-char-no-hang str)))

(defmethod terminal-get-key ((tty terminal-dumb))
  "Read a character from the terminal."
  (terminal-get-char tty))

(defmethod terminal-listen-for ((tty terminal-dumb) seconds)
  (declare (ignore seconds))
  (listen (terminal-input-stream tty)))

(defmethod terminal-input-mode ((tty terminal-dumb))
  (declare (ignore tty))
  :line)

(defmethod (setf terminal-input-mode) (mode (tty terminal-dumb))
  (declare (ignore tty mode)))

(defmethod terminal-reset ((tty terminal-dumb))
  (declare (ignore tty)))
    
(defmethod terminal-save-cursor ((tty terminal-dumb))
  "Save the cursor position."
  (declare (ignore tty)))

(defmethod terminal-restore-cursor ((tty terminal-dumb))
  "Restore the cursor position, from the last saved postion."
  (declare (ignore tty)))

(defmethod terminal-title ((tty terminal-dumb))
  (declare (ignore tty)))

(defmethod (setf terminal-title) (title (tty terminal-dumb))
  "Set the title of a terminal window."
  (declare (ignore title tty)))

(defmethod terminal-has-attribute ((tty terminal-dumb) attribute)
  "Return true if the terminal can display the character attribute."
  (declare (ignore attribute))
  nil)

(defmethod terminal-alternate-characters ((tty terminal-dumb) state)
  (declare (ignore state)))

(defun update-column-for-char (tty char)
  (with-slots (fake-column) tty
    (cond
      ((graphic-char-p char)
       (cond
	 ((combining-char-p char) 0)
	 ((double-wide-char-p char) 2)
	 (t 1)))			;normal case
      (t
       (case char
	 (#\return
	  (setf fake-column 0))
	 (#\tab
	  (incf fake-column (- (1+ (logior 7 fake-column)) fake-column)))
	 (otherwise
	  0 ;; some non-graphic control char?
	  ))))))

(defun update-column (tty thing &key start end)
  (etypecase thing
    (character (update-column-for-char tty thing))
    (string
     (loop
	:with the-end = (or end (length thing))
	:and the-start = (or start 0)
	:for i :from the-start :below the-end
	:do (update-column-for-char tty (char thing i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stream methods

(defmethod-quiet close ((stream terminal-dumb) &key abort)
  (declare (ignore abort))
  (terminal-done stream))

;; output stream methods

(defmethod stream-clear-output ((stream terminal-dumb))
  (clear-output (terminal-output-stream stream)))

(defmethod stream-finish-output ((stream terminal-dumb))
  (terminal-finish-output stream))

(defmethod stream-force-output ((stream terminal-dumb))
  (force-output (terminal-output-stream stream)))

(defmethod stream-write-sequence ((stream terminal-dumb) seq start end
				  &key &allow-other-keys)
  (etypecase seq
    (string
     (terminal-write-string
      (terminal-output-stream stream) seq :start start :end end))
    (list
     (with-slots (output-stream) stream
       (loop :with i = 0 :and l = seq
	  :while (and l (< i end))
	  :do
	    (when (>= i start)
	      (write-char (car l) output-stream)
	      (update-column stream (car l)))
	    (setf l (cdr l))
	    (incf i))))))

;; character output stream methods

;; This is a weird trick to presumably make it so we don't have to do our own
;; buffering and we can also be relatively quick?
(defvar *endless-spaces* '#1=(#\space . #1#)
  "The vast emptyness of space.")

(defmethod stream-line-column ((stream terminal-dumb))
  (terminal-dumb-stream-fake-column stream))

(defmethod stream-start-line-p ((stream terminal-dumb))
  (zerop (stream-line-column stream)))

(defmethod stream-advance-to-column ((stream terminal-dumb) column)
  (write-sequence *endless-spaces*
		  (terminal-output-stream stream) :start 0
		  :end (- column (stream-line-column stream)))
  t)

;;(defmethod stream-fresh-line ((stream terminal-dumb-stream))

;; #+sbcl
;; (defmethod sb-gray:stream-line-length ((stream terminal-dumb-stream))
;;   (declare (ignore stream))
;;   *cols*)

(defmethod stream-write-char ((stream terminal-dumb) char
			     #| &optional start end |#)
  (write-char char (terminal-output-stream stream)))

(defmethod stream-write-string ((stream terminal-dumb) string
			       &optional start end)
  (apply #'write-string `(,string ,(terminal-output-stream stream)
				  ,@(and start `(:start ,start))
				  ,@(and end `(:end ,end))))
  ;; (write-string string
  ;; 		(terminal-output-stream stream)
  ;; 		:start start :end end)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stream methods for terminal-dumb, which is also an input stream.

(defmethod stream-clear-input ((stream terminal-dumb))
  (clear-input (terminal-input-stream stream)))

(defmethod stream-read-sequence ((stream terminal-dumb) seq start end
				 &key &allow-other-keys
					#| &optional (start 0) end |#)
  (declare (ignore seq start end))
  nil)

;;(defgeneric stream-peek-char ((stream terminal-dumb))
  ;; This is used to implement ‘peek-char’; this corresponds to
  ;; ‘peek-type’ of ‘nil’.  It returns either a character or ‘:eof’.
  ;; The default method calls ‘stream-read-char’ and
  ;; ‘stream-unread-char’.
;; )

(defmethod stream-read-char-no-hang ((stream terminal-dumb))
  ;; This is used to implement ‘read-char-no-hang’.  It returns either a
  ;; character, or ‘nil’ if no input is currently available, or ‘:eof’
  ;; if end-of-file is reached.  The default method provided by
  ;; ‘fundamental-character-input-stream’ simply calls
  ;; ‘stream-read-char’; this is sufficient for file streams, but
  ;; interactive streams should define their own method.
  (read-char-no-hang (terminal-input-stream stream)))

(defmethod stream-read-char ((stream terminal-dumb))
  (read-char (terminal-input-stream stream)))

(defmethod stream-read-line ((stream terminal-dumb))
  ;; This is used by ‘read-line’.  A string is returned as the first
  ;; value.  The second value is true if the string was terminated by
  ;; end-of-file instead of the end of a line.  The default method uses
  ;; repeated calls to ‘stream-read-char’.
  (let ((result (read-line (terminal-input-stream stream) nil :eof)))
    (if (eq result :eof)
	(values "" t)
	(values result nil))))

(defmethod stream-listen ((stream terminal-dumb))
  ;; This is used by ‘listen’.  It returns true or false.  The default
  ;; method uses ‘stream-read-char-no-hang’ and ‘stream-unread-char’.
  ;; Most streams should define their own method since it will usually
  ;; be trivial and will always be more efficient than the default
  ;; method.
  (listen (terminal-input-stream stream)))

(defmethod stream-unread-char ((stream terminal-dumb) character)
  ;; Undo the last call to ‘stream-read-char’, as in ‘unread-char’.
  ;; Return ‘nil’.  Every subclass of
  ;; ‘fundamental-character-input-stream’ must define a method for this
  ;; function.
  (unread-char character (terminal-input-stream stream))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-terminal-type :dumb 'terminal-dumb)
;;(register-terminal-type :dumb-stream 'terminal-dumb-stream)

;; EOF
