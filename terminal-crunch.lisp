;;
;; terminal-crunch.lisp - Crunch terminal output.
;;

(defpackage :terminal-crunch
  (:documentation "Crunch terminal output.

This outputs the difference of the end result of what you did to the
terminal, acting like some kind of terminal compression. It allows software
that uses terminal to be simpler, since it doesn't have to worry about
how to effiecntly update the screen. This is similar to techniques used by text
editors, such as Emacs and Vim, and the curses library.

Terminal-crunch acts like just another terminal type, so that software designed
to use terminals, can use it, or write directly to the terminal.
Terminal-crunch wraps around another terminal and does input and output through
it. It is only when input is done or finish-output is called that it figures
out how to update the wrapped terminal.

Other terminal types should help terminal-crunch work by providing cost metrics
for various operations through the OUTPUT-COST methods.
")
  (:use :cl :dlib :char-util :fatchar :terminal :trivial-gray-streams)
  (:export
   #:terminal-crunch
   #:allow-scrolling
   #:output-cost
   ))
(in-package :terminal-crunch)

;; (declaim
;;  (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
(declaim
 (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))

(defstruct screen
  "Representation of the screen."
  (x 0)
  (y 0)
  width
  height
  background
  scrolling-region
  cursor-state
  (beep-count 0)
  lines
  index
  hashes)

(defstruct change
  start-x start-y
  end-x end-y
  op)

(defparameter *blank-char*
  (make-fatchar :c #\space))

(defun blank-char ()
  (copy-fatchar *blank-char*))

(defmacro clamp (n start end)
  `(cond
     ((< ,n ,start) (setf ,n ,start))
     ((> ,n ,end) (setf ,n ,end))))

(defclass terminal-crunch-stream (terminal-stream)
  ((old-screen
    :initarg :old-screen :accessor old-screen :initform nil
    :documentation "The screen that's currently displayed on the device")
   (new-screen
    :initarg :new-screen :accessor new-screen :initform nil
    :documentation "The screen that we're constructing.")
   (wrapped-terminal
    :initarg :wrapped-terminal :accessor wrapped-terminal
    :documentation "The terminal we are wrapping and we output to.")
   (fg
    :initarg :fg :accessor fg :initform nil
    :documentation "Foreground color.")
   (bg
    :initarg :bg :accessor bg :initform nil
    :documentation "Background color.")
   (attrs
    :initarg :attrs :accessor attrs :initform nil
    :documentation "Text attributes.")
   (saved-pos
    :initarg :saved-pos :accessor saved-pos :initform nil
    :documentation "Saved cursor position. A cons cell of X and Y.")
   (allow-scrolling
    :initarg :allow-scrolling :accessor allow-scrolling
    :initform t :type boolean
    :documentation
    "True to allow scrolling when outputing past the bottom of the screen.")
   (update-x
    :initarg :update-x :accessor update-x :initform 0 :type fixnum
    :documentation "Column while we're updating.")
   (update-y
    :initarg :update-y :accessor update-y :initform 0 :type fixnum
    :documentation "Row while we're updating.")
   (cleared
    :initarg :cleared :accessor cleared :initform nil :type boolean
    :documentation
    "True if clear was called and we have to really clear the screen.")
   (started
    :initarg :started :accessor started :initform nil
    :documentation "True if we started and not ended.")
   (start-line
    :initarg :start-line :accessor start-line :initform 0 :type fixnum
    :documentation
    "Line of the screen that we start our manangment on. This can change if we
are directed to move above it, or if we scroll.")
;;    (start-at-current-line
;;     :initarg :start-at-current-line :accessor start-at-current-line
;;     #|:initform nil|# :type boolean
;;     :documentation
;;     "True to set START-LINE to the cursor position when starting the wrapped
;; terminal.")
   (really-scroll-amount
    :initarg :really-scroll-amount :accessor really-scroll-amount
    :initform 0 :type fixnum
    :documentation
    "Amount we should actually scroll on refresh, for scrolling unmanaged
content, when there's a start-line.")
   (delay-scroll
    :initarg :delay-scroll :accessor delay-scroll :initform nil :type boolean
    :documentation "True to delay scrolling until the next character is output.")
   ;; Hints
   (text-change
    :initarg :text-change :accessor text-change :initform nil :type boolean
    :documentation "True if there was text changed.")
   (single-char-change
    :initarg :single-char-change :accessor single-char-change :initform t
    :documentation
    "CHANGE object of a single character change, or NIL if not, T if unknown.")
   (single-line-change
    :initarg :single-line-change :accessor single-line-change :initform t
    :documentation
    "Line number of a single line only change. Or NIL if there was more than one line changed.")
   )
  (:default-initargs
   #| :start-at-current-line nil |#
   )
  (:documentation
   "Terminal output crunching."))

#| @@@ Is there a point in supporting the stream only version?

(defmethod terminal-start ((tty terminal-crunch-stream))
  "This doesn't do anything for a stream."
  (declare (ignore tty)))

(defmethod terminal-end ((tty terminal-crunch-stream) &optional state)
  "Stop using a stream."
  (terminal-finish-output tty))

(defmethod terminal-done ((tty terminal-crunch-stream) &optional state)
  "Forget about the whole terminal stream."
  (terminal-end tty)
  ;; don't close the stream
  (values))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass terminal-crunch (terminal terminal-crunch-stream)
  ()
  (:default-initargs
    :file-descriptor		nil
    :output-stream		nil
    ;;:wrapped-terminal
    )
  (:documentation "Fake."))

(defmethod initialize-instance
    :after ((o terminal-crunch) &rest initargs &key &allow-other-keys)
  "Initialize a terminal-crunch."
  (declare (ignore initargs))

  ;; Create the wrapped terminal if it's not already set.
  (when (or (not (slot-boundp o 'wrapped-terminal))
	    (not (slot-value o 'wrapped-terminal)))
    (let ((type (platform-default-terminal-type)))
      (when (eq type :crunch)
	(cerror "Yeah, I do."
		"You probably don't want to wrap a terminal-crunch in a ~
                 terminal-crunch."))
      (setf (slot-value o 'wrapped-terminal)
	    ;; Make sure the use the right device.
	    (if (terminal-device-name o)
		(make-instance (find-terminal-class-for-type type)
			       :device-name (terminal-device-name o))
		(make-instance (find-terminal-class-for-type type))))))

  ;; Set the initial update position based on the start-line
  (when (slot-boundp o 'start-line)
    (setf (slot-value o 'update-y) (slot-value o 'start-line))))

(defmethod terminal-get-cursor-position ((tty terminal-crunch-stream))
  "Try to somehow get the row of the screen the cursor is on. Returns the
two values ROW and COLUMN."
  (values (screen-y (new-screen tty))
	  (screen-x (new-screen tty))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hashing

;; Here's some typical hash functions. I think we mostly care about it being
;; collision resistant. Perhaps we should make the code robust with collisions.
;;
;; But all these are meant to work on bytes? Do they work decently on the
;; randomly sized integers we're throwing at it with a fatchar? Also with
;; Lisp numbers vs C integers?
;;
;; @@@ or maybe we could use sxhash?

(defparameter *keyword-differentiator*
  #.(loop :with i = 0 :for c :across "keyword" :do
       (setf i (logior (ash i 8) (char-code c))) :finally (return i)))

;; I'm just deciding it's 64bits. We could use most-positive-fixnum, but that
;; might be too small on some platforms.
(defconstant +cut-off+ #xffffffffffffffff)

(defun curses-hash-seed () 0)
(defun curses-hash (integer value)
  (logand (+ value (ash value 5) integer) +cut-off+))

(defun bix-hash-seed () 0)
(defun bix-hash (integer value)
  (logand (logxor (ash value 3) integer) +cut-off+))

(defparameter *fnv64-offset* #xCBF29CE484222325)
(defparameter *fnv64-prime* #x100000001b3)
(defun fnv-like-hash-seed () *fnv64-offset*)
(defun fnv-like-hash (integer value)
  (logand (* (logxor value integer) *fnv64-prime*) +cut-off+))

(defun sxhash-hash-seed () #xCBF29CE484222325)
(defun sxhash-hash (integer value)
  (logand (+ (sxhash integer) value) +cut-off+))

;; @@@ Am I being too paranoid doing this with a macro?
(defmacro hash-thing-with (thing value hash-func hash-seed-func)
  (with-unique-names (hv c)
    `(typecase ,thing
       (character (,hash-func (char-code ,thing) ,value))
       (integer   (,hash-func ,thing ,value))
       ;; If it is a more complicated number than an integer, defer to sxhash
       ;; since how to do it properly can be architecture dependent, such as
       ;; getting the bits out of a float.
       (number    (logand (+ value (sxhash thing)) +cut-off+))
       ;; Add in *keyword-differentiator* so keywods hash to different values
       ;; than the hash of their symbol names.
       (keyword   (hash-thing (symbol-name ,thing)
			      (,hash-func *keyword-differentiator* ,value)))
       (array ;; also string of course
	(loop :with ,hv = (or ,value (,hash-seed-func))
	   :for ,c :across ,thing
	   :do (setf ,hv (hash-thing ,c ,hv))
	   :finally (return ,hv)))
       (list
	(loop :with ,hv = (or ,value (,hash-seed-func))
	   :for ,c :in ,thing
	   :do (setf ,hv (hash-thing ,c ,hv))
	   :finally (return ,hv)))
       (fatchar
	(let ((,hv ,value))
	  (setf ,hv (hash-thing (fatchar-c ,thing) ,hv))
	  (setf ,hv (hash-thing (fatchar-fg ,thing) ,hv))
	  (setf ,hv (hash-thing (fatchar-bg ,thing) ,hv))
	  (setf ,hv (hash-thing (fatchar-line ,thing) ,hv))
	  (setf ,hv (hash-thing (fatchar-attrs ,thing) ,hv))
	  ,hv))
       (otherwise
	(error "I don't know how to hash a ~s." (type-of ,thing))))))

(defun hash-thing (thing &optional (value (fnv-like-hash-seed)))
  (hash-thing-with thing value fnv-like-hash fnv-like-hash-seed))
;; (defun hash-thing (thing &optional (value (fnv-like-hash-seed)))
;;    (hash-thing-with thing value curses-hash curses-hash-seed))
;; (defun hash-thing (thing &optional (value (bix-hash-seed)))
;;     (hash-thing-with thing value bix-hash bix-hash-seed))
;; (defun hash-thing (thing &optional (value (sxhash-hash-seed)))
;;   (hash-thing-with thing value sxhash-hash sxhash-hash-seed))

(defun compute-hashes (screen &optional (start 0) end)
  (with-slots (lines hashes) screen
    (loop :for i :from start :below (or end (length lines))
       :do (setf (aref hashes i) (hash-thing (aref lines i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-screen-contents (from-screen to-screen)
  "Copy the contents of FROM-SCREEN to TO-SCREEN. It's fine if they're diffrent
sizes. It only copies the smaller of the two regions."
  (setf (screen-x to-screen)                (screen-x from-screen)
	(screen-y to-screen)                (screen-y from-screen)
	(screen-background to-screen)       (screen-background from-screen)
	(screen-scrolling-region to-screen) (screen-scrolling-region from-screen)
	(screen-cursor-state to-screen)     (screen-cursor-state from-screen)
	(screen-beep-count to-screen)       (screen-beep-count to-screen))
  (loop :for i :from 0 :below (min (length (screen-lines from-screen))
				   (length (screen-lines to-screen)))
     :do
     (map-into (aref (screen-lines to-screen) i)
	       #'copy-fatchar (aref (screen-lines from-screen) i))
     (setf (aref (screen-index to-screen) i)
	   (aref (screen-index from-screen) i)))
  (compute-hashes to-screen))

(defun make-new-screen (rows cols)
  (let* ((lines  (make-array rows :element-type 'fatchar-string))
	 (hashes (make-array rows :element-type 'integer))
	 (index  (make-array rows :element-type 'fixnum))
	 (result (make-screen
		  :x 0 :y 0 :width cols :height rows
		  :cursor-state t ;; don't really know?
		  :lines lines
		  :hashes hashes
		  :index index)))
    (dotimes (i rows)
      (setf (aref lines i)
	    (make-array cols
			:element-type 'fatchar
			:initial-element (make-fatchar))
	    (aref index i) i)
      (dotimes (j cols)
	(setf (aref (aref lines i) j)
	      (copy-fatchar *blank-char*))))
    (compute-hashes result)
    result))

(defmethod terminal-get-size ((tty terminal-crunch))
  "Get the window size from the wrapped terminal and store it in tty."
  (with-slots ((wtty wrapped-terminal)) tty
    (terminal-get-size wtty)
    ;; Resize the screens
    (when (or (/= (terminal-window-rows tty) (terminal-window-rows wtty))
	      (/= (terminal-window-columns tty) (terminal-window-columns wtty)))
      (let ((screen (make-new-screen (terminal-window-rows wtty)
				     (terminal-window-columns wtty))))
	(copy-screen-contents (old-screen tty) screen)
	(setf (old-screen tty) screen)
	(setf screen (make-new-screen (terminal-window-rows wtty)
				      (terminal-window-columns wtty)))
	(copy-screen-contents (new-screen tty) screen)
	(setf (new-screen tty) screen)))
    (setf (terminal-window-rows tty) (terminal-window-rows wtty)
	  (terminal-window-columns tty) (terminal-window-columns wtty))))

(defun unset-fatchar (c)
  "Make a fatchar unset."
  (setf (fatchar-c     c)	(code-char 0)
	(fatchar-fg    c)	nil
	(fatchar-bg    c)	nil
	(fatchar-line  c)	0
	(fatchar-attrs c)	nil))

(defun invalidate-before-start-row (tty screen)
  (with-slots (start-line) tty
    (loop :for i :from 0 :below (start-line tty)
       :do
       (loop :for c :across (aref (screen-lines screen) i)
	  :do (unset-fatchar c))
       (setf (aref (screen-hashes screen) i)
	     (hash-thing (aref (screen-lines screen) i)))
       ;; @@@ do we really need to set the index?
       (setf (aref (screen-index screen) i) i))))

(defmethod terminal-start ((tty terminal-crunch))
  "Set up the terminal for reading a character at a time without echoing."
  (with-slots ((wtty wrapped-terminal) start-line
	       (start-at-current-line terminal::start-at-current-line)) tty
    (if (started tty)
	(progn
	  (dbugf :crunch "Crunch ~s not recursivley re-started.~%" tty)
	  ;; Non-dumb terminals are supposed to start in :char mode.
	  (setf (terminal-input-mode wtty) :char)
	  (when start-at-current-line
	    (setf start-line (terminal-get-cursor-position wtty)
		  (screen-y (new-screen tty)) start-line
		  (screen-y (old-screen tty)) start-line)
	    (invalidate-before-start-row tty (new-screen tty))
	    (invalidate-before-start-row tty (old-screen tty))
	    (dbugf :crunch "Crunch auto re-starting at ~s.~%" start-line))
	  (incf (started tty)))
	(let ((state (terminal-start wtty)))
	  (terminal-get-size wtty)
	  (when start-at-current-line
	    (dbugf :crunch "Crunch auto starting at ~s.~%" start-line)
	    (setf start-line (terminal-get-cursor-position wtty)))

	  ;; new screen
	  (when (not (new-screen tty))
	    (setf (new-screen tty)
		  (make-new-screen (terminal-window-rows wtty)
				   (terminal-window-columns wtty)))
	    (when (not (zerop start-line))
	      (setf (screen-y (new-screen tty)) start-line)))

	  ;; old screen
	  (when (not (old-screen tty))
	    (setf (old-screen tty)
		  (make-new-screen (terminal-window-rows wtty)
				   (terminal-window-columns wtty)))
	    (when (not (zerop start-line))
	      (setf (screen-y (old-screen tty)) start-line))
	    (compute-hashes (old-screen tty)))

	  (setf (terminal-window-rows tty) (terminal-window-rows wtty)
		(terminal-window-columns tty) (terminal-window-columns wtty))

	  ;; Start with a clean slate.
	  (if (zerop start-line)
	      (progn
		(terminal-clear wtty)
		(terminal-home wtty)
		(dbugf :crunch "Crunch ~s started.~%" tty))
	      (progn
		(invalidate-before-start-row tty (new-screen tty))
		(invalidate-before-start-row tty (old-screen tty))
		(terminal-move-to wtty start-line 0)
		(terminal-erase-below wtty)
		(dbugf :crunch "Crunch ~s started at ~d.~%" tty start-line)))
	  (terminal-finish-output wtty)
	  (setf (started tty) 1)
	  state))))

(defmethod terminal-end ((tty terminal-crunch) &optional state)
  "Put the terminal back to the way it was before we called terminal-start."
  (if (zerop (decf (started tty)))
    (progn
      (terminal-finish-output tty)
      (terminal-end (wrapped-terminal tty) state)
      (dbugf :crunch "Crunch ~s ended.~%" tty)
      (setf (started tty) nil))
    (progn
      (dbugf :crunch "Crunch ~s pop, but not ended.~%" tty))))

(defmethod terminal-done ((tty terminal-crunch) &optional state)
  "Forget about the whole terminal thing and stuff."
  (terminal-done (wrapped-terminal tty) state)
  (values))

;; @@@ this needs to be complicated by the scrolling-region
;; As you may know:
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
||          <Before>             ||            <After>            ||
|| ,-- 0                         || ,--                           ||
|| |                             || |                             ||
|| | N lines to discard          || |                             ||
|| |                             || |                             ||
|| `-- n - 1                     || |                             ||
|| ,-- n                         || |  height - N lines to keep   ||
|| |                             || |                             ||
|| |                             || |                             ||
|| |  height - N lines to keep   || |                             ||
|| |                             || |                             ||
|| |                             || |                             ||
|| |                             || `--                           ||
|| |           ,--               || ,--                           ||
|| |           |                 || |                             ||
|| |           | N blank lines   || | N blank lines               ||
|| |           |                 || |                             ||
|| `-- height  `--               || `--                           ||
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

(defun scroll (tty n)
  (with-slots (x y width height fg bg attrs scrolling-region lines index)
      (new-screen tty)
    (labels ((index-blanker (x)
	       (fill x -1))
	     (line-blanker (x)
	       (loop :for line :across x :do
		  (fill-by line #'blank-char)))
	     (scroll-copy (array blanker)
	       (let ((new-blanks (subseq array 0 n))) ; save overwritten lines
		 ;; Copy the retained lines up
		 (setf (subseq array 0 (- height n))
		       ;; (subseq lines n (1- height)))
		       (subseq array n height))
		 ;; Move the new blank lines in place.
		 (setf (subseq array (- height n)) new-blanks)
		 ;; Blank out the newly blank lines
		 (funcall blanker new-blanks))))
      (if (< n height)
	  (progn
	    (scroll-copy lines #'line-blanker)
	    (scroll-copy index #'index-blanker))
	  (progn
	    ;; Just erase everything
	    (line-blanker lines)
	    (index-blanker index)))
      (when (not (zerop (start-line tty)))
	(setf (start-line tty) (max 0 (- (start-line tty) n)))
	(incf (really-scroll-amount tty) n)))))

(defun copy-char (tty char)
  "Put the CHAR at the current screen postion."
  (with-slots (fg bg attrs delay-scroll) tty
    (with-slots (x y width height scrolling-region lines) (new-screen tty)
      (labels ((char-char (c)
	       (etypecase c
		 (fatchar (fatchar-c c))
		 (character c)))
	       (set-char (fc char)
		 (etypecase char
		   (character
		    (setf (fatchar-c fc) char
			  (fatchar-fg fc) fg
			  (fatchar-bg fc) bg
			  (fatchar-attrs fc) attrs
			  (fatchar-line fc) 0)) ;; unless it's a line char??
		   (fatchar
		    (setf (fatchar-c fc) (fatchar-c char)
			  (fatchar-fg fc) (fatchar-fg char)
			  (fatchar-bg fc) (fatchar-bg char)
			  (fatchar-attrs fc) (fatchar-attrs char)
			  (fatchar-line fc) (fatchar-line char)))))
	       (scroll-one-line ()
		 (no-hints tty)
		 (scroll tty 1)
		 (setf x 0))
	       (delayed-scroll ()
		 (when (delay-scroll tty)
		   (setf (delay-scroll tty) nil)
		   (when (and (= y (1- height))
			      (= x (1- width)))
		     (scroll-one-line))))
	       (next-line ()
		 (if (< y (1- height))
		     (progn
		       (incf y)
		       (setf x 0))
		     (when (allow-scrolling tty)
		       (if (= x (1- width))
			   (progn
			     ;; @@@ horrible
			     (setf (delay-scroll tty) t))
			   (progn
			     (scroll-one-line)))))))
	(case (char-char char)
	  (#\newline
	   (delayed-scroll)
	   (terminal-erase-to-eol tty)
	   (setf x 0)
	   (next-line))
	  (#\return
	   (setf x 0))
	  (#\backspace
	   (setf x (max 0 (1- x))))
	  (#\tab
	   (let ((new-x (+ x (- (1+ (logior 7 x)) x))))
	     ;; @@@ should tabs actually wrap?
	     (setf x (min new-x (1- width)))))
	  (t
	   (delayed-scroll)
	   (set-char (aref (aref lines y) x) char)
	   (note-single-line tty)
	   (let ((new-x (+ x (display-length char))))
	     (if (< new-x width)
		 (setf x new-x)
		 (next-line)))))))))

(defun copy-to-screen (tty string &key (start 0) end)
  (with-slots (x y width height fg bg attrs scrolling-region) (new-screen tty)
    (loop
       :with i = (or start 0)
       :and str = (if (or (and start (not (zerop start))) end)
		      (if end
			  (displaced-subseq string (or start 0) end)
			  (displaced-subseq string start))
		      string)
       :with len = (length str)
       :while (< i len)
       :do
       (copy-char tty (aref str i))
       (incf i))))

(defun no-hints (tty)
  "Can't do any easy optimizations."
  (setf (text-change tty) t
	(single-char-change tty) nil
	(single-line-change tty) nil))

(defun reset-hints (tty)
  "Reset hints to the starting state before any operations."
  (setf (text-change tty) nil
	(single-char-change tty) t
	(single-line-change tty) t))

(defun note-single-line-only (tty)
  "Note a possible single line change. But if this is called on different lines,
then no."
  (when (single-line-change tty) ;; not already determined NOT to be
    (setf (single-line-change tty)
	  (if (eq t (single-line-change tty)) ; undtermined
	      (screen-y (new-screen tty))     ; set it to this line
	      (if (eql (single-line-change tty)
		       (screen-y (new-screen tty))) ; same line
		  (screen-y (new-screen tty))	    ; keep it
		  nil)))))			    ; or nope

(defun note-single-line (tty)
  "Note a possible single line change, and also set the other hints, assuming
that the change is more than one character."
  (note-single-line-only tty)
  (when (not (text-change tty))
    (setf (text-change tty) t))
  (when (single-char-change tty)
    (setf (single-char-change tty) nil)))

(defun rest-of-the-line-blank-p (line start)
  "Return true if the rest of the fatchar LINE is blank, starting from START."
  ;; This seems faster than the equivalent position-if.
  (and (not (position *blank-char* line
		      :start start
		      :test (lambda (a b) (not (equalp a b)))))
       t))

(defun new-change (tty operation)
  "Return a new CHANGE object for OPERATION."
  (make-change
   :start-x (screen-x (old-screen tty))
   :start-y (screen-y (old-screen tty))
   :end-x   (screen-x (new-screen tty))
   :end-y   (screen-y (new-screen tty))
   :op operation))

(defun note-single-char (tty operation)
  "Return a cons of the screen position or NIL if we shouldn't do the
optimization."
  (flet ()
    (case operation
      ((:insert :delete)
       ;; We can only do single character optimization if the line is blank
       ;; after the position.
       (let ((new (new-screen tty)))
	 (if (rest-of-the-line-blank-p (aref (screen-lines new) (screen-y new))
				       (screen-x new))
	     (new-change tty operation)
	     nil)))
      (otherwise
       ;; Set it to this position.
       (new-change tty operation)))))

(defun note-length-based (tty len operation)
  (when (> len 0)
    (setf (text-change tty) t
	  (single-char-change tty)
	  (if (or (> len 1)
		  (not (single-char-change tty))) ;; already determined not
	      nil
	      (if (eq t (single-char-change tty)) ; undetermined
		  (note-single-char tty operation) ; maybe set it to this pos
		  (let ((change (new-change tty operation)))
		    (if (equal (single-char-change tty) change) ; same
			(single-char-change tty)		; keep it
			nil)))))				; or nope
    (note-single-line-only tty)))

(defun note-change (tty thing &optional operation)
  "Set hints for TTY based on THING as a change."
  (etypecase thing
    (character
     (case thing
       ((#\tab #\return)
	#| nothing |#)
       (#\newline
	;; XXX I don't think this is right, since it could be not a text change
	;; if it doesn't scroll.
	(no-hints tty))
       (otherwise
	(note-length-based tty (display-length thing) operation))))
    (fatchar
     (note-change tty (fatchar-c thing) operation))
    ((or string fat-string)
     ;; or maybe just length??
     (note-length-based tty (display-length thing) operation))))

(defmethod terminal-format ((tty terminal-crunch-stream) fmt &rest args)
  "Output a formatted string to the terminal."
  (let ((string (apply #'format nil fmt args)))
    ;; This can have print-object do some other stuff...
    ;; @@@ maybe?
    ;; (apply #'format tty fmt args)
    (copy-to-screen tty string)
    (note-change tty string)))

(defmethod terminal-alternate-characters ((tty terminal-crunch) state)
  (declare (ignore tty state))
  ;; Let's just assume it will work.
  )

(defmethod terminal-write-string ((tty terminal-crunch-stream) str
				  &key start end)
  "Output a string to the terminal."
  (copy-to-screen tty str :start start :end end)
  (note-change tty str))

(defmethod terminal-write-line ((tty terminal-crunch-stream) str
				  &key start end)
  "Output a string to the terminal."
  (copy-to-screen tty str :start start :end end)
  (note-change tty str)
  (copy-char tty #\newline)
  (note-change tty #\newline))

(defmethod terminal-write-string ((tty terminal-crunch-stream) (str fat-string)
				  &key start end)
  "Output a string to the terminal."
  (copy-to-screen tty (fat-string-string str) :start start :end end)
  (note-change tty str))

(defmethod terminal-write-line ((tty terminal-crunch-stream) (str fat-string)
				  &key start end)
  "Output a string to the terminal."
  (copy-to-screen tty (fat-string-string str) :start start :end end)
  (note-change tty str)
  (copy-char tty #\newline)
  (note-change tty #\newline))

(defmethod terminal-write-char ((tty terminal-crunch-stream) char)
  "Output a character to the terminal. Flush output if it is a newline,
i.e. the terminal is 'line buffered'."
  (copy-char tty char)
  (note-change tty char))

(defmethod terminal-write-char ((tty terminal-crunch-stream) (char fatchar))
  "Output a character to the terminal. Flush output if it is a newline,
i.e. the terminal is 'line buffered'."
  (copy-char tty char)
  (note-change tty char))

(defmethod terminal-move-to ((tty terminal-crunch-stream) row col)
  (setf (screen-y (new-screen tty))
	(max 0 (min row (1- (screen-height (new-screen tty)))))
	(screen-x (new-screen tty))
	(max 0 (min col (1- (screen-width (new-screen tty))))))
  (when (not (zerop (start-line tty)))
    (setf (start-line tty) (max 0 (min (start-line tty) row)))))

(defmethod terminal-move-to-col ((tty terminal-crunch-stream) col)
  (setf (screen-x (new-screen tty))
	(max 0 (min col (1- (screen-width (new-screen tty)))))))

(defmethod terminal-beginning-of-line ((tty terminal-crunch-stream))
  (setf (screen-x (new-screen tty)) 0))

(defmethod terminal-delete-char ((tty terminal-crunch-stream) n)
  (with-slots (x y width lines) (new-screen tty)
    (clamp n 0 (- width x))
    (setf (subseq (aref lines y) x (max 0 (- width n)))
	  (subseq (aref lines y) (min (+ x n) (1- width))))
    (fill-by (aref lines y) #'blank-char :start (max 0 (- width n)))
    (note-length-based tty n :delete)))

(defmethod terminal-insert-char ((tty terminal-crunch-stream) n)
  (with-slots (x y width lines) (new-screen tty)
    (clamp n 0 (- width x))
    (setf (subseq (aref lines y) (min (+ x n) (1- width)))
	  (subseq (aref lines y) x))
    (fill-by (aref lines y) #'blank-char :start x :end (+ x n))
    (note-length-based tty n :insert)))

;; Note that we don't have to replicate the somewhat bizarre line wrapping
;; behavior of real terminals or emulators. If you relied on such things in
;; other terminals, like terminal-ansi, than I'm sorry, but don't. On the
;; other hand, if it works on terminal-crunch, it should work (almost
;; entirely**) the same on other terminals too. One of the few problems comes
;; in when we are at bottom right corner of the screen. To output a character
;; there without scrolling, we have to have some way of telling it not to
;; scroll. Most full screen applications don't want automatic bottom line
;; scrolling anyway.
;;
;; ** I'm guessing the exceptions are: being better at getting the current
;; cursor position, and perhaps the fact that we can pretend to know
;; what characters are on the screen at a given time. Also the end of line
;; ‘hyperspace’ behaviour in terminals.

(defmethod terminal-backward ((tty terminal-crunch-stream) n)
  (setf (screen-x (new-screen tty))
	(max 0 (- (screen-x (new-screen tty)) n))))

(defmethod terminal-forward ((tty terminal-crunch-stream) n)
  (setf (screen-x (new-screen tty))
	(min (1- (screen-width (new-screen tty)))
	     (+ (screen-x (new-screen tty)) n))))

(defmethod terminal-up ((tty terminal-crunch-stream) n)
  (setf (screen-y (new-screen tty))
	(max 0 (- (screen-y (new-screen tty)) n)))
  (when (not (zerop (start-line tty)))
    (setf (start-line tty)
	  (max 0 (min (start-line tty) (screen-y (new-screen tty)))))))

(defmethod terminal-down ((tty terminal-crunch-stream) n)
  (setf (screen-y (new-screen tty))
	(min (1- (screen-height (new-screen tty)))
	     (+ (screen-y (new-screen tty)) n))))

(defmethod terminal-scroll-down ((tty terminal-crunch-stream) n)
  ;; Even if allow-scrolling is false.
  (when (> n 0)
    (terminal-down tty n)
    (with-slots (y height) (new-screen tty)
      (when (> n (- height y))
	(scroll tty (- n (- height y)))))))

(defmethod terminal-erase-to-eol ((tty terminal-crunch-stream))
  (fill-by (aref (screen-lines (new-screen tty)) (screen-y (new-screen tty)))
	   #'blank-char
	   :start (screen-x (new-screen tty)))
  (note-single-line tty))

(defmethod terminal-erase-line ((tty terminal-crunch-stream))
  (fill-by (aref (screen-lines (new-screen tty)) (screen-y (new-screen tty)))
	   #'blank-char)
  (note-single-line tty))

(defmethod terminal-erase-above ((tty terminal-crunch-stream))
  (with-slots (x y height lines) (new-screen tty)
    (loop :for i :from 0 :below y :do
       (fill-by (aref lines i) #'blank-char))
    (fill-by (aref lines y) #'blank-char :start 0 :end x)
    (if (zerop y)
	(note-single-line tty)
	(no-hints tty))))

(defmethod terminal-erase-below ((tty terminal-crunch-stream))
  (with-slots (x y height lines) (new-screen tty)
    (fill-by (aref lines y) #'blank-char :start x)
    (loop :for i :from (1+ y) :below height
       :do (fill-by (aref lines i) #'blank-char))
    (if (= y (1- height))
	(note-single-line tty)
	(no-hints tty))))

(defmethod terminal-clear ((tty terminal-crunch-stream))
  (loop :for line :across (screen-lines (new-screen tty)) :do
     (fill-by line #'blank-char))
  (setf (cleared tty) t
	(start-line tty) 0)
  (no-hints tty))

(defmethod terminal-home ((tty terminal-crunch-stream))
  (setf (screen-x (new-screen tty)) 0
	(screen-y (new-screen tty)) 0)
  (when (not (zerop (start-line tty)))
    (setf (start-line tty) 0)))

(defmethod terminal-cursor-off ((tty terminal-crunch-stream))
  (setf (screen-cursor-state (new-screen tty)) nil))

(defmethod terminal-cursor-on ((tty terminal-crunch-stream))
  (setf (screen-cursor-state (new-screen tty)) t))

(defun set-attr (tty attr state)
  (if state
      (pushnew attr (attrs tty))
      (setf (attrs tty) (delete attr (attrs tty)))))

(defmethod terminal-standout ((tty terminal-crunch-stream) state)
  (set-attr tty :standout state))

(defmethod terminal-normal ((tty terminal-crunch-stream))
  (setf (attrs tty) nil
	(fg tty) nil
	(bg tty) nil))

(defmethod terminal-underline ((tty terminal-crunch-stream) state)
  (set-attr tty :underline state))

(defmethod terminal-bold ((tty terminal-crunch-stream) state)
  (set-attr tty :bold state))

(defmethod terminal-inverse ((tty terminal-crunch-stream) state)
  (set-attr tty :inverse state))

#| We could consider doing these?

(defun set-foreground-color (color)
  (tt-format "~a10;~a~a" +osc+
	     (format-color (color-red   color)
			   (color-green color)
			   (color-blue  color)) +st+))

(defun set-background-color (color)
  (tt-format "~a11;~a~a" +osc+
	     (format-color (color-red   color)
			   (color-green color)
			   (color-blue  color)) +st+))
|#

(defmethod terminal-color ((tty terminal-crunch-stream) fg bg)
  (setf (fg tty) fg (bg tty) bg))

(defmethod terminal-beep ((tty terminal-crunch-stream))
  (incf (screen-beep-count (new-screen tty))))

(defmethod terminal-set-scrolling-region ((tty terminal-crunch-stream) start end)
  (setf (screen-scrolling-region (new-screen tty) ) (cons start end)))

(defmethod terminal-set-attributes ((tty terminal-crunch) attributes)
  "Set the attributes given in the list. If NIL turn off all attributes.
Attributes are usually keywords."
  (setf (attrs tty)
	(etypecase attributes
	  (list attributes)
	  (keyword (list attributes)))))

(defmethod terminal-title ((tty terminal-crunch))
  (terminal-title (wrapped-terminal tty)))

(defmethod (setf terminal-title) (title (tty terminal-crunch))
  "Set the title of a terminal window. The terminal is assumed to work like
XTerm or something."
  (setf (terminal-title (wrapped-terminal tty)) title))

(defmethod terminal-has-attribute ((tty terminal-crunch) attribute)
  "Return true if the terminal can display the character attribute."
  (terminal-has-attribute (wrapped-terminal tty) attribute))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Somewhat unlike the old world, our costs could be unrelated to the number
;; of characters output.

(defgeneric output-cost (terminal operation &rest params)
  (:documentation
   "Return a number representing the cost of OPERATION on TERMINAL."))

#|
;; Generic cost methods, that are based on ANSI terminals.
;; Other terminal types can tweak their costs if they like.

(defun number-chars (n)
  "Return how many characters are needed to output the number in decimal."
  (1+ (floor (log n 10))))

(defmethod output-cost ((tty terminal) (op (eql :move-to)) &rest params)
  (+ 2					     ; ^[[
     (number-chars (first params))	     ; row
     1					     ; ;
     (number-chars (second params))	     ; col
     1))				     ; H

(defmethod output-cost ((tty terminal) (op (eql :move-to-col)) &rest params)
  (+ 2					     ; ^[[
     (number-chars (first params))	     ; col
     1))				     ; G

(defmethod output-cost ((tty terminal) (op (eql :color)) &rest params)
  (flet ((complex-color-len (color)
	   (let ((c (convert-color-to color :rgb8)))
	     (+ 2			; ^[[
		2			; x8
		3			; ; 2 ;
		(number-chars (color-component c :red))
		1			; ;
		(number-chars (color-component c :red))
		1			; ;
		(number-chars (color-component c :red))
		1)))			; m
	 (simple-color-len (color)
	   (+ 2				; ^[[
	      (if color
		  (number-chars 2)	; 3x
		  0)
	      1)))			; m
    (let ((fg (first params))
	  (bg (second params)))
      (+ (cond
	   ((or (null fg) (keywordp fg))
	    (simple-color-len fg))
	   ((or (arrayp fg) (listp fg))
	    (complex-color-len fg))
	   (t 5)) ; ¿guess?
	 (cond
	   ((or (null fg) (keywordp fg))
	    (simple-color-len fg))
	   ((or (arrayp fg) (listp fg))
	    (complex-color-len fg))
	   (t 5)))))) ; ¿guess?

(defmethod output-cost ((tty terminal) (op (eql :write-fatchar)) &rest params)
  (let ((fc (first params)))
    ()))
|#

(defun crunched-move-to (tty new-x new-y old-x old-y)
  "Move to NEW-X NEW-Y from OLD-X OLD-Y in a hopefully efficient way.
Set the current update position UPDATE-X UPDATE-Y in the TTY."
  (let ((wtty (wrapped-terminal tty)))
    (when (or (not (eql new-x old-x)) (not (eql new-y old-y)))
      (cond
	;; both
	((and (not (eql new-x old-x)) (not (eql new-y old-y)))
	 (cond
	   ;; When the cost of potential single character movement
	   ;; is greater than a full move-to, just do the move-to
	   ((> (+ (abs (- new-x old-x))
		  (abs (- new-y old-y)))
	       (output-cost wtty :move-to new-x new-y))
	    (terminal-move-to wtty new-y new-x))
	   ;; - see if combo of backspace and newline will be cheaper than
	   ;;   a move-to
	   ;; - see if a wrap around is cheaper than a move-to
	   ;; - see if a return (and maybe forward) is cheaper than a move-to
	   ;; - see if a home and another something is cheaper than a move-to
	   (t
	    ;; @@ fuck it for now
	    (terminal-move-to wtty new-y new-x))))
	((not (eql new-x old-x))
	 ;; Horizontal movement
	 (let ((n (abs (- new-x old-x))))
	   (if (< new-x old-x)
	       (progn
		 (let ((move-to-col-cost (output-cost wtty :move-to-col new-x)))
		   ;; @@@ if a return + spaces will work
		   ;; (if (and (< new-x move-to-col-cost)
		   ;;          (is-blank line 0 new-x))
		   ;;     (progn
		   ;; 	    (terminal-write-char wtty #\return)
		   ;;       (dotimes (i n) (terminal-write-char wtty #\space))))
		   (if (< move-to-col-cost n)
		       (terminal-move-to-col wtty new-x)
		       (dotimes (i n) (terminal-write-char wtty #\backspace)))))
	       (progn
		 (if (< (output-cost wtty :forward n) n)
		     (terminal-forward wtty n)
		     ;;; @@@ check of we can space over
		     ;; (if (position *blank-char* (subseq line old-x new-x))
		     (terminal-forward wtty n))))))
	((not (eql new-y old-y))
	 ;; Vertical movement
	 (let ((n (abs (- new-y old-y))))
	   (if (> new-y old-y)
	       ;; @@@ This only works if newline isn't translated CR-NL!
	       ;; (if (< (output-cost wtty :down n) n)
	       ;; 	   (terminal-down wtty n)
	       ;; 	   (dotimes (i n) (terminal-write-char wtty #\newline)))
	       (terminal-down wtty n)
	       (terminal-up wtty n)))))
      (setf (update-x tty) new-x
	    (update-y tty) new-y))))

(defun update-position (tty new old)
  (crunched-move-to tty
		    (screen-x new) (screen-y new)
		    (screen-x old) (screen-y old)))

(defun update-cursor-state (wtty new old)
  (when (not (eq (screen-cursor-state new)
		 (screen-cursor-state old)))
    (if (screen-cursor-state new)
	(terminal-cursor-on wtty)
	(terminal-cursor-off wtty))))

(defun update-beeps (tty new)
  (when (not (zerop (screen-beep-count new)))
    ;; This is ridiculous. We could just compress multiple beeps to one.
    (dotimes (i (screen-beep-count new))
      (terminal-beep (wrapped-terminal tty)))
    ;; We've drained the beeps, so they're not still there next time.
    (setf (screen-beep-count new) 0)))

(defun move-cost (tty from-x from-y to-x to-y)
  (declare (ignore tty))
  ;; @@@ FIX ME
  ;; @@@ we should probably just "cost" a crunched-move-to
  (cond
    ((or (eql to-y from-y)
	 (eql to-x from-x))
     ;; horizontal or vertical
     5)
    (t
     ;; both
     8)))

(defun update-line (tty line)
  (let* ((old-line (aref (screen-lines (old-screen tty)) line))
	 (new-line (aref (screen-lines (new-screen tty)) line))
	 (wtty (wrapped-terminal tty))
	 new-line-cost
	 (change-cost 0)
	 first-change
	 (last-change 0)
	 change-range
	 changes)

    (dbugf :crunch "update-line ~s~%" line)
    
    ;; Go through chars and calculate approximate cost to output differences.
    (flet ((note-change-end (i)
	     (when change-range
	       (setf (cdr change-range) i)
	       (push change-range changes)
	       (setf change-range nil))))
      (loop
	 :for i :from 0 :below (length new-line) :do
	 (if (not (fatchar= (aref new-line i) (aref old-line i)))
	     (progn
	       (when (not first-change)
		 (setf first-change i))
	       ;; Note change start
	       (when (not change-range)
		 (setf change-range (cons i nil)))

	       ;; Cost of writing this char (@@@ as if it was new)
	       (incf change-cost
		     (output-cost wtty :write-fatchar (aref new-line i)))

	       ;; If we have to move since the last change, add that.
	       (if (> (- i last-change) 1)
		   (incf change-cost
			 (move-cost tty last-change line i line)))

	       (setf last-change i))
	     (progn ;; chars are equal
	       ;; Note change end
	       (note-change-end i)))
	 :finally (note-change-end (1- i))))

    (when changes
      (setf changes (nreverse changes))

      ;; We don't really need to calculate the whole line cost every time,
      ;; but what is a good heuristic for when we do?
      (when (> (length changes) (/ (length new-line) 2))
	(setf new-line-cost
	      (output-cost wtty :write-fatchar-string new-line))))

    ;; (dbugf :crunch "new-line-cost ~a change-cost ~s~%~
    ;;                 first-change ~s last-change ~a~%~
    ;;                 change-range ~s~%"
    ;; 	   new-line-cost change-cost first-change last-change changes)

    (when (not first-change)
      (error "We thought we had to update line ~s, but we didn't?" line))

    ;; @@@ Try to see if we can use insert / delete.
    (crunched-move-to tty first-change line (update-x tty) (update-y tty))
    (if (or (not new-line-cost) (> new-line-cost change-cost))
	(progn
	  ;; Output changes
	  (loop :with start :and end
	     :for c :in changes :do
	     (setf start (car c)
		   end (cdr c))
	     (crunched-move-to tty start line (update-x tty) (update-y tty))
	     ;;(dbugf :crunch "update-line FLOOB ~s ~s ~s~%" line start c)
	     (if (= start end)
		 (terminal-write-char wtty (aref new-line start))
		 (terminal-write-string wtty (make-fat-string :string new-line)
					:start start
					:end (1+ end)))
	     ;; @@@ This is wrong since it should be display-length or *better*
	     (setf (update-x tty) (1+ end))
	     ))
	(progn
	  ;; Write a whole new line
	  ;; (dbugf :crunch "update-line WINKY ~s ~s-~s~%" line first-change
	  ;; 	 (1+ last-change))
	  (terminal-write-string wtty (make-fat-string :string new-line)
				 :start first-change
				 :end (1+ last-change))
	  (setf (update-x tty) (1+ last-change))
	  ))))

(defun copy-new-to-old (tty)
  (let ((new (new-screen tty))
	(old (old-screen tty)))
    (when (or (/= (screen-width old) (screen-width new))
	      (/= (screen-height old) (screen-height new)))
      (error "Screen size changed without us noticing."))
    (setf (screen-x old)                (screen-x new)
	  (screen-y old)                (screen-y new)
	  (screen-background old)       (screen-background new)
	  (screen-scrolling-region old) (screen-scrolling-region new)
	  (screen-cursor-state old)     (screen-cursor-state new)
	  (screen-beep-count old)       0)
    (loop :for i :from 0 :below (length (screen-lines new))
       :do
       (map-into (aref (screen-lines old) i)
		 #'copy-fatchar (aref (screen-lines new) i))
       ;; Sync both the new and old indexes.
       (setf (aref (screen-index old) i) i
	     (aref (screen-index new) i) i))
    (setf (screen-hashes old) (copy-seq (screen-hashes new)))))

(defun update-ending-position (tty new)
  "Make sure we're at the right cursor position at the end of the update."
  (when (or (not (eql (update-x tty) (screen-x new)))
	    (not (eql (update-y tty) (screen-y new))))
    (crunched-move-to tty
		      (screen-x new) (screen-y new)
		      (update-x tty) (update-y tty))))

(defun update-display (tty)
  "This is the big crunch."
  (with-slots ((wtty wrapped-terminal)
	       (old old-screen)
	       (new new-screen)
	       start-line really-scroll-amount) tty

    ;; Set starting point.
    (setf (update-x tty) (screen-x old)
	  (update-y tty) (screen-y old))

    (when (cleared tty)
      (terminal-clear wtty)
      (terminal-finish-output wtty)
      (setf (cleared tty) nil)
      (no-hints tty)
      (setf (old-screen tty)
	    (make-new-screen (screen-height old)
			     (screen-width old))))

    (dbugf :crunch "****** start update @ ~s~%" start-line)

    ;; First, actually scroll unmanaged content if we have to.
    (when (and (not (zerop start-line))
	       (not (zerop really-scroll-amount)))
      (dbugf :crunch "-------- Really scroll ~s <-----~%" really-scroll-amount)
      (crunched-move-to tty 0 (1- (screen-height old))
			(update-x tty) (update-y tty))
      (terminal-scroll-down wtty really-scroll-amount))

    ;; Try to speed things up with hints.
    (cond
      ((not (text-change tty))
       (dbugf :crunch "position only ~s ~s -> ~s ~s~%"
	      (screen-x old) (screen-y old)
	      (screen-x new) (screen-y new))
       (update-position tty new old))
      ((single-char-change tty)
       (let* ((cx (change-start-x (single-char-change tty)))
	      (cy (change-start-y (single-char-change tty)))
	      (op (change-op (single-char-change tty)))
	      move-x
	      char-x
	      c)
	 (dbugf :crunch "single char ~s~%" (single-char-change tty))
	 (flet ((put-it ()
		  (crunched-move-to tty (max 0 move-x) cy
				    (update-x tty) (update-y tty))
		  (setf c (aref (aref (screen-lines new) cy) (max 0 char-x)))
		  (terminal-write-char wtty c)
		  (incf (update-x tty) (display-length c))))
	   (compute-hashes new cy (1+ cy))
	   (case op
	     (:delete
	      ;; Move to the position AT the spot and write the character
	      ;; AT the point.
	      (setf move-x (change-end-x (single-char-change tty))
		    char-x cx)
	      (put-it)
	      ;; Then back up
	      ;; (crunched-move-to tty (max 0 move-x) cy
	      ;; 			(update-x tty) (update-y tty))
	      )
	     (:insert
	      ;; Move to the position AT the spot and write the character
	      ;; BEFORE the point.
	      (setf move-x cx
		    char-x (change-end-x (single-char-change tty)))
	      (put-it))
	     (otherwise			; an append
	      ;; Move to the position BEFORE the spot and write the character
	      ;; BEFORE the point.
	      (setf move-x cx
		    char-x cx)
	      (put-it)))
	   (update-ending-position tty new))
	 (dbugf :crunch "char = '~s'~%" c)))
      ((single-line-change tty)
       ;; diff the line
       ;; move, overwite, insert / delete as appropriate
       ;; @@@ it could be something else? like insert or delete?
       (let ((line (single-line-change tty)))
	 (dbugf :crunch "single-line-change ~s~%" line)
	 (compute-hashes new line (1+ line))
	 (when (/= (aref (screen-hashes new) line)
		   (aref (screen-hashes old) line))
	   (update-line tty line)))
       (update-ending-position tty new))
      ;; No hints.
      (t
       ;; Make the line hashes.
       (compute-hashes new start-line)

       ;; @@@
       ;; handle scrolling
       ;;   detect scrolling
       ;;   move same lines

       ;; Update changed lines.
       (loop :for i :from start-line :below (length (screen-hashes new)) :do
	  (when (/= (aref (screen-hashes new) i) (aref (screen-hashes old) i))
	    (dbugf :crunch "hash diff ~s old ~s new ~s~%" i
		   (aref (screen-hashes new) i)
		   (aref (screen-hashes old) i))
	    (update-line tty i)))

       ;; Make sure we're at the right cursor position.
       (update-ending-position tty new)))
    (update-cursor-state wtty new old)
    (update-beeps tty new)
    (copy-new-to-old tty)
    (finish-output wtty)
    (setf really-scroll-amount 0
	  (delay-scroll tty) nil)
    (reset-hints tty)))

(defmethod terminal-finish-output ((tty terminal-crunch-stream))
  "Make sure everything is output to the terminal."
  (update-display tty))

(defmethod terminal-get-char ((tty terminal-crunch))
  (terminal-finish-output tty)
  (terminal-get-char (wrapped-terminal tty)))

(defmethod terminal-get-key ((tty terminal-crunch))
  (terminal-finish-output tty)
  (terminal-get-key (wrapped-terminal tty)))

(defmethod terminal-listen-for ((tty terminal-crunch) seconds)
  (terminal-finish-output tty)
  (terminal-listen-for (wrapped-terminal tty) seconds))

(defmethod terminal-input-mode ((tty terminal-crunch))
  (terminal-input-mode (wrapped-terminal tty)))

(defmethod (setf terminal-input-mode) (mode (tty terminal-crunch))
  (setf (terminal-input-mode (wrapped-terminal tty)) mode))

(defmethod terminal-reset ((tty terminal-crunch-stream))
  "Try to reset the terminal to a sane state, without being too disruptive."
  ;; @@@ wrapped terminal reset? or what?
  (terminal-finish-output tty))

(defmethod terminal-reset ((tty terminal-crunch))
  (call-next-method)) ;; Do the terminal-stream version

(defmethod terminal-save-cursor ((tty terminal-crunch))
  "Save the cursor position."
  (setf (saved-pos tty) (cons (screen-x (new-screen tty))
			      (screen-y (new-screen tty)))))

(defmethod terminal-restore-cursor ((tty terminal-crunch))
  "Restore the cursor position, from the last saved postion."
  (when (saved-pos tty)
    (setf (screen-x (new-screen tty)) (car (saved-pos tty))
	  (screen-y (new-screen tty)) (cdr (saved-pos tty)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stream methods

;; common methods

(defmethod-quiet close ((stream terminal-crunch-stream) &key abort)
  (declare (ignore abort))
  (terminal-done stream))

;; output stream methods

(defmethod stream-clear-output ((stream terminal-crunch-stream))
  (declare (ignore stream))
  ;;(clear-output (terminal-output-stream stream))
  ;; really?
  ;; like copy old-screen to new-screen?
  )

(defmethod stream-finish-output ((stream terminal-crunch-stream))
  (terminal-finish-output stream))

(defmethod stream-force-output ((stream terminal-crunch-stream))
  (terminal-finish-output stream)
  (force-output (wrapped-terminal stream)))

(defmethod stream-write-sequence ((stream terminal-crunch-stream) seq start end
				  &key &allow-other-keys)
  (etypecase seq
    (string
     (terminal-write-string stream seq :start start :end end))
    (list
     (with-slots (output-stream) stream
       (loop :with i = 0 :and l = seq
	  :while (and l (< i end))
	  :do
	  (when (>= i start)
	    (terminal-write-char stream (car l)))
	  (setf l (cdr l))
	  (incf i))))))

;; character output stream methods

;; It's "that time".

(defmethod stream-line-column ((stream terminal-crunch-stream))
  (screen-x (new-screen stream)))

(defmethod stream-start-line-p ((stream terminal-crunch-stream))
  (zerop (stream-line-column stream)))

(defmethod stream-advance-to-column ((stream terminal-crunch-stream) column)
  (with-slots (x) (new-screen stream)
    (loop :while (< x column)
       :do (terminal-write-char stream #\space)))
  t)

;;(defmethod stream-fresh-line ((stream terminal-crunch-stream))

;; #+sbcl (defmethod sb-gray:stream-line-length ((stream terminal-crunch-stream))
;;   )

(defmethod stream-write-char ((stream terminal-crunch-stream) char
			     #| &optional start end |#)
  (terminal-write-char stream char))

(defmethod stream-write-string ((stream terminal-crunch-stream) string
			       &optional start end)
  (terminal-write-string stream string :start start :end end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stream methods for terminal-crunch, which is also an input stream.

;; For input just call the wrapped stream's version.

(defmethod stream-clear-input ((stream terminal-crunch))
  (stream-clear-input (wrapped-terminal stream)))

(defmethod stream-read-sequence ((stream terminal-crunch) seq start end
				 &key &allow-other-keys
				 #| &optional (start 0) end |#)
  (stream-read-sequence (wrapped-terminal stream) seq start end))

;;(defgeneric stream-peek-char ((stream terminal-crunch))
  ;; This is used to implement ‘peek-char’; this corresponds to
  ;; ‘peek-type’ of ‘nil’.  It returns either a character or ‘:eof’.
  ;; The default method calls ‘stream-read-char’ and
  ;; ‘stream-unread-char’.
;; )

(defmethod stream-read-char-no-hang ((stream terminal-crunch))
  ;; This is used to implement ‘read-char-no-hang’.  It returns either a
  ;; character, or ‘nil’ if no input is currently available, or ‘:eof’
  ;; if end-of-file is reached.  The default method provided by
  ;; ‘fundamental-character-input-stream’ simply calls
  ;; ‘stream-read-char’; this is sufficient for file streams, but
  ;; interactive streams should define their own method.
  (stream-read-char-no-hang (wrapped-terminal stream)))

(defmethod stream-read-char ((stream terminal-crunch))
  (stream-read-char (wrapped-terminal stream)))

(defmethod stream-read-line ((stream terminal-crunch))
  ;; This is used by ‘read-line’.  A string is returned as the first
  ;; value.  The second value is true if the string was terminated by
  ;; end-of-file instead of the end of a line.  The default method uses
  ;; repeated calls to ‘stream-read-char’.
  (stream-read-line (wrapped-terminal stream)))

(defmethod stream-listen ((stream terminal-crunch))
  ;; This is used by ‘listen’.  It returns true or false.  The default
  ;; method uses ‘stream-read-char-no-hang’ and ‘stream-unread-char’.
  ;; Most streams should define their own method since it will usually
  ;; be trivial and will always be more efficient than the default
  ;; method.
  (stream-listen (wrapped-terminal stream)))

(defmethod stream-unread-char ((stream terminal-crunch) character)
  ;; Undo the last call to ‘stream-read-char’, as in ‘unread-char’.
  ;; Return ‘nil’.  Every subclass of
  ;; ‘fundamental-character-input-stream’ must define a method for this
  ;; function.
  (stream-unread-char (wrapped-terminal stream) character))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-terminal-type :crunch 'terminal-crunch)
;;(register-terminal-type :crunch-stream 'terminal-crunch-stream)

;; EOF
