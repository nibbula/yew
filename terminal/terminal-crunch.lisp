;;;
;;; terminal-crunch.lisp - Crunch terminal output.
;;;

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
  (:use :cl :dlib :collections :char-util :dcolor :fatchar :terminal
	:trivial-gray-streams :fatchar-io :ostring)
  (:import-from :terminal #:wrapped-terminal)
  (:export
   #:terminal-crunch
   #:allow-scrolling
   #:output-cost
   ))
(in-package :terminal-crunch)

;; (declaim #.`(optimize ,.(getf terminal-config::*config* :optimization-settings)))

;; (declaim (optimize (safety 3)))

;; (declaim
;;  (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
;; (declaim
;;  (optimize (speed 3) (safety 0) (debug 1) (space 0) (compilation-speed 0)))

;; As you may know, many of the world's lovely scripts, do not fit perfectly
;; into a character grid, so neither will all unicode characters. This will only
;; really work for those scripts that do. But we do at least try to make it a
;; grid of unicode graphemes, instead of just characters. That means there can
;; be multiple characters per screen cell, or sort of hidden empty cells that
;; are under double wide characters.
;;
;; So, grid-char is like a fatchar with coalesced graphemes.
;; @@@ Someday do the experiment to make them a class and see what happens.

(defstruct (grid-char (:copier nil))
  "A grapheme with attributes."
  (c nil :type (or null character string))
  (fg nil)
  (bg nil)
  (line 0 :type fixnum)
  (attrs nil :type list))

(defun copy-grid-char (c)
  "Return a new copy of the grid-char ‘c’."
  (make-grid-char :c     (grid-char-c c)
		  :fg    (copy-color (grid-char-fg c))
		  :bg    (copy-color (grid-char-bg c))
		  :line  (grid-char-line c)
		  :attrs (copy-list (grid-char-attrs c))))

(deftype grid-string (&optional n) `(vector grid-char ,(or n '*)))
(defun make-grid-string (n) (make-array n :element-type 'grid-char))

(defmethod display-length ((c grid-char))
  (cond
    ((not (zerop (grid-char-line c)))
     1)				    ; assume line drawing can happen in 1 cell
    ;; ((char= #\nul (grid-char-c c))
    ;;  0)		; since an unset fatchar is #\nul
    (t (display-length (grid-char-c c)))))

(defmethod display-length ((c null))
  0) ;; somewhat bogus

(defun gs-display-length (gs)
  (loop :for c :across gs :sum (display-length c)))

(defgeneric grid-char-same-effects (a b)
  (:documentation
   "Return true if the two fatchars have the same colors and attributes.")
  (:method ((a grid-char) (b grid-char))
    (and (equalp (grid-char-fg a) (grid-char-fg b))
	 (equalp (grid-char-bg a) (grid-char-bg b))
	 (not (set-exclusive-or (grid-char-attrs a) (grid-char-attrs b)
				:test #'eq))))
  (:method ((a grid-char) (b fatchar))
    (and (equalp (grid-char-fg a) (fatchar-fg b))
	 (equalp (grid-char-bg a) (fatchar-bg b))
	 (not (set-exclusive-or (grid-char-attrs a) (fatchar-attrs b)
				:test #'eq)))))

(defgeneric grid-char= (a b)
  (:documentation "True if everything about a grid-char is the equivalent.")
  (:method ((a grid-char) (b grid-char))
    (and (equal (grid-char-c a) (grid-char-c b))
	 (grid-char-same-effects a b)
	 (= (grid-char-line a) (grid-char-line b))))
  (:method ((a grid-char) (b fatchar))
    (and (characterp (grid-char-c a))
	 (char= (grid-char-c a) (fatchar-c b))
	 (grid-char-same-effects a b)
	 (= (grid-char-line a) (fatchar-line b)))))

(defgeneric set-grid-char (char value)
  (:documentation "Set the grid-char CHAR to VALUE.")
  (:method ((char grid-char) (value grid-char))
    (setf (grid-char-c char)     (grid-char-c value)
	  (grid-char-fg char)    (copy-color (grid-char-fg value))
	  (grid-char-bg char)    (copy-color (grid-char-bg value))
	  (grid-char-attrs char) (copy-list (grid-char-attrs value))
	  (grid-char-line char)  (grid-char-line value)))
  (:method ((char grid-char) (value fatchar))
    (setf (grid-char-c char)     (fatchar-c value)
	  (grid-char-fg char)    (copy-color (fatchar-fg value))
	  (grid-char-bg char)    (copy-color (fatchar-bg value))
	  (grid-char-attrs char) (copy-list (fatchar-attrs value))
	  (grid-char-line char)  (fatchar-line value))))

(defun grapheme-to-grid-char (grapheme &key tty)
  "Make a GRID-CHAR from GRAPHEME, which can be any of fat-string,
fatchar-string, string, fatchar, or character. Note that in the case of a fat
strings, only the attributes of the first character are preserved."
  (typecase grapheme
    (fat-string
     (grapheme-to-grid-char (fat-string-string grapheme) :tty tty))
    (fatchar-string
     ;; Take the attributes from the first character only.
     (if (not (zerop (length grapheme)))
	 (make-grid-char :fg    (copy-color (fatchar-fg    (elt grapheme 0)))
			 :bg    (copy-color (fatchar-bg    (elt grapheme 0)))
			 :attrs (copy-list (fatchar-attrs (elt grapheme 0)))
			 :line  (fatchar-line  (elt grapheme 0))
			 :c (if (= 1 (length grapheme))
				(fatchar-c (elt grapheme 0))
				(coerce (loop :for c :across grapheme
					   :collect (fatchar-c c))
					'string)))
	 (make-grid-char :c nil)))
    (string
     (if tty ;; @@@ the non tty case probably isn't used, also below
	 (make-grid-char :c (case (length grapheme)
			      (0 nil)
			      (1 (char grapheme 0))
			      (t grapheme))
			 :fg (copy-color (fg tty))
			 :bg (copy-color (bg tty))
			 :attrs (copy-list (attrs tty)))
	 (make-grid-char :c (case (length grapheme)
			      (0 nil)
			      (1 (char grapheme 0))
			      (t grapheme)))))
    (fatchar
     (let ((result (make-grid-char)))
       (set-grid-char result grapheme)
       result))
    (character
     (if tty ;; @@@
	 (make-grid-char :c grapheme
			 :fg (copy-color (fg tty))
			 :bg (copy-color (bg tty))
			 :attrs (copy-list (attrs tty)))
	 (make-grid-char :c grapheme)))))

(defun set-fat-char (fc gc)
  "Set the fatchar CHAR to a grid-char VALUE."
  (let* ((c (grid-char-c gc))
	 (cc (etypecase c
	       (character c)
	       (string (if (>= (length c) 1) (char c 0) +default-char+))
	       (null +default-char+)))) ;; @@@ o'really?
    ;; (assert (characterp cc))
    (setf (fatchar-c fc)     cc
	  (fatchar-fg fc)    (copy-color (grid-char-fg gc))
	  (fatchar-bg fc)    (copy-color (grid-char-bg gc))
	  (fatchar-attrs fc) (copy-list (grid-char-attrs gc))
	  (fatchar-line fc)  (grid-char-line gc))
    fc))

(defun grid-char-character-length (c)
  "Return the length of a grid-char in characters."
  (if c
      (etypecase (grid-char-c c)
	(null 0)
	(character 1)
	(string (length (grid-char-c c))))
      0))

(defun grid-string-character-length (s)
  "Return the length of a grid-char string in characters."
  (etypecase s
    (null 0)
    (grid-char (grid-char-character-length s))
    (vector
     (loop :for g :across s :sum
	(grid-char-character-length g)))))

;; @@@ no-nulls seems broken at least w/regard to *-line
(defun grid-to-fat-string (s &key (start 0) end no-nulls)
  "Return a fat-string equivalent to S. S can be a grid-string or a grid-char."
  ;; (with-output-to-fat-string (str)
  ;;   (let ((fc (make-fatchar)))
  ;;     (flet ((print-it (c)
  ;; 	       (set-fat-char fc c)
  ;; 	       (if (characterp (grid-char-c c))
  ;; 		   (progn
  ;; 		     (setf (fatchar-c fc) (grid-char-c c))
  ;; 		     (princ fc str))
  ;; 		   (loop :for j :from 0 :below (length (grid-char-c c))
  ;; 		      :do
  ;; 		      (setf (fatchar-c fc) (aref (grid-char-c c) j))
  ;; 		      (princ fc str)))))
  ;; 	(if (grid-char-p s)
  ;; 	    (print-it s)
  ;; 	    (loop :for i :from start :below (or end (length s))
  ;; 	       :do (print-it (aref s i))))))))
  (let* ((len (grid-string-character-length s))
	 (result (make-array len
			     :element-type 'fatchar
			     :initial-element (make-fatchar)))
	 (j 0))
    (flet ((add-grid-char (char)
	     "Add CHAR to the result."
	     (etypecase (grid-char-c char)
	       (null
		;; Ignore it unless it has other data.
		;; @@@ It this reasonable?
		(when (and (or (grid-char-fg    char)
			       (grid-char-bg    char)
			       (grid-char-attrs char)
			       (not (zerop (grid-char-line char))))
			   (not no-nulls))
		  (setf (aref result j)
			(make-fatchar :fg    (copy-color (grid-char-fg char))
				      :bg    (copy-color (grid-char-bg char))
				      :attrs (copy-list (grid-char-attrs char))
				      :line  (grid-char-line char)))
		  (incf j)))
	       (character
		(when (or (not no-nulls)
			  (char/= (grid-char-c char) #.(code-char 0)))
		  (setf (aref result j)
			(make-fatchar :c     (grid-char-c char)
				      :fg    (copy-color (grid-char-fg char))
				      :bg    (copy-color (grid-char-bg char))
				      :attrs (copy-list (grid-char-attrs char))
				      :line  (grid-char-line char)))
		  (incf j)))
	       (string
		(loop :for c :across (grid-char-c char)
		   :do
		     (when (or (not no-nulls)
			       (char/= (grid-char-c char) #.(code-char 0)))
		       (setf (aref result j)
			     (make-fatchar
			      :c c
			      :fg (copy-color (grid-char-fg char))
			      :bg (copy-color (grid-char-bg char))
			      :attrs (copy-list (grid-char-attrs char))
			      :line (grid-char-line  char)))
		       (incf j)))))))
    (etypecase s
      (null result)
      (grid-char (add-grid-char s))
      (vector
       (loop
	  :for i :from start :below (or end (length s))
	  :do
	  (add-grid-char (aref s i)))))
    (make-fat-string :string result))))

;; @@@ This is probably only for debugging
(defun fat-string-to-grid-string (fs)
  "Make a grid-char string from a fat-string."
  (coerce
   (loop :for c :in (graphemes fs)
      :collect (grapheme-to-grid-char c))
   'vector))

(defmethod print-object ((obj grid-char) stream)
  "Print a ‘grid-char’ to a ‘fat-string-output-stream’."
  ;;(format t "stream is a ~a ~a~%" (type-of stream) stream)
  (cond
    ((or *print-readably* *print-escape*)
     ;; Print as a structure:
     ;;(dbugf :crunch "NOPE ~s~%" (type-of obj))
     ;;(print-unreadable-object (obj stream :identity t :type t))
     (call-next-method obj stream)
     )
    ((typep stream 'terminal:terminal-stream)
     ;;(format t "BLURB~s~%" (type-of obj)) (finish-output)
     (let ((str (grid-to-fat-string obj)))
       (render-fat-string str)))
    ((typep stream 'fat-string-output-stream)
     ;;(dbugf :crunch "BLURB Good ~s~%" (type-of obj))
     (let ((str (grid-to-fat-string obj)))
       (write-fat-string str :stream stream)))
    (t
     ;; (dbugf :crunch "NLURB not so good ~s ~s~%"
     ;; 	    (type-of obj) (type-of stream))
     (let ((str (grid-to-fat-string obj)))
       (write (fat-string-to-string str) :stream stream))
     )))

(defstruct screen
  "Representation of the screen."
  (x      0 :type fixnum)
  (y      0 :type fixnum)
  (width  0 :type fixnum)
  (height 0 :type fixnum)
  background
  scrolling-region
  (cursor-state t :type boolean)
  (beep-count 0 :type fixnum)
  (lines  nil :type (or null (vector grid-string)))
  (index  nil :type (or null (vector fixnum)))
  (hashes nil :type (or null (vector fixnum))))

(defstruct change
  (start-x 0 :type fixnum)
  (start-y 0 :type fixnum)
  (end-x   0 :type fixnum)
  (end-y   0 :type fixnum)
  op)

(defparameter *blank-char* (make-grid-char :c #\space))
(defun blank-char ()
  (copy-grid-char *blank-char*))

(defparameter *nil-char* (make-grid-char :c nil))
(defun nil-char ()
  (copy-grid-char *nil-char*))

(defparameter *nul-char* (make-fatchar :c +default-char+))
(defun nul-char ()
  (copy-fatchar *nul-char*))

(defclass terminal-crunch-stream (terminal-stream terminal-wrapper)
  ((old-screen
    :initarg :old-screen :accessor old-screen :initform nil
    :documentation "The screen that's currently displayed on the device")
   (new-screen
    :initarg :new-screen :accessor new-screen :initform nil
    :documentation "The screen that we're constructing.")
   ;; (wrapped-terminal
   ;;  :initarg :wrapped-terminal :accessor wrapped-terminal
   ;;  :documentation "The terminal we are wrapping and we output to.")
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
    :initarg :cleared :accessor cleared :initform nil :type (member t nil saved)
    :documentation
    "True if clear was called and we have to really clear the screen, or SAVED
if we have to clear the saved history lines.")
   (started
    :initarg :started :accessor started :initform nil
    :documentation "True if we started and not ended.")
   (start-line
    :initarg :start-line :accessor start-line :initform 0 :type fixnum
    :documentation
    "Line of the screen that we start our manangment on. This can change if we
are directed to move above it, or if we scroll.")
   (last-time
    :initarg :last-time :accessor last-time :initform nil
    :documentation "Last time the wrapped terminal's device was touched, or NIL
if we dont' know.")
   (last-time-start-line
    :initarg :last-time :accessor last-time-start-line :initform nil
    :documentation "The start line active for last-time, or NIL.")
   (touched
    :initarg :touched :accessor touched :initform nil :type boolean
    :documentation "True if we did actual output.")
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
  ;; (:default-initargs
  ;;  )
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

(defgeneric touch (tty)
  (:method ((tty terminal-crunch)) (setf (touched tty) t)))

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

;(deftype hash-value fixnum)

;; I'm just deciding it's 64bits. We could use most-positive-fixnum, but that
;; might be too small on some platforms.
;; (defconstant +cut-off+ #xffffffffffffffff)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +cut-off+ most-positive-fixnum)
  (defconstant +cut-off-shift+ (- (integer-length +cut-off+))))

(defparameter *keyword-differentiator*
  #.(loop :with i = 0
       :for c :across #+64-bit-target "keyword" #+32-bit-target "keyw"
       :do (setf i (logand (logior (ash i 8) (char-code c)) +cut-off+))
       :finally (return i)))
(declaim (type fixnum *keyword-differentiator*))

(defun curses-hash-seed () 0)
(declaim (ftype (function () fixnum) curses-hash-seed))

(defun curses-hash (integer value)
  (declare (type fixnum integer value))
  (logand (+ value (ash value 5) integer) +cut-off+))
(declaim (ftype (function (fixnum fixnum) fixnum) curses-hash))

(defun bix-hash-seed () 0)
(declaim (ftype (function () fixnum) bix-hash-seed))
(defun bix-hash (integer value)
  (logand (logxor (ash value 3) integer) +cut-off+))
(declaim (ftype (function (fixnum fixnum) fixnum) bix-hash))

#+64-bit-target
(progn
  (defparameter *fnv64-offset*
    #.(logand most-positive-fixnum #xCBF29CE484222325))
  (declaim (type fixnum *fnv64-offset*))
  (defparameter *fnv64-prime* #x100000001b3)
  (declaim (type fixnum *fnv64-prime*))

  (defun fnv-like-hash-seed () *fnv64-offset*)
  (declaim (ftype (function () fixnum) bix-hash-seed))
  (defun fnv-like-hash-fixnum (integer value)
    (declare (type fixnum integer value))
    (logand (* (logxor value integer) *fnv64-prime*) +cut-off+))
  (declaim (ftype (function (fixnum fixnum) fixnum) fnv-like-hash-fixnum))
  (defun fnv-like-hash-integer (integer value)
    (declare (type integer integer)
	     (type fixnum value))
    (let ((v value)
	  (i integer))
      (loop :do
	(setf v (logand (* (logxor v i) *fnv64-prime*) +cut-off+))
	:while (> i +cut-off+)
	:do
	(setf i (ash +cut-off+ +cut-off-shift+)))
      v))
  (declaim (ftype (function (integer fixnum) fixnum) fnv-like-hash-integer))
  (defun fnv-like-hash-float (f value)
    (multiple-value-bind (i-significand i-exponent i-sign)
	(integer-decode-float f)
      (let ((v value))
	(setf v (fnv-like-hash-integer i-significand v)
	      v (fnv-like-hash-integer i-exponent v)
	      v (fnv-like-hash-integer i-sign v))
	v)))
  (declaim (ftype (function (float fixnum) fixnum) fnv-like-hash-float))

  ;;(defun sxhash-hash-seed () #xCBF29CE484222325)
  (defun sxhash-hash-seed () #.(logand most-positive-fixnum #xCBF29CE484222325))
  (declaim (ftype (function () fixnum) sxhash-hash-seed))
  (defun sxhash-hash (integer value)
    (logand (+ (sxhash integer) value) +cut-off+))
  (declaim (ftype (function (fixnum fixnum) fixnum) sxhash-hash)))

#+32-bit-target
(progn
  (defparameter *fnv32-offset*
    #.(logand most-positive-fixnum #x84222325))
  (declaim (type fixnum *fnv32-offset*))
  ;; (defparameter *fnv32-prime* #x1FFFFFFD)
  (defparameter *fnv32-prime* #x1F2347E1)
  (declaim (type fixnum *fnv32-prime*))

  (defun fnv-like-hash-seed () *fnv32-offset*)
  (declaim (ftype (function () fixnum) bix-hash-seed))
  (defun fnv-like-hash-fixnum (integer value)
    (declare (type fixnum integer value))
    (logand (* (logxor value integer) *fnv32-prime*) +cut-off+))
  (declaim (ftype (function (fixnum fixnum) fixnum) fnv-like-hash-fixnum))
  (defun fnv-like-hash-integer (integer value)
    (declare (type integer integer)
	     (type fixnum value))
    (let ((v value)
	  (i integer))
      (loop :do
	(setf v (logand (* (logxor v i) *fnv64-prime*) +cut-off+))
	:while (> i +cut-off+)
	:do
	(setf i (ash +cut-off+ +cut-off-shift+)))
      v))
  (declaim (ftype (function (integer fixnum) fixnum) fnv-like-hash-integer))
  (defun fnv-like-hash-float (f value)
    (multiple-value-bind (i-significand i-exponent i-sign)
	(integer-decode-float f)
      (let ((v value))
	(fnv-like-hash-integer i-significand v)
	(fnv-like-hash-integer i-exponent v)
	(fnv-like-hash-integer i-sign v)
	v)))
  (declaim (ftype (function (float fixnum) fixnum) fnv-like-hash-float))

  ;;(defun sxhash-hash-seed () #xCBF29CE484222325)
  (defun sxhash-hash-seed () #.(logand most-positive-fixnum #x1F2347E1))
  (declaim (ftype (function () fixnum) sxhash-hash-seed))
  (defun sxhash-hash (integer value)
    (logand (+ (sxhash integer) value) +cut-off+))
  (declaim (ftype (function (fixnum fixnum) fixnum) sxhash-hash)))

;; @@@ Am I being too paranoid doing this with a macro?
(defmacro hash-thing-with (thing value hash-func hash-seed-func)
  (declare (optimize (speed 3) (safety 0)))
  (with-names (hv c)
    (let ((f-func (symbolify (s+ hash-func "-FIXNUM")))
	  (i-func (symbolify (s+ hash-func "-INTEGER")))
	  (fl-func (symbolify (s+ hash-func "-FLOAT"))))
    `(typecase ,thing
       (character (,f-func (char-code ,thing) ,value))
       (fixnum    (,f-func ,thing ,value))
       (integer   (,i-func ,thing ,value))
       (float     (,fl-func ,thing ,value))
       ;; If it is a more complicated number than an fixnum, integer, or float,
       ;; defer to sxhash.
       (number    (logand (+ value (sxhash thing)) +cut-off+))
       ;; Add in *keyword-differentiator* so keywods hash to different values
       ;; than the hash of their symbol names.
       (keyword   (hash-thing (symbol-name ,thing)
			      (,f-func *keyword-differentiator* ,value)))
       (array ;; also string of course
	(loop :with ,hv fixnum = (or ,value (,hash-seed-func))
	   :for ,c :across ,thing
	   :do (setf ,hv (hash-thing ,c ,hv))
	   :finally (return ,hv)))
       (list
	(loop :with ,hv fixnum = (or ,value (,hash-seed-func))
	   :for ,c :in ,thing
	   :do (setf ,hv (hash-thing ,c ,hv))
	   :finally (return ,hv)))
       (fatchar
	(let ((,hv ,value))
	  (declare (type fixnum ,hv))
	  (setf ,hv (hash-thing (fatchar-c ,thing) ,hv))
	  (setf ,hv (hash-thing (fatchar-fg ,thing) ,hv))
	  (setf ,hv (hash-thing (fatchar-bg ,thing) ,hv))
	  (setf ,hv (hash-thing (fatchar-line ,thing) ,hv))
	  (setf ,hv (hash-thing (fatchar-attrs ,thing) ,hv))
	  ,hv))
       (grid-char
	(let ((,hv ,value))
	  (declare (type fixnum ,hv))
	  (setf ,hv (hash-thing (grid-char-c ,thing) ,hv))
	  (setf ,hv (hash-thing (grid-char-fg ,thing) ,hv))
	  (setf ,hv (hash-thing (grid-char-bg ,thing) ,hv))
	  (setf ,hv (hash-thing (grid-char-line ,thing) ,hv))
	  (setf ,hv (hash-thing (grid-char-attrs ,thing) ,hv))
	  ,hv))
       (otherwise
	(cond
	  ((typep ,thing '(or structure-object standard-object))
	   (let ((,hv ,value))
	     (declare (type fixnum ,hv))
	     (setf ,hv
		   #+sbcl (sb-kernel:get-lisp-obj-address ,thing)
		   #-sbcl (sxhash ,thing))
	     ,hv))
	  (t
	   (error "I don't know how to hash a ~s." (type-of ,thing)))))))))

(defun hash-thing (thing &optional (value (fnv-like-hash-seed)))
  (hash-thing-with thing value fnv-like-hash fnv-like-hash-seed))
;; (defun hash-thing (thing &optional (value (fnv-like-hash-seed)))
;;    (hash-thing-with thing value curses-hash curses-hash-seed))
;; (defun hash-thing (thing &optional (value (bix-hash-seed)))
;;     (hash-thing-with thing value bix-hash bix-hash-seed))
;; (defun hash-thing (thing &optional (value (sxhash-hash-seed)))
;;   (hash-thing-with thing value sxhash-hash sxhash-hash-seed))
(declaim (ftype (function (t &optional fixnum) fixnum) hash-thing))

(defun compute-hashes (screen &optional (start 0) end)
  (with-slots (lines hashes) screen
    (loop :for i :from start :below (or end (length lines))
       :do (setf (aref hashes i) (hash-thing (aref lines i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-screen-contents (from-screen to-screen)
  "Copy the contents of FROM-SCREEN to TO-SCREEN. It's fine if they're diffrent
sizes. It only copies the smaller of the two regions."
  (let ((min-y (min (length (screen-lines from-screen))
		    (length (screen-lines to-screen))))
	(min-x (min (length (aref (screen-lines from-screen) 0))
		    (length (aref (screen-lines to-screen) 0)))))
    (setf
     (screen-x to-screen)                (min min-x (screen-x from-screen))
     (screen-y to-screen)                (min min-y (screen-y from-screen))
     (screen-background to-screen)       (screen-background from-screen)
     (screen-scrolling-region to-screen) (screen-scrolling-region from-screen)
     (screen-cursor-state to-screen)     (screen-cursor-state from-screen)
     (screen-beep-count to-screen)       (screen-beep-count to-screen))
    (loop :for i :from 0 :below min-y :do
      (map-into (aref (screen-lines to-screen) i)
		#'copy-grid-char (aref (screen-lines from-screen) i))
      (setf (aref (screen-index to-screen) i)
	    (aref (screen-index from-screen) i)))
    (compute-hashes to-screen)))

(defun make-new-screen (rows cols)
  (let* ((lines  (make-array rows :element-type 'grid-string))
	 (hashes (make-array rows :element-type 'fixnum))
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
			:element-type 'grid-char
			:initial-element (make-grid-char))
	    (aref index i) i)
      (dotimes (j cols)
	(setf (aref (aref lines i) j) (blank-char))))
    (compute-hashes result)
    result))

(defun size-changed-p (tty)
  (with-slots ((wtty wrapped-terminal)) tty
    (or (/= (terminal-window-rows tty) (terminal-window-rows wtty))
	(/= (terminal-window-columns tty) (terminal-window-columns wtty)))))

(defun update-size (tty)
  (with-slots ((wtty wrapped-terminal)) tty
    (when (size-changed-p tty)
      ;; (dbugf :crunch "resize ~s ~s -> ~s ~s~%"
      ;; 	   (terminal-window-rows tty)
      ;; 	   (terminal-window-columns tty)
      ;; 	   (terminal-window-rows wtty)
      ;; 	   (terminal-window-columns wtty))
      ;; Resize the screens
      (let ((screen (make-new-screen (terminal-window-rows wtty)
				     (terminal-window-columns wtty))))
	(copy-screen-contents (old-screen tty) screen)
	(setf (old-screen tty) screen)
	(setf screen (make-new-screen (terminal-window-rows wtty)
				      (terminal-window-columns wtty)))
	(copy-screen-contents (new-screen tty) screen)
	(setf (new-screen tty) screen))
      (setf (terminal-window-rows tty) (terminal-window-rows wtty)
	    (terminal-window-columns tty) (terminal-window-columns wtty)))))

(defmethod terminal-get-size ((tty terminal-crunch))
  "Get the window size from the wrapped terminal and store it in tty."
  (with-slots ((wtty wrapped-terminal)) tty
    (multiple-value-prog1 (terminal-get-size wtty)
      ;; (dbugf :crunch "get-size ~s ~s~%"
      ;; 	   (terminal-window-rows wtty)
      ;; 	   (terminal-window-columns wtty))
      ;; Potentially resize the screens
      (update-size tty))))

(defmethod terminal-char-at ((tty terminal-crunch) row col)
  "Return the character at ROW and COL of terminal, or NIL if there is none or
the terminal doesn't support it."
  (grid-to-fat-string (aref (aref (screen-lines (new-screen tty)) row) col)))

(defun unset-grid-char (c)
  "Make a fatchar unset."
  (setf (grid-char-c     c)	nil
	(grid-char-fg    c)	nil
	(grid-char-bg    c)	nil
	(grid-char-line  c)	0
	(grid-char-attrs c)	nil))

#|
(defun invalidate-before-start-row (tty screen)
  (with-slots (start-line) tty
    (loop :for i :from 0 :below (min (1+ start-line)
				     (length (screen-lines screen)))
       :do
       (loop :for c :across (aref (screen-lines screen) i)
	  :do (unset-grid-char c))
       ;; (setf (aref (screen-lines screen) i)
       ;; 	     (make-grid-string (screen-width screen)))
       (setf (aref (screen-hashes screen) i)
	     (hash-thing (aref (screen-lines screen) i)))
       ;; @@@ do we really need to set the index?
       (setf (aref (screen-index screen) i) i))))
|#

(defun invalidate-lines (tty screen start end)
  (assert (and (>= start 0) (< start (terminal-window-rows tty))))
  (assert (and (>= end 0) (<= end (terminal-window-rows tty))))
  (loop :for i :from start :below end
     :do
     (loop :for c :across (aref (screen-lines screen) i)
	:do (unset-grid-char c))
     ;; (setf (aref (screen-lines screen) i)
     ;; 	     (make-grid-string (screen-width screen)))
     (setf (aref (screen-hashes screen) i)
	   (hash-thing (aref (screen-lines screen) i)))
     ;; @@@ do we really need to set the index?
     (setf (aref (screen-index screen) i) i)))

(defmethod terminal-reinitialize ((tty terminal-crunch))
  ;;(dbugf :crunch "Crunch ~s not recursivley re-started.~%" tty)
  ;; Non-dumb terminals are supposed to start in :char mode.
  ;; (setf (terminal-input-mode wtty) :char)
  (with-slots ((wtty wrapped-terminal) start-line last-time last-time-start-line
	       (start-at-current-line terminal::start-at-current-line)) tty
    (when start-at-current-line
      #+(or) ;; @@@@ FORKED UP
      (let ((keep-stuff (equalp (terminal-device-time tty) last-time)))
	(dbugf :kaka "@@@@@ Keeping stuff from ~s ~s~%" last-time-start-line
	       start-line)
	(setf start-line
	      (if keep-stuff
		  last-time-start-line
		  (terminal-get-cursor-position wtty)))
	(when (not keep-stuff)
	  (setf (screen-y (new-screen tty)) start-line
		(screen-y (old-screen tty)) start-line
		last-time-start-line start-line))
	(update-size tty)
	;; @@@ Maybe everything should be invalidated?
	;; (invalidate-before-start-row tty (new-screen tty))
	;; (invalidate-before-start-row tty (old-screen tty))
	;; (invalidate-lines tty (new-screen tty) start-line
	;; 			(1- (terminal-window-rows tty)))
	;; (invalidate-lines tty (old-screen tty) start-line
	;; 			(1- (terminal-window-rows tty)))
	(when (not keep-stuff)
	  (invalidate-lines tty (new-screen tty) 0
			    (terminal-window-rows tty))
	  (invalidate-lines tty (old-screen tty) 0
			    (terminal-window-rows tty)))
	;; (dbugf :crunk "Crunch auto re-starting at ~s~%" start-line)
	;; (dbugf :crunch "allow-scrolling = ~s.~%" (allow-scrolling tty))
	))
    (setf start-line
	  (terminal-get-cursor-position wtty)
	  (screen-y (new-screen tty)) start-line
	  (screen-y (old-screen tty)) start-line)
    (update-size tty)
    ;; @@@ Maybe everything should be invalidated?
    ;; (invalidate-before-start-row tty (new-screen tty))
    ;; (invalidate-before-start-row tty (old-screen tty))
    ;; (invalidate-lines tty (new-screen tty) start-line
    ;; 			(1- (terminal-window-rows tty)))
    ;; (invalidate-lines tty (old-screen tty) start-line
    ;; 			(1- (terminal-window-rows tty)))
    (invalidate-lines tty (new-screen tty) 0
		      (terminal-window-rows tty))
    (invalidate-lines tty (old-screen tty) 0
		      (terminal-window-rows tty))
    ;; (dbugf :crunk "Crunch auto re-starting at ~s~%" start-line)
    ;; (dbugf :crunch "allow-scrolling = ~s.~%" (allow-scrolling tty))
    ;; @@@ Is this reasonable?
    ;; (terminal-erase-below tty)
    ;; (terminal-erase-below wtty)
    ;; (terminal-reset tty)
    (incf (started tty))
    (terminal-reinitialize wtty)))

(defmethod terminal-start ((tty terminal-crunch))
  "Set up the terminal for reading a character at a time without echoing."
  (with-slots ((wtty wrapped-terminal) start-line
	       (start-at-current-line terminal::start-at-current-line)) tty
    (if (started tty)
	;; Already started
	(terminal-reinitialize tty)
	;; Not already started
	(let ((state (terminal-start wtty)))
	  ;; Set our file descriptor to the wrapped terminals.
	  (when (terminal-file-descriptor wtty)
	    (setf (terminal-file-descriptor tty)
		  (terminal-file-descriptor wtty)))
	  (terminal-get-size wtty)
	  (when start-at-current-line
	    ;;(dbugf :crunk "Crunch auto starting at ~s~%" start-line)
	    (setf start-line (terminal-get-cursor-position wtty)))

	  ;; new screen
	  ;; (dbugf :crunch "new-screen.~%")
	  (when (not (new-screen tty))
	    (setf (new-screen tty)
		  (make-new-screen (terminal-window-rows wtty)
				   (terminal-window-columns wtty)))
	    (when (not (zerop start-line))
	      ;; (dbugf :crunk "Crunch new starting at ~s~%" start-line)
	      (setf (screen-y (new-screen tty)) start-line)))

	  ;; old screen
	  ;; (dbugf :crunch "old-screen.~%")
	  (when (not (old-screen tty))
	    (setf (old-screen tty)
		  (make-new-screen (terminal-window-rows wtty)
				   (terminal-window-columns wtty)))
	    (when (not (zerop start-line))
	      ;; (dbugf :crunk "Crunch old starting at ~s~%" start-line)
	      (setf (screen-y (old-screen tty)) start-line))
	    (compute-hashes (old-screen tty)))

	  ;; (dbugf :crunch "rows and cols.~%")
	  (setf (terminal-window-rows tty) (terminal-window-rows wtty)
		(terminal-window-columns tty) (terminal-window-columns wtty))

	  ;; Start with a clean slate.
	  (if (zerop start-line)
	      (progn
		(terminal-clear wtty)
		(terminal-home wtty)
		;; (dbugf :crunch "Crunch ~s started.~%" tty)
		)
	      (progn
		(dbugf :crunk "non-zero start line ~s~%" start-line)
		;; (invalidate-before-start-row tty (new-screen tty))
		;; (invalidate-before-start-row tty (old-screen tty))
		;; (invalidate-lines tty (new-screen tty) start-line
		;; 		  (1- (terminal-window-rows tty)))
		;; (invalidate-lines tty (old-screen tty) start-line
		;; 		  (1- (terminal-window-rows tty)))
		(invalidate-lines tty (new-screen tty) 0
				  (terminal-window-rows tty))
		(invalidate-lines tty (old-screen tty) 0
				  (terminal-window-rows tty))
		(terminal-move-to wtty start-line 0)
		(terminal-erase-below wtty)
		;; (dbugf :crunk "Crunch ~s started at ~d.~%" tty start-line)
		))
	  (terminal-finish-output wtty)
	  (setf (started tty) 1)
	  state))))

(defmethod terminal-end ((tty terminal-crunch) &optional state)
  "Put the terminal back to the way it was before we called terminal-start."
  (when (not (started tty))
    ;;(dbugf :crunch "Crunch ~s not started or already ended!!.~%" tty)
    ;; @@@ This is probably a bug I should fix.
    (warn "Crunch double ended.")
    (return-from terminal-end nil))
  (terminal-finish-output tty)
  (terminal-end (terminal-wrapped-terminal tty) state)
  (if (zerop (decf (started tty)))
      (progn
	;;(dbugf :crunch "Crunch ~s ended.~%" tty)
	(setf (started tty) nil))
      (progn
	;; (dbugf :crunch "Crunch ~s pop, but not ended.~%" tty)
	)))

(defmethod terminal-done ((tty terminal-crunch) &optional state)
  "Forget about the whole terminal thing and stuff."
  (terminal-done (terminal-wrapped-terminal tty) state)
  (values))

(defmethod terminal-device-time ((tty terminal-crunch))
  "Return the last time the terminal device was modified, or NIL if we don't
know."
  (terminal-device-time (terminal-wrapped-terminal tty)))

;; @@@ this needs to be complicated by the scrolling-region
;; As you may know: scrolling up
#|────────────────────────────────────┬─────────────────────────────────────╮
 │                                    │                                     │
 │           <Before>                 │              <After>                │
 │  ,-- 0                             │   ,--                               │
 │  |                                 │   |                                 │
 │  | N lines to discard              │   |                                 │
 │  |                                 │   |                                 │
 │  `-- n - 1                         │   |                                 │
 │  ,-- n                             │   |  height - N lines to keep       │
 │  |                                 │   |                                 │
 │  |                                 │   |                                 │
 │  |  height - N lines to keep       │   |                                 │
 │  |                                 │   |                                 │
 │  |                                 │   |                                 │
 │  |                                 │   `--                               │
 │  |           ,--                   │   ,--                               │
 │  |           |                     │   |                                 │
 │  |           | N blank lines       │   | N blank lines                   │
 │  |           |                     │   |                                 │
 │  `-- height  `--                   │   `--                               │
 │                                    │                                     │
 ╰────────────────────────────────────┴─────────────────────────────────────|#

;; scrolling back or down
#|────────────────────────────────────┬─────────────────────────────────────╮
 │                                    │                                     │
 │           <Before>                 │              <After>                │
 │  ,-- 0        ,--                  │   ,--                               │
 │  |            |                    │   |                                 │
 │  |            | N blank lines      │   | N blank lines                   │
 │  |            |                    │   |                                 │
 │  |            `--                  │   `--                               │
 │  |                                 │   ,-- n + 1                         │
 │  |  height - N lines to keep       │   |                                 │
 │  |                                 │   |                                 │
 │  |                                 │   |                                 │
 │  |                                 │   |                                 │
 │  |                                 │   |  height - N lines to keep       │
 │  `-- height - n                    │   |                                 │
 │  ,-- (height - n) + 1              │   |                                 │
 │  |                                 │   |                                 │
 │  | N lines to discard              │   |                                 │
 │  |                                 │   |                                 │
 │  `-- height                        │   `-- height                        │
 │                                    │                                     │
 ╰────────────────────────────────────┴─────────────────────────────────────|#

(defun index-blanker (x)
  "Blank out line index X."
  (fill x -1))

(defun line-blanker (x)
  "Blank out screen lines X."
  (loop :for line :across x :do
       (fill-by line #'blank-char)))

;; (defun xline-blanker (x)
;;   "Blank out screen lines X."
;;   (loop :for line :across x :do
;;        (fill-by line (lambda () (make-grid-char :c #\x)))))

#|
(defun scroll-copy (n height array blanker)
  "Copy ‘array’ for scrolling ‘N’ lines in a ‘height’ window. ‘blanker’ is a
function to blank with."
  (cond
    ((plusp n)
     (let ((new-blanks (subseq array 0 n))) ; save overwritten lines
       ;; Copy the retained lines up
       (setf (subseq array 0 (- height n))
	     (subseq array n height))
       ;; (dbugf :crunch "scroll-copy ~d ~d -> 0 ~d~%" n height (- height n))
       ;; Move the new blank lines in place.
       (setf (subseq array (- height n)) new-blanks)
       ;; Blank out the newly blank lines
       (funcall blanker new-blanks)))
    (t ;; minusp
     (let ((offset (abs n))
	   (new-blanks (subseq array (+ height n))))
       ;; Copy the retained lines down
       ;;(setf (subseq array (1+ offset))
       (setf (subseq array offset)
	     (subseq array 0 (+ height n)))
       ;; Move the new blank lines in place.
       (setf (subseq array 0 offset) new-blanks)
       ;; Blank out the newly blank lines
       (funcall blanker new-blanks)))))
|#

(defun scroll-copy (n height array blanker start)
  "Copy ‘array’ for scrolling ‘N’ lines in a ‘height’ window. ‘blanker’ is a
function to blank with. ‘start’ is the line to start from, which for scrolling
is zero, but for inserting and deleting is the current line."
  (dbugf :crunch "scroll-copy ~d ~d ~d~%" n height start)
  (cond
    ((plusp n) ; up
     ;; save overwritten lines
     (let ((new-blanks (subseq array start (+ start n))))
       ;; Copy the retained lines up
       (setf (subseq array start (- height n))
	     (subseq array (+ start n) height))
       ;; (dbugf :crunch "scroll-copy ~d ~d -> 0 ~d~%" n height (- height n))
       ;; Move the new blank lines in place.
       (setf (subseq array (- height n)) new-blanks)
       ;; Blank out the newly blank lines
       (funcall blanker new-blanks)))
    (t ;; minusp aka down
     (let ((offset (abs n))
	   (new-blanks (subseq array (+ height n))))
       ;; Copy the retained lines down
       ;;(setf (subseq array (1+ offset))
       (setf (subseq array (+ start offset))
	     (subseq array start (- height offset)))
       ;; Move the new blank lines in place.
       (setf (subseq array start (+ start offset)) new-blanks)
       ;; Blank out the newly blank lines
       (funcall blanker new-blanks)))))

(defun scroll (tty n)
  "Scroll by N lines."
  ;; (dbugf :crunch "(scroll ~s)~%" n)
  (with-slots (height lines index) (new-screen tty)
    (when (not (zerop n))
      (no-hints tty))
    ;; (if-dbugf (:crunch) (dump-screen tty))
    (if (< (abs n) height)
	(progn
	  (scroll-copy n height lines #'line-blanker 0)
	  (scroll-copy n height index #'index-blanker 0))
	(progn
	  ;; Just erase everything
	  (line-blanker lines)
	  (index-blanker index)))
    ;; @@@ Maybe this triggers the thing??
    (when (not (zerop (start-line tty)))
      ;; (dbugf :crunk "start-line ~s -> ~s~%" (start-line tty)
      ;; 	     (max 0 (- (start-line tty) n)))
      (setf (start-line tty) (max 0 (- (start-line tty) n)))
      (incf (really-scroll-amount tty) n))))

(defun copy-char (tty char)
  "Put the CHAR at the current screen postion. Return true if we actually
changed the screen content."
  (with-slots (fg bg attrs delay-scroll) tty
    (with-slots (x y width height scrolling-region lines) (new-screen tty)
      (let (changed)
	(labels ((char-char (c)
		   (etypecase c
		     (grid-char (grid-char-c c))
		     (fatchar (fatchar-c c))
		     (fatchar-string (if (= (length c) 1) (elt c 0) c))
		     (string (if (= (length c) 1) (char c 0) c))
		     (character c)))
		 (regularize (c)
		   (etypecase c
		     ((or fatchar character) c)
		     (string (case (length c)
			       (0 nil)
			       (1 (char c 0))
			       (otherwise c)))))
		 (set-char (gc char)
		   (etypecase char
		     ((or character string)
		      (let* ((rc (regularize char))
			     (new-gc
			      (make-grid-char
			       :c rc :fg fg :bg bg :attrs attrs
			       :line 0))) ;; unless it's a line char??
			(when (not (grid-char= gc new-gc))
			  (setf (grid-char-c gc) rc
				(grid-char-fg gc) fg
				(grid-char-bg gc) bg
				(grid-char-attrs gc) attrs
				(grid-char-line gc) 0
				changed t))))
		     (fatchar
		      (when (not (grid-char= gc char))
			(setf (grid-char-c gc)     (fatchar-c char)
			      (grid-char-fg gc)    (fatchar-fg char)
			      (grid-char-bg gc)    (fatchar-bg char)
			      (grid-char-attrs gc) (fatchar-attrs char)
			      (grid-char-line gc)  (fatchar-line char)
			      changed t)))
		     (grid-char
		      (when (not (grid-char= gc char))
			(set-grid-char gc char)
			(setf changed t)))))
		 (scroll-one-line ()
		   (no-hints tty)
		   (scroll tty 1)
		   (setf x 0 changed t))
		 (delayed-scroll ()
		   (when (delay-scroll tty)
		     (setf (delay-scroll tty) nil)
		     ;; Actually scroll when in the bottom right corner.
		     (when (and (= y (1- height))
				(= x (1- width)))
		       ;; (dbugf :crunch "Delayed scroll~%")
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
			       ;; (dbugf :crunch "Delaying scroll @ ~d ~d~%" x y)
			       (setf (delay-scroll tty) t))
			     (progn
			       ;; (dbugf :crunch "next-line scroll-one-line~%")
			       (scroll-one-line)))))))
	  (case (char-char char)
	    (#\newline
	     (delayed-scroll)
	     ;; (terminal-erase-to-eol tty)
	     (setf x 0 changed t)
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
	     (let* ((len (display-length char))
		    (new-x (+ x len)))
	       (cond
		 ;; Normal, no wrap
		 ((< new-x width)
		  ;; (dbugf :fuk "Normal, no wrap. new-x = ~s x ~s y ~s~%"
		  ;; 	 new-x x y)
		  (set-char (aref (aref lines y) x) char)
		  (when (> len 1)
		    ;; "Underchar removal"
		    (unset-grid-char (aref (aref lines y) (1+ x))))
		  (setf x new-x))
		 ;; Single cell wrap
		 ((= new-x width)
		  ;; (dbugf :fuk "Single cell wrap. new-x = ~s x ~s y ~s~%"
		  ;; 	 new-x x y)
		  (set-char (aref (aref lines y) x) char)
		  (when (> len 1)
		    ;; "Underchar removal"
		    (unset-grid-char (aref (aref lines y) (1+ x))))
		  (next-line))
		 ;; Multi-cell character wrap
		 ((> new-x width)
		  ;; (dbugf :fuk "Multi-cell wrap. new-x = ~s x ~s y ~s~%"
		  ;; 	 new-x x y)
		  (next-line)
		  (set-char (aref (aref lines y) x) char)
		  (when (> len 1)
		    ;; "Underchar removal"
		    (unset-grid-char (aref (aref lines y) (1+ x))))
		  (setf x len))))
	     (when changed
	       (note-single-line tty))
	     )))
	changed))))

(defun copy-to-screen (tty string &key start end)
  "Copy the STRING from START to END to the screen. Return true if we actually
changed the screen contents."
  (with-slots (#|x y width height fg bg attrs scrolling-region|#) (new-screen tty)
    (loop
       :with changed
       ;; :and str = (if (or (and start (not (zerop start))) end)
       ;; 		      (if end
       ;; 			  (displaced-subseq string (or start 0) end)
       ;; 			  (displaced-subseq string start))
       ;; 		      string)
       ;; :with len = (or end (length string))
       ;; :while (< i len)
       :for c :in (graphemes
		   (cond ;; @@@ What's better? this or splicing?
		     ((and start end) (osubseq string start end))
		     (start (osubseq string start))
		     (end (osubseq string 0 end))
		     (t string)))
       :do
       (when (copy-char tty (grapheme-to-grid-char c :tty tty))
	 (setf changed t))
       ;;(incf i)
       :finally (return changed))))

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
  "Return true if the rest of the grid-char LINE is blank, starting from START."
  ;; This seems faster than the equivalent position-if.
  (and (not (position *blank-char* line
		      :start start
		      :test (lambda (a b) (not (grid-char= a b)))))
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
    (when (copy-to-screen tty string)
      (note-change tty string))))

(defmethod terminal-alternate-characters ((tty terminal-crunch) state)
  (declare (ignore tty state))
  ;; Let's just assume it will work.
  )

(defmethod terminal-write-string ((tty terminal-crunch-stream) (str string)
				  &key start end)
  "Output a string to the terminal."
  (when (copy-to-screen tty str :start start :end end)
    (note-change tty str)))

(defmethod terminal-write-line ((tty terminal-crunch-stream) str
				  &key start end)
  "Output a string to the terminal."
  (when (copy-to-screen tty str :start start :end end)
    (note-change tty str))
  (when (copy-char tty #\newline)
    (note-change tty #\newline)))

(defmethod terminal-write-string ((tty terminal-crunch-stream) (str fat-string)
				  &key start end)
  "Output a string to the terminal."
  ;;(when (copy-to-screen tty (fat-string-string str) :start start :end end)
  (when (copy-to-screen tty str :start start :end end)
    (note-change tty str)))

(defmethod terminal-write-line ((tty terminal-crunch-stream) (str fat-string)
				  &key start end)
  "Output a string to the terminal."
  ;;(when (copy-to-screen tty (fat-string-string str) :start start :end end)
  (when (copy-to-screen tty str :start start :end end)
    (note-change tty str))
  (when (copy-char tty #\newline)
    (note-change tty #\newline)))

(defmethod terminal-write-char ((tty terminal-crunch-stream) char)
  "Output a character to the terminal. Flush output if it is a newline,
i.e. the terminal is 'line buffered'."
  (when (copy-char tty char)
    (note-change tty char)))

(defmethod terminal-write-char ((tty terminal-crunch-stream) (char fatchar))
  "Output a character to the terminal. Flush output if it is a newline,
i.e. the terminal is 'line buffered'."
  (when (copy-char tty char)
    (note-change tty char)))

(defmethod terminal-newline ((tty terminal-crunch-stream))
  (terminal-write-char tty #\newline))

(defmethod terminal-fresh-line ((tty terminal-crunch-stream))
  (when (not (zerop (screen-x (new-screen tty))))
    (terminal-write-char tty #\newline)
    t))

(defmethod terminal-move-to ((tty terminal-crunch-stream) row col)
  (setf (screen-y (new-screen tty))
	(max 0 (min row (1- (screen-height (new-screen tty)))))
	(screen-x (new-screen tty))
	(max 0 (min col (1- (screen-width (new-screen tty))))))
  (when (not (zerop (start-line tty)))
    ;; (if-dbugf (:crunk) (when (< row (start-line tty))
    ;; 		       (dbugf :crunk "moving start-line to ~s" row)))
    (setf (start-line tty) (max 0 (min (start-line tty) row)))))

(defmethod terminal-move-to-col ((tty terminal-crunch-stream) col)
  (setf (screen-x (new-screen tty))
	(max 0 (min col (1- (screen-width (new-screen tty)))))))

(defmethod terminal-move-to-row ((tty terminal-crunch-stream) row)
  (setf (screen-y (new-screen tty))
	(max 0 (min row (1- (screen-height (new-screen tty)))))))

(defmethod terminal-beginning-of-line ((tty terminal-crunch-stream))
  (setf (screen-x (new-screen tty)) 0))

(defmethod terminal-delete-char ((tty terminal-crunch-stream) &optional (n 1))
  (with-slots (x y width lines) (new-screen tty)
    (clampf n 0 (- width x))
    (setf (subseq (aref lines y) x (max 0 (- width n)))
	  (subseq (aref lines y) (min (+ x n) (1- width))))
    (fill-by (aref lines y) #'blank-char :start (max 0 (- width n)))
    (note-length-based tty n :delete)))

(defmethod terminal-insert-char ((tty terminal-crunch-stream) &optional (n 1))
  (with-slots (x y width lines) (new-screen tty)
    (clampf n 0 (- width x))
    (setf (subseq (aref lines y) (min (+ x n) (1- width)))
	  (subseq (aref lines y) x))
    (fill-by (aref lines y) #'blank-char :start x :end (+ x n))
    (note-length-based tty n :insert)))

(defmethod terminal-delete-line ((tty terminal-crunch-stream) &optional (n 1))
  (with-slots (y height lines index) (new-screen tty)
    (clampf n 0 (- height y))
    (when (not (zerop n))
      ;; deleting lines forces the cursor to the left edge
      (setf (screen-x (new-screen tty)) 0)

      ;; deleting is like scrolling up as if the current line is the top
      (scroll-copy n height lines #'line-blanker y)
      (scroll-copy n height index #'index-blanker y))
      (no-hints tty)))

(defmethod terminal-insert-line ((tty terminal-crunch-stream) &optional (n 1))
  (with-slots (y height lines index) (new-screen tty)
    (clampf n 0 (- height y))
    (when (not (zerop n))
      ;; inserting lines forces the cursor to the left edge
      (setf (screen-x (new-screen tty)) 0)

      ;; inserting is like scrolling down as if the current line is the top
      (scroll-copy (- n) height lines #'line-blanker y)
      (scroll-copy (- n) height index #'index-blanker y))
      (no-hints tty)))

;; @@@ Actually this is wrong. We do. :(
;;
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
;;
;; Except that it turns out we have little choice but to implement a form
;; of ‘hyperspace’ in the bottom right corner with "delay-scroll".

(defmethod terminal-backward ((tty terminal-crunch-stream) &optional (n 1))
  (setf (screen-x (new-screen tty))
	(max 0 (- (screen-x (new-screen tty)) n))))

(defmethod terminal-forward ((tty terminal-crunch-stream) &optional (n 1))
  (setf (screen-x (new-screen tty))
	(min (1- (screen-width (new-screen tty)))
	     (+ (screen-x (new-screen tty)) n))))

(defmethod terminal-up ((tty terminal-crunch-stream) &optional (n 1))
  (setf (screen-y (new-screen tty))
	(max 0 (- (screen-y (new-screen tty)) n)))
  (when (not (zerop (start-line tty)))
    ;; (if-dbugf (:crunk) (when (< (screen-y (new-screen tty)) (start-line tty))
    ;; 			 (dbugf :crunk "moving start-line to ~s"
    ;; 				(screen-y (new-screen tty)))))
    (setf (start-line tty)
	  (max 0 (min (start-line tty) (screen-y (new-screen tty)))))))

(defmethod terminal-down ((tty terminal-crunch-stream) &optional (n 1))
  (setf (screen-y (new-screen tty))
	(min (1- (screen-height (new-screen tty)))
	     (+ (screen-y (new-screen tty)) n))))

(defmethod terminal-scroll-down ((tty terminal-crunch-stream) n)
  ;; Even if allow-scrolling is false.
  (when (> n 0)
    (with-slots (y height) (new-screen tty)
      (let ((start-y y))
	(terminal-down tty n)
	(when (> n (- height (1+ start-y)))
	  (scroll tty (- n (- height (1+ start-y)))))))))

(defmethod terminal-scroll-up ((tty terminal-crunch-stream) n)
  ;; Even if allow-scrolling is false.
  (when (> n 0)
    (with-slots (y height) (new-screen tty)
      (let ((start-y y))
	(terminal-up tty n)
	(when (> n start-y)
	  (scroll tty (- (- n start-y))))))))

(defmethod terminal-scroll-screen-up ((tty terminal-crunch-stream)
				      &optional (n 1))
  (scroll tty n))

(defmethod terminal-scroll-screen-down ((tty terminal-crunch-stream)
					&optional (n 1))
  (scroll tty (- n)))

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

(defmethod terminal-clear ((tty terminal-crunch-stream) &key saved-p)
  (loop :for line :across (screen-lines (new-screen tty)) :do
     (fill-by line #'blank-char))
  (setf (cleared tty) (if saved-p 'saved t)
	(start-line tty) 0)
  ;; (dbugf :crunk "clear start-line = 0~%")
  (no-hints tty))

(defmethod terminal-home ((tty terminal-crunch-stream))
  (setf (screen-x (new-screen tty)) 0
	(screen-y (new-screen tty)) 0)
  (when (not (zerop (start-line tty)))
    ;; (dbugf :crunk "home start-line = 0~%")
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
  (when fg
    (setf (fg tty) (dcolor:copy-color fg)))
  (when bg
    (setf (bg tty) (dcolor:copy-color bg))))

(defmethod terminal-colors ((tty terminal-crunch-stream))
  ;; Just call the wrapped one.
  (terminal-colors (terminal-wrapped-terminal tty)))

(defmethod terminal-window-foreground ((tty terminal-crunch))
  (terminal-window-foreground (terminal-wrapped-terminal tty)))

(defmethod (setf terminal-window-foreground) (color (tty terminal-crunch))
  (setf (terminal-window-foreground (terminal-wrapped-terminal tty)) color))

(defmethod terminal-window-background ((tty terminal-crunch))
  (terminal-window-background (terminal-wrapped-terminal tty)))

(defmethod (setf terminal-window-background) (color (tty terminal-crunch))
  (setf (terminal-window-background (terminal-wrapped-terminal tty)) color))

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

(defmethod terminal-set-attribute ((tty terminal-crunch) attribute
				   &optional (state t))
  "Turn the given ‘attribute’ on or off, according to the boolean ‘state’."
  (if state
      (pushnew attribute (attrs tty))
      (setf (attrs tty) (remove attribute (attrs tty))))
  nil)

(defmethod terminal-set-rendition ((tty terminal-crunch) fatchar)
  "Set the colors and attributes given in the ‘fatchar’."
  (setf (attrs tty) (copy-list (fatchar-attrs fatchar))
	(fg tty) (dcolor:copy-color (fatchar-fg fatchar))
	(bg tty) (dcolor:copy-color (fatchar-bg fatchar))))

(defmethod terminal-title ((tty terminal-crunch))
  (terminal-title (terminal-wrapped-terminal tty)))

(defmethod (setf terminal-title) (title (tty terminal-crunch))
  "Set the title of a terminal window. The terminal is assumed to work like
XTerm or something."
  (setf (terminal-title (terminal-wrapped-terminal tty)) title))

(defmethod terminal-selection ((tty terminal-crunch) &key type)
  (terminal-selection (terminal-wrapped-terminal tty) :type type))

(defmethod (setf terminal-selection) (title (tty terminal-crunch) &key type)
  "Set the title of a terminal window. The terminal is assumed to work like
XTerm or something."
  (setf (terminal-selection (terminal-wrapped-terminal tty) :type type) title))

(defmethod terminal-has-attribute ((tty terminal-crunch) attribute)
  "Return true if the terminal can display the character attribute."
  (terminal-has-attribute (terminal-wrapped-terminal tty) attribute))

(defmethod terminal-has-autowrap-delay ((tty terminal-crunch))
  "Return true if the terminal delays automatic wrapping at the end of a line."
  nil)

(defmethod terminal-events-enabled ((tty terminal-crunch))
  "Allow event and return true if the terminal can allow event."
  (terminal-events-enabled (terminal-wrapped-terminal tty)))

(defmethod terminal-enable-event ((tty terminal-crunch) event)
  "Allow event and return true if the terminal can allow event."
  (terminal-enable-event (terminal-wrapped-terminal tty) event))

(defmethod terminal-disable-event ((tty terminal-crunch) event)
  "Allow event and return true if the terminal can allow event."
  (terminal-disable-event (terminal-wrapped-terminal tty) event))

(defmethod terminal-events-supported ((tty terminal-crunch))
  "Return a list of events this terminal supports."
  (terminal-events-supported (terminal-wrapped-terminal tty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Somewhat unlike the old world, our costs could be unrelated to the number
;; of characters output.

(defgeneric output-cost (terminal operation &rest params)
  (:documentation
   "Return a number representing the cost of OPERATION on TERMINAL."))

(defmethod output-cost ((tty t) (op t) &rest params)
  "Fallback for unknown costs."
  (declare (ignore tty op params))
  1)

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
  ;;(declare (ignore old-x old-y))
  (let ((wtty (terminal-wrapped-terminal tty)))
    (cond
      ((and (not (eql new-x old-x)) (not (eql new-y old-y)))
       (touch tty)
       (terminal-move-to wtty new-y new-x))
      ((not (eql new-x old-x))
       (touch tty)
       (let ((n (abs (- new-x old-x))))
	 (if (> new-x old-x)
	     ;; (terminal-move-to-col wtty new-x)
	     (terminal-forward wtty n)
	     ;; (dotimes (i n) (terminal-write-char wtty #\space))
	     (terminal-move-to-col wtty new-x)
	     ;;(terminal-backward wtty n)
	     )))
      ((not (eql new-y old-y))
       (touch tty)
       (let ((n (abs (- new-y old-y))))
	 (if (> new-y old-y)
	     (terminal-down wtty n)
	     (terminal-up wtty n)))))
    (setf (update-x tty) new-x
	  (update-y tty) new-y)))

#|
(defun crunched-move-to (tty new-x new-y old-x old-y)
  "Move to NEW-X NEW-Y from OLD-X OLD-Y in a hopefully efficient way.
Set the current update position UPDATE-X UPDATE-Y in the TTY."
  (let ((wtty (terminal-wrapped-terminal tty)))
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
		     ;;; @@@ check if we can space over
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
|#

(defun update-position (tty new old)
  (crunched-move-to tty
		    (screen-x new) (screen-y new)
		    (screen-x old) (screen-y old)))

(defun update-cursor-state (tty new old)
  (when (not (eq (screen-cursor-state new)
		 (screen-cursor-state old)))
    (let ((wtty (terminal-wrapped-terminal tty)))
      (touch tty)
      (if (screen-cursor-state new)
	  (terminal-cursor-on wtty)
	  (terminal-cursor-off wtty)))))

(defun update-beeps (tty new)
  (when (not (zerop (screen-beep-count new)))
    (touch tty)
    ;; This is ridiculous. We could just compress multiple beeps to one.
    (dotimes (i (screen-beep-count new))
      (terminal-beep (terminal-wrapped-terminal tty)))
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

;; @@@ see if this helps performance
(defun fake-output-cost (tty op &rest params)
  (declare (ignore tty))
  (case op
    (:write-fatchar 1)
    (:write-fatchar-string (length (first params)))
    (t 1)))

(defun update-line (tty line)
  (let* ((old-line (aref (screen-lines (old-screen tty)) line))
	 (new-line (aref (screen-lines (new-screen tty)) line))
	 (wtty (terminal-wrapped-terminal tty))
	 new-line-cost
	 (change-cost 0)
	 first-change
	 (last-change 0)
	 change-range
	 changes
	 fs its-okay)

    ;; (dbugf :crunch "update-line ~s~%" line)
    
    ;; Go through chars and calculate approximate cost to output differences.
    (flet ((note-change-end (i)
	     (when change-range
	       (setf (cdr change-range) i)
	       (push change-range changes)
	       (setf change-range nil))))
      (loop :with disp-len
	 :for i :from 0 :below (length new-line)
	 :do
	 (if (not (grid-char= (aref new-line i) (aref old-line i)))
	     (progn
	       (when (not first-change)
		 (setf first-change i))
	       ;; Note change start
	       (when (not change-range)
		 (setf change-range (cons i nil)))

	       ;; Cost of writing this char (@@@ as if it was new)
	       (incf change-cost
		     (fake-output-cost wtty :write-fatchar
				  ;; (aref new-line i)
				  ;; @@@ faked, should convert ^^
				  (make-fatchar)
				  ))

	       ;; If we have to move since the last change, add that.
	       (when (> (- i last-change) 1)
		 (incf change-cost
		       (move-cost tty last-change line i line)))

	       (setf last-change i))
	     (progn ;; chars are equal
	       ;; Note change end
	       (note-change-end i)))
	 (when (> (setf disp-len (display-length (aref new-line i))) 1)
	   ;; Skip over occluded cells
	   ;; (dbugf :crunch "skipping occluded cells at ~s~%" i)
	   (loop :for zz :from (1+ i) :below (min (+ i disp-len)
						  (length new-line))
	      :do
	      (when (not (grid-char= (aref new-line zz) (aref old-line zz)))
		(setf its-okay t)) ;; it actually differs so don't fail below
	      (incf i)))
	 :finally (note-change-end (1- i))))

    (when changes
      (setf changes (nreverse changes))

      ;; We don't really need to calculate the whole line cost every time,
      ;; but what is a good heuristic for when we do?
      (when (> (length changes) (/ (length new-line) 2))
	(setf new-line-cost
	      (fake-output-cost wtty :write-fatchar-string new-line))))

    ;; (dbugf :crunch "new-line-cost ~a change-cost ~s~%~
    ;;                 first-change ~s last-change ~a~%~
    ;;                 change-range ~s~%"
    ;; 	   new-line-cost change-cost first-change last-change changes)

    (when (not first-change)
      (if (not its-okay)
	  (cerror "Forget about it."
		  "We thought we had to update line ~s, but we didn't?" line)
	  (return-from update-line)))

    ;; @@@ problems:
    ;;   - unbuffered write of each line?
    ;;   - uselesly writing all the null at end?

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
	     ;; (dbugf :crunch "update-line FLOOB ~s ~s ~s~%" line start c)
	     ;; (if (= start end)
	     ;; 	 (progn
	     ;; 	   (when (not fc)
	     ;; 	     (setf fc (make-fatchar)))
	     ;; 	   (terminal-write-char wtty (set-fat-char
	     ;; 				      fc (aref new-line start))))
	     ;; 	 (terminal-write-string
	     ;; 	  wtty
	     ;; 	  ;;(make-fat-string :string new-line)
	     ;; 	  ;;:start start
	     ;; 	  ;;:end (min (1+ end) (length new-line))
	     ;; 	  (grid-to-fat-string new-line
	     ;; 			      :start start
	     ;; 			      :end (min (1+ end) (length new-line)))))
	     (setf fs (ostring-right-trim
		       (list *nul-char*)
		       (grid-to-fat-string
			new-line
			:start start
			:end (min (1+ end) (length new-line))
			#| :no-nulls t |#)))
	     ;; (dbugf :koo "floob len ~s ~s~%" (olength fs) fs)
	     (terminal-write-string wtty fs)
	     (touch tty)
	     ;; (dbugf :crunch "write-string ~s ->~s<-~%"
	     ;; 	    (length (fat-string-string fs)) fs)
	     (setf (update-x tty)
		   ;; (1+ end)
		   (+ start
		      (display-length fs)
		      ;; (loop :for i :from start :to end
		      ;; 	 ;; @@@ is this good enough?
		      ;; 	 :sum (display-length (aref new-line i)))
		      )
		   )
	     ))
	(progn
	  ;; Write a whole new line
	  ;; (dbugf :crunch "update-line WINKY ~s ~s-~s~%" line first-change
	  ;; 	 (1+ last-change))
	  (setf fs (ostring-right-trim
		    (list *nul-char*)
		    (grid-to-fat-string new-line
					:start first-change
					:end (min (1+ last-change)
						  (length new-line))
					#|:no-nulls t|#)))
	  ;; (dbugf :koo "winky len ~s~%" (olength fs))
	  ;; (terminal-write-string wtty (ostring-right-trim
	  ;; 			       (list *nul-char*) fs))
	  (terminal-write-string wtty fs)
	  ;;(setf (update-x tty) (1+ last-change))
	  (setf (update-x tty) ;; @@@ useless subseq
		(display-length fs)
		;; (gs-display-length (subseq new-line first-change
		;; 			   (min (1+ last-change)
		;; 				(length new-line))))
		)
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
		 #'copy-grid-char (aref (screen-lines new) i))
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

(defun line-sames (s1 s2 &key (start 0))
  "Return a list of regions that are the same in the sequences S1 and S1,
starting from the index START, which defaults to 0. Each region is a list of
two index range pairs that are the same, e.g. :
  ((<start-1> . <end-1>) (<start-2> . <end-2>))
which says the region from <start-1> to <end-1> is the same as region from
<start-2> to <end-2>.

The algorithm it uses isn't very good. It only finds the first one of
duplicated sequences, and can have worst case O(n*m) performance."
  (let (regions j si s1-start s2-start)
    (loop :with i = start
       :while (< i (length s1))
       :do
       (setf j start si i)
       (loop :while (and (< j (length s2))
			 (< si (length s1)))
	  :do
	  (if (equal (elt s1 si) (elt s2 j))
	      (progn
		(when (not s1-start)
		  (setf s1-start si s2-start j))
		(incf si)
		(incf i))
	      (when s1-start
		(return)))
	  (incf j))
       (if s1-start
	   (progn
	     (push `((,s1-start . ,si) (,s2-start . ,j)) regions)
	     (setf s1-start nil))
	   (incf i)))
    (nreverse regions)))

(defun can-scroll (same start height)
  "If we can scroll, return values:
  DIRECTION -- :up or :down
  AMOUNT    -- lines to scroll."
  (let ((first (first same))
	(last (car (last same))))
    (labels ((moved-to-top (d)
	       "True if the diff moved to the top."
	       (and (zerop (car (second d)))
		    (not (zerop (car (first d))))))
	     (moved-to-bottom (d)
	       (and (= (cdr (second d)) (1- height))
		    (not (= (cdr (first d)) (1- height)))))
	     (size (range)
	       "The size of the range."
	       (- (cdr range) (car range)))
	     (big-enough (range)
	       "True if the range is big enough to consider scrolling."
	       (> (size range) (round height 3)))
	     (whole-p ()
	       "True if the whole thing matched."
	       (and (= (length same) 1)
		    (destructuring-bind (((s1 . e1) (s2 . e2))) same
		      ;; (and (= s1 start) (= e1 (1- height))
		      (and (= s1 start) (= e1 height)
			   (eql s1 s2) (eql e1 e2))))))
      (cond
	((whole-p) (values :same 0))
	((and (moved-to-top first) (big-enough (second first)))
	 ;; The offset of the start of the "from" range, is the amount to scroll
	 (values :up (car (first first)))
	 )
	((and (moved-to-bottom last) (big-enough (second last)))
	 ;; (values :down (- (1- height) (size (second last)))))))))
	 ;; The offset from the bottom of the end of the "from" range
	 (values :down (- height (size (second last))))
	 )))))

(defun update-display (tty)
  "This is the big crunch."
  (with-slots ((wtty wrapped-terminal)
	       (old old-screen)
	       (new new-screen)
	       start-line really-scroll-amount) tty

    ;; (if-dbugf (:crunch) (dump-screen tty))
    
    ;; Set starting point.
    (setf (update-x tty) (screen-x old)
	  (update-y tty) (screen-y old))

    (when (cleared tty)
      (terminal-clear wtty :saved-p (eq (cleared tty) 'saved))
      (terminal-finish-output wtty)
      (touch tty)
      ;; (dbugf :crunk "actually clearing!~%")
      (setf (cleared tty) nil)
      (no-hints tty)
      ;; I don't think this ends up being a good "optimization" considering
      ;; memory bandwith.
      ;; (setf (old-screen tty)
      ;; 	    (make-new-screen (screen-height old)
      ;; 			     (screen-width old)))
      ;; So just clear the old screen manually.
      (loop :for i :from 0 :below (screen-height old)
	    :do (fill-by (aref (screen-lines old) i) #'blank-char))
      (compute-hashes old))

    ;; (dbugf :crunch "****** start update @ ~s~%" start-line)
    ;; (dbugf :crunch-update "****** start update @ ~s~%" start-line)
    ;; (if-dbugf (:crunch-update)
    ;; 	      (deblarg::debugger-backtrace 10))

    ;; First, actually scroll unmanaged content if we have to.
    (when (and (not (zerop start-line))
	       (not (zerop really-scroll-amount)))
      ;;(dbugf :crunch "-------- Really scroll ~s <-----~%" really-scroll-amount)
      (crunched-move-to tty 0 (1- (screen-height old))
			(update-x tty) (update-y tty))
      (terminal-scroll-down wtty really-scroll-amount)
      (touch tty))

    ;; Try to speed things up with hints.
    (cond
      ((not (text-change tty))
       ;; (dbugf :crunch "position only ~s ~s -> ~s ~s~%"
       ;; 	      (screen-x old) (screen-y old)
       ;; 	      (screen-x new) (screen-y new))
       (update-position tty new old))
      ((single-char-change tty)
       (let* ((cx (change-start-x (single-char-change tty)))
	      (cy (change-start-y (single-char-change tty)))
	      (op (change-op (single-char-change tty)))
	      move-x
	      char-x
	      c)
	 ;; (dbugf :crunch "single char ~s~%" (single-char-change tty))
	 (flet ((put-it ()
		  (crunched-move-to tty (max 0 move-x) cy
				    (update-x tty) (update-y tty))
		  ;;(setf c (aref (aref (screen-lines new) cy) (max 0 char-x)))
		  (setf c (aref (aref (screen-lines new) cy) (max 0 char-x)))
		  (if (characterp (grid-char-c c))
		      (let ((fc (make-fatchar)))
			(set-fat-char fc c)
			(terminal-write-char wtty fc))
		      (terminal-write-string wtty (grid-to-fat-string c)))
		  (incf (update-x tty) (display-length c))))
	   (compute-hashes new cy (1+ cy))
	   ;; (if-dbugf (:crunch) (dump-hashes tty))
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
	   (update-ending-position tty new)
	   (touch tty))
	 ;;(dbugf :crunch "char = '~s'~%" c)
	 ))
      ((single-line-change tty)
       ;; diff the line
       ;; move, overwite, insert / delete as appropriate
       ;; @@@ it could be something else? like insert or delete?
       (let ((line (single-line-change tty)))
	 ;; (dbugf :crunch "single-line-change ~s~%" line)
	 (compute-hashes new line (1+ line))
	 ;; (if-dbugf (:crunch) (dump-hashes tty))
	 (when (/= (aref (screen-hashes new) line)
		   (aref (screen-hashes old) line))
	   (update-line tty line)))
       (update-ending-position tty new))
      ;; No hints.
      (t
       ;; Make the line hashes.
       (compute-hashes new start-line)
       ;; (if-dbugf (:crunch) (dump-hashes tty))

       ;; handle scrolling
       ;;   detect scrolling
       ;;   move same lines
       (let ((sames (line-sames (screen-hashes old) (screen-hashes new)
				:start start-line))
	     (start start-line)
	     (end (length (screen-hashes new)))
	     no-change)
	 (when (and sames
		    ;; We can't really scroll unless we have control of the
		    ;; whole screen, i.e. start-line is zero. It would be
		    ;; possible to use insert/delete line, but I think it
		    ;; usually looks bad, so just pretend we can't scroll.
		    (zerop start-line))
	   ;; (dbugf :crunch "sames ~s~%" sames)
	   (multiple-value-bind (dir amount)
	       (can-scroll sames start (screen-height old))
	     (case dir
	       (:same
		(setf no-change t)
		;; (dbugf :crunch "scroll: no-change~%")
		)
	       (:up
		;; actually scroll
		(crunched-move-to tty 0 (1- (screen-height old))
				  (update-x tty) (update-y tty))
		(terminal-scroll-down wtty amount)
		(touch tty)
		;; move the old screen lines so we update properly
		(scroll-copy amount (screen-height old) (screen-lines old)
			     #'line-blanker 0)
		;;(setf start (1+ (cdr (second (first sames)))))
		(setf start (cdr (second (first sames))))
		;; re-compute the hashes of the lines we have to re-draw
		(compute-hashes old start)
		;; (dbugf :crunch "scroll :up ~s start = ~s~%" amount start)
		)
	       (:down
		(crunched-move-to tty 0 0 (update-x tty) (update-y tty))
		(terminal-scroll-up wtty amount)
		(touch tty)
		(scroll-copy (- amount) (screen-height old) (screen-lines old)
			     #'line-blanker 0)
		;; (setf end (1+ (cdr (second (car (last sames))))))
		;; (setf end (car (second (car (last sames)))))
		(compute-hashes old start end)
		;; (dbugf :crunch "scroll :down ~s end = ~s~%" amount end)
		))))

	 ;; Update changed lines.
	 (when (not no-change)
	   (let ((diff-count 0)
		 (update-to end)
		 blank-start)

	     ;; Check if we can do an erase-below, when all the lines below a
	     ;; some point are changed and blank.
	     (loop :for i :from (1- end) :downto start :do
		(when (/= (aref (screen-hashes new) i)
			  (aref (screen-hashes old) i))
		  (incf diff-count))
		:until (not (every (_ (grid-char= _ *blank-char*))
				   (aref (screen-lines new) i)))
		:do (setf blank-start i))
	     (when (and (> diff-count 0)
			(and blank-start (< blank-start (1- end))))
	        (setf update-to (1+ blank-start)))

	     ;; Update the lines
	     (loop :for i :from start :below update-to :do
		(when (/= (aref (screen-hashes new) i)
			  (aref (screen-hashes old) i))
		  ;; (dbugf :crunch "hash diff ~s old ~s new ~s~%" i
		  ;; 	   (aref (screen-hashes new) i)
		  ;; 	   (aref (screen-hashes old) i))
		  (update-line tty i)))

	     ;; Erase below if we can.
	     (when (and (> diff-count 0)
			(and blank-start (< blank-start (1- end))))
	       (crunched-move-to tty
				 0 (1+ blank-start)		;; new-x new-y
				 (update-x tty) (update-y tty)) ;; old-x old-y
	       (terminal-erase-below wtty)
	       (touch tty)))))

       ;; Make sure we're at the right cursor position.
       (update-ending-position tty new)))
    (update-cursor-state tty new old)
    (update-beeps tty new)
    (copy-new-to-old tty)
    (finish-output wtty)
    ;; @@@ Here's a race condition.
    (when (touched tty)
      (dbugf :kaka "Did something. ~s~%" (terminal-device-time wtty))
      (setf (last-time tty) (terminal-device-time wtty)
	    (last-time-start-line tty) (start-line tty))
      (setf (touched tty) nil))
    (setf really-scroll-amount 0
	  (delay-scroll tty) nil)
    (reset-hints tty)))

(defmethod terminal-finish-output ((tty terminal-crunch-stream))
  "Make sure everything is output to the terminal."
  (let ((clear
	 (block nil
	   ;; One might hope that this might help? But does it?
	   (restart-case
	       (update-display tty)
	     (stop-it ()
	       :report (lambda (s) (format s "Stop updating the display."))
	       (return nil))
	     (stop-and-clear ()
	       :report (lambda (s)
			 (format s "Stop updating and clear the display."))
	       (return t)))
	   nil)))
    (when clear
      (terminal-clear tty))))

(defmethod terminal-get-char ((tty terminal-crunch))
  (terminal-finish-output tty)
  (prog1 (terminal-get-char (terminal-wrapped-terminal tty))
    (when (size-changed-p tty)
      (update-size tty))))

(defmethod terminal-get-key ((tty terminal-crunch))
  (terminal-finish-output tty)
  (prog1 (terminal-get-key (terminal-wrapped-terminal tty))
    (when (size-changed-p tty)
      (update-size tty))))

(defmethod terminal-listen-for ((tty terminal-crunch) seconds)
  (terminal-finish-output tty)
  (prog1 (terminal-listen-for (terminal-wrapped-terminal tty) seconds)
    (when (size-changed-p tty)
      (update-size tty))))

(defmethod terminal-input-mode ((tty terminal-crunch))
  (terminal-input-mode (terminal-wrapped-terminal tty)))

(defmethod (setf terminal-input-mode) (mode (tty terminal-crunch))
  (setf (terminal-input-mode (terminal-wrapped-terminal tty)) mode))

(defmethod terminal-reset ((tty terminal-crunch-stream))
  "Try to reset the terminal to a sane state, without being too disruptive."
  (with-slots (fg bg attrs (wtty wrapped-terminal)) tty
    (setf fg nil bg nil attrs nil)
    (terminal-normal wtty)
    (terminal-cursor-on tty)
    ;; @@@ wrapped terminal reset? or what? Is this too much?
    ;; (terminal-reset wtty)
    )
  (terminal-finish-output tty))

(defmethod terminal-reset ((tty terminal-crunch))
  (call-next-method) ;; Do the terminal-stream version
  (terminal-reset (terminal-wrapped-terminal tty))
  )

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
  (force-output (terminal-wrapped-terminal stream)))

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
  (stream-clear-input (terminal-wrapped-terminal stream)))

(defmethod stream-read-sequence ((stream terminal-crunch) seq start end
				 &key &allow-other-keys
				 #| &optional (start 0) end |#)
  (stream-read-sequence (terminal-wrapped-terminal stream) seq start end))

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
  (stream-read-char-no-hang (terminal-wrapped-terminal stream)))

(defmethod stream-read-char ((stream terminal-crunch))
  (stream-read-char (terminal-wrapped-terminal stream)))

(defmethod stream-read-line ((stream terminal-crunch))
  ;; This is used by ‘read-line’.  A string is returned as the first
  ;; value.  The second value is true if the string was terminated by
  ;; end-of-file instead of the end of a line.  The default method uses
  ;; repeated calls to ‘stream-read-char’.
  (stream-read-line (terminal-wrapped-terminal stream)))

(defmethod stream-listen ((stream terminal-crunch))
  ;; This is used by ‘listen’.  It returns true or false.  The default
  ;; method uses ‘stream-read-char-no-hang’ and ‘stream-unread-char’.
  ;; Most streams should define their own method since it will usually
  ;; be trivial and will always be more efficient than the default
  ;; method.
  (stream-listen (terminal-wrapped-terminal stream)))

(defmethod stream-unread-char ((stream terminal-crunch) character)
  ;; Undo the last call to ‘stream-read-char’, as in ‘unread-char’.
  ;; Return ‘nil’.  Every subclass of
  ;; ‘fundamental-character-input-stream’ must define a method for this
  ;; function.
  (stream-unread-char (terminal-wrapped-terminal stream) character))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging

(defun dump-hashes (tty)
  (let ((old-hashes (screen-hashes (old-screen tty)))
	(new-hashes (screen-hashes (new-screen tty))))
    (format *trace-output* "Old -> New~%")
    (loop :for i :from 0 :below (length old-hashes) :do
       (format *trace-output*
	       "~s ~s~%" (aref old-hashes i) (aref new-hashes i)))))

(defun dump-screen (tty)
  (let ((old-lines (screen-lines (old-screen tty)))
	(new-lines (screen-lines (new-screen tty))))
    (format *dbug-output* "Old ~s ~s -> New ~s ~s +~s~%"
	    (screen-x (old-screen tty))
	    (screen-y (old-screen tty))
	    (screen-x (new-screen tty))
	    (screen-y (new-screen tty))
	    (start-line tty))
    (flet (
	   ;; (line-str (line) (grid-to-fat-string line))
	   (line-str (line)
	     (let (str)
	       ;; (with-terminal-output-to-string (#|:ansi-stream |#)
	       (with-terminal-output-to-string (:dumb-color)
		 (setf str (grid-to-fat-string line))
		 (when str
		   (tt-write-string str)))))
	   )
      (loop :for i :from 0 :below (length old-lines) :do
	 (format *dbug-output* "[~a] [~a]~%"
		 (line-str (aref old-lines i))
		 (line-str (aref new-lines i))))))
  (finish-output *trace-output*)
  )
      ;; (loop :for i :from 0 :below (length old-lines) :do
      ;; 	 (format *debug-io* "[~a] [~a]~%"
      ;; 		 (make-fat-string
      ;; 		  :string (grid-to-fat-string (aref old-lines i)))
      ;; 		 (make-fat-string
      ;; 		  :string (grid-to-fat-string (aref new-lines i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-terminal-type :crunch 'terminal-crunch)
;;(register-terminal-type :crunch-stream 'terminal-crunch-stream)

;; EOF
