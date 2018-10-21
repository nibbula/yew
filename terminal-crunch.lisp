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
  (:use :cl :dlib :collections :char-util :fatchar :terminal
	:trivial-gray-streams :fatchar-io)
  (:import-from :terminal #:wrapped-terminal)
  (:export
   #:terminal-crunch
   #:allow-scrolling
   #:output-cost
   ))
(in-package :terminal-crunch)

(declaim
 (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
;; (declaim
;;  (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))

;; As you may know, many of the world's lovely scripts, do not fit perfectly
;; into a character grid, so neither will all unicode characters. This will only
;; really work for those scripts that do. But we do at least try to make it a
;; grid of unicode graphemes, instead of just characters. That means there can
;; be multiple characters per screen cell, or sort of hidden empty cells that
;; are under double wide characters.
;;
;; So, grid-char is like a fatchar with coalesced graphemes.
;; @@@ Someday do the experiment to make them a class and see what happens.

(defstruct grid-char
  "A grapheme with attributes."
  (c nil :type (or null character string))
  (fg nil)
  (bg nil)
  (line 0 :type fixnum)
  (attrs nil :type list))

(deftype grid-string (&optional n) `(vector grid-char ,(or n '*)))
(defun make-grid-string (n) (make-array n :element-type 'grid-char))

(defmethod display-length ((c grid-char))
  (display-length (grid-char-c c)))

(defmethod display-length ((c null))
  0) ;; @@@ so bogus

(defun gs-display-length (gs)
  (loop :for c :across gs :sum (display-length c)))

(defgeneric grid-char-same-effects (a b)
  (:documentation
   "Return true if the two fatchars have the same colors and attributes.")
  (:method ((a grid-char) (b grid-char))
    (and (equal (grid-char-fg a) (grid-char-fg b))
	 (equal (grid-char-bg a) (grid-char-bg b))
	 (not (set-exclusive-or (grid-char-attrs a) (grid-char-attrs b)
				:test #'eq))))
  (:method ((a grid-char) (b fatchar))
    (and (equal (grid-char-fg a) (fatchar-fg b))
	 (equal (grid-char-bg a) (fatchar-bg b))
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
	  (grid-char-fg char)    (grid-char-fg value)
	  (grid-char-bg char)    (grid-char-bg value)
	  (grid-char-attrs char) (grid-char-attrs value)
	  (grid-char-line char)  (grid-char-line value)))
  (:method ((char grid-char) (value fatchar))
    (setf (grid-char-c char)     (fatchar-c value)
	  (grid-char-fg char)    (fatchar-fg value)
	  (grid-char-bg char)    (fatchar-bg value)
	  (grid-char-attrs char) (fatchar-attrs value)
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
	 (make-grid-char :fg    (fatchar-fg    (elt grapheme 0))
			 :bg    (fatchar-bg    (elt grapheme 0))
			 :attrs (fatchar-attrs (elt grapheme 0))
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
			 :fg (fg tty)
			 :bg (bg tty)
			 :attrs (attrs tty))
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
			 :fg (fg tty)
			 :bg (bg tty)
			 :attrs (attrs tty))
	 (make-grid-char :c grapheme)))))

(defun set-fat-char (fc gc)
  "Set the fatchar CHAR to a grid-char VALUE."
  (let* ((c (grid-char-c gc))
	 (cc (etypecase c
	       (character c)
	       (string (if (>= (length c) 1) (char c 0) nil))
	       (null (code-char 0))))) ;; @@@ o'really?
    ;; (assert (characterp cc))
    (setf (fatchar-c fc)     cc
	  (fatchar-fg fc)    (grid-char-fg gc)
	  (fatchar-bg fc)    (grid-char-bg gc)
	  (fatchar-attrs fc) (grid-char-attrs gc)
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

(defun grid-to-fat-string (s &key (start 0) end)
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
		(when (or (grid-char-fg    char)
			  (grid-char-bg    char)
			  (grid-char-attrs char)
			  (not (zerop (grid-char-line  char))))
		  (setf (aref result j)
			(make-fatchar :fg    (grid-char-fg    char)
				      :bg    (grid-char-bg    char)
				      :attrs (grid-char-attrs char)
				      :line  (grid-char-line  char)))
		  (incf j)))
	       (character
		(setf (aref result j)
		      (make-fatchar :c     (grid-char-c char)
				    :fg    (grid-char-fg    char)
				    :bg    (grid-char-bg    char)
				    :attrs (grid-char-attrs char)
				    :line  (grid-char-line  char)))
		(incf j))
	       (string
		(loop :for c :across (grid-char-c char)
		   :do
		   (setf (aref result j)
			 (make-fatchar :c c
			               :fg    (grid-char-fg    char)
			               :bg    (grid-char-bg    char)
			               :attrs (grid-char-attrs char)
			               :line  (grid-char-line  char)))
		   (incf j))))))
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
  "Print a FATCHAR to a FAT-STRING-OUTPUT-STREAM."
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

(defmacro clamp (n start end)
  `(cond
     ((< ,n ,start) (setf ,n ,start))
     ((> ,n ,end) (setf ,n ,end))))

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

;(deftype hash-value fixnum)

;; I'm just deciding it's 64bits. We could use most-positive-fixnum, but that
;; might be too small on some platforms.
;; (defconstant +cut-off+ #xffffffffffffffff)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +cut-off+ most-positive-fixnum))

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
  (defun fnv-like-hash (integer value)
    (declare (type fixnum integer value))
    (logand (* (logxor value integer) *fnv64-prime*) +cut-off+))
  (declaim (ftype (function (fixnum fixnum) fixnum) fnv-like-hash))

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
  (defun fnv-like-hash (integer value)
    (declare (type fixnum integer value))
    (logand (* (logxor value integer) *fnv32-prime*) +cut-off+))
  (declaim (ftype (function (fixnum fixnum) fixnum) fnv-like-hash))

  ;;(defun sxhash-hash-seed () #xCBF29CE484222325)
  (defun sxhash-hash-seed () #.(logand most-positive-fixnum #x1F2347E1))
  (declaim (ftype (function () fixnum) sxhash-hash-seed))
  (defun sxhash-hash (integer value)
    (logand (+ (sxhash integer) value) +cut-off+))
  (declaim (ftype (function (fixnum fixnum) fixnum) sxhash-hash)))

;; @@@ Am I being too paranoid doing this with a macro?
(defmacro hash-thing-with (thing value hash-func hash-seed-func)
  (declare (optimize (speed 3) (safety 0)))
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
	(error "I don't know how to hash a ~s." (type-of ,thing))))))

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
	       #'copy-grid-char (aref (screen-lines from-screen) i))
     (setf (aref (screen-index to-screen) i)
	   (aref (screen-index from-screen) i)))
  (compute-hashes to-screen))

(defun make-new-screen (rows cols)
  (let* ((lines  (make-array rows :element-type 'fatchar-string))
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
    (terminal-get-size wtty)
    ;; (dbugf :crunch "get-size ~s ~s~%"
    ;; 	   (terminal-window-rows wtty)
    ;; 	   (terminal-window-columns wtty))
    ;; Potentially resize the screens
    (update-size tty)))

(defun unset-grid-char (c)
  "Make a fatchar unset."
  (setf (grid-char-c     c)	nil
	(grid-char-fg    c)	nil
	(grid-char-bg    c)	nil
	(grid-char-line  c)	0
	(grid-char-attrs c)	nil))

(defun invalidate-before-start-row (tty screen)
  (with-slots (start-line) tty
    (loop :for i :from 0 :below (start-line tty)
       :do
       (loop :for c :across (aref (screen-lines screen) i)
	  :do (unset-grid-char c))
       ;; (setf (aref (screen-lines screen) i)
       ;; 	     (make-grid-string (screen-width screen)))
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
	  ;;(dbugf :crunch "Crunch ~s not recursivley re-started.~%" tty)
	  ;; Non-dumb terminals are supposed to start in :char mode.
	  (setf (terminal-input-mode wtty) :char)
	  (when start-at-current-line
	    (setf start-line (terminal-get-cursor-position wtty)
		  (screen-y (new-screen tty)) start-line
		  (screen-y (old-screen tty)) start-line)
	    ;; (update-size tty)
	    (invalidate-before-start-row tty (new-screen tty))
	    (invalidate-before-start-row tty (old-screen tty))
	    ;;(dbugf :crunch "Crunch auto re-starting at ~s.~%" start-line)
	    )
	  ;; @@@ Is this reasonable?
	  ;; (terminal-erase-below tty)
	  ;; (terminal-erase-below wtty)
	  ;; (terminal-reset tty)
	  (incf (started tty))
	  nil) ;; no state
	(let ((state (terminal-start wtty)))
	  ;; Set our file descriptor to the wrapped terminals.
	  (when (terminal-file-descriptor wtty)
	    (setf (terminal-file-descriptor tty)
		  (terminal-file-descriptor wtty)))
	  (terminal-get-size wtty)
	  (when start-at-current-line
	    ;;(dbugf :crunch "Crunch auto starting at ~s.~%" start-line)
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
		;;(dbugf :crunch "Crunch ~s started.~%" tty)
		)
	      (progn
		(invalidate-before-start-row tty (new-screen tty))
		(invalidate-before-start-row tty (old-screen tty))
		(terminal-move-to wtty start-line 0)
		(terminal-erase-below wtty)
		;;(dbugf :crunch "Crunch ~s started at ~d.~%" tty start-line)
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

;; scrolling back
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
||          <Before>             ||            <After>            ||
|| ,-- 0        ,--              || ,--                           ||
|| |            |                || |                             ||
|| |            | N blank lines  || | N blank lines               ||
|| |            |                || |                             ||
|| |            `--              || `--                           ||
|| |                             || ,-- n + 1                     ||
|| |  height - N lines to keep   || |                             ||
|| |                             || |                             ||
|| |                             || |                             ||
|| |                             || |                             ||
|| |                             || |  height - N lines to keep   ||
|| `-- height - n                || |                             ||
|| ,-- (height - n) + 1          || |                             ||
|| |                             || |                             ||
|| | N lines to discard          || |                             ||
|| |                             || |                             ||
|| `-- height                    || `-- height                    ||
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
	       (cond
		 ((plusp n)
		  (let ((new-blanks (subseq array 0 n))) ; save overwritten lines
		    ;; Copy the retained lines up
		    (setf (subseq array 0 (- height n))
			  ;; (subseq lines n (1- height)))
			  (subseq array n height))
		    ;; Move the new blank lines in place.
		    (setf (subseq array (- height n)) new-blanks)
		    ;; Blank out the newly blank lines
		    (funcall blanker new-blanks)))
		 (t ;; minusp
		  (let ((offset (abs n))
			(new-blanks (subseq array (+ height n))))
		    ;; Copy the retained lines down
		    (setf (subseq array (1+ offset))
			  (subseq array 0 (+ height n)))
		    ;; Move the new blank lines in place.
		    (setf (subseq array 0 offset) new-blanks)
		    ;; Blank out the newly blank lines
		    (funcall blanker new-blanks))))))
      (if (< (abs n) height)
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
	     (set-char (aref (aref lines y) x) char)
	     (when changed
	       (note-single-line tty))
	     (let* ((len (display-length char))
		    (new-x (+ x len)))
	       (if (< new-x width)
		   (progn
		     (when (> len 1)
		       ;; "Underchar removal"
		       (unset-grid-char (aref (aref lines y) (1+ x))))
		     (setf x new-x))
		   (next-line)))
	     )))
	changed))))

(defun copy-to-screen (tty string &key start end)
  "Copy the STRING from START to END to the screen. Return true if we actually
changed the screen contents."
  (with-slots (x y width height fg bg attrs scrolling-region) (new-screen tty)
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

(defmethod terminal-write-string ((tty terminal-crunch-stream) str
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
    (setf (start-line tty)
	  (max 0 (min (start-line tty) (screen-y (new-screen tty)))))))

(defmethod terminal-down ((tty terminal-crunch-stream) &optional (n 1))
  (setf (screen-y (new-screen tty))
	(min (1- (screen-height (new-screen tty)))
	     (+ (screen-y (new-screen tty)) n))))

(defmethod terminal-scroll-down ((tty terminal-crunch-stream) n)
  ;; Even if allow-scrolling is false.
  (when (> n 0)
    (terminal-down tty n)
    (with-slots (y height) (new-screen tty)
      (when (> n (- height (1+ y)))
	(scroll tty (- n (- height (1+ y))))))))

(defmethod terminal-scroll-up ((tty terminal-crunch-stream) n)
  ;; Even if allow-scrolling is false.
  (when (> n 0)
    (terminal-up tty n)
    (with-slots (y height) (new-screen tty)
      (when (> n y)
	(scroll tty (- (- n y)))))))

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
  (terminal-title (terminal-wrapped-terminal tty)))

(defmethod (setf terminal-title) (title (tty terminal-crunch))
  "Set the title of a terminal window. The terminal is assumed to work like
XTerm or something."
  (setf (terminal-title (terminal-wrapped-terminal tty)) title))

(defmethod terminal-has-attribute ((tty terminal-crunch) attribute)
  "Return true if the terminal can display the character attribute."
  (terminal-has-attribute (terminal-wrapped-terminal tty) attribute))

(defmethod terminal-enable-event ((tty terminal-crunch) event)
  "Allow event and return true if the terminal can allow event."
  ;; Just call the wrapped one.
  (terminal-enable-event (terminal-wrapped-terminal tty) event))

(defmethod terminal-disable-event ((tty terminal-crunch) event)
  "Allow event and return true if the terminal can allow event."
  ;; Just call the wrapped one.
  (terminal-disable-event (terminal-wrapped-terminal tty) event))

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
	 fs)

    (dbugf :crunch "update-line ~s~%" line)
    
    ;; Go through chars and calculate approximate cost to output differences.
    (flet ((note-change-end (i)
	     (when change-range
	       (setf (cdr change-range) i)
	       (push change-range changes)
	       (setf change-range nil))))
      (loop :with disp-len
	 :for i :from 0 :below (length new-line) :do
	 (if (not (grid-char= (aref new-line i) (aref old-line i)))
	     (progn
	       (when (not first-change)
		 (setf first-change i))
	       ;; Note change start
	       (when (not change-range)
		 (setf change-range (cons i nil)))

	       ;; Cost of writing this char (@@@ as if it was new)
	       (incf change-cost
		     (output-cost wtty :write-fatchar
				  ;; (aref new-line i)
				  ;; @@@ faked, should convert ^^
				  (make-fatchar)
				  ))

	       ;; If we have to move since the last change, add that.
	       (if (> (- i last-change) 1)
		   (incf change-cost
			 (move-cost tty last-change line i line)))

	       (setf last-change i))
	     (progn ;; chars are equal
	       ;; Note change end
	       (note-change-end i)))
	 (when (> (setf disp-len (display-length (aref new-line i))) 1)
	   (incf i (1- disp-len)))
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
      (cerror "Forget about it."
	      "We thought we had to update line ~s, but we didn't?" line)
      (return-from update-line))

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
	     (dbugf :crunch "update-line FLOOB ~s ~s ~s~%" line start c)
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
	     (setf fs (grid-to-fat-string new-line
					  :start start
					  :end (min (1+ end) (length new-line))))
	     (terminal-write-string wtty fs)
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
	  (dbugf :crunch "update-line WINKY ~s ~s-~s~%" line first-change
		 (1+ last-change))
	  (setf fs (grid-to-fat-string new-line
				       :start first-change
				       :end (min (1+ last-change)
						 (length new-line))))
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

(defun update-display (tty)
  "This is the big crunch."
  (with-slots ((wtty wrapped-terminal)
	       (old old-screen)
	       (new new-screen)
	       start-line really-scroll-amount) tty

    (if-dbugf (:crunch) (dump-screen tty))
    
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
    ;; (dbugf :crunch-update "****** start update @ ~s~%" start-line)
    ;; (if-dbugf (:crunch-update)
    ;; 	      (deblarg::debugger-backtrace 10))

    ;; First, actually scroll unmanaged content if we have to.
    (when (and (not (zerop start-line))
	       (not (zerop really-scroll-amount)))
      ;;(dbugf :crunch "-------- Really scroll ~s <-----~%" really-scroll-amount)
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
		  ;;(setf c (aref (aref (screen-lines new) cy) (max 0 char-x)))
		  (setf c (aref (aref (screen-lines new) cy) (max 0 char-x)))
		  (if (characterp (grid-char-c c))
		      (let ((fc (make-fatchar)))
			(set-fat-char fc c)
			(terminal-write-char wtty fc))
		      (terminal-write-string wtty (grid-to-fat-string c)))
		  (incf (update-x tty) (display-length c))))
	   (compute-hashes new cy (1+ cy))
	   (if-dbugf (:crunch) (dump-hashes tty))
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
	 ;;(dbugf :crunch "char = '~s'~%" c)
	 ))
      ((single-line-change tty)
       ;; diff the line
       ;; move, overwite, insert / delete as appropriate
       ;; @@@ it could be something else? like insert or delete?
       (let ((line (single-line-change tty)))
	 (dbugf :crunch "single-line-change ~s~%" line)
	 (compute-hashes new line (1+ line))
	 (if-dbugf (:crunch) (dump-hashes tty))
	 (when (/= (aref (screen-hashes new) line)
		   (aref (screen-hashes old) line))
	   (update-line tty line)))
       (update-ending-position tty new))
      ;; No hints.
      (t
       ;; Make the line hashes.
       (compute-hashes new start-line)
       (if-dbugf (:crunch) (dump-hashes tty))

       ;; @@@
       ;; handle scrolling
       ;;   detect scrolling
       ;;   move same lines

       ;; Update changed lines.
       ;;(time
       (loop :for i :from start-line :below (length (screen-hashes new)) :do
	  (when (/= (aref (screen-hashes new) i) (aref (screen-hashes old) i))
	    ;; (dbugf :crunch "hash diff ~s old ~s new ~s~%" i
	    ;; 	   (aref (screen-hashes new) i)
	    ;; 	   (aref (screen-hashes old) i))
	    (update-line tty i)))
       ;;)

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
  ;; @@@ wrapped terminal reset? or what?
  (with-slots (fg bg attrs (wtty wrapped-terminal)) tty
    (setf fg nil bg nil attrs nil)
    (terminal-normal wtty)
    (terminal-cursor-on tty))
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
    (format *debug-io* "Old -> New~%")
    (loop :for i :from 0 :below (length old-hashes) :do
       (format *debug-io* "~s ~s~%" (aref old-hashes i) (aref new-hashes i)))))

(defun dump-screen (tty)
  (let ((old-lines (screen-lines (old-screen tty)))
	(new-lines (screen-lines (new-screen tty))))
    (format *debug-io* "Old ~s ~s -> New ~s ~s +~s~%"
	    (screen-x (old-screen tty))
	    (screen-y (old-screen tty))
	    (screen-x (new-screen tty))
	    (screen-y (new-screen tty))
	    (start-line tty))
    (flet (
	   ;; (line-str (line) (grid-to-fat-string line))
	   (line-str (line)
	     (let (str)
	       (with-terminal-output-to-string (:ansi-stream)
		 (setf str (grid-to-fat-string line))
		 (when str
		   (tt-write-string str)))))
	   )
      (loop :for i :from 0 :below (length old-lines) :do
	 (format *debug-io* "[~a] [~a]~%"
		 (line-str (aref old-lines i))
		 (line-str (aref new-lines i)))))))
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
