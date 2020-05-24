;;;
;;; utf8.lisp - Things for UTF8 encoding.
;;;

(in-package :unicode)

;; Most implementations have something like this built in, but of course not
;; portably accessible. It seems stupid to have another (probably not as good)
;; copy of something that's already in every implementation, but it's either
;; that or dig out and wrap around whatever is the implementation.  Also this
;; is in other common systems, such as BABEL, but not in a single character
;; way, that it seems we need.

;; The regular kind, that throws a lot of errors, in case you want to make
;; sure your UTF-8 is valid.
(defmacro %get-utf8-char (byte-getter char-setter)
  (with-names (u1 u2 u3 u4)
    `(prog ((,u1 0) (,u2 0) (,u3 0) (,u4 0))
	(declare (type fixnum ,u1 ,u2 ,u3, u4))
      RESYNC
      ;; ONE
      (setf ,u1 (,byte-getter))
      (cond
	((< ,u1 #x80)
	 (,char-setter (code-char ,u1)) ; one valid octet
	 (return))
	((< ,u1 #xc0)
	 (cerror "Discard bytes and start again."
		 "Invalid UTF8 starter byte ‘~s’." ,u1)
	 (go resync)))
      ;; TWO
      (setf ,u2 (,byte-getter))
      (cond
	((not (< #x7f ,u2 #xc0))
	 (cerror "Discard bytes and start again."
		 "Invalid UTF8 continuation byte ‘~s’." ,u2)
	 (go resync))
	((< ,u1 #xc2)
	 (cerror "Discard bytes and start again."
		 "Overlong UTF8 sequence ~x." ,u2)
	 (go resync))
	((< ,u1 #xe0)			; 2 octets
	 (,char-setter
	  (code-char (logior (the fixnum (ash (logand #x1f ,u1) 6))
			     (the fixnum (logxor ,u2 #x80)))))
	 (return)))
      ;; THREE
      (setf ,u3 (,byte-getter))
      (cond
	((not (< #x7f ,u2 #xc0))
	 (cerror "Discard bytes and start again."
		 "Invalid UTF8 continuation byte ‘~s’." ,u3)
	 (go resync))
	((and (= ,u1 #xe0) (< ,u2 #xa0))
	 (cerror "Discard bytes and start again."
		 "Overlong UTF8 sequence ~x." ,u3)
	 (go resync))
	((< ,u1 #xf0)			; 3 octets
	 (,char-setter (code-char (logior
				   (the fixnum (ash (logand ,u1 #x0f) 12))
				   (logior
				    (the fixnum (ash (logand ,u2 #x3f) 6))
				    (logand ,u3 #x3f)))))
	 (return)))
      ;; FOUR
      (setf ,u4 (,byte-getter))
      (cond
	((not (< #x7f ,u2 #xc0))
	 (cerror "Discard bytes and start again."
		 "Invalid UTF8 continuation byte ‘~s’." ,u3)
	 (go resync))
	((and (= ,u1 #xf0) (< ,u2 #x90))
	 (cerror "Discard bytes and start again."
		 "Overlong UTF8 sequence.")
	 (go resync))
	((< ,u1 #xf8)
	 (if (or (> ,u1 #xf4) (and (= ,u1 #xf4) (> ,u2 #x8f)))
	     (progn
	       (cerror "Discard bytes and start again."
		       "Character out of range.")
	       (go resync))
	     (,char-setter (code-char (logior
				       (the fixnum (ash (logand ,u1 7) 18))
				       (the fixnum (ash (logxor ,u2 #x80) 12))
				       (the fixnum (ash (logxor ,u3 #x80) 6))
				       (logxor ,u4 #x80)))))
	 (return)))
      ;; FIVE or SIX even
      (cerror "Discard bytes and start again."
	      "Character out of range OR overlong UTF8 sequence.")
      (go resync))))

;; The good kind, that doesn't throw any errors, and allows preserving of
;; input, thanks to Markus Kuhn.
(defmacro %get-utf8b-char (byte-getter char-setter)
  (with-names (bonk u1 u2 u3 u4 u5)
    `(macrolet ((,bonk (&rest args)
		  `(,',char-setter (code-char (logior ,@args)))))
       (prog ((,u1 0) (,u2 0) (,u3 0) (,u4 0) (,u5 0))
	  (declare (type (unsigned-byte 8) ,u1 ,u2 ,u3 ,u4 ,u5))
	  ;; ONE
	  (setf ,u1 (,byte-getter))
	  (cond
	    ;; one valid octet
	    ((< ,u1 #x80) (,bonk ,u1) (return))
	    ;; Invalid UTF8 starter byte
	    ((< ,u1 #xc0) (,bonk #xdc00 ,u1) (return)))
	  ;; TWO
	  (setf ,u2 (,byte-getter))
	  (cond
	    ;; "Invalid UTF8 continuation byte
	    ((not (< #x7f ,u2 #xc0))
	     (,bonk #xdc00 ,u1)
	     (,bonk #xdc00 ,u2)
	     (return))
	    ;; "Overlong UTF8 sequence ~x."
	    ((< ,u1 #xc2)
	     (,bonk #xdc00 ,u1)
	     (,bonk #xdc00 ,u2)
	     (return))
	    ;; 2 octets
	    ((< ,u1 #xe0)
	     (,bonk (ash (logand #x1f ,u1) 6)
		   (logxor ,u2 #x80))
	     (return)))
	  ;; THREE
	  (setf ,u3 (,byte-getter))
	  (cond
	    ;; "Invalid UTF8 continuation byte ‘~s’."
	    ((not (< #x7f ,u2 #xc0))
	     (,bonk #xdc00 ,u1)
	     (,bonk #xdc00 ,u2)
	     (,bonk #xdc00 ,u3)
	     (return))
	    ;; "Overlong UTF8 sequence ~x."
	    ((and (= ,u1 #xe0) (< ,u2 #xa0))
	     (,bonk #xdc00 ,u1)
	     (,bonk #xdc00 ,u2)
	     (,bonk #xdc00 ,u3))
	    ;; 3 octets
	    ((< ,u1 #xf0)
	     (,bonk (ash (logand ,u1 #x0f) 12)
		   (ash (logand ,u2 #x3f) 6)
		   (logand ,u3 #x3f))
	     (return)))
	  ;; FOUR
	  (setf ,u4 (,byte-getter))
	  (cond
	    ;; "Invalid UTF8 continuation byte ‘~s’."
	    ((not (< #x7f ,u2 #xc0))
	     (,bonk #xdc00 ,u1)
	     (,bonk #xdc00 ,u2)
	     (,bonk #xdc00 ,u3)
	     (,bonk #xdc00 ,u4)
	     (return))
	    ((and (= ,u1 #xf0) (< ,u2 #x90))
	     ;; "Overlong UTF8 sequence."
	     (,bonk #xdc00 ,u1)
	     (,bonk #xdc00 ,u2)
	     (,bonk #xdc00 ,u3)
	     (,bonk #xdc00 ,u4)
	     (return))
	    ((< ,u1 #xf8)
	     (if (or (> ,u1 #xf4) (and (= ,u1 #xf4) (> ,u2 #x8f)))
		 (progn
		   ;; "Character out of range."
		   (,bonk #xdc00 ,u1)
		   (,bonk #xdc00 ,u2)
		   (,bonk #xdc00 ,u3)
		   (,bonk #xdc00 ,u4))
		 (,bonk (ash (logand ,u1 #x07) 18)
			(ash (logxor ,u2 #x80) 12)
			(ash (logxor ,u3 #x80) 6)
			(logxor ,u4 #x80)))
	     (return)))
	  ;; FIVE or SIX even
	  (setf ,u5 (,byte-getter))
	  ;; "Character out of range OR overlong UTF8 sequence."
	  (,bonk #xdc00 ,u1)
	  (,bonk #xdc00 ,u2)
	  (,bonk #xdc00 ,u3)
	  (,bonk #xdc00 ,u4)
	  (,bonk #xdc00 ,u5)))))

(defun get-utf8-char (byte-getter char-setter)
  "Convert bytes of (unsigned-byte 8) returned by BYTE-GETTER to a character
to be given to CHAR-SETTER."
  (flet ((our-byte-getter () (funcall byte-getter))
	 (our-char-setter (c) (funcall char-setter c)))
    (%get-utf8-char our-byte-getter our-char-setter)))

(defun get-utf8b-char (byte-getter char-setter)
  (flet ((our-byte-getter () (funcall byte-getter))
	 (our-char-setter (c) (funcall char-setter c)))
    (%get-utf8b-char our-byte-getter our-char-setter)))

(defun %length-in-utf8-bytes (code)
  (declare (optimize speed (safety 0))
           (type (integer 0 #.char-code-limit) code))
  (cond ((< code #x80) 1)
        ((< code #x800) 2)
        ((< code #x10000) 3)
        ((< code #x110000) 4)
        (t (error "character code too big for UTF-8 #x~x" code))))

(defun length-in-utf8-bytes (code)
  (typecase code
    (character
     (%length-in-utf8-bytes (char-code code)))
    ((integer 0 #.char-code-limit)
     (%length-in-utf8-bytes code))
    (t
     (error "code #x~x isn't a character or an integer in range." code))))

(defmacro %put-utf8-char (char-getter byte-setter)
  (with-names (code)
    `(prog ((,code (char-code (,char-getter))))
	(case (%length-in-utf8-bytes ,code)
	  (1 (,byte-setter ,code))
	  (2 (,byte-setter (logior #xc0 (ldb (byte 5 6) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6 0) ,code))))
	  (3 (when (<= #xd800 ,code #xdfff)
	       (error "Yalls' got an invalid unicode character?"))
	     (,byte-setter (logior #xe0 (ldb (byte 4 12) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6  6) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6  0) ,code))))
	  (4 (,byte-setter (logior #xf0 (ldb (byte 3 18) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6 12) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6  6) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6  0) ,code))))))))

#| @@@ Do the 'b' specific part!
(defmacro %put-utf8b-char (char-getter byte-setter)
  (with-names (code)
    `(prog ((,code (char-code (,char-getter))))
	(case (%length-in-utf8-bytes ,code)
	  (1 (,byte-setter ,code))
	  (2 (,byte-setter (logior #xc0 (ldb (byte 5 6) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6 0) ,code))))
	  (3 (when (<= #xd800 ,code #xdfff)
	       (error "Yalls' got an invalid unicode character?"))
	     (,byte-setter (logior #xe0 (ldb (byte 4 12) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6  6) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6  0) ,code))))
	  (4 (,byte-setter (logior #xf0 (ldb (byte 3 18) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6 12) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6  6) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6  0) ,code))))))))
|#

(defun put-utf8-char (char-getter byte-setter)
  "Convert a character returned by CHAR-GETTER to bytes to be given to
BYTE-SETTER, which takes an (unsigned-byte 8)."
  (flet ((our-char-getter () (funcall char-getter))
	 (our-byte-setter (c) (funcall byte-setter c)))
    (%put-utf8-char our-char-getter our-byte-setter)))

#|
(defun put-utf8b-char (char-getter byte-setter)
  (flet ((our-char-getter () (funcall char-getter))
	 (our-byte-setter (c) (funcall byte-setter c)))
    (%put-utf8b-char our-char-getter our-byte-setter)))
|#

(defun string-to-utf8-bytes (string)
  "Return a vector of (unsigned-byte 8) representing the STRING."
  (declare (optimize speed (safety 0))
	   (type simple-string string))
  (let* ((result-length
	  (loop :with sum fixnum = 0
	     :for i fixnum :from 0 :below (length string) :do
	     (incf sum (%length-in-utf8-bytes (char-code (char string i))))
	     :finally (return sum)))
	 (result (make-array result-length :element-type '(unsigned-byte 8)
			     :initial-element 0 :adjustable nil))
	 (i 0) (byte-num 0))
    (declare (type fixnum i byte-num result-length))
    (labels ((getter () (char string i))
	     (putter (c)
	       (setf (aref result byte-num) c)
	       (incf byte-num)))
      (dotimes (x (length string))
	(%put-utf8-char getter putter)
	(incf i)))
    result))

(defun utf8-bytes-to-string (bytes)
  "Convert the simple-array of (unsigned-byte 8) in BYTES to the string of
characters they represent."
  (declare (optimize speed (safety 0))
	   ;; (type (simple-array (unsigned-byte 8) *) bytes)
	   )
  (let ((result-length 0) result (i 0) (byte-num 0) (source-len (length bytes)))
    (declare (type fixnum result-length i byte-num source-len))
    (labels ((getter ()
	       (prog1 (aref bytes byte-num) (incf byte-num)))
	     (putter (c)
	       (declare (type character c))
	       (setf (char result i) c) (incf i))
	     (fake-putter (c)
	       (declare (ignore c)) (incf i)))
      (loop :while (< byte-num source-len)
	 :do (%get-utf8-char getter fake-putter))
      (setf result-length i
	    result (make-string result-length)
	    i 0
	    byte-num 0)
      (loop :while (< byte-num source-len)
	 :do (%get-utf8-char getter putter))
      result)))

;; End
