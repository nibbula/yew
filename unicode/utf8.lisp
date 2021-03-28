;;;
;;; utf8.lisp - Things for UTF-8 encoding.
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

(defun get-utf8-char (byte-getter char-setter)
  "Convert bytes of (unsigned-byte 8) returned by BYTE-GETTER to a character
to be given to CHAR-SETTER."
  (flet ((our-byte-getter () (funcall byte-getter))
	 (our-char-setter (c) (funcall char-setter c)))
    (%get-utf8-char our-byte-getter our-char-setter)))

(defun %length-in-utf8-bytes (code)
  (declare #| (optimize speed (safety 0)) |#
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

(defun put-utf8-char (char-getter byte-setter)
  "Convert a character returned by CHAR-GETTER to bytes to be given to
BYTE-SETTER, which takes an (unsigned-byte 8)."
  (flet ((our-char-getter () (funcall char-getter))
	 (our-byte-setter (c) (funcall byte-setter c)))
    (%put-utf8-char our-char-getter our-byte-setter)))

(define-string-converters "UTF8")

;; End
