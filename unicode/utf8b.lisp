;;;
;;; utf8b.lisp - Unicode Transformation Format 8 bit binary.
;;;

(in-package :unicode)

;; This doesn't throw any errors, and allows preserving of input, provided you
;; don't have private use characters. Thanks to Markus Kuhn for the idea, but
;; blame me for using it. According to Wikipedia, python uses something like
;; this too.

(defmacro %get-utf8b-char (byte-getter char-setter)
  (with-names (bonk pull splort u1 u2 u3 u4 u5)
    `(macrolet ((,bonk (&rest args)
		  "Put the xor'd args as a char."
		  `(,',char-setter (code-char (logior ,@args))))
		(,splort ()
		  "Put any alredy read bytes as invalid chars and return."
		  `(progn
		     (when ,',u1
		       (,',bonk #xdc00 ,',u1))
		     (when ,',u2
		       (,',bonk #xdc00 ,',u2))
		     (when ,',u3
		       (,',bonk #xdc00 ,',u3))
		     (when ,',u4
		       (,',bonk #xdc00 ,',u4))
		     (when ,',u5
		       (,',bonk #xdc00 ,',u5))
		     (return nil)))
		(,pull (n)
		  `(when (null (setf ,n (,',byte-getter)))
		     (return nil)))
		)
       (prog ((,u1 nil) (,u2 nil) (,u3 nil) (,u4 nil) (,u5 nil))
	  (declare (type (or (unsigned-byte 8) null) ,u1 ,u2 ,u3 ,u4 ,u5))
	  ;; ONE
	  (when (null (setf ,u1 (,byte-getter)))
	    (,splort))
	  (cond
	    ;; one valid octet
	    ((< ,u1 #x80) (,bonk ,u1) (return))
	    ;; Invalid UTF8 starter byte
	    ((< ,u1 #xc0) (,bonk #xdc00 ,u1) (return)))
	  ;; TWO
	  (when (null (setf ,u2 (,byte-getter)))
	    (,splort))
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
	  (when (null (setf ,u3 (,byte-getter)))
	    (,splort))
	  (cond
	    ;; "Invalid UTF8 continuation byte ‘~s’."
	    ((not (< #x7f ,u3 #xc0))
	     (,bonk #xdc00 ,u1)
	     (,bonk #xdc00 ,u2)
	     (,bonk #xdc00 ,u3)
	     (return))
	    ;; "Overlong UTF8 sequence ~x."
	    ((and (= ,u1 #xe0) (< ,u2 #xa0))
	     (,bonk #xdc00 ,u1)
	     (,bonk #xdc00 ,u2)
	     (,bonk #xdc00 ,u3)
	     (return))
	    ;; 3 octets
	    ((< ,u1 #xf0)
	     (,bonk (ash (logand ,u1 #x0f) 12)
		    (ash (logand ,u2 #x3f) 6)
		    (logand ,u3 #x3f))
	     (return)))
	  ;; FOUR
	  (when (null (setf ,u4 (,byte-getter)))
	    (,splort))
	  (cond
	    ;; "Invalid UTF8 continuation byte ‘~s’."
	    ((not (< #x7f ,u4 #xc0))
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
	  (when (null (setf ,u5 (,byte-getter)))
	    (,splort))
	  ;; "Character out of range OR overlong UTF8 sequence."
	  (,bonk #xdc00 ,u1)
	  (,bonk #xdc00 ,u2)
	  (,bonk #xdc00 ,u3)
	  (,bonk #xdc00 ,u4)
	  (,bonk #xdc00 ,u5)))))

(defun get-utf8b-char (byte-getter char-setter)
  "Convert bytes of (unsigned-byte 8) returned by BYTE-GETTER to a character
to be given to CHAR-SETTER."
  (flet ((our-byte-getter () (funcall byte-getter))
	 (our-char-setter (c) (funcall char-setter c)))
    (%get-utf8b-char our-byte-getter our-char-setter)))

(defun %length-in-utf8b-bytes (code)
  "Return the length of character codepoint ‘code’ in bytes."
  (declare #| (optimize speed (safety 0)) |#
           (type (integer 0 #.char-code-limit) code))
  (cond ((< code #x80) 1)
        ((< code #x800) 2)
	((and (> code #xdc00) (<= code #xdcff)) 1) ;; Encoded invalid byte
        ((< code #x10000) 3)
        ((< code #x110000) 4)
        (t (error "character code too big for UTF-8 #x~x" code))))

(defun length-in-utf8b-bytes (code)
  "Return the length of ‘code’ in bytes. ‘code’ can be a character or an
integer codepoint."
  (typecase code
    (character
     (%length-in-utf8b-bytes (char-code code)))
    ((integer 0 #.char-code-limit)
     (%length-in-utf8b-bytes code))
    (t
     (error "code #x~x isn't a character or an integer in range." code))))

(defmacro %put-utf8b-char (char-getter byte-setter)
  (with-names (code)
    `(prog ((,code (char-code (,char-getter))))
	(case (%length-in-utf8b-bytes ,code)
	  (1
	   (if (and (> ,code #xdc00) (<= ,code #xdcff)) ; Encoded invalid byte
	       (,byte-setter (logand ,code #xff))	; Take the low byte
	       (,byte-setter ,code)))
	  (2 (,byte-setter (logior #xc0 (ldb (byte 5 6) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6 0) ,code))))
	  (3
	   ;; (when (<= #xd800 ,code #xdfff)
	   ;;     (error "Yalls' got an invalid unicode character?"))
	     (,byte-setter (logior #xe0 (ldb (byte 4 12) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6  6) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6  0) ,code))))
	  (4 (,byte-setter (logior #xf0 (ldb (byte 3 18) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6 12) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6  6) ,code)))
	     (,byte-setter (logior #x80 (ldb (byte 6  0) ,code))))))))

(defun put-utf8b-char (char-getter byte-setter)
  (flet ((our-char-getter () (funcall char-getter))
	 (our-byte-setter (c) (funcall byte-setter c)))
    (%put-utf8b-char our-char-getter our-byte-setter)))

(define-string-converters "UTF8B")

;; End
