;;;
;;; encoding.lisp - Interface to encodings.
;;;

(in-package :unicode)

;; @@@ It would really be good to have some super-fast platform specific bulk
;; vectorized and/or paralellized versions, like described in this:
;; <https://arxiv.org/pdf/2010.03090.pdf> Maybe even in-place, low copying,
;; versions?

;; @@@ supposedly sbcl has vectorized converters now?

(defparameter *encodings* nil
  "List of registered encodings.")

;; (defstruct encoding
;;   name
;;   description)
;;
;; (defun register-encoding (encoding)
;;  "Add the ‘encoding’ to the list of encodings. ‘encoding’ should be a encoding
;; struct."
;;   (check-type encoding 'encoding)
;;   (pushnew encoding *encodings*))

(defun register-encoding (encoding)
  "Add the ‘encoding’ to the list of encodings. ‘encoding’ should probably
be a keyword."
  (check-type encoding keyword)
  (pushnew encoding *encodings*))

(defun function-name (type charset-name &optional (package *package*))
  "Return the name of the function of ‘type’ for ‘charset’."
  (symbolify
   (case type
     (:encoding         (s+ "STRING-TO-" charset-name "-BYTES"))
     (:decoding         (s+ charset-name "-BYTES-TO-STRING"))
     (:foreign-encoding (s+ "STRING-TO-FOREIGN-" charset-name))
     (:foreign-decoding (s+ "FOREIGN-TO-STRING-" charset-name))
     (:putter           (s+ "%PUT-" charset-name "-CHAR"))
     (:getter           (s+ "%GET-" charset-name "-CHAR"))
     (:length           (s+ "%LENGTH-IN-" charset-name "-BYTES"))
     (otherwise
      (error "Bad type of encoding function name: ~s" type)))
   :package package))

(defmacro define-string-converters (charset-name)
  "Define two functions ‘string-to-<charset-name>-bytes’ and
‘<charset-name>-bytes-to-string.’. They use 3 functions or macros which must be
defined for the charset:
   (‘%put-<charset-name>-char’ get-func set-func)
   (‘%get-<charset-name>-char’ get-func set-func)
   (‘%length-in-<charset-name>-bytes’ code)

Where get-func and put-func are:
  (get-func)    Returns the unit or NIL if there are no more.
  (set-func c)  Sets the unit to ‘c’.

The unit is either a character or byte depending on which direction conversion
is happening. So:
                             │ get-func  │ put-func
   ──────────────────────────┼───────────┼───────────
    %put-<charset-name>-char │ character │ byte
    %get-<charset-name>-char │ byte      │ character

%length-in-<charset-name>-bytes returns how many bytes are needed to represent
‘code’.
"
  (let ((encode-func (function-name :encoding charset-name))
	(decode-func (function-name :decoding charset-name))
	(putter-name (function-name :putter   charset-name))
	(getter-name (function-name :getter   charset-name))
	(length-name (function-name :length   charset-name)))
    `(progn
       (defun ,encode-func (string)
	 "Return a vector of (unsigned-byte 8) representing the STRING."
	 (declare #| (optimize speed (safety 0)) |#
		  (type simple-string string))
	 (let* ((result-length
		 (loop :with sum fixnum = 0
		    :for i fixnum :from 0 :below (length string) :do
		    (incf sum (,length-name
			       (char-code (char string i))))
		    :finally (return sum)))
		(result (make-array result-length
				    :element-type '(unsigned-byte 8)
				    :initial-element 0 :adjustable nil))
		(i 0) (byte-num 0))
	   (declare (type fixnum i byte-num result-length))
	   (labels ((getter () (char string i))
		    (putter (c)
		      (setf (aref result byte-num) c)
		      (incf byte-num)))
	     (dotimes (x (length string))
	       (,putter-name getter putter)
	       (incf i)))
	   result))

       (defun ,decode-func (bytes)
	 "Convert the simple-array of (unsigned-byte 8) in BYTES to the string
of characters they represent."
	 (declare #| (optimize speed (safety 0)) |#
		  ;; (type (simple-array (unsigned-byte 8) *) bytes)
		  )
	 (let ((result-length 0) result (i 0) (byte-num 0)
	       (source-length (length bytes)))
	   (declare (type fixnum result-length i byte-num source-length))
	   (labels ((getter ()
		      (prog1 (if (< byte-num source-length)
				 (progn
				   (aref bytes byte-num))
				 nil)
			(incf byte-num)))
		    (putter (c)
		      (declare (type character c))
		      (setf (char result i) c) (incf i))
		    (count-putter (c)
		      (declare (ignore c))
		      (incf i)))
	     (loop :while (< byte-num source-length)
	       :do (,getter-name getter count-putter))
	     (setf result-length i
		   result (make-string result-length)
		   i 0
		   byte-num 0)
	     (loop :while (< byte-num source-length)
		:do (,getter-name getter putter))
	     result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compibility with babel?

;; @@@ This should probably be from the implementation or something.
(defvar *default-character-encoding* :utf-8
  "The default encoding for characters.")

(defvar *suppress-character-coding-errors* nil
  "If true, don't signal encoding errors. Instead use the default replacement
character for the encoding.")

(defun list-character-encodings ()
  "Does what it says."
  *encodings*)

(defun octets-to-string (vector &key
				  (encoding *default-character-encoding*)
				  (start 0) end
				  (errorp
				   (not *suppress-character-coding-errors*)))
  "Given a VECTOR of octets, return a string of characters according to the
ENCODING. The vector should have elements of type compatible with
(unsigned-byte 8) with the characters encoded in ENCODING. START and END
designate the range of octets converted. START defaults to 0, and END defaults
to NIL, which means the end of the vector. If ERRORP is true, signal errors,
otherwise the string returned may have substituted, missing, or incorrect
characters depending on the encoding."
  (declare (ignore vector encoding start end errorp))
  )

(defun string-to-octets (string &key
				  (encoding *default-character-encoding*)
				  (start 0) end
				  (use-bom :default)
				  (errorp
				   (not *suppress-character-coding-errors*)))
  "Given a string of characters, return a vector of (unsigned-byte 8) encoded
in the ENCODING. START and END designate the range of characters converted.
START defaults to 0, and END defaults to NIL, which means the end of the vector.
If USE-BOM is true and the encoding optionally uses ‘Byte Order Marks’, 
If ERRORP is true, signal errors,
otherwise the string returned may have substituted, missing, or incorrect
characters depending on the encoding."
  (declare (ignore string encoding start end use-bom errorp))
  )

(defun string-size-in-octets (string
			      &key
				(encoding *default-character-encoding*)
				(start 0) end
				(max -1 max-provided-p)
				(errorp
				 (not *suppress-character-coding-errors*)))
  (declare (ignore string encoding start end max max-provided-p errorp))
  )

(defun vector-size-in-chars (vector &key
				      (encoding *default-character-encoding*)
				      (start 0) end
				      (max -1 max-provided-p)
				      (errorp
				       (not *suppress-character-coding-errors*)))
  (declare (ignore vector encoding start end max max-provided-p errorp))
  )

;; End
