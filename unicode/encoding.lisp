;;;
;;; encoding.lisp - Interface to encodings.
;;;

(in-package :unicode)

(defmacro define-string-converters (charset-name)
  (let ((encode-func (symbolify (s+ "STRING-TO-" charset-name "-BYTES")))
	(decode-func (symbolify (s+ charset-name "-BYTES-TO-STRING")))
	(putter-name (symbolify (s+ "%PUT-" charset-name "-CHAR")))
	(getter-name (symbolify (s+ "%GET-" charset-name "-CHAR")))
	(length-name (symbolify (s+ "%LENGTH-IN-" charset-name "-BYTES"))))
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
	       (source-len (length bytes)))
	   (declare (type fixnum result-length i byte-num source-len))
	   (labels ((getter ()
		      (prog1 (aref bytes byte-num) (incf byte-num)))
		    (putter (c)
		      (declare (type character c))
		      (setf (char result i) c) (incf i))
		    (fake-putter (c)
		      (declare (ignore c)) (incf i)))
	     (loop :while (< byte-num source-len)
		:do (,getter-name getter fake-putter))
	     (setf result-length i
		   result (make-string result-length)
		   i 0
		   byte-num 0)
	     (loop :while (< byte-num source-len)
		:do (,getter-name getter putter))
	     result))))))

;; Compibility with babel?

;; @@@ Th
(defvar *default-character-encoding* :utf-8
  "The default encoding for characters.")

(defvar *suppress-character-coding-errors* nil
  "If true, don't signal encoding errors. Instead use the default replacement
character for the encoding.")

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
