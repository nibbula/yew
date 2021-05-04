;;;
;;; ostring.lisp - Objectable strings
;;;

(defpackage :ostring
  (:documentation
   "If you're going to object to strings, these are some strings to object to.")
  (:use :cl)
  (:export
   #:ostring
   #:ochar		; too much like ochar?
   #:oschar		; useless?
   #:ostring
   #:ostringp
   #:ostring-upcase
   #:ostring-downcase
   #:ostring-capitalize
   #:onstring-upcase
   #:onstring-downcase
   #:onstring-capitalize
   #:ostring-trim
   #:ostring-left-trim
   #:ostring-right-trim
   #:ostring-simplify
   #:ostring=
   #:ostring/=
   #:ostring<
   #:ostring>
   #:ostring<=
   #:ostring>=
   #:ostring-equal
   #:ostring-not-equal
   #:ostring-lessp
   #:ostring-greaterp
   #:ostring-not-greaterp
   #:ostring-not-lessp
   #:ostring-to-span
   #:oparse-integer
   ))
(in-package :ostring)

(defclass ostring ()
  ()
  (:documentation "An objectable string."))

;; I might be nice if we could do a deftype, but then we'd have to have a
;; different name for the class?
;(deftype ostring () '(or string ostring-class))

(defun ostringp (object)
  (typep object '(or string ostring)))

;; @@@ Should this be like simplify or should it be allowed to return complex
;; strings?
(defgeneric ostring (thing)
  (:documentation "Make an ostring from the thing.")
  (:method ((thing t)) (string thing)))

(defmethod ochar:osimplify ((thing string))
  thing)

(defgeneric ostring-simplify (thing)
  (:documentation
   "Return a simplified version of THING. This can lose information. For
example, for a character with attribues the attributes may be removed. This
most likely just calls ochar:osimplify.")
  (:method (thing) (ochar:osimplify thing)))

(defgeneric ochar (thing index)
  (:documentation "Return the INDEX-th character of THING.")
  (:method ((thing string) index) (char thing index)))

(defgeneric (setf ochar) (value thing index)
  (:documentation "Return the INDEX-th character of THING.")
  (:method ((value character) (thing string) index)
    (setf (char thing index) value)))

;; Is there some better way than this bullshit? Without messing up the lambda
;; list with a &rest and without using apply??
;; You can do this:
;;   (apply #'string-upcase `(,string ,@(when start-p `(:start ,start))
;; 	  			       ,@(when end-p `(:end end))))
;; but there's some overhead of consing and applying.

(defmacro call-with-start-and-end (func args)
  "Call func with args and START and END keywords, assume that an environemnt
that has START and START-P and END and END-P."
  `(progn
     (if start-p
	 (if end-p
	     (,func ,@args :start start :end end)
	     (,func ,@args :start start))
	 (if end-p
	     (,func ,@args ::end end)
	     (,func ,@args)))))

(defgeneric ostring-upcase (string &key start end)
  (:documentation "")
  (:method ((string string) &key (start nil start-p) (end nil end-p))
    (call-with-start-and-end string-upcase (string))))

(defgeneric ostring-downcase (string &key start end)
  (:documentation "")
  (:method ((string string) &key (start nil start-p) (end nil end-p))
    (call-with-start-and-end string-downcase (string))))

(defgeneric ostring-capitalize (string &key start end)
  (:documentation "")
  (:method ((string string) &key (start nil start-p) (end nil end-p))
    (call-with-start-and-end string-capitalize (string))))

(defgeneric onstring-upcase (string &key start end)
  (:documentation "")
  (:method ((string string) &key (start nil start-p) (end nil end-p))
    (call-with-start-and-end nstring-upcase (string))))

(defgeneric onstring-downcase (string &key start end)
  (:documentation "")
  (:method ((string string) &key (start nil start-p) (end nil end-p))
    (call-with-start-and-end nstring-downcase (string))))

(defgeneric onstring-capitalize (string &key start end)
  (:documentation "")
  (:method ((string string) &key (start nil start-p) (end nil end-p))
    (call-with-start-and-end nstring-capitalize (string))))

(defgeneric ostring-trim (character-bag string)
  (:documentation "")
  (:method (character-bag (string string))
    (string-trim character-bag string)))

(defgeneric ostring-left-trim (character-bag string)
  (:documentation "")
  (:method (character-bag (string string))
    (string-left-trim character-bag string)))

(defgeneric ostring-right-trim (character-bag string)
  (:documentation "")
  (:method (character-bag (string string))
    (string-right-trim character-bag string)))

(defgeneric ostring= (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string= string-1 string-2)))

(defgeneric ostring/= (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string/= string-1 string-2)))

(defgeneric ostring< (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string< string-1 string-2)))

(defgeneric ostring> (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string> string-1 string-2)))

(defgeneric ostring<= (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string<= string-1 string-2)))

(defgeneric ostring>= (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string>= string-1 string-2)))

(defgeneric ostring-equal (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string-equal string-1 string-2)))

(defgeneric ostring-not-equal (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string-not-equal string-1 string-2)))

(defgeneric ostring-lessp (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string-lessp string-1 string-2)))

(defgeneric ostring-greaterp (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string-greaterp string-1 string-2)))

(defgeneric ostring-not-greaterp (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string-not-greaterp string-1 string-2)))

(defgeneric ostring-not-lessp (string-1 string-2)
  (:documentation "")
  (:method ((string-1 string) (string-2 string))
    (string-not-lessp string-1 string-2)))

(defgeneric ostring-to-span (string)
  (:documentation "Convert STRING to a span.")
  (:method ((string string)) (list string)))

;; @@@ I'm not sure this is really the best place for this.
(defgeneric oparse-integer (string &key start end radix junk-allowed)
  (:documentation
   "Examine the substring of STRING delimited by START and END, which default
to the beginning and end of the string. Skip over whitespace characters and then
try to parse an integer. The radix parameter must be between 2 and 36, and
defaults to 10.

If JUNK-ALLOWED is false, an error of type parse-error is signaled if the
substring isn't entirely a signed integer, possibly surrounded on either side
by whitespace characters.

The first value returned is either the integer that was parsed, or else
NIL if no integer was seen and junk-allowed is true.

The second value is either the index of the character that terminated the parse,
or the end of the substring.")
   (:method ((string string) &key (start 0) end (radix 10) junk-allowed)
     (parse-integer string :start start :end end :radix radix
		    :junk-allowed junk-allowed)))

;; EOF
