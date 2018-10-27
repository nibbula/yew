;;
;; ochar.lisp - Objectable characters
;;

(defpackage :ochar
  (:documentation "Objectable characters.
Wherein we take more of the simple performant Common Lisp types and make them
bloated with CLOS encrustations, so we can eventually float our giant
bloatmobile to the heavens.")
  (:use :cl)
  (:export
   #:ocharacter
   #:obase-char
   #:oextended-char
   #:ochar=
   #:ochar/=
   #:ochar<
   #:ochar>
   #:ochar<=
   #:ochar>=
   #:ochar-equal
   #:ochar-not-equal
   #:ochar-lessp
   #:ochar-greaterp
   #:ochar-not-greaterp
   #:ochar-not-lessp
   #:ocharacterp
   #:oalpha-char-p
   #:oalphanumericp
   #:odigit-char
   #:odigit-char-p
   #:ographic-char-p
   #:ostandard-char-p
   #:ochar-upcase
   #:ochar-downcase
   #:oupper-case-p
   #:olower-case-p
   #:oboth-case-p
   #:ochar-code
   #:ochar-int
   #:ocode-char
   #:ochar-code-limit
   #:ochar-name
   #:oname-char
   ))
(in-package :ochar)

(defclass ocharacter ()
  ()
  (:documentation "Objectable character."))

(defclass obase-char (ocharacter)
  ()
  (:documentation "Objectable base character."))

(defclass oextended-char (ocharacter)
  ()
  (:documentation "Objectable extended character."))

;; @@@ Should we use this like simplify-char, or would the equivalent be just
;; <character-type-name> for any character type? Or maybe we should have both?
(defgeneric ocharacter (char)
  (:documentation "Returns the character denoted by the character designator.")
  (:method ((char character)) (character char)))

;; We have taken the mighty aribitrary arity functions and turned them into
;; two arg bimbos. Why? Don't ask questions! It's already very stupid in here.
;; We're lucky we even have this. And thankfully this whole crap will evaporate
;; when we have the successor!

(defgeneric ochar= (char-1 char-2)
  (:documentation "Return true if all characters are the same.")
  (:method ((char-1 character) (char-2 character)) (char= char-1 char-2)))

(defgeneric ochar/= (char-1 char-2)
  (:documentation "Return true if all characters are different.")
  (:method ((char-1 character) (char-2 character)) (char/= char-1 char-2)))

(defgeneric ochar< (char-1 char-2)
  (:documentation "Return true if the characters are monotonically increasing.")
  (:method ((char-1 character) (char-2 character)) (char< char-1 char-2)))

(defgeneric ochar> (char-1 char-2)
  (:documentation "Return true if the characters are monotonically decreasing.")
  (:method ((char-1 character) (char-2 character)) (char> char-1 char-2)))

(defgeneric ochar<= (char-1 char-2)
  (:documentation
   "Return true if the characters are monotonically nondecreasing. So boring.")
  (:method ((char-1 character) (char-2 character)) (char<= char-1 char-2)))

(defgeneric ochar>= (char-1 char-2)
  (:documentation
  "Return true if the characters are monotonically nonincreasing, or something.")
  (:method ((char-1 character) (char-2 character)) (char>= char-1 char-2)))

(defgeneric ochar-equal (char-1 char-2)
  (:documentation "Like char= but ignoring case.")
  (:method ((char-1 character) (char-2 character)) (char-equal char-1 char-2)))

(defgeneric ochar-not-equal (char-1 char-2)
  (:documentation "Like char/= but ignoring case.")
  (:method ((char-1 character) (char-2 character))
    (char-not-equal char-1 char-2)))

(defgeneric ochar-lessp (char-1 char-2)
  (:documentation "Like char< but ignoring case.")
  (:method ((char-1 character) (char-2 character))
    (char-lessp char-1 char-2)))

(defgeneric ochar-greaterp (char-1 char-2)
  (:documentation "Like char> but ignoring case.")
  (:method ((char-1 character) (char-2 character))
    (char-greaterp char-1 char-2)))

(defgeneric ochar-not-greaterp (char-1 char-2)
  (:documentation "Like char>= but ignoring case.")
  (:method ((char-1 character) (char-2 character))
    (char-not-greaterp char-1 char-2)))

(defgeneric ochar-not-lessp (char-1 char-2)
  (:documentation "Like char<= but ignoring case.")
  (:method ((char-1 character) (char-2 character))
    (char-not-lessp char-1 char-2)))

(defgeneric ocharacterp (object)
  (:documentation
   "Is this thing an objectable character? If so I shall giveth thee this
 mighty T.")
  (:method ((object t)) nil)
  (:method ((object character)) #|🎺|# T #|🎺|#))

(defgeneric oalpha-char-p (character)
  (:documentation "Return true if character is alphabetic.")
  (:method ((character character)) (alpha-char-p character)))

(defgeneric oalphanumericp (character)
  (:documentation "Return true if character is alphabetic or numeric.")
  (:method ((character character)) (alphanumericp character)))

(defgeneric odigit-char (weight type &optional radix) ;; Different
  (:documentation
   "Return a character which has that weight when considered as a digit in the
specified radix, or NIL if weight >= radix.")
  (:method (weight (type (eql 'character)) &optional radix)
    (if radix (digit-char weight radix) (digit-char weight))))

(defgeneric odigit-char-p (char &optional radix)
  (:documentation "Return the weight as integer if CHAR is a digit in RADIX.")
  (:method ((character character) &optional radix)
    (if radix (digit-char-p character radix) (digit-char-p character))))

(defgeneric ographic-char-p (character)
  (:documentation "Return true if character is a graphic character.")
  (:method ((character character)) (graphic-char-p character)))

(defgeneric ostandard-char-p (character)
  (:documentation "Return true if character is just a boring standard-char.")
  (:method ((character character)) (standard-char-p character)))

(defgeneric ochar-upcase (character)
  (:documentation "Return the corresponding uppercase character.")
  (:method ((character character)) (char-upcase character)))

(defgeneric ochar-downcase (character)
  (:documentation "Return the corresponding lowercase character.")
  (:method ((character character)) (char-downcase character)))

(defgeneric oupper-case-p (character)
  (:documentation "Return true if character is uppercase.")
  (:method ((character character)) (upper-case-p character)))

(defgeneric olower-case-p (character)
  (:documentation "Return true if character is lowercase.")
  (:method ((character character)) (lower-case-p character)))

(defgeneric oboth-case-p (character)
  (:documentation
   "Return true if character is a character at least with case, if not
with class.")
  (:method ((character character)) (both-case-p character)))

(defgeneric ochar-code (character)
  (:documentation "Return the code attribute of the character.")
  (:method ((character character)) (char-code character)))

(defgeneric ochar-int (character)
  (:documentation "Return a non-negative integer encoding of the character.")
  (:method ((character character)) (char-int character)))

(defgeneric ocode-char (code type) ;; Different
  (:documentation "Return a character with the given code attribute and type.")
  (:method (code (type (eql 'character))) (code-char code)))

(defgeneric ochar-name (character)
  (:documentation "Return a string that is the name of the character.")
  (:method ((character character)) (char-name character)))

(defgeneric oname-char (name type) ;; Different
  (:documentation "Return a character of the given name and type.")
  (:method (name (type (eql 'character))) (name-char name)))

;; This whole file could have probably been generated by a macro, if the
;; specification had been written in Lisp! Consider that next time you write
;; a language.

;; EOF
