;;;
;;; extract.lisp - Extract and transform character width data for Lisp.
;;;

(defpackage :extract
  (:documentation "Extract character width data files.")
  (:use :cl :dlib :opsys :dlib-misc :table :puri :drakma)
  (:export
   #:load-data
   #:extract
   ))
(in-package :extract)

;; Based on:
;; https://github.com/ridiculousfish/widecharwidth/generate.py

(defparameter *version* 1.23)

(defparameter *default-output-file* "char-width-data.lisp")

(defvar *log* t
  "Output stream for verbose messages.")

(defparameter *files* nil
  "List of data files.")

(defparameter *categories*
  '(:unassigned  "Cn"	;; unassigned codepoints.
    :private-use "Co"	;; private use codepoints.
    :surrogate   "Cs")) ;; surrogates.

(deftype category () '(member :unassigned :private-use :surrogate))

(defstruct mini-char
  (code 0 :type fixnum)
  (width 0 :type fixnum)
  (category :unassigned :type category))

;; Field types in the file:
;;
;; Name               Abbrev    Example                          Keyword
;; ------------------ --------- -------------------------------- ----------
;; Catalog 		C 	Age, Block                       :catalog
;; Enumeration 		E 	Joining_Type, Line_Break         :enum
;; Binary 	        B 	Uppercase, White_Space           :binary
;; String 	        S 	Uppercase_Mapping, Case_Folding  :string
;; Numeric 		N 	Numeric_Value                    :numeric
;; Miscellaneous 	M 	Name, Jamo_Short_Name            :misc
;;
;; Status field values:
;;   :normative :informative :contributory :provisional

(deftype decomposition ()
  `(member :compat :super :sub :fraction :nobreak :font :initial :final
	   :isolated :medial :narrow :small :circle :square :vertical :wide))

;; If I really wanted to waste time I could have scraped this class from the
;; damn html document. For that matter, I wish this code would just read the
;; documents, then generate code to read the data files, then load the data
;; then do the right thing with it. I know that if we were using C or Java,
;; they basically did it for us already in the form of ICU
;; http://site.icu-project.org/, but it requires C++11 and a bunch of other
;; odious crap. Just look at the git history of ICU to see what a moving
;; target we're up against.

(defclass unichar ()
  ((code ;; 0
    :initarg :code :accessor unichar-code :initform 0 :type fixnum
    :documentation
    "The code point of the character, or the start or end of the range.")
   (name ; :unitype :misc :status :normative
    :initarg :name :accessor unichar-name :initform "" :type string
    :documentation ;; 1
    "When a string value not enclosed in <angle brackets> occurs in this
    field, it specifies the character's Name property value, which matches
    exactly the name published in the code charts. The Name property value for
    most ideographic characters and for Hangul syllables is derived instead by
    various rules. See Section 4.8, Name in [Unicode] for a full specification
    of those rules. Strings enclosed in <angle brackets> in this field either
    provide label information used in the name derivation rules, or—in the
    case of characters which have a null string as their Name property value,
    such as control characters—provide other information about their code
    point type.")
   (general-category ; :unitype :enum :status :normative
    :initarg :general-category :accessor unichar-general-category
    :initform nil :type symbol
    :documentation ;; 2
    "This is a useful breakdown into various character types which can be
     used as a default categorization in implementations. For the property
     values, see General Category Values.")
   (canonical-combining-class
    :initarg :canonical-combining-class
    :accessor unichar-canonical-combining-class :initform 0 :type integer
    :documentation ;; 3
    "The classes used for the Canonical Ordering Algorithm in the Unicode
     Standard. This property could be considered either an enumerated property
     or a numeric property: the principal use of the property is in terms of
     the numeric values. For the property value names associated with
     different numeric values, see DerivedCombiningClass.txt and Canonical
     Combining Class Values.")
   (bidi-class ;; :unitype :enum :status :normative
    :initarg :bidi-class :accessor unichar-bidi-class :initform nil :type symbol
    :documentation ;; 4
    "These are the categories required by the Unicode Bidirectional Algorithm.
     For the property values, see Bidirectional Class Values. For more
     information, see Unicode Standard Annex #9, Unicode Bidirectional
     Algorithm [UAX9].

     The default property values depend on the code point, and are explained
     in DerivedBidiClass.txt")
   (decomposition-type ;; :unitype :enum :status :normative
    :initarg :decomposition-type :accessor unichar-decomposition-type
    :initform nil
    :type (or decomposition null)
    :documentation  ;; 5
    "The formmatting tag for non-canonical or compatibility decomposition
     mappings.")
   (decomposition-mapping ;; :unitype :string :status :normative
    :initarg :decomposition-mapping :accessor unichar-decomposition-mapping
    :initform nil :type (or null number (vector number))
    :documentation ;; Also 5
    "This field contains both values, with the type in angle brackets. The
    decomposition mappings exactly match the decomposition mappings published
    with the character names in the Unicode Standard. For more information,
    see Character Decomposition Mappings.")
   (numeric-type ;; :unitype :enum :status :normative
    :initarg :numeric-type :accessor unichar-numeric-type
    ;; :type numeric-type
    :documentation ;; 6
    "If the character has the property value Numeric_Type=Decimal, then the
    Numeric_Value of that digit is represented with an integer value (limited
    to the range 0..9) in fields 6, 7, and 8. Characters with the property
    value Numeric_Type=Decimal are restricted to digits which can be used in a
    decimal radix positional numeral system and which are encoded in the
    standard in a contiguous ascending range 0..9. See the discussion of
    decimal digits in Chapter 4, Character Properties in [Unicode].")
   (numeric-value-1 ;; :unitype (:enum :numeric) :status :normative
    :initarg :numeric-value-1 :accessor unichar-numeric-value-1
    :type (or number null) ;; 7
    :documentation
    "If the character has the property value Numeric_Type=Digit, then the
     Numeric_Value of that digit is represented with an integer value (limited
     to the range 0..9) in fields 7 and 8, and field 6 is null. This covers
     digits that need special handling, such as the compatibility superscript
     digits.

     Starting with Unicode 6.3.0, no newly encoded numeric characters will be
     given Numeric_Type=Digit, nor will existing characters with
     Numeric_Type=Numeric be changed to Numeric_Type=Digit. The distinction
     between those two types is not considered useful.")
   (numeric-value-2 ;; :unitype (:enum :numeric) :status :normative
    :initarg :numeric-value-2 :accessor unichar-numeric-value-2
    :type (or number null) ;; 8
    :documentation
    "If the character has the property value Numeric_Type=Numeric, then the
     Numeric_Value of that character is represented with a positive or
     negative integer or rational number in this field, and fields 6 and 7 are
     null. This includes fractions such as, for example, \"1/5\" for U+2155
     VULGAR FRACTION ONE FIFTH.

     Some characters have these properties based on values from the Unihan
     data files. See Numeric_Type, Han.")
   (bidi-mirrored ;; :unitype :binary :status :normative
    :initarg :bidi-mirrored :accessor unichar-bidi-mirrored
    :initform nil :type boolean
    :documentation ;; 9
    "If the character is a \"mirrored\" character in bidirectional text, this
     field has the value \"Y\"; otherwise \"N\". See Section 4.7, Bidi
     Mirrored of [Unicode]. Do not confuse this with the Bidi_Mirroring_Glyph
     property.")
   (unicode-1-name ;; :unitype :misc :sttus :informative
    :initarg :unicode-1-name :accessor unichar-unicode-1-name
    :initform "" :type string
    :documentation ;; 10 (Obsolete as of 6.2.0)
    "Old name as published in Unicode 1.0 or ISO 6429 names for control
     functions. This field is empty unless it is significantly different from
     the current name for the character. No longer used in code chart
     production. See Name_Alias.")
   (iso-comment  ;; :unitype :misc :status :informative
    :initarg :iso-comment :accessor unichar-iso-comment :initform "" :type string
    :documentation ;; 11 
    "(Obsolete as of 5.2.0; Deprecated and Stabilized as of 6.0.0) ISO 10646
     comment field. It was used for notes that appeared in parentheses in the
     10646 names list, or contained an asterisk to mark an Annex P note.

     As of Unicode 5.2.0, this field no longer contains any non-null values.")
   (simple-uppercase-mapping ;; :unitype :string :status :normative
    :initarg :simple-uppercase-mapping
    :accessor unichar-simple-uppercase-mapping
    :initform nil :type (or number null)
    :documentation ;; 12
    "Simple uppercase mapping (single character result). If a character is
     part of an alphabet with case distinctions, and has a simple uppercase
     equivalent, then the uppercase equivalent is in this field. The simple
     mappings have a single character result, where the full mappings may have
     multi-character results. For more information, see Case and Case Mapping.")
   (simple-lowercase-mapping ;; :unitype :string :status :normative
    :initarg :simple-lowercase-mapping
    :accessor unichar-simple-lowercase-mapping
    :initform nil :type (or number null)
    :documentation ;; 13
    "Simple lowercase mapping (single character result).")
   (simple-titlecase-mapping ;; :unitype :string :status :normative
    :initarg :simple-titlecase-mapping
    :accessor unichar-simple-titlecase-mapping
    :initform nil :type (or number null)
    :documentation ;; 14
    "Simple titlecase mapping (single character result).

    Note: If this field is null, then the Simple_Titlecase_Mapping is the same
    as the Simple_Uppercase_Mapping for this character."))
  (:documentation
   "Fields from UnicodeData.txt described in: 
    https://www.unicode.org/reports/tr44/

    There should be one slot for each field so this can be populated from the
    file."))

;; This is really only class instead of a struct for the initialize-instance.
(defclass data-file ()
  ((name
    :initarg :name :accessor data-file-name
    :documentation "Pathname of the file.")
   (url
    :initarg :url :accessor data-file-url
    :documentation "URL to download from.")
   (data
    :initarg :data :accessor data-file-data
    :documentation "Table of data read.")
   (hash
    :initarg :hash :accessor data-file-hash  
    :documentation "Digest hash of data.")
   (columns
    :initarg :columns :accessor data-file-columns
    :documentation "Initializer for the table columns.")
   (process-line
    :initarg :process-line :accessor data-file-process-line
    :documentation
    "Function to process a line. Takes a string and returns a sequence of
fields, or NIL to skip the line."))
  (:documentation "A Unicode data file."))

(defmethod initialize-instance
    :after ((o data-file) &rest initargs &key &allow-other-keys)
  "Initialize a data-file."
  (declare (ignore initargs))
  (when (or (not (slot-boundp o 'url)) (not (slot-value o 'url)))
    (error "You had better give it a URL."))
  (setf (slot-value o 'name)
	(car (last (puri:uri-parsed-path (puri:uri (slot-value o 'url))))))
  (push o *files*))

(defparameter *comment-char* #\# "Character that starts a comment.")
(defparameter *field-separator* #\; "Character that separates fields on a line.")

(defun ignoreable-line-p (line)
  "Return true if this is a line we should igonre, which are comment lines and
blank lines."
  (or (zerop (length line))
      (char= (char (ltrim line) 0) *comment-char*)))

(defgeneric read-file (file &key force)
  (:documentation "Read the data and set the hash value."))

(defmethod read-file ((file data-file) &key force)
  "Read the data and set the hash value."
  (with-slots (name url data hash columns process-line) file
    (let (content)
      (if (or (not (file-exists name)) force)
	  (progn
	    (format *log* "Downloading ~a.~%" name)
	    (setf content (drakma:http-request url))
	    (with-open-file (stream name :direction :output
				    :if-does-not-exist :create
				    :if-exists :supersede)
	      (write-string content stream)))
	  (progn
	    (setf content (slurp name))
	    (format *log* "~s already exists. Not downloading.~%" name)))
      (format *log* "Content length ~s~%" (length content))
      (setf hash (ironclad:digest-file :sha256 name)
	    data (make-table-from
		  (loop :with processed-line
		     :for line :in (split-sequence #\newline content
						   :omit-empty t)
		     :when (setf processed-line (funcall process-line line))
		     :collect processed-line)
		  :columns columns)))))

(defun read-property-range-line (line &key (symbolify t))
  "Read a line of the form:
FFFF(..FFFF) ; property
With optional comments starting with *comment-char*."
  (when (not (ignoreable-line-p line))
    (let ((pos (position *comment-char* line))
	  codepoints property start end)
      ;; Possibly clip a comment off the end of the line.
      (when pos
	(setf line (subseq line 0 pos)))
      (let ((f (split-sequence *field-separator* line)))
	(setf codepoints (trim (first f))
	      property (trim (second f)))
	(when symbolify
	  (setf property (symbolify property)))
	(if (setf pos (position #\. codepoints))
	    (setf start (parse-integer codepoints :radix 16 :end pos)
		  end (parse-integer codepoints
				     :radix 16 :start (+ pos 2)))
	    (setf start (parse-integer codepoints :radix 16)
		  end nil))
	(vector start end property)))))

(defun read-property-range-string-line (line)
  (read-property-range-line line :symbolify nil))

(defun get-decomposition-type (string)
  "Return the decomposition type as a keyword from the STRING, or nil if there
is none."
  (if (or (zerop (length string))
	  (not (char= (char string 0) #\<)))
      nil
      (let ((end-pos (position #\> string)))
	(when (not end-pos)
	  (error "Missing end bracket in decompoition type ~s" string))
	(keywordify (subseq string 1 end-pos)))))

(defun get-decomposition-mapping (string)
  (if (zerop (length string))
      nil ;; but it's really the code-point itself
      (let* ((end-pos (position #\> string))
	     (dc-list
	      (mapcar
	       (_ (parse-integer _ :radix 16))
	       (split-sequence
		#\space
		(subseq string
			(or (and end-pos (+ end-pos 2)) 0))))))
	(if (= (length dc-list) 1)
	    (car dc-list)
	    (coerce dc-list 'simple-vector)))))

(defun convert-integer (string &key (radix 10))
  "Convert a string to integer or NIL if it's zero length."
  (when (not (zerop (length string)))
    (parse-integer string :radix radix)))

(defun convert-hex (string)
  (convert-integer string :radix 16))

(defun convert-number (string)
  "Convert a string to number or NIL if it's zero length."
  (when (not (zerop (length string)))
    (safe-read-from-string string)))

(defparameter *url-prefix*
  "http://www.unicode.org/Public/UCD/latest/ucd/")

(defparameter *unicode-file-field-count* 15
  "Number of fields in the file, which we have to set manually because it's
different than the number of slots in the class.")

(defparameter *unicode-file* 
  (make-instance
   'data-file
   :url (s+ *url-prefix* "UnicodeData.txt")
   :columns '((:name "code" :type number)
	      (:name "name" :type string)
	      (:name "general-category" :type symbol)
	      (:name "canonical-combining-class" :type integer)
	      (:name "bidi-class" :type symbol)
	      (:name "decomposition-type" :type decomposition)
	      (:name "decomposition-mapping" :type string)
	      (:name "numeric-type" :type number)
	      (:name "numeric-value-1" :type number)
	      (:name "numeric-value-2" :type number)
	      (:name "bidi-mirrored" :type boolean)
	      (:name "unicode-1-name"  :type string)
	      (:name "iso-comment" :type string)
	      (:name "simple-uppercase-mapping" :type number)
	      (:name "simple-lowercase-mapping" :type number)
	      (:name "simple-titlecase-mapping" :type number))
   :process-line  #| cut -d ";" -t -f 1- |#
   (lambda (line)
     (let (;; (unichar-class (find-class 'unichar))
	   (fields (coerce (split-sequence *field-separator* line)
			   'simple-vector))
	   uc code-point)
       ;; We have to manually call finalize because we're asking for the slots
       ;; possibly before any instance has been created.
       ;; (mop:finalize-inheritance unichar-class)
       ;; (when (not (= (length (mop:class-slots unichar-class))
       ;; 		     (length fields)))
       ;; 	 ;; (error "Missing fields for ~a" (svref fields 0)))
       ;; 	 (error "Missing fields for ~a" fields))
       (when (not (= (length fields) *unicode-file-field-count*))
	 (error "Missing fields for ~a" fields))
       (setf uc
	     (make-instance
	      'unichar
	      :code
	      (setf code-point (parse-integer (svref fields 0) :radix 16))
	      :name                       (svref fields 1)
	      :general-category           (symbolify (svref fields 2))
	      :canonical-combining-class  (convert-integer (svref fields 3))
	      :bidi-class                 (symbolify (svref fields 4))
	      :decomposition-type
	      (get-decomposition-type (svref fields 5))
	      :decomposition-mapping
	      (get-decomposition-mapping (svref fields 5))
	      :numeric-type		  (convert-integer (svref fields 6))
	      :numeric-value-1	          (convert-integer (svref fields 7))
	      :numeric-value-2            (convert-number (svref fields 8))
	      :bidi-mirrored              (if (equal (svref fields 9) "Y") t nil)
	      :unicode-1-name             (svref fields 10)
	      :iso-comment                (svref fields 11)
	      :simple-uppercase-mapping   (convert-hex (svref fields 12))
	      :simple-lowercase-mapping   (convert-hex (svref fields 13))
	      :simple-titlecase-mapping   (convert-hex (svref fields 14))))))))

(defparameter *eaw-file*
  (make-instance
   'data-file
   :url (s+ *url-prefix* "EastAsianWidth.txt")
   :columns '((:name "Start" :type integer)
	      (:name "End" :type integer)
	      (:name "Property" :type keyword))
   :process-line #'read-property-range-line))

(defparameter *emoji-file*
  (make-instance
   'data-file
   :url (s+ *url-prefix* "emoji/emoji-data.txt")
   :columns '((:name "Start" :type integer)
	      (:name "End" :type integer)
	      (:name "Property" :type keyword))
   :process-line #'read-property-range-line))

(defparameter *block-file*
  (make-instance
   'data-file
   :url (s+ *url-prefix* "Blocks.txt")
   :columns '((:name "Start" :type integer)
	      (:name "End" :type integer)
	      (:name "Property" :type string))
   :process-line #'read-property-range-string-line))

(defun check-hashes (output-file)
  "Compare the hashes in the OUTPUT-FILE with the downloaded files, and return
true if there's the same."
  (with-simple-restart (dont-check
			"Don't bother checking if generating is unnecessary.")
    (when (file-exists output-file)
      (with-open-file (stream output-file :direction :input)
	(let ((header (safe-read stream)))
	  (when (not (consp header))
	    (cerror "Whatever." "Output file header is wrong.")
	    (invoke-restart 'dont-check))
	  (when (not (equal (car header) *version*))
	    (cerror "Whatever." "Output file header version is wrong.")
	    (invoke-restart 'dont-check))
	  (when (not (consp (cdr header)))
	    (cerror "Whatever." "Output file header is wrong.")
	    (invoke-restart 'dont-check))
	  (loop :with entry
	     :for f :in *files*
	     :do
	     (setf entry (assoc (data-file-name f) (cdr header) :test #'equal))
	     (when (not entry)
	       (cerror "Whatever." "Missing hash for ~s." (data-file-name f))
	       (invoke-restart 'dont-check))
	     (when (not (equal (cdr entry) (data-file-hash f)))
	       (format *log* "~s~%" (data-file-name f))
	       (return-from check-hashes nil)))
	  t)))))

(defun write-header (stream)
  (let ((header (list *version*
		      (loop :for f :in *files*
			 :collect (cons (data-file-name f)
					(data-file-hash f))))))
    (dolist (l '(";; Hi. This is the generated Unicode data for -*- Lisp -*-"
		 ";; Go ahead and edit it all you want. I don't care."
		 ""
		 ";; This is the version number and the file and checksums,"
		 ";; which is read to see if we have to regenerate."))
      (write-line l stream))
    (write header :stream stream :readably t)
    (terpri stream)
    (dotimes (i 75) (write-char #\; stream))
    (terpri stream)))

(defun dump-var (name stream)
  "Dump the varible named NAME to STREAM."
  (format stream "~s~%" `(defparameter ,(symbol-name name)
			   ,(symbol-value name)
			   ,(documentation name 'symbol))))

(defun load-data (&key force)
  "Download and/or read the data files."
  (loop :for f :in *files*
     :do (read-file f :force force)))

(defun extract (&key (output-file *default-output-file*)
		  (verbose t) force check)
  "Extract Unicode data from the Unicode data files. Possibly download the files
from the Unicode web site. If the content is different from the existing output
file, re-create the file.
  VERBOSE - True to say what we're up to.
  FORCE   - True to force regenerating the output, even if the files exist and
	    there's no change.
  CHECK   - Download the files and check the content, even the files already
            exist."
  (let ((*log* (when verbose *standard-output*)))
    ;; Download and/or read the files.
    (load-data :force (or force check))

    ;; Check if we need to regenerate
    (block nil
      (when (check-hashes output-file)
	(format *log* "We don't need to regenerate the file.~%")
	(when (not force)
	  (return t))
	(format *log* "But we will anyway, since you said so.~%"))

      (with-open-file (stream output-file :direction :output
			      :if-exists :rename
			      :if-does-not-exist :create)
	(write-header stream)
	;; (dump-var *double-wide-chars* stream)
	;; (dump-var *combining-chars* stream)
	;; (dump-var *unknown-chars* stream)
	(format *log* "Output to ~s~%" output-file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lookat (filename)
  (symbol-call :table-viewer :view-table
    (data-file-data
     (find-if (_ (equalp filename (data-file-name _))) *files*))))

;; End
