;;
;; char-util.lisp - General utility functions dealing with characters.
;;

(defpackage :char-util
  (:documentation "Utility functions for characters.")
  (:use :cl :dlib :stretchy)
  (:export
   #:meta-char
   #:meta-char-p
   #:ctrl
   #:control-char-p
   #:un-meta
   #:nice-char
   #:char-as-ascii
   #:displayable-char
   #:combining-char-p
   #:double-wide-char-p
   #:zero-width-char-p
   #:control-char-graphic
   #:display-length
   #:do-graphemes
   #:graphemes
   #:grapheme-length
   #:simplify-char
   #:simplify-string
   #:get-utf8-char #:%get-utf8-char
   #:get-utf8b-char #:%get-utf8b-char
   #:length-in-utf8-bytes
   #:put-utf8-char #:%put-utf8-char
   #:string-to-utf8-bytes
   #:utf8-bytes-to-string
   ))
(in-package :char-util)

#+sbcl
;; Older versions of SBCL don't have this.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :sb-unicode)
    (d-add-feature :has-sb-unicode)))

;(

(declaim (optimize (speed 3) (safety 0) (debug 1) (space 0) (compilation-speed 0)))

;; Sadly #\^A is not portable. This assumes ASCII or UTF8 or something. 
(defun ctrl (c)
  "Return the control character corresponding to the normal character."
  (code-char (1+ (- (char-code (char-upcase c)) (char-code #\A)))))

(defun control-char-p (c)
  "Return true if C is a control character. In ASCII that means anything less
than space and delete."
  (and (characterp c)
       (let ((cc (char-code c)))
	 ;; Sadly ASCII / UTF-8 specific.
	 (or (< cc (char-code #\space)) (eql cc (char-code #\rubout))))))

(defun meta-char (c)
  "Turn the meta (8th) bit on in the code representation of the
   given character."
  (code-char (logior (ash 1 7) (char-code c))))

(defun meta-char-p (c)
  "Is the given number a meta character as a char code?"
  (> (logand (ash 1 7) c) 0))

;; perhaps: one for chars, one for numbers (as char codes)
; (defmethod meta-char-p ((c @@@))
;   (> (logand (ash 1 7) c) 0))

(defun un-meta (c)
  "Return the non-meta character verion of character code C"
  (code-char (logand (- (ash 1 7) 1) c)))

(defun nice-char (c &key caret)
  "Nice character formatting, for ASCII compatible encodings."
  (let ((cc (if (characterp c) (char-code c) nil)))
    (cond
      ((and cc (meta-char-p cc))
       (format nil "M-~a" (nice-char (un-meta cc))))
      ((and cc (= cc 27))
       (format nil "ESC"))
      ((and cc (= cc (char-code #\space)))
       (format nil "SPACE"))
      ((and cc (< cc (char-code #\space)))
       (format nil "~:[C-~(~c~)~;^~c~]" caret
	       (code-char (+ cc (char-code #\@)))))
      ((and cc (= cc 127)
       (format nil (if caret "^?" "C-?"))))
      (cc (format nil "~a" c))
      (t (format nil "~s" c)))))

(defun char-as-ascii (c &key (caret t))
  "Return an ASCII string to display character."
  (let ((cc (if (characterp c) (char-code c) nil)))
    (cond
      ((and cc (meta-char-p cc))
       (format nil "M-~a" (nice-char (un-meta cc))))
      ((and cc (< cc (char-code #\space)))
       (format nil "~:[C-~(~c~)~;^~c~]" caret
	       (code-char (+ cc (char-code #\@)))))
      ((and cc (= cc 127))
       (format nil (if caret "^?" "C-?")))
      ((and cc (> cc 127) (< cc 255))
       (format nil "\\~o" cc))
      ((and cc (> cc 255))
       (format nil "#x~x" cc))
      (cc (format nil "~a" c))
      (t (format nil "~s" c)))))

(defun displayable-char (c &key (caret t) all-control (show-meta t) stream)
  "Make non-graphic characters visible."
  (let ((cc (if (characterp c) (char-code c) nil)))
    (cond
      ((and cc (not all-control) (or (eql c #\tab) (eql c #\newline)))
       (if stream
	   (princ c stream)
	   (princ-to-string c)))
      ((and cc show-meta (meta-char-p cc))
       (format stream "M-~a" (nice-char (un-meta cc))))
      ((and cc (< cc (char-code #\space)))
       (format stream "~:[C-~(~c~)~;^~c~]" caret
	       (code-char (+ cc (char-code #\@)))))
      ((and cc (= cc 127))
       (format stream (if caret "^?" "C-?")))
      ;; These can mess with some terminals, so don't display them directly.
      ((and cc (> cc 127) (< cc 160))
       (format stream "\\~o" cc))
      (cc
       (if stream
	   (princ c stream)
	   (princ-to-string c)))
      (t (format stream "~s" c)))))

(defun normalize-string (string &optional (form :nfd))
  "Return a Unicode normalized string based on STRING. FORM can be one of
:NFD, :NFC, :NFKD, or :NFKC."
  #+sbcl (normalize-string string form)
  #-(or sbcl) (declare (ignore string form))
  #-(or sbcl) (error "Missing implementation: normalize-string"))

(defparameter *general-category-name*
  #(
(:Lu . :Uppercase_Letter)      ;an uppercase letter
(:Ll . :Lowercase_Letter)      ;a lowercase letter
(:Lt . :Titlecase_Letter)      ;a digraphic character, with first part uppercase
(:LC . :Cased_Letter)          ;Lu | Ll | Lt
(:Lm . :Modifier_Letter)       ;a modifier letter
(:Lo . :Other_Letter)          ;other letters, including syllables and ideographs
(:L  . :Letter)                ;Lu | Ll | Lt | Lm | Lo
(:Mn . :Nonspacing_Mark)       ;a nonspacing combining mark (zero advance width)
(:Mc . :Spacing_Mark)          ;a spacing combining mark (positive advance width)
(:Me . :Enclosing_Mark)        ;an enclosing combining mark
(:M  . :Mark)                  ;Mn | Mc | Me
(:Nd . :Decimal_Number)        ;a decimal digit
(:Nl . :Letter_Number)         ;a letterlike numeric character
(:No . :Other_Number)          ;a numeric character of other type
(:N  . :Number)                ;Nd | Nl | No
(:Pc . :Connector_Punctuation) ;a connecting punctuation mark, like a tie
(:Pd . :Dash_Punctuation)      ;a dash or hyphen punctuation mark
(:Ps . :Open_Punctuation)      ;an opening punctuation mark (of a pair)
(:Pe . :Close_Punctuation)     ;a closing punctuation mark (of a pair)
(:Pi . :Initial_Punctuation)   ;an initial quotation mark
(:Pf . :Final_Punctuation)     ;a final quotation mark
(:Po . :Other_Punctuation)     ;a punctuation mark of other type
(:P  . :Punctuation)           ;Pc | Pd | Ps | Pe | Pi | Pf | Po
(:Sm . :Math_Symbol)           ;a symbol of mathematical use
(:Sc . :Currency_Symbol)       ;a currency sign
(:Sk . :Modifier_Symbol)       ;a non-letterlike modifier symbol
(:So . :Other_Symbol)          ;a symbol of other type
(:S  . :Symbol)                ;Sm | Sc | Sk | So
(:Zs . :Space_Separator)       ;a space character (of various non-zero widths)
(:Zl . :Line_Separator)        ;U+2028 LINE SEPARATOR only
(:Zp . :Paragraph_Separator)   ;U+2029 PARAGRAPH SEPARATOR only
(:Z  . :Separator)             ;Zs | Zl | Zp
(:Cc . :Control)               ;a C0 or C1 control code
(:Cf . :Format)                ;a format control character
(:Cs . :Surrogate)             ;a surrogate code point
(:Co . :Private_Use)           ;a private-use character
(:Cn . :Unassigned)            ;a reserved unassigned code point or a noncharacter
(:C  . :Other)                 ;Cc | Cf | Cs | Co | Cn
))

(defparameter *high-combining-chars*
  (when (> char-code-limit (1+ #xffff))
    (mapcar #'code-char
	    '(#x000101E4  ;   PHAISTOS_DISC_SIGN_COMB
	      #x000101FD  ;   PHAISTOS_DISC_SIGN_COMBINING_OBLIQUE_STROKE
	      #x00010376  ;   COMBINING_OLD_PERMIC_LETTER_AN
	      #x00010377  ;   COMBINING_OLD_PERMIC_LETTER_DOI
	      #x00010378  ;   COMBINING_OLD_PERMIC_LETTER_ZATA
	      #x00010379  ;   COMBINING_OLD_PERMIC_LETTER_NENOE
	      #x0001037A  ;   COMBINING_OLD_PERMIC_LETTER_SII
	      #x00011366  ;   COMBINING_GRANTHA_DIGIT_ZERO
	      #x00011367  ;   COMBINING_GRANTHA_DIGIT_ONE
	      #x00011368  ;   COMBINING_GRANTHA_DIGIT_TWO
	      #x00011369  ;   COMBINING_GRANTHA_DIGIT_THREE
	      #x0001136A  ;   COMBINING_GRANTHA_DIGIT_FOUR
	      #x0001136B  ;   COMBINING_GRANTHA_DIGIT_FIVE
	      #x0001136C  ;   COMBINING_GRANTHA_DIGIT_SIX
	      #x00011370  ;   COMBINING_GRANTHA_LETTER_A
	      #x00011371  ;   COMBINING_GRANTHA_LETTER_KA
	      #x00011372  ;   COMBINING_GRANTHA_LETTER_NA
	      #x00011373  ;   COMBINING_GRANTHA_LETTER_VI
	      #x00011374  ;   COMBINING_GRANTHA_LETTER_PA
	      #x00016AF0  ;   BASSA_VAH_COMBINING_HIGH_TONE
	      #x00016AF1  ;   BASSA_VAH_COMBINING_LOW_TONE
	      #x00016AF2  ;   BASSA_VAH_COMBINING_MID_TONE
	      #x00016AF3  ;   BASSA_VAH_COMBINING_LOW-MID_TONE
	      #x00016AF4  ;   BASSA_VAH_COMBINING_HIGH-LOW_TONE
	      #x0001D165  ; 𝅥 MUSICAL_SYMBOL_COMBINING_STEM
	      #x0001D166  ; 𝅦 MUSICAL_SYMBOL_COMBINING_SPRECHGESANG_STEM
	      #x0001D167  ; 𝅧 MUSICAL_SYMBOL_COMBINING_TREMOLO-1
	      #x0001D168  ; 𝅨 MUSICAL_SYMBOL_COMBINING_TREMOLO-2
	      #x0001D169  ; 𝅩 MUSICAL_SYMBOL_COMBINING_TREMOLO-3
	      #x0001D16D  ; 𝅭 MUSICAL_SYMBOL_COMBINING_AUGMENTATION_DOT
	      #x0001D16E  ; 𝅮 MUSICAL_SYMBOL_COMBINING_FLAG-1
	      #x0001D16F  ; 𝅯 MUSICAL_SYMBOL_COMBINING_FLAG-2
	      #x0001D170  ; 𝅰 MUSICAL_SYMBOL_COMBINING_FLAG-3
	      #x0001D171  ; 𝅱 MUSICAL_SYMBOL_COMBINING_FLAG-4
	      #x0001D172  ; 𝅲 MUSICAL_SYMBOL_COMBINING_FLAG-5
	      #x0001D17B  ; 𝅻 MUSICAL_SYMBOL_COMBINING_ACCENT
	      #x0001D17C  ; 𝅼 MUSICAL_SYMBOL_COMBINING_STACCATO
	      #x0001D17D  ; 𝅽 MUSICAL_SYMBOL_COMBINING_TENUTO
	      #x0001D17E  ; 𝅾 MUSICAL_SYMBOL_COMBINING_STACCATISSIMO
	      #x0001D17F  ; 𝅿 MUSICAL_SYMBOL_COMBINING_MARCATO
	      #x0001D180  ; 𝆀 MUSICAL_SYMBOL_COMBINING_MARCATO-STACCATO
	      #x0001D181  ; 𝆁 MUSICAL_SYMBOL_COMBINING_ACCENT-STACCATO
	      #x0001D182  ; 𝆂 MUSICAL_SYMBOL_COMBINING_LOURE
	      #x0001D185  ; 𝆅 MUSICAL_SYMBOL_COMBINING_DOIT
	      #x0001D186  ; 𝆆 MUSICAL_SYMBOL_COMBINING_RIP
	      #x0001D187  ; 𝆇 MUSICAL_SYMBOL_COMBINING_FLIP
	      #x0001D188  ; 𝆈 MUSICAL_SYMBOL_COMBINING_SMEAR
	      #x0001D189  ; 𝆉 MUSICAL_SYMBOL_COMBINING_BEND
	      #x0001D18A  ; 𝆊 MUSICAL_SYMBOL_COMBINING_DOUBLE_TONGUE
	      #x0001D18B  ; 𝆋 MUSICAL_SYMBOL_COMBINING_TRIPLE_TONGUE
	      #x0001D1AA  ; 𝆪 MUSICAL_SYMBOL_COMBINING_DOWN_BOW
	      #x0001D1AB  ; 𝆫 MUSICAL_SYMBOL_COMBINING_UP_BOW
	      #x0001D1AC  ; 𝆬 MUSICAL_SYMBOL_COMBINING_HARMONIC
	      #x0001D1AD  ; 𝆭 MUSICAL_SYMBOL_COMBINING_SNAP_PIZZICATO
	      #x0001D242  ;   COMBINING_GREEK_MUSICAL_TRISEME
	      #x0001D243  ;   COMBINING_GREEK_MUSICAL_TETRASEME
	      #x0001D244  ;   COMBINING_GREEK_MUSICAL_PENTASEME
	      #x0001E8D0  ;   MENDE_KIKAKUI_COMBINING_NUMBER_TEENS
	      #x0001E8D1  ;   MENDE_KIKAKUI_COMBINING_NUMBER_TENS
	      #x0001E8D2  ;   MENDE_KIKAKUI_COMBINING_NUMBER_HUNDREDS
	      #x0001E8D3  ;   MENDE_KIKAKUI_COMBINING_NUMBER_THOUSANDS
	      #x0001E8D4  ;   MENDE_KIKAKUI_COMBINING_NUMBER_TEN_THOUSANDS
	      #x0001E8D5  ;   MENDE_KIKAKUI_COMBINING_NUMBER_HUNDRED_THOUSANDS
	      #x0001E8D6  ;   MENDE_KIKAKUI_COMBINING_NUMBER_MILLIONS
	      ))))

(defparameter *low-combining-chars*
  '(#x00000300 ; ̀ COMBINING_GRAVE_ACCENT
    #x00000301 ; ́ COMBINING_ACUTE_ACCENT
    #x00000302 ; ̂ COMBINING_CIRCUMFLEX_ACCENT
    #x00000303 ; ̃ COMBINING_TILDE
    #x00000304 ; ̄ COMBINING_MACRON
    #x00000305 ; ̅ COMBINING_OVERLINE
    #x00000306 ; ̆ COMBINING_BREVE
    #x00000307 ; ̇ COMBINING_DOT_ABOVE
    #x00000308 ; ̈ COMBINING_DIAERESIS
    #x00000309 ; ̉ COMBINING_HOOK_ABOVE
    #x0000030A ; ̊ COMBINING_RING_ABOVE
    #x0000030B ; ̋ COMBINING_DOUBLE_ACUTE_ACCENT
    #x0000030C ; ̌ COMBINING_CARON
    #x0000030D ; ̍ COMBINING_VERTICAL_LINE_ABOVE
    #x0000030E ; ̎ COMBINING_DOUBLE_VERTICAL_LINE_ABOVE
    #x0000030F ; ̏ COMBINING_DOUBLE_GRAVE_ACCENT
    #x00000310 ; ̐ COMBINING_CANDRABINDU
    #x00000311 ; ̑ COMBINING_INVERTED_BREVE
    #x00000312 ; ̒ COMBINING_TURNED_COMMA_ABOVE
    #x00000313 ; ̓ COMBINING_COMMA_ABOVE
    #x00000314 ; ̔ COMBINING_REVERSED_COMMA_ABOVE
    #x00000315 ; ̕ COMBINING_COMMA_ABOVE_RIGHT
    #x00000316 ; ̖ COMBINING_GRAVE_ACCENT_BELOW
    #x00000317 ; ̗ COMBINING_ACUTE_ACCENT_BELOW
    #x00000318 ; ̘ COMBINING_LEFT_TACK_BELOW
    #x00000319 ; ̙ COMBINING_RIGHT_TACK_BELOW
    #x0000031A ; ̚ COMBINING_LEFT_ANGLE_ABOVE
    #x0000031B ; ̛ COMBINING_HORN
    #x0000031C ; ̜ COMBINING_LEFT_HALF_RING_BELOW
    #x0000031D ; ̝ COMBINING_UP_TACK_BELOW
    #x0000031E ; ̞ COMBINING_DOWN_TACK_BELOW
    #x0000031F ; ̟ COMBINING_PLUS_SIGN_BELOW
    #x00000320 ; ̠ COMBINING_MINUS_SIGN_BELOW
    #x00000321 ; ̡ COMBINING_PALATALIZED_HOOK_BELOW
    #x00000322 ; ̢ COMBINING_RETROFLEX_HOOK_BELOW
    #x00000323 ; ̣ COMBINING_DOT_BELOW
    #x00000324 ; ̤ COMBINING_DIAERESIS_BELOW
    #x00000325 ; ̥ COMBINING_RING_BELOW
    #x00000326 ; ̦ COMBINING_COMMA_BELOW
    #x00000327 ; ̧ COMBINING_CEDILLA
    #x00000328 ; ̨ COMBINING_OGONEK
    #x00000329 ; ̩ COMBINING_VERTICAL_LINE_BELOW
    #x0000032A ; ̪ COMBINING_BRIDGE_BELOW
    #x0000032B ; ̫ COMBINING_INVERTED_DOUBLE_ARCH_BELOW
    #x0000032C ; ̬ COMBINING_CARON_BELOW
    #x0000032D ; ̭ COMBINING_CIRCUMFLEX_ACCENT_BELOW
    #x0000032E ; ̮ COMBINING_BREVE_BELOW
    #x0000032F ; ̯ COMBINING_INVERTED_BREVE_BELOW
    #x00000330 ; ̰ COMBINING_TILDE_BELOW
    #x00000331 ; ̱ COMBINING_MACRON_BELOW
    #x00000332 ; ̲ COMBINING_LOW_LINE
    #x00000333 ; ̳ COMBINING_DOUBLE_LOW_LINE
    #x00000334 ; ̴ COMBINING_TILDE_OVERLAY
    #x00000335 ; ̵ COMBINING_SHORT_STROKE_OVERLAY
    #x00000336 ; ̶ COMBINING_LONG_STROKE_OVERLAY
    #x00000337 ; ̷ COMBINING_SHORT_SOLIDUS_OVERLAY
    #x00000338 ; ̸ COMBINING_LONG_SOLIDUS_OVERLAY
    #x00000339 ; ̹ COMBINING_RIGHT_HALF_RING_BELOW
    #x0000033A ; ̺ COMBINING_INVERTED_BRIDGE_BELOW
    #x0000033B ; ̻ COMBINING_SQUARE_BELOW
    #x0000033C ; ̼ COMBINING_SEAGULL_BELOW
    #x0000033D ; ̽ COMBINING_X_ABOVE
    #x0000033E ; ̾ COMBINING_VERTICAL_TILDE
    #x0000033F ; ̿ COMBINING_DOUBLE_OVERLINE
    #x00000340 ; ̀ COMBINING_GRAVE_TONE_MARK
    #x00000341 ; ́ COMBINING_ACUTE_TONE_MARK
    #x00000342 ; ͂ COMBINING_GREEK_PERISPOMENI
    #x00000343 ; ̓ COMBINING_GREEK_KORONIS
    #x00000344 ; ̈́ COMBINING_GREEK_DIALYTIKA_TONOS
    #x00000345 ; ͅ COMBINING_GREEK_YPOGEGRAMMENI
    #x00000346 ; ͆ COMBINING_BRIDGE_ABOVE
    #x00000347 ; ͇ COMBINING_EQUALS_SIGN_BELOW
    #x00000348 ; ͈ COMBINING_DOUBLE_VERTICAL_LINE_BELOW
    #x00000349 ; ͉ COMBINING_LEFT_ANGLE_BELOW
    #x0000034A ; ͊ COMBINING_NOT_TILDE_ABOVE
    #x0000034B ; ͋ COMBINING_HOMOTHETIC_ABOVE
    #x0000034C ; ͌ COMBINING_ALMOST_EQUAL_TO_ABOVE
    #x0000034D ; ͍ COMBINING_LEFT_RIGHT_ARROW_BELOW
    #x0000034E ; ͎ COMBINING_UPWARDS_ARROW_BELOW
    #x0000034F ;   COMBINING_GRAPHEME_JOINER
    #x00000350 ; ͐ COMBINING_RIGHT_ARROWHEAD_ABOVE
    #x00000351 ; ͑ COMBINING_LEFT_HALF_RING_ABOVE
    #x00000352 ; ͒ COMBINING_FERMATA
    #x00000353 ; ͓ COMBINING_X_BELOW
    #x00000354 ; ͔ COMBINING_LEFT_ARROWHEAD_BELOW
    #x00000355 ; ͕ COMBINING_RIGHT_ARROWHEAD_BELOW
    #x00000356 ; ͖ COMBINING_RIGHT_ARROWHEAD_AND_UP_ARROWHEAD_BELOW
    #x00000357 ; ͗ COMBINING_RIGHT_HALF_RING_ABOVE
    #x00000358 ; ͘ COMBINING_DOT_ABOVE_RIGHT
    #x00000359 ; ͙ COMBINING_ASTERISK_BELOW
    #x0000035A ; ͚ COMBINING_DOUBLE_RING_BELOW
    #x0000035B ; ͛ COMBINING_ZIGZAG_ABOVE
    #x0000035C ; ͜ COMBINING_DOUBLE_BREVE_BELOW
    #x0000035D ; ͝ COMBINING_DOUBLE_BREVE
    #x0000035E ; ͞ COMBINING_DOUBLE_MACRON
    #x0000035F ; ͟ COMBINING_DOUBLE_MACRON_BELOW
    #x00000360 ; ͠ COMBINING_DOUBLE_TILDE
    #x00000361 ; ͡ COMBINING_DOUBLE_INVERTED_BREVE
    #x00000362 ; ͢ COMBINING_DOUBLE_RIGHTWARDS_ARROW_BELOW
    #x00000363 ; ͣ COMBINING_LATIN_SMALL_LETTER_A
    #x00000364 ; ͤ COMBINING_LATIN_SMALL_LETTER_E
    #x00000365 ; ͥ COMBINING_LATIN_SMALL_LETTER_I
    #x00000366 ; ͦ COMBINING_LATIN_SMALL_LETTER_O
    #x00000367 ; ͧ COMBINING_LATIN_SMALL_LETTER_U
    #x00000368 ; ͨ COMBINING_LATIN_SMALL_LETTER_C
    #x00000369 ; ͩ COMBINING_LATIN_SMALL_LETTER_D
    #x0000036A ; ͪ COMBINING_LATIN_SMALL_LETTER_H
    #x0000036B ; ͫ COMBINING_LATIN_SMALL_LETTER_M
    #x0000036C ; ͬ COMBINING_LATIN_SMALL_LETTER_R
    #x0000036D ; ͭ COMBINING_LATIN_SMALL_LETTER_T
    #x0000036E ; ͮ COMBINING_LATIN_SMALL_LETTER_V
    #x0000036F ; ͯ COMBINING_LATIN_SMALL_LETTER_X
    #x00000483 ; ҃ COMBINING_CYRILLIC_TITLO
    #x00000484 ; ҄ COMBINING_CYRILLIC_PALATALIZATION
    #x00000485 ; ҅ COMBINING_CYRILLIC_DASIA_PNEUMATA
    #x00000486 ; ҆ COMBINING_CYRILLIC_PSILI_PNEUMATA
    #x00000487 ;   COMBINING_CYRILLIC_POKRYTIE
    #x00000488 ; ҈ COMBINING_CYRILLIC_HUNDRED_THOUSANDS_SIGN
    #x00000489 ; ҉ COMBINING_CYRILLIC_MILLIONS_SIGN
    #x000007EB ;   NKO_COMBINING_SHORT_HIGH_TONE
    #x000007EC ;   NKO_COMBINING_SHORT_LOW_TONE
    #x000007ED ;   NKO_COMBINING_SHORT_RISING_TONE
    #x000007EE ;   NKO_COMBINING_LONG_DESCENDING_TONE
    #x000007EF ;   NKO_COMBINING_LONG_HIGH_TONE
    #x000007F0 ;   NKO_COMBINING_LONG_LOW_TONE
    #x000007F1 ;   NKO_COMBINING_LONG_RISING_TONE
    #x000007F2 ;   NKO_COMBINING_NASALIZATION_MARK
    #x000007F3 ;   NKO_COMBINING_DOUBLE_DOT_ABOVE
    #x00000C00 ;   TELUGU_SIGN_COMBINING_CANDRABINDU_ABOVE
    #x0000135D ;   ETHIOPIC_COMBINING_GEMINATION_AND_VOWEL_LENGTH_MARK
    #x0000135E ;   ETHIOPIC_COMBINING_VOWEL_LENGTH_MARK
    #x0000135F ;   ETHIOPIC_COMBINING_GEMINATION_MARK
    #x00001A7F ;   TAI_THAM_COMBINING_CRYPTOGRAMMIC_DOT
    #x00001AB0 ;   COMBINING_DOUBLED_CIRCUMFLEX_ACCENT
    #x00001AB1 ;   COMBINING_DIAERESIS-RING
    #x00001AB2 ;   COMBINING_INFINITY
    #x00001AB3 ;   COMBINING_DOWNWARDS_ARROW
    #x00001AB4 ;   COMBINING_TRIPLE_DOT
    #x00001AB5 ;   COMBINING_X-X_BELOW
    #x00001AB6 ;   COMBINING_WIGGLY_LINE_BELOW
    #x00001AB7 ;   COMBINING_OPEN_MARK_BELOW
    #x00001AB8 ;   COMBINING_DOUBLE_OPEN_MARK_BELOW
    #x00001AB9 ;   COMBINING_LIGHT_CENTRALIZATION_STROKE_BELOW
    #x00001ABA ;   COMBINING_STRONG_CENTRALIZATION_STROKE_BELOW
    #x00001ABB ;   COMBINING_PARENTHESES_ABOVE
    #x00001ABC ;   COMBINING_DOUBLE_PARENTHESES_ABOVE
    #x00001ABD ;   COMBINING_PARENTHESES_BELOW
    #x00001ABE ;   COMBINING_PARENTHESES_OVERLAY
    #x00001B6B ;   BALINESE_MUSICAL_SYMBOL_COMBINING_TEGEH
    #x00001B6C ;   BALINESE_MUSICAL_SYMBOL_COMBINING_ENDEP
    #x00001B6D ;   BALINESE_MUSICAL_SYMBOL_COMBINING_KEMPUL
    #x00001B6E ;   BALINESE_MUSICAL_SYMBOL_COMBINING_KEMPLI
    #x00001B6F ;   BALINESE_MUSICAL_SYMBOL_COMBINING_JEGOGAN
    #x00001B70 ;   BALINESE_MUSICAL_SYMBOL_COMBINING_KEMPUL_WITH_JEGOGAN
    #x00001B71 ;   BALINESE_MUSICAL_SYMBOL_COMBINING_KEMPLI_WITH_JEGOGAN
    #x00001B72 ;   BALINESE_MUSICAL_SYMBOL_COMBINING_BENDE
    #x00001B73 ;   BALINESE_MUSICAL_SYMBOL_COMBINING_GONG
    #x00001DC0 ; ᷀ COMBINING_DOTTED_GRAVE_ACCENT
    #x00001DC1 ; ᷁ COMBINING_DOTTED_ACUTE_ACCENT
    #x00001DC2 ; ᷂ COMBINING_SNAKE_BELOW
    #x00001DC3 ; ᷃ COMBINING_SUSPENSION_MARK
    #x00001DC4 ;   COMBINING_MACRON-ACUTE
    #x00001DC5 ;   COMBINING_GRAVE-MACRON
    #x00001DC6 ;   COMBINING_MACRON-GRAVE
    #x00001DC7 ;   COMBINING_ACUTE-MACRON
    #x00001DC8 ;   COMBINING_GRAVE-ACUTE-GRAVE
    #x00001DC9 ;   COMBINING_ACUTE-GRAVE-ACUTE
    #x00001DCA ;   COMBINING_LATIN_SMALL_LETTER_R_BELOW
    #x00001DCB ;   COMBINING_BREVE-MACRON
    #x00001DCC ;   COMBINING_MACRON-BREVE
    #x00001DCD ;   COMBINING_DOUBLE_CIRCUMFLEX_ABOVE
    #x00001DCE ;   COMBINING_OGONEK_ABOVE
    #x00001DCF ;   COMBINING_ZIGZAG_BELOW
    #x00001DD0 ;   COMBINING_IS_BELOW
    #x00001DD1 ;   COMBINING_UR_ABOVE
    #x00001DD2 ;   COMBINING_US_ABOVE
    #x00001DD3 ;   COMBINING_LATIN_SMALL_LETTER_FLATTENED_OPEN_A_ABOVE
    #x00001DD4 ;   COMBINING_LATIN_SMALL_LETTER_AE
    #x00001DD5 ;   COMBINING_LATIN_SMALL_LETTER_AO
    #x00001DD6 ;   COMBINING_LATIN_SMALL_LETTER_AV
    #x00001DD7 ;   COMBINING_LATIN_SMALL_LETTER_C_CEDILLA
    #x00001DD8 ;   COMBINING_LATIN_SMALL_LETTER_INSULAR_D
    #x00001DD9 ;   COMBINING_LATIN_SMALL_LETTER_ETH
    #x00001DDA ;   COMBINING_LATIN_SMALL_LETTER_G
    #x00001DDB ;   COMBINING_LATIN_LETTER_SMALL_CAPITAL_G
    #x00001DDC ;   COMBINING_LATIN_SMALL_LETTER_K
    #x00001DDD ;   COMBINING_LATIN_SMALL_LETTER_L
    #x00001DDE ;   COMBINING_LATIN_LETTER_SMALL_CAPITAL_L
    #x00001DDF ;   COMBINING_LATIN_LETTER_SMALL_CAPITAL_M
    #x00001DE0 ;   COMBINING_LATIN_SMALL_LETTER_N
    #x00001DE1 ;   COMBINING_LATIN_LETTER_SMALL_CAPITAL_N
    #x00001DE2 ;   COMBINING_LATIN_LETTER_SMALL_CAPITAL_R
    #x00001DE3 ;   COMBINING_LATIN_SMALL_LETTER_R_ROTUNDA
    #x00001DE4 ;   COMBINING_LATIN_SMALL_LETTER_S
    #x00001DE5 ;   COMBINING_LATIN_SMALL_LETTER_LONG_S
    #x00001DE6 ;   COMBINING_LATIN_SMALL_LETTER_Z
    #x00001DE7 ;   COMBINING_LATIN_SMALL_LETTER_ALPHA
    #x00001DE8 ;   COMBINING_LATIN_SMALL_LETTER_B
    #x00001DE9 ;   COMBINING_LATIN_SMALL_LETTER_BETA
    #x00001DEA ;   COMBINING_LATIN_SMALL_LETTER_SCHWA
    #x00001DEB ;   COMBINING_LATIN_SMALL_LETTER_F
    #x00001DEC ;   COMBINING_LATIN_SMALL_LETTER_L_WITH_DOUBLE_MIDDLE_TILDE
    #x00001DED ;   COMBINING_LATIN_SMALL_LETTER_O_WITH_LIGHT_CENTRALIZATION_STROKE
    #x00001DEE ;   COMBINING_LATIN_SMALL_LETTER_P
    #x00001DEF ;   COMBINING_LATIN_SMALL_LETTER_ESH
    #x00001DF0 ;   COMBINING_LATIN_SMALL_LETTER_U_WITH_LIGHT_CENTRALIZATION_STROKE
    #x00001DF1 ;   COMBINING_LATIN_SMALL_LETTER_W
    #x00001DF2 ;   COMBINING_LATIN_SMALL_LETTER_A_WITH_DIAERESIS
    #x00001DF3 ;   COMBINING_LATIN_SMALL_LETTER_O_WITH_DIAERESIS
    #x00001DF4 ;   COMBINING_LATIN_SMALL_LETTER_U_WITH_DIAERESIS
    #x00001DF5 ;   COMBINING_UP_TACK_ABOVE
    #x00001DFC ;   COMBINING_DOUBLE_INVERTED_BREVE_BELOW
    #x00001DFD ;   COMBINING_ALMOST_EQUAL_TO_BELOW
    #x00001DFE ;   COMBINING_LEFT_ARROWHEAD_ABOVE
    #x00001DFF ;   COMBINING_RIGHT_ARROWHEAD_AND_DOWN_ARROWHEAD_BELOW
    #x000020D0 ; ⃐ COMBINING_LEFT_HARPOON_ABOVE
    #x000020D1 ; ⃑ COMBINING_RIGHT_HARPOON_ABOVE
    #x000020D2 ; ⃒ COMBINING_LONG_VERTICAL_LINE_OVERLAY
    #x000020D3 ; ⃓ COMBINING_SHORT_VERTICAL_LINE_OVERLAY
    #x000020D4 ; ⃔ COMBINING_ANTICLOCKWISE_ARROW_ABOVE
    #x000020D5 ; ⃕ COMBINING_CLOCKWISE_ARROW_ABOVE
    #x000020D6 ; ⃖ COMBINING_LEFT_ARROW_ABOVE
    #x000020D7 ; ⃗ COMBINING_RIGHT_ARROW_ABOVE
    #x000020D8 ; ⃘ COMBINING_RING_OVERLAY
    #x000020D9 ; ⃙ COMBINING_CLOCKWISE_RING_OVERLAY
    #x000020DA ; ⃚ COMBINING_ANTICLOCKWISE_RING_OVERLAY
    #x000020DB ; ⃛ COMBINING_THREE_DOTS_ABOVE
    #x000020DC ; ⃜ COMBINING_FOUR_DOTS_ABOVE
    #x000020DD ; ⃝ COMBINING_ENCLOSING_CIRCLE
    #x000020DE ; ⃞ COMBINING_ENCLOSING_SQUARE
    #x000020DF ; ⃟ COMBINING_ENCLOSING_DIAMOND
    #x000020E0 ; ⃠ COMBINING_ENCLOSING_CIRCLE_BACKSLASH
    #x000020E1 ; ⃡ COMBINING_LEFT_RIGHT_ARROW_ABOVE
    #x000020E2 ; ⃢ COMBINING_ENCLOSING_SCREEN
    #x000020E3 ; ⃣ COMBINING_ENCLOSING_KEYCAP
    #x000020E4 ; ⃤ COMBINING_ENCLOSING_UPWARD_POINTING_TRIANGLE
    #x000020E5 ; ⃥ COMBINING_REVERSE_SOLIDUS_OVERLAY
    #x000020E6 ; ⃦ COMBINING_DOUBLE_VERTICAL_STROKE_OVERLAY
    #x000020E7 ; ⃧ COMBINING_ANNUITY_SYMBOL
    #x000020E8 ; ⃨ COMBINING_TRIPLE_UNDERDOT
    #x000020E9 ; ⃩ COMBINING_WIDE_BRIDGE_ABOVE
    #x000020EA ; ⃪ COMBINING_LEFTWARDS_ARROW_OVERLAY
    #x000020EB ; ⃫ COMBINING_LONG_DOUBLE_SOLIDUS_OVERLAY
    #x000020EC ;   COMBINING_RIGHTWARDS_HARPOON_WITH_BARB_DOWNWARDS
    #x000020ED ;   COMBINING_LEFTWARDS_HARPOON_WITH_BARB_DOWNWARDS
    #x000020EE ;   COMBINING_LEFT_ARROW_BELOW
    #x000020EF ;   COMBINING_RIGHT_ARROW_BELOW
    #x000020F0 ;   COMBINING_ASTERISK_ABOVE
    #x00002CEF ;   COPTIC_COMBINING_NI_ABOVE
    #x00002CF0 ;   COPTIC_COMBINING_SPIRITUS_ASPER
    #x00002CF1 ;   COPTIC_COMBINING_SPIRITUS_LENIS
    #x00002DE0 ;   COMBINING_CYRILLIC_LETTER_BE
    #x00002DE1 ;   COMBINING_CYRILLIC_LETTER_VE
    #x00002DE2 ;   COMBINING_CYRILLIC_LETTER_GHE
    #x00002DE3 ;   COMBINING_CYRILLIC_LETTER_DE
    #x00002DE4 ;   COMBINING_CYRILLIC_LETTER_ZHE
    #x00002DE5 ;   COMBINING_CYRILLIC_LETTER_ZE
    #x00002DE6 ;   COMBINING_CYRILLIC_LETTER_KA
    #x00002DE7 ;   COMBINING_CYRILLIC_LETTER_EL
    #x00002DE8 ;   COMBINING_CYRILLIC_LETTER_EM
    #x00002DE9 ;   COMBINING_CYRILLIC_LETTER_EN
    #x00002DEA ;   COMBINING_CYRILLIC_LETTER_O
    #x00002DEB ;   COMBINING_CYRILLIC_LETTER_PE
    #x00002DEC ;   COMBINING_CYRILLIC_LETTER_ER
    #x00002DED ;   COMBINING_CYRILLIC_LETTER_ES
    #x00002DEE ;   COMBINING_CYRILLIC_LETTER_TE
    #x00002DEF ;   COMBINING_CYRILLIC_LETTER_HA
    #x00002DF0 ;   COMBINING_CYRILLIC_LETTER_TSE
    #x00002DF1 ;   COMBINING_CYRILLIC_LETTER_CHE
    #x00002DF2 ;   COMBINING_CYRILLIC_LETTER_SHA
    #x00002DF3 ;   COMBINING_CYRILLIC_LETTER_SHCHA
    #x00002DF4 ;   COMBINING_CYRILLIC_LETTER_FITA
    #x00002DF5 ;   COMBINING_CYRILLIC_LETTER_ES-TE
    #x00002DF6 ;   COMBINING_CYRILLIC_LETTER_A
    #x00002DF7 ;   COMBINING_CYRILLIC_LETTER_IE
    #x00002DF8 ;   COMBINING_CYRILLIC_LETTER_DJERV
    #x00002DF9 ;   COMBINING_CYRILLIC_LETTER_MONOGRAPH_UK
    #x00002DFA ;   COMBINING_CYRILLIC_LETTER_YAT
    #x00002DFB ;   COMBINING_CYRILLIC_LETTER_YU
    #x00002DFC ;   COMBINING_CYRILLIC_LETTER_IOTIFIED_A
    #x00002DFD ;   COMBINING_CYRILLIC_LETTER_LITTLE_YUS
    #x00002DFE ;   COMBINING_CYRILLIC_LETTER_BIG_YUS
    #x00002DFF ;   COMBINING_CYRILLIC_LETTER_IOTIFIED_BIG_YUS
    #x00003099 ; ゙ COMBINING_KATAKANA-HIRAGANA_VOICED_SOUND_MARK
    #x0000309A ; ゚ COMBINING_KATAKANA-HIRAGANA_SEMI-VOICED_SOUND_MARK
    #x0000A66F ;   COMBINING_CYRILLIC_VZMET
    #x0000A670 ;   COMBINING_CYRILLIC_TEN_MILLIONS_SIGN
    #x0000A671 ;   COMBINING_CYRILLIC_HUNDRED_MILLIONS_SIGN
    #x0000A672 ;   COMBINING_CYRILLIC_THOUSAND_MILLIONS_SIGN
    #x0000A674 ;   COMBINING_CYRILLIC_LETTER_UKRAINIAN_IE
    #x0000A675 ;   COMBINING_CYRILLIC_LETTER_I
    #x0000A676 ;   COMBINING_CYRILLIC_LETTER_YI
    #x0000A677 ;   COMBINING_CYRILLIC_LETTER_U
    #x0000A678 ;   COMBINING_CYRILLIC_LETTER_HARD_SIGN
    #x0000A679 ;   COMBINING_CYRILLIC_LETTER_YERU
    #x0000A67A ;   COMBINING_CYRILLIC_LETTER_SOFT_SIGN
    #x0000A67B ;   COMBINING_CYRILLIC_LETTER_OMEGA
    #x0000A67C ;   COMBINING_CYRILLIC_KAVYKA
    #x0000A67D ;   COMBINING_CYRILLIC_PAYEROK
    #x0000A69F ;   COMBINING_CYRILLIC_LETTER_IOTIFIED_E
    #x0000A6F0 ;   BAMUM_COMBINING_MARK_KOQNDON
    #x0000A6F1 ;   BAMUM_COMBINING_MARK_TUKWENTIS
    #x0000A8E0 ;   COMBINING_DEVANAGARI_DIGIT_ZERO
    #x0000A8E1 ;   COMBINING_DEVANAGARI_DIGIT_ONE
    #x0000A8E2 ;   COMBINING_DEVANAGARI_DIGIT_TWO
    #x0000A8E3 ;   COMBINING_DEVANAGARI_DIGIT_THREE
    #x0000A8E4 ;   COMBINING_DEVANAGARI_DIGIT_FOUR
    #x0000A8E5 ;   COMBINING_DEVANAGARI_DIGIT_FIVE
    #x0000A8E6 ;   COMBINING_DEVANAGARI_DIGIT_SIX
    #x0000A8E7 ;   COMBINING_DEVANAGARI_DIGIT_SEVEN
    #x0000A8E8 ;   COMBINING_DEVANAGARI_DIGIT_EIGHT
    #x0000A8E9 ;   COMBINING_DEVANAGARI_DIGIT_NINE
    #x0000A8EA ;   COMBINING_DEVANAGARI_LETTER_A
    #x0000A8EB ;   COMBINING_DEVANAGARI_LETTER_U
    #x0000A8EC ;   COMBINING_DEVANAGARI_LETTER_KA
    #x0000A8ED ;   COMBINING_DEVANAGARI_LETTER_NA
    #x0000A8EE ;   COMBINING_DEVANAGARI_LETTER_PA
    #x0000A8EF ;   COMBINING_DEVANAGARI_LETTER_RA
    #x0000A8F0 ;   COMBINING_DEVANAGARI_LETTER_VI
    #x0000A8F1 ;   COMBINING_DEVANAGARI_SIGN_AVAGRAHA
    #x0000FE20 ; ︠ COMBINING_LIGATURE_LEFT_HALF
    #x0000FE21 ; ︡ COMBINING_LIGATURE_RIGHT_HALF
    #x0000FE22 ; ︢ COMBINING_DOUBLE_TILDE_LEFT_HALF
    #x0000FE23 ; ︣ COMBINING_DOUBLE_TILDE_RIGHT_HALF
    #x0000FE24 ;   COMBINING_MACRON_LEFT_HALF
    #x0000FE25 ;   COMBINING_MACRON_RIGHT_HALF
    #x0000FE26 ;   COMBINING_CONJOINING_MACRON
    #x0000FE27 ;   COMBINING_LIGATURE_LEFT_HALF_BELOW
    #x0000FE28 ;   COMBINING_LIGATURE_RIGHT_HALF_BELOW
    #x0000FE29 ;   COMBINING_TILDE_LEFT_HALF_BELOW
    #x0000FE2A ;   COMBINING_TILDE_RIGHT_HALF_BELOW
    #x0000FE2B ;   COMBINING_MACRON_LEFT_HALF_BELOW
    #x0000FE2C ;   COMBINING_MACRON_RIGHT_HALF_BELOW
    #x0000FE2D ;   COMBINING_CONJOINING_MACRON_BELOW
    ))

(defparameter *combining-chars*
  (let ((contents (append (mapcar #'code-char *low-combining-chars*)
			  *high-combining-chars*)))
    (sort (make-array (length contents) :element-type 'character
		      :initial-contents contents)
	  #'char<))
  "Vector of combining characters.")

;; @@@ This is a temporary hack. We should patch cl-unicode to get data from
;; the EastAsianWidth.txt file from unicode.org and make it available perhaps
;; with something like: (sb-unicode:east-asian-width c)
#-has-sb-unicode
(defparameter *wide-character-ranges*
  #((#x1100 . #x115F)
    (#x231A . #x231B)
    (#x2329 . #x232A)
    (#x23E9 . #x23EC)
    (#x23F0 . #x23F0)
    (#x23F3 . #x23F3)
    (#x25FD . #x25FE)
    (#x2614 . #x2615)
    (#x2648 . #x2653)
    (#x267F . #x267F)
    (#x2693 . #x2693)
    (#x26A1 . #x26A1)
    (#x26AA . #x26AB)
    (#x26BD . #x26BE)
    (#x26C4 . #x26C5)
    (#x26CE . #x26CE)
    (#x26D4 . #x26D4)
    (#x26EA . #x26EA)
    (#x26F2 . #x26F3)
    (#x26F5 . #x26F5)
    (#x26FA . #x26FA)
    (#x26FD . #x26FD)
    (#x2705 . #x2705)
    (#x270A . #x270B)
    (#x2728 . #x2728)
    (#x274C . #x274C)
    (#x274E . #x274E)
    (#x2753 . #x2755)
    (#x2757 . #x2757)
    (#x2795 . #x2797)
    (#x27B0 . #x27B0)
    (#x27BF . #x27BF)
    (#x2B1B . #x2B1C)
    (#x2B50 . #x2B50)
    (#x2B55 . #x2B55)
    (#x2E80 . #x2E99)
    (#x2E9B . #x2EF3)
    (#x2F00 . #x2FD5)
    (#x2FF0 . #x2FFB)
    (#x3000 . #x303E)
    (#x3041 . #x3096)
    (#x3099 . #x30FF)
    (#x3105 . #x312E)
    (#x3131 . #x318E)
    (#x3190 . #x31BA)
    (#x31C0 . #x31E3)
    (#x31F0 . #x321E)
    (#x3220 . #x3247)
    (#x3250 . #x32FE)
    (#x3300 . #x4DBF)
    (#x4E00 . #xA48C)
    (#xA490 . #xA4C6)
    (#xA960 . #xA97C)
    (#xAC00 . #xD7A3)
    (#xF900 . #xFAFF)
    (#xFE10 . #xFE19)
    (#xFE30 . #xFE52)
    (#xFE54 . #xFE66)
    (#xFE68 . #xFE6B)
    (#xFF01 . #xFF60)
    (#xFFE0 . #xFFE6)
    (#x16FE0 . #x16FE1)
    (#x17000 . #x187EC)
    (#x18800 . #x18AF2)
    (#x1B000 . #x1B11E)
    (#x1B170 . #x1B2FB)
    (#x1F004 . #x1F004)
    (#x1F0CF . #x1F0CF)
    (#x1F18E . #x1F18E)
    (#x1F191 . #x1F19A)
    (#x1F200 . #x1F202)
    (#x1F210 . #x1F23B)
    (#x1F240 . #x1F248)
    (#x1F250 . #x1F251)
    (#x1F260 . #x1F265)
    (#x1F300 . #x1F320)
    (#x1F32D . #x1F335)
    (#x1F337 . #x1F37C)
    (#x1F37E . #x1F393)
    (#x1F3A0 . #x1F3CA)
    (#x1F3CF . #x1F3D3)
    (#x1F3E0 . #x1F3F0)
    (#x1F3F4 . #x1F3F4)
    (#x1F3F8 . #x1F43E)
    (#x1F440 . #x1F440)
    (#x1F442 . #x1F4FC)
    (#x1F4FF . #x1F53D)
    (#x1F54B . #x1F54E)
    (#x1F550 . #x1F567)
    (#x1F57A . #x1F57A)
    (#x1F595 . #x1F596)
    (#x1F5A4 . #x1F5A4)
    (#x1F5FB . #x1F64F)
    (#x1F680 . #x1F6C5)
    (#x1F6CC . #x1F6CC)
    (#x1F6D0 . #x1F6D2)
    (#x1F6EB . #x1F6EC)
    (#x1F6F4 . #x1F6F8)
    (#x1F910 . #x1F93E)
    (#x1F940 . #x1F94C)
    (#x1F950 . #x1F96B)
    (#x1F980 . #x1F997)
    (#x1F9C0 . #x1F9C0)
    (#x1F9D0 . #x1F9E6)
    (#x20000 . #x2FFFD)
    (#x30000 . #x3FFFD)
    )
  "Array of pairs of character codes, which are ranges to consider wide.")

(defun %ordered-char-search (char array
			    &optional (low 0) (high (length array)))
  "Search for CHAR in VECTOR which is a sorted vector of characters."
  (declare (type fixnum low high)
	   (type character char)
	   (type (simple-array character) array)
	   ;;(optimize speed (safety 0))
	   )
  (let* ((mid (+ low (truncate (- high low) 2)))
	 (target (aref array mid)))
    (declare (type fixnum mid)
	     (type character target))
    (cond
      ((<= (1- high) low)
       (and (char= char target) target))
      ((char< char target)
       (%ordered-char-search char array low mid))
      ((char= char target)
       target)
      (t
       (%ordered-char-search char array mid high)))))

(defun ordered-char-search (char array)
  "Search for CHAR in VECTOR which is a sorted vector of characters."
  (declare (type character char)
	   (type (simple-array character) array)
	   ;;(optimize speed (safety 0))
	   )
  (cond
    ((or (char< char (aref array 0))
	 (char> char (aref array (1- (length array)))))
     nil)
    (t
     (%ordered-char-search char array))))

;; @@@ Make these better on non-SBCL. And move them out of here!
(defun combining-char-p (c)
  ;; #+sbcl (/= (sb-unicode:combining-class c) 0)
  ;; This is just plain better.
  ;;(position c *combining-chars*)
  (cond
    ((or (char< c (aref *combining-chars* 0))
	 (char> c (aref *combining-chars* (1- (length *combining-chars*)))))
     nil)
    (t
     (ordered-char-search c *combining-chars*))))

(defun general-category (c)
  #+sbcl (sb-unicode:general-category c)
  #-sbcl (keywordify (cl-unicode:general-category c)))

(defun has-property (c property)
  #+sbcl (sb-unicode:proplist-p c property)
  #-sbcl (cl-unicode:has-property
	  c (typecase property
	      ;; @@@ This probably has bad performance.
	      (keyword
	       (substitute #\_ #\- (string-capitalize property)))
	      (t property))))

(defun is-zero-width-type (c)
  (member (general-category c) '(:mn	; non spacing mark
				 :me	; combining spacing mark
				 :cf)))	; format control

(defun zero-width-char-p (c)
  (or (combining-char-p c)
      (char= c (code-char #x00ad))	;; #\soft_hyphen
      (is-zero-width-type c)
      (and (char> c (code-char #x1160)) ;; Hangul combining chars
	   (char< c (code-char #x1200)))
      (char= c (code-char #x200B))))	;; #\zero_width_space

(defun double-wide-char-p (c)
  #+(and sbcl has-sb-unicode) (let ((w (sb-unicode:east-asian-width c)))
				(or (eq w :w) (eq w :f)))
  #-(and sbcl has-sb-unicode)
  ;; @@@ fix ordered search to do this
  (position c *wide-character-ranges*
	    :test #'(lambda (a b)
		      (let ((cc (char-code a)))
			(and (>= cc (car b))
			     (<= cc (cdr b)))))))

;; adapted from sbcl/src/code/target-unicode.lisp

(defun hangul-syllable-type (character)
  "Returns the Hangul syllable type of CHARACTER.
The syllable type can be one of :L, :V, :T, :LV, or :LVT.
If the character is not a Hangul syllable or Jamo, returns NIL"
  (let ((cp (char-code character)))
    (cond
      ((or
        (and (<= #x1100 cp) (<= cp #x115f))
        (and (<= #xa960 cp) (<= cp #xa97c))) :L)
      ((or
        (and (<= #x1160 cp) (<= cp #x11a7))
        (and (<= #xd7B0 cp) (<= cp #xd7C6))) :V)
      ((or
        (and (<= #x11a8 cp) (<= cp #x11ff))
        (and (<= #xd7c8 cp) (<= cp #xd7fb))) :T)
      ((and (<= #xac00 cp) (<= cp #xd7a3))
       (if (= 0 (rem (- cp #xac00) 28)) :LV :LVT)))))

(defparameter *not-spacing-marks*
  #(
    #x102B
    #x102C
    #x1038
    #x1062
    #x1063
    #x1064
    #x1067
    #x1068
    #x1069
    #x106A
    #x106B
    #x106C
    #x106D
    #x1083
    #x1087
    #x1088
    #x1089
    #x108A
    #x108B
    #x108C
    #x108F
    #x109A
    #x109B
    #x109C
    #x19B0
    #x19B1
    #x19B2
    #x19B3
    #x19B4
    #x19B8
    #x19B9
    #x19BB
    #x19BC
    #x19BD
    #x19BE
    #x19BF
    #x19C0
    #x19C8
    #x19C9
    #x1A61
    #x1A63
    #x1A64
    #xAA7B
    #xAA7D
    ))

;; @@@ do we need this?
(defvar *other-break-special-graphemes* nil
  "Word breaking sets this to make their algorithms less tricky.")

(defun grapheme-break-class (char)
  "Returns the grapheme breaking class of CHARACTER, as specified in UAX #29."
  (let ((code (when char (char-code char)))
        (category (when char (general-category char))))
    (cond
      ((not char) nil)
      ((= code 10) :LF)
      ((= code 13) :CR)
      ((or (member category '(:Mn :Me))
           (has-property char :other-grapheme-extend)
           (and *other-break-special-graphemes*
                (member category '(:Mc :Cf)) (not (<= #x200B code #x200D))))
       :extend)
      ((or (member category '(:Zl :Zp :Cc :Cs :Cf))
           ;; From Cn and Default_Ignorable_Code_Point
           (eql code #x2065) (eql code #xE0000)
           (<= #xFFF0 code #xFFF8)
           (<= #xE0002 code #xE001F)
           (<= #xE0080 code #xE00FF)
           (<= #xE01F0 code #xE0FFF)) :control)
      ((<= #x1F1E6 code #x1F1FF) :regional-indicator)
      ((and (or (eql category :Mc)
                (eql code #x0E33) (eql code #x0EB3))
            (not
	     (ordered-char-search char *not-spacing-marks*)
	     ;; (combining-char-p char)
	     )) :spacing-mark)
      (t (hangul-syllable-type char)))))

(defun grapheme-break-p (c1 c2)
  "Return true if we should break a grapheme between characters of class
C1 and C2."
  (cond
    ;; ((and (eql c1 :cr) (eql c2 :lf))
    ;;  ;; Don't break between CR-LF
    ;;  nil)
    ((or (member c1 '(:control :cr :lf))
	 (member c2 '(:control :cr :lf)))
     ;; Control characters or line endings not between CRLF are separate.
     t)
    ((or (and (eql c1 :l) (member c2 '(:l :v :lv :lvt)))
	 (and (or (eql c1 :v) (eql c1 :lv))
	      (or (eql c2 :v) (eql c2 :t)))
	 (and (eql c2 :t) (or (eql c1 :lvt) (eql c1 :t))))
     ;; Don't break up Hangul composite characters.
     nil)
    ((and (eql c1 :regional-indicator)
	  (eql c2 :regional-indicator))
     ;; Don't break Emoji flags.
     nil)
    ((or (eql c2 :extend) (eql c2 :spacing-mark) (eql c1 :prepend))
     ;; Don't break between non-spacing things or a thing and a non-spacing
     ;; thing. @@@ where would :prepend comming from??
     nil)
    (t t))) ;; Break the fuck out of everything else.

(defmacro do-graphemes ((grapheme-var string
			 &key key (result-type 'character)) &body body)
  "Evaluate BODY once for each grapheme in STRING, with GRAPHEME-VAR bound to
the grapheme. STRING can be any vector type as long as KEY can get a character
out of an element of it. RESULT-TYPE is the type of vector which the
GRAPHEME-VARs will be, which defaults to character so it's compatable with a
'normal' string."
  (with-unique-names (class last-class c i)
    (let ((grapheme-maker `(make-stretchy-vector 0 :element-type ',result-type))
	  (cc (if key `(,key ,c) c)))
      `(progn
	 (let ((,grapheme-var ,grapheme-maker))
	   (flet ((thunk () (progn ,@body)))
	     (loop
		:with ,class :and ,last-class
		:for ,i :from 0
		:for ,c :across ,string
		:do
		(shiftf ,last-class ,class (grapheme-break-class ,cc))
		:when (and (> ,i 0) (grapheme-break-p ,last-class ,class))
		:do
		  (thunk)
		  (setf ,grapheme-var ,grapheme-maker)
		  (stretchy-append ,grapheme-var ,c)
		:else :do
		(stretchy-append ,grapheme-var ,c))
	     (when (not (zerop (length ,grapheme-var)))
	       (thunk))))))))

(defgeneric graphemes (string)
  (:documentation "Return a sequence of graphemes in STRING.")
  (:method ((string string))
    (dbugf :char-util "grapheme ~s ~s~%" (type-of string) string)
    ;; #+(and sbcl has-sb-unicode) (sb-unicode:graphemes string)
    ;; #-(and sbcl has-sb-unicode)
    (let (result)
      (do-graphemes (g string)
	(push g result))
      (nreverse result))))

;; (char-util:graphemes "d⃝u⃝c⃝k⃝")
;; (values (char-util:graphemes "수도") (char-util:graphemes "수도"))

#|
;; Note: no tab or newline
(defparameter *control-char-graphics-vec*
  `((#\Null . #\@) (,(ctrl #\A) . #\A) (,(ctrl #\B) . #\B) (,(ctrl #\C) . #\C)
    (,(ctrl #\D) . #\D) (,(ctrl #\E) . #\E) (,(ctrl #\F) . #\F)
    (,(ctrl #\G) . #\G) (,(ctrl #\H) . #\H) (,(ctrl #\J) . #\J)
    (,(ctrl #\K) . #\K) (,(ctrl #\L) . #\L) (,(ctrl #\M) . #\M)
    (,(ctrl #\N) . #\N) (,(ctrl #\O) . #\O) (,(ctrl #\P) . #\P)
    (,(ctrl #\Q) . #\Q) (,(ctrl #\R) . #\R) (,(ctrl #\S) . #\S)
    (,(ctrl #\T) . #\T) (,(ctrl #\U) . #\U) (,(ctrl #\V) . #\V)
    (,(ctrl #\W) . #\W) (,(ctrl #\X) . #\X) (,(ctrl #\Y) . #\Y)
    (,(ctrl #\Z) . #\Z) (#\Escape . #\[) (#\Fs . #\\) (#\Gs . #\])
    (#\Rs . #\^) (#\Us . #\_) (#\Rubout . #\?))
  "Vector of control characters and corresponding caret notation char.")

    (setf *control-char-graphics* (make-hash-table :test #'eql))
    (alist-to-hash-table *control-char-graphics-vec*
			 *control-char-graphics*))

(2400) ␀ SYMBOL_FOR_NULL
(2401) ␁ SYMBOL_FOR_START_OF_HEADING
(2402) ␂ SYMBOL_FOR_START_OF_TEXT
(2403) ␃ SYMBOL_FOR_END_OF_TEXT
(2404) ␄ SYMBOL_FOR_END_OF_TRANSMISSION
(2405) ␅ SYMBOL_FOR_ENQUIRY
(2406) ␆ SYMBOL_FOR_ACKNOWLEDGE
(2407) ␇ SYMBOL_FOR_BELL
(2408) ␈ SYMBOL_FOR_BACKSPACE
(2409) ␉ SYMBOL_FOR_HORIZONTAL_TABULATION
(240A) ␊ SYMBOL_FOR_LINE_FEED
(240B) ␋ SYMBOL_FOR_VERTICAL_TABULATION
(240C) ␌ SYMBOL_FOR_FORM_FEED
(240D) ␍ SYMBOL_FOR_CARRIAGE_RETURN
(240E) ␎ SYMBOL_FOR_SHIFT_OUT
(240F) ␏ SYMBOL_FOR_SHIFT_IN
(2410) ␐ SYMBOL_FOR_DATA_LINK_ESCAPE
(2411) ␑ SYMBOL_FOR_DEVICE_CONTROL_ONE
(2412) ␒ SYMBOL_FOR_DEVICE_CONTROL_TWO
(2413) ␓ SYMBOL_FOR_DEVICE_CONTROL_THREE
(2414) ␔ SYMBOL_FOR_DEVICE_CONTROL_FOUR
(2415) ␕ SYMBOL_FOR_NEGATIVE_ACKNOWLEDGE
(2416) ␖ SYMBOL_FOR_SYNCHRONOUS_IDLE
(2417) ␗ SYMBOL_FOR_END_OF_TRANSMISSION_BLOCK
(2418) ␘ SYMBOL_FOR_CANCEL
(2419) ␙ SYMBOL_FOR_END_OF_MEDIUM
(241A) ␚ SYMBOL_FOR_SUBSTITUTE
(241B) ␛ SYMBOL_FOR_ESCAPE
(241C) ␜ SYMBOL_FOR_FILE_SEPARATOR
(241D) ␝ SYMBOL_FOR_GROUP_SEPARATOR
(241E) ␞ SYMBOL_FOR_RECORD_SEPARATOR
(241F) ␟ SYMBOL_FOR_UNIT_SEPARATOR
(2420) ␠ SYMBOL_FOR_SPACE
(2421) ␡ SYMBOL_FOR_DELETE

|#

(defun pair-vector-to-hash-table (vec table)
  (loop :for (k . v) :across vec
     :do (setf (gethash k table) v))
  table)

(defparameter *control-char-graphics* nil)

;; This is, of course, ASCII (and therefore also LATIN1 and UTF-8) specific.
(defun make-control-char-graphic-table ()
  (setf *control-char-graphics* (make-hash-table :test #'eql))
  (loop :for c :from 0 :to 31
     :when (and (/= c (char-code #\tab)) (/= c (char-code #\newline)))
     :do (setf (gethash (code-char c) *control-char-graphics*)
	       (code-char (+ 64 c))))
  (setf (gethash (code-char 127) *control-char-graphics*) #\?))

;; There's no guarantee to have such characters in the font, and they might
;; be less readable anyway. But they're possibly more compact.
#|
(defun make-control-char-unicode-graphic-table ()
  (setf *control-char-graphics* (make-hash-table :test #'eql))
  (loop :for c :from 0 :to 31
     :when (and (/= c (char-code #\tab)) (/= c (char-code #\newline)))
     :do (setf (gethash *control-char-graphics* (code-char c))
	       (code-char (+ #x2400 c))))
  (setf (gethash *control-char-graphics* (code-char #\rubout))
	(code-char #x2421)))
|#

;; @@@ Perhaps this should so be somewhere else.
(defun control-char-graphic (c)
  (when (not *control-char-graphics*)
    (make-control-char-graphic-table))
  (gethash c *control-char-graphics*))

;; @@@ This simplify bullcrap is really only necessary because
;; of CLOS incompleteness.

(defgeneric simplify-char (char)
  (:documentation "Return a simplified version of the character.")
  (:method ((char character)) char))

(defgeneric simplify-string (string)
  (:documentation "Return a simplified version of the string."))

(defmethod simplify-string ((s array))
  "Return a simplified version of the string."
  (typecase s
    (string s)
    (t (map 'string #'simplify-char s))))

(defgeneric display-length (obj)
  (:documentation "Return how long the object should be when displayed."))

;; @@@ Perhaps this should so be somewhere else.
;; (defun double-wide-p (c)
;;   #+(and sbcl has-sb-unicode) (eq (sb-unicode:east-asian-width c) :w)
;;   #-(and sbcl has-sb-unicode) (declare (ignore c))
;;   #-(and sbcl has-sb-unicode) nil	; @@@ too hard without tables
;;   )

;; @@@ Only Just for debugging
(defmethod display-length ((c t))
  (cerror "No." "Display length fail ~s ~s" (type-of c) c))

;; XXX This is still wrong for unicode! @@@
(defmethod display-length ((c character))
  "Return the length of the character for display."
  (cond
    ((graphic-char-p c)
     (cond
       ((zero-width-char-p c) 0)
       ((combining-char-p c) 0)
       ((double-wide-char-p c) 2)
       ;; We could have an option to check if it's a CJK char and return 2,
       ;; which is what the "ambiguous width" settings on terminal emulators do.
       (t 1)))				; normal case
    ;;XXX @@@ The cases for tab and newline are wrong!
    ;; We should probably just return 1, since these have to be handled at
    ;; another level.
    ((eql c #\tab)
     8)
    ((eql c #\newline)
     0)
    (t
     (if (control-char-graphic c)
	 2   ; ^X
	 4)  ; \000
     )))

(defun grapheme-length (g)
  "Return the length of the character for display."
  (loop :for c :across g
     :sum (display-length c)))
#|     (cond
       ((graphic-char-p c)
	(cond
	  ((combining-char-p c) 0)
	  ((double-wide-char-p c) 2)
	  (t 1)))				;normal case
       ((eql c #\tab)
	8)					;XXX @@@ wrong!
       ((eql c #\newline)
	0)					;XXX @@@ wrong!
       (t
	(if (control-char-graphic c)
	    2   ; ^X
	    4)  ; \000
	)))) |#

(defmethod display-length ((s array))
  "Return the length of the string for display."
  (let ((sum 0))
    ;;(map nil #'(lambda (c) (incf sum (display-length c))) s)
    (map nil (_ (incf sum (grapheme-length _)))
	 (graphemes (simplify-string s)))
    sum))

;; @@@ This doesn't seem the most appropriate place to put this UTF-8 stuff.
;; Most implementations have something like this built in, but of course not
;; portably accessible. It seems stupid to have another (probably not as good)
;; copy of something that's already in every implementation, but it's either
;; that or dig out and wrap around whatever is the implementation. Perhaps I
;; should try to get the missing stuff added to a portable wrapper such as
;; cl-unicode? Also, the grapheme and normalization stuff are missing.

;; The regular kind, that throws a lot of errors, in case you want to make
;; sure your UTF-8 is valid.
(defmacro %get-utf8-char (byte-getter char-setter)
  (with-unique-names (u1 u2 u3 u4)
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
  (with-unique-names (bonk u1 u2 u3 u4 u5)
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
        (t (error "character code to big for UTF-8 #x~x" code))))

(defun length-in-utf8-bytes (code)
  (typecase code
    (character
     (%length-in-utf8-bytes (char-code code)))
    ((integer 0 #.char-code-limit)
     (%length-in-utf8-bytes code))
    (t
     (error "code #x~x isn't a character or an integer in range." code))))

(defmacro %put-utf8-char (char-getter byte-setter)
  (with-unique-names (code)
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
  (with-unique-names (code)
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

;; EOF
