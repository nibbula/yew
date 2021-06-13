;;;
;;; unicode-test.lisp - Tests for unicode package.
;;;

(defpackage :unicode-test
  (:documentation "Tests for unicode package.")
  (:use :cl :test :unicode)
  (:export
   #:run
   ))
(in-package :unicode-test)

(defun utf8b-transparent (bytes)
  "Return true if ‘bytes’ are encoded and decoded transparently."
  (and (equalp bytes (string-to-utf8b-bytes (utf8b-bytes-to-string bytes)))
       ;; Try inside ascii double quotes.
       (let ((quoted (concatenate 'vector #(#x22) bytes #(#x22))))
	 (equalp quoted
		 (string-to-utf8b-bytes (utf8b-bytes-to-string quoted))))))

(deftests (utf8b :doc "Test utf8b encoding and decoding.")
  "first byte continuation"
  (utf8b-transparent #(#x80))
  "last byte continuation"
  (utf8b-transparent #(#xbf))
  "2 continuation bytes"
  (utf8b-transparent #(#x80 #xbf))
  "3 continuation bytes"
  (utf8b-transparent #(#x80 #xbf #x80))
  "4 continuation bytes"
  (utf8b-transparent #(#x80 #xbf #x80 #xbf))
  "5 continuation bytes"
  (utf8b-transparent #(#x80 #xbf #x80 #xbf #x80))
  "6 continuation bytes"
  (utf8b-transparent #(#x80 #xbf #x80 #xbf #x80 #xbf))
  "7 continuation bytes"
  (utf8b-transparent #(#x80 #xbf #x80 #xbf #x80 #xbf #x80))
  "all continuation bytes"
  (utf8b-transparent #(#x80 #x81 #x82 #x83 #x84 #x85 #x86 #x87 #x88 #x89 #x8A
  #x8B #x8C #x8D #x8E #x8F #x90 #x91 #x92 #x93 #x94 #x95 #x96 #x97 #x98 #x99
  #x9A #x9B #x9C #x9D #x9E #x9F #xA0 #xA1 #xA2 #xA3 #xA4 #xA5 #xA6 #xA7 #xA8
  #xA9 #xAA #xAB #xAC #xAD #xAE #xAF #xB0 #xB1 #xB2 #xB3 #xB4 #xB5 #xB6 #xB7
  #xB8 #xB9 #xBA #xBB #xBC #xBD #xBE #xBF))
  "lonely start characters 2-byte"
  (utf8b-transparent #(#xC0 #x20 #xC1 #x20 #xC2 #x20 #xC3 #x20 #xC4 #x20 #xC5
  #x20 #xC6 #x20 #xC7 #x20 #xC8 #x20 #xC9 #x20 #xCA #x20 #xCB #x20 #xCC #x20
  #xCD #x20 #xCE #x20 #xCF #xD0 #x20 #xD1 #x20 #xD2 #x20 #xD3 #x20 #xD4 #x20
  #xD5 #x20 #xD6 #x20 #xD7 #x20 #xD8 #x20 #xD9 #x20 #xDA #x20 #xDB #x20 #xDC
  #x20 #xDD #x20 #xDE #x20 #xDF #x20))
  "lonely start characters 3-byte"
  (utf8b-transparent #(#x0A #x20 #x20 #x20 #x22 #xE0 #x20 #xE1 #x20 #xE2 #x20
  #xE3 #x20 #xE4 #x20 #xE5 #x20 #xE6 #x20 #xE7 #x20 #xE8 #x20 #xE9 #x20 #xEA
  #x20 #xEB #x20 #xEC #x20 #xED #x20 #xEE #x20 #xEF #x20))
  "lonely start characters 4-byte"
  (utf8b-transparent #(#xF0 #x20 #xF1 #x20 #xF2 #x20 #xF3 #x20 #xF4 #x20 #xF5
		       #x20 #xF6 #x20 #xF7 #x20))
  "lonely start characters 5-byte"
  (utf8b-transparent #(#xF8 #x20 #xF9 #x20 #xFA #x20 #xFB #x20))
  "lonely start characters 6-byte"
  (utf8b-transparent #(#xFC #x20 #xFD #x20))
  "2-byte sequence with the last byte missing"
  (utf8b-transparent #(#xC0))
  "3-byte sequence with the last byte missing"
  (utf8b-transparent #(#xE0 #x80))
  "3-byte sequence with the last byte missing surrounded"
  (utf8b-transparent #(#x22 #xE0 #x80 #x22))
  "4-byte sequence with the last byte missing"
  (utf8b-transparent #(#xF0 #x80 #x80))
  "5-byte sequence with the last byte missing"
  (utf8b-transparent #(#xF8 #x80 #x80 #x80))
  "6-byte sequence with the last byte missing"
  (utf8b-transparent #(#xFC #x80 #x80 #x80 #x80))

  "2-byte sequence with the last byte missing"
  (utf8b-transparent #(#xDF))
  "3-byte sequence with the last byte missing"
  (utf8b-transparent #(#xEF #xBF))
  "3-byte sequence with the last byte missing surrounded"
  (utf8b-transparent #(#x22 #xEF #xBF #x22))
  "4-byte sequence with the last byte missing"
  (utf8b-transparent #(#xF7 #xBF #xBF))
  "5-byte sequence with the last byte missing"
  (utf8b-transparent #(#xFB #xBF #xBF #xBF))
  "6-byte sequence with the last byte missing"
  (utf8b-transparent #(#xFD #xBF #xBF #xBF #xBF))

  "impossible bytes"
  (utf8b-transparent #(#xfe))
  (utf8b-transparent #(#xff))
  (utf8b-transparent #(#xfe #xfe #xff #xff))

  "overlong sequences"
  (utf8b-transparent #(#xc0 #xaf))
  (utf8b-transparent #(#xe0 #x80 #xaf))
  (utf8b-transparent #(#xf0 #x80 #x80 #xaf))
  (utf8b-transparent #(#xf8 #x80 #x80 #x80 #xaf))
  (utf8b-transparent #(#xfc #x80 #x80 #x80 #x80 #xaf))

  "maximum overlong sequences"
  (utf8b-transparent #(#xc1 #xbf))
  (utf8b-transparent #(#xe0 #x9f #xbf))
  (utf8b-transparent #(#xf0 #x8f #xbf #xbf))
  (utf8b-transparent #(#xf8 #x87 #xbf #xbf #xaf))
  (utf8b-transparent #(#xfc #x83 #xbf #xbf #xbf #xbf))

  "overlong sequences with nul"
  (utf8b-transparent #(#xc0 #x80))
  (utf8b-transparent #(#xe0 #x80 #x80))
  (utf8b-transparent #(#xf0 #x80 #x80 #x80))
  (utf8b-transparent #(#xf8 #x80 #x80 #x80 #x80))
  (utf8b-transparent #(#xfc #x80 #x80 #x80 #x80 #x80))

  "single utf-16 surrogates"
  (utf8b-transparent #(#xed #xa0 #x80))
  (utf8b-transparent #(#xed #xad #xbf))
  (utf8b-transparent #(#xed #xae #x80))
  (utf8b-transparent #(#xed #xaf #xbf))
  (utf8b-transparent #(#xed #xb0 #x80))
  (utf8b-transparent #(#xed #xbe #x80))
  (utf8b-transparent #(#xed #xbf #xbf))

  "paired utf-16 surrogates"
  (utf8b-transparent #(#xed #xa0 #x80 #xed #xb0 #x80))
  (utf8b-transparent #(#xed #xa0 #x80 #xed #xbf #xbf))
  (utf8b-transparent #(#xed #xad #xbf #xed #xb0 #x80))
  (utf8b-transparent #(#xed #xad #xbf #xed #xbf #xbf))
  (utf8b-transparent #(#xed #xae #x80 #xed #xb0 #x80))
  (utf8b-transparent #(#xed #xae #x80 #xed #xbf #xbf))
  (utf8b-transparent #(#xed #xaf #xbf #xed #xb0 #x80))
  (utf8b-transparent #(#xed #xaf #xbf #xed #xbf #xbf))

  "non-characters"
  (utf8b-transparent #(#xef #xbf #xbe))
  (utf8b-transparent #(#xef #xbf #xbf))

  "noncharacters fdd8 - fdef"
  (utf8b-transparent #(#xEF #xB7 #x90 #xEF #xB7 #x91 #xEF #xB7 #x92 #xEF #xB7
 #x93 #xEF #xB7 #x94 #xEF #xB7 #x95 #xEF #xB7 #x96 #xEF #xB7 #x97 #xEF #xB7
 #x98 #xEF #xB7 #x99 #xEF #xB7 #x9A #xEF #xB7 #x9B #xEF #xB7 #x9C #xEF #xB7
 #x9D #xEF #xB7 #x9E #xEF #xB7 #x9F #xEF #xB7 #xA0 #xEF #xB7 #xA1 #xEF #xB7
 #xA2 #xEF #xB7 #xA3 #xEF #xB7 #xA4 #xEF #xB7 #xA5 #xEF #xB7 #xA6 #xEF #xB7
 #xA7 #xEF #xB7 #xA8 #xEF #xB7 #xA9 #xEF #xB7 #xAA #xEF #xB7 #xAB #xEF #xB7
 #xAC #xEF #xB7 #xAD #xEF #xB7 #xAE #xEF #xB7 #xAF))

  "noncharacters 1fffe - affff"
  (utf8b-transparent #(
  #xF0 #x9F #xBF #xBE #xF0 #x9F #xBF #xBF #xF0 #xAF #xBF
  #xBE #xF0 #xAF #xBF #xBF #xF0 #xBF #xBF #xBE #xF0 #xBF #xBF #xBF #xF1 #x8F
  #xBF #xBE #xF1 #x8F #xBF #xBF #xF1 #x9F #xBF #xBE #xF1 #x9F #xBF #xBF #xF1
  #xAF #xBF #xBE #xF1 #xAF #xBF #xBF #xF1 #xBF #xBF #xBE #xF1 #xBF #xBF #xBF
  #xF2 #x8F #xBF #xBE #xF2 #x8F #xBF #xBF #x20 #x20 #x20 #x20 #x20 #x20 #x20
  #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20
  #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x7C
  #x0A #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 #xF2 #x9F #xBF #xBE #xF2 #x9F
  #xBF #xBF #xF2 #xAF #xBF #xBE #xF2 #xAF #xBF #xBF #xF2 #xBF #xBF #xBE #xF2
  #xBF #xBF #xBF #xF3 #x8F #xBF #xBE #xF3 #x8F #xBF #xBF #xF3 #x9F #xBF #xBE
  #xF3 #x9F #xBF #xBF #xF3 #xAF #xBF #xBE #xF3 #xAF #xBF #xBF #xF3 #xBF #xBF
  #xBE #xF3 #xBF #xBF #xBF #xF4 #x8F #xBF #xBE #xF4 #x8F #xBF #xBF)))

(deftests (unicode-all :doc "All test for unicode.")
  "foo"
  utf8b
  )

(defun run ()
  (run-group-name 'unicode-all :verbose t))

;; End
