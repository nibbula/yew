;;;
;;; package.lisp - Package definition for UNICODE.
;;;

(defpackage :unicode
  (:documentation "Package definition for UNICODE.")
  (:use :cl :dlib)
  (:export
   ;; char-width
   #:char-grid-width
   ;; utf8
   #:get-utf8-char #:%get-utf8-char
   #:length-in-utf8-bytes
   #:put-utf8-char #:%put-utf8-char
   #:string-to-utf8-bytes
   #:utf8-bytes-to-string
   ;; utf8b
   #:get-utf8b-char #:%get-utf8b-char
   #:length-in-utf8b-bytes
   #:put-utf8b-char #:%put-utf8b-char
   #:string-to-utf8b-bytes
   #:utf8b-bytes-to-string
   ;; Generic / compatibility
   #:list-character-encodings
   #:string-to-octets
   #:octets-to-string
   #:string-size-in-octets
   #:vector-size-in-chars
   ;; encoding construction
   #:define-string-converters
   ))
(in-package :unicode)

;; End
