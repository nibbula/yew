;;
;; package.lisp - Package definition for UNICODE.
;;

(defpackage :unicode
  (:documentation "Package definition for UNICODE.")
  (:use :cl :dlib)
  (:export
   ;; char-width
   #:char-grid-width
   ;; utf8
   #:get-utf8-char #:%get-utf8-char
   #:get-utf8b-char #:%get-utf8b-char
   #:length-in-utf8-bytes
   #:put-utf8-char #:%put-utf8-char
   #:string-to-utf8-bytes
   #:utf8-bytes-to-string
   ))
(in-package :unicode)

;; End
