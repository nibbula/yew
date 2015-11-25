;;
;; char-util.lisp - General utility functions dealing with characters.
;;

(defpackage :char-util
  (:documentation "Utility functions for characters.")
  (:use :cl)
  (:export
   #:meta-char
   #:ctrl
   #:meta-char-p
   #:un-meta
   #:nice-char
   #:char-as-ascii
   #:displayable-char
   ))
(in-package :char-util)

(defun meta-char (c)
  "Turn the meta (8th) bit on in the code representation of the
   given character."
  (code-char (logior (ash 1 7) (char-code c))))

;(

;; Sadly #\^A is not portable. This assumes ASCII or UTF8 or something. 
(defun ctrl (c)
  "Return the control character corresponding to the normal character."
  (code-char (1+ (- (char-code (char-upcase c)) (char-code #\A)))))

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

(defun displayable-char (c &key (caret t) all-control (show-meta t))
  "Make non-graphic characters visible."
  (let ((cc (if (characterp c) (char-code c) nil)))
    (cond
      ((and cc (not all-control) (or (eql c #\tab) (eql c #\newline)))
       (princ-to-string c))
      ((and cc show-meta (meta-char-p cc))
       (format nil "M-~a" (nice-char (un-meta cc))))
      ((and cc (< cc (char-code #\space)))
       (format nil "~:[C-~(~c~)~;^~c~]" caret
	       (code-char (+ cc (char-code #\@)))))
      ((and cc (= cc 127))
       (format nil (if caret "^?" "C-?")))
      (cc (princ-to-string c))
      (t (format nil "~s" c)))))

(defun normalize-string (string &optional (form :nfd))
  "Return a Unicode normalized string based on STRING. FORM can be one of
:NFD, :NFC, :NFKD, or :NFKC."
  #+sbcl (normalize-string string form)
  #-(or sbcl) (error "Missing implementation: normalize-string"))

;; EOF
