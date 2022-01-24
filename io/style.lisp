;;;
;;; style.lisp - Functions for styled objects.
;;;

(defpackage :style
  (:documentation "Functions for styled objects.")
  (:use :cl :dlib :theme :fatchar :opsys :grout)
  (:export
   #:styled-string
   #:styled-span
   #:styled-char
   #:char-style
   #:themed-string
   #:styled-file-name
   #:show-styles
   ))
(in-package :style)

;; (defun spannify-style-item (style item)
;;   "Convert STYLE into a span with ITEM in it."
;;   (labels ((flurp (n)
;; 	     "A typical function that only Lisp addled brains find sensible."
;; 	     (cond ((null (cdr n)) (car n))
;; 		   ((atom n) n)
;; 		   (t (list (car n) (flurp (cdr n)))))))
;;     (flurp (append style (list item)))))

(defun styled-string (style string)
  "Return a string or a fat-string with STYLE applied to it."
  (if style
      ;; Don't really need this since span-to-* is fixed.
      ;;(span-to-fat-string (spannify-style-item style string))
      (span-to-fat-string (append style (list string)))
      string))

;; @@@ I'm not sure this is the best name?
(defun styled-span (style span)
  "Return SPAN enclosed in STYLE."
  (if style
      (append style (list span))
      span))

(defun styled-char (style char)
  "Apply style to CHAR, which can be character or a fatchar."
  (let* ((c (etypecase char
	      (character char)
	      (fatchar (fatchar-c char))))
	 (s (span-to-fatchar-string `(,@style ,c))))
    (typecase char
      (character (aref s 0))
      (fatchar (copy-fatchar-effects (aref s 0) char)))))

;; @@@ I feel like this maybe should be in fatchar.lisp.
;; So we could just call fatchar-string-to-span for one char, but that seems
;; overly expensive. Something like this could be factored out of
;; fatchar-string-to-span, but it doesn't seem obvious. Maybe someday we'll
;; overhaul all the span stuff.
(defun char-style (char)
  "Returnt the style of CHAR."
  (when (fatchar-p char)
    (let (result)
      (when (fatchar-fg char)
	(setf result (append (fatchar::span-start :fg char) result)))
      (when (fatchar-bg char)
	(setf result (append (fatchar::span-start :bg char) result)))
      (when (fatchar-attrs char)
	(setf result (append (fatchar::span-start :attr char) result)))
      (nreverse result))))

(defun themed-string (theme-item string &key (theme *theme*))
  "Return a string or a fat-string with the style from THEME-ITEM applied to it."
  (styled-string (and theme-item (theme-value theme theme-item)) string))

(defun styled-file-name (name &key type object)
  "Return a stylized string for a file NAME and TYPE. The TYPE values are from
OPSYS:DIR-ENTRY-TYPE or whatever. NAME can be a DIR-ENTRY in which the NAME
and TYPE are extracted from. If TYPE is passed in, it overrides a type in the
DIR-ENTRY."
  (when (dir-entry-p name)
    (setf type (or type (dir-entry-type name))
	  name (dir-entry-name name))) ; losing the original
  (let (style)
    (cond
      ((not *theme*)
       name)
      ((setf style (or (theme-value *theme*
				    (list :file :type type :style))
		       (theme-value *theme*
				    (list :file :suffix
					  (theme:file-suffix-type name)
					  :style))))
       (styled-string style
		      (if object
			  (styled-span `(:object ,object) name)
			  name)))
      (t
       name))))

(defun show-styles (&key (theme *theme*))
  "List all the styles in a THEME in each style."
  (with-grout ()
    (loop :for (name . style) :in (theme-list theme)
       :do
       ;; (tt-format "~s ---> ~s~%" name style)
       (when (and (listp name) (eq (car (last name)) :style)
		  (listp style))
	 (grout-span (append (car style)
				(string-downcase (prin1-to-string name))))
	 (grout-princ #\newline))))
  (values))

;; End
