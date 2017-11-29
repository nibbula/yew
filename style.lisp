;;
;; style.lisp - Functions for styled objects.
;;

(defpackage :style
  (:documentation "Functions for styled objects.")
  (:use :cl :theme :fatchar :opsys)
  (:export
   #:spannify-style-item
   #:themed-string
   #:styled-file-name
   ))
(in-package :style)

(defun spannify-style-item (style item)
  "Convert STYLE into a span with ITEM in it."
  (labels ((flurp (n)
	     "A typical function that only Lisp addled brains find sensible."
	     (cond ((null (cdr n)) (car n))
		   ((atom n) n)
		   (t (list (car n) (flurp (cdr n)))))))
    (flurp (append style (list item)))))

(defun styled-string (style string)
  "Return a string or a fat-string with STYLE applied to it."
  (if style
      (span-to-fat-string (spannify-style-item style string))
      string))

(defun themed-string (theme-item string)
  "Return a string or a fat-string with the style from THEME-ITEM applied to it."
  (styled-string (and theme-item (theme-value *theme* theme-item)) string))

(defun styled-file-name (dir-entry)
  "Return a stylized string for a OPSYS:DIR-ENTRY."
  (let (style)
    (cond
      ((not *theme*)
       (dir-entry-name dir-entry))
      ((setf style (or (theme-value *theme*
				    (list :file :type
					  (dir-entry-type dir-entry) :style))
		       (theme-value *theme*
				    (list :file :suffix
					  (theme:file-suffix-type
					   (dir-entry-name dir-entry))
					  :style))))
       (styled-string style (dir-entry-name dir-entry)))
      (t
       (dir-entry-name dir-entry)))))

;; EOF
