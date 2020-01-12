;;
;; style.lisp - Functions for styled objects.
;;

(defpackage :style
  (:documentation "Functions for styled objects.")
  (:use :cl :theme :fatchar :opsys :grout)
  (:export
   #:spannify-style-item
   #:styled-string
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

(defun themed-string (theme-item string)
  "Return a string or a fat-string with the style from THEME-ITEM applied to it."
  (styled-string (and theme-item (theme-value *theme* theme-item)) string))

(defun styled-file-name (name &optional type)
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
       (styled-string style name))
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
