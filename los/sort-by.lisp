;;;
;;; sort-by.lisp - Sort sequences.
;;;

(defpackage :sort-by
  (:documentation "Sort sequences.")
  (:use :cl :dlib :collections :table :grout)
  (:export
   #:sort-by
   #:!sort-by
   ))
(in-package :sort-by)

(defun sort-by (predicate sequence &key key)
  "Return the items from ‘sequence’ sorted by ‘predicate’.
‘predicate’ can be a function of two arguments, or a list which which is the
body of a function of the two arguments A and B."
;; @@@@
;;;If ‘sequence’ is a table or something that can be converted to a table,
;; and ‘predicate’ is a symbol or keyword do sort by column ???.
;; @@@@
  (let (key-func)
    (cond
      ;; Sort by table column name
      ((and key (typep sequence 'table)
	    (typep key '(or symbol string)))
       (dbugf :sort-by "table column key ~s~%" key)
       (let ((col-num (position key (table:table-columns sequence)
				:key #'table:column-name :test #'string-equal)))
	 (if col-num
	     (setf key-func (_ (oelt _ col-num)))
	     (error "The table doesn't have a column named ~s." key))))
      ;; Sort by structure slot name
      ((and key (typep (oelt sequence 0) '(or structure-object standard-object))
	    (typep key '(or symbol string)))
       (dbugf :sort-by "slot key ~s~%" key)
       (let* ((obj (oelt sequence 0))
	      (class-name (class-name (class-of obj))))
	 (setf key-func
	       (if (find (string key) (mop:class-slots (class-of obj))
			 :key (_ (string (mop:slot-definition-name _)))
			 :test #'string-equal)
		   (symbolify (s+ class-name "-" key)
			      :package (symbol-package class-name))
		   ;; hope it will work
		   key))))
      ((and key (consp key))
       (dbugf :sort-by "cons key ~s~%" key)
       (setf key-func (eval `(lambda (_) (declare (ignorable _)) ,key))))
      ;; Key should be something that can be used as a key arg to osort
      (t
       (dbugf :sort-by "other key ~s~%" key)
       (setf key-func key)))

    (dbugf :sort-by "key-func ~s ~s~%" (type-of key-func) key-func)
    (etypecase predicate
      (cons
       (dbugf :sort-by "List predicate ~s~%" predicate)
       (osort sequence (eval `(lambda (_)
				(declare (ignorable _))
				,predicate))
	      :key key-func))
      (function
       (dbugf :sort-by "function predicate ~s~%" predicate)
       (osort sequence predicate :key key-func))
      (symbol
       (dbugf :sort-by "symbol predicate ~s~%" predicate)
       ;; (if (fboundp predicate)
       ;; 	 (osort predicate sequence :key key-func)
       ;; 	 (error "Symbol predicate isn't a function."))))))
       (osort sequence predicate :key key-func)))))

;; End
