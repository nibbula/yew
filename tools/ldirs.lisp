;;
;; ldirs.lisp - List directories where things are loaded from.
;;

(defpackage :ldirs
  (:documentation "List directories where things are loaded from.")
  (:use :cl :dlib :opsys :lish :grout)
  (:export
   #:ldirs
   ))
(in-package :ldirs)

;; This isn't exactly right.
(defun load-dirs ()
  "Directories where things may get loaded from."
  (remove-if #'keywordp
	     (flatten (concatenate 'list
				   asdf:*central-registry* 
				   asdf:*source-registry-parameter*))))

(defun ldirs ()
  "List maybe some directories where things are loaded from. Missing directories
are printed in red on some terminals. Redundant prefixes are omitted."
  (with-grout ()
    (let (last-line)
      (flet ((pr (d)
	       "Print a line omitting prefixes from the last line."
	       (let* ((line (format nil "~a" d)) (i 0)
		      l1 l2)
		 (if last-line
		     (progn
		       (setf l1 (coerce (split-path last-line) 'vector)
		             l2 (coerce (split-path line) 'vector))
		       (loop :while (< i (min (length l1) (length l2)))
		       	  :do
		       	    (if (string= (aref l1 i) (aref l2 i))
		       		(dotimes (n (1+ (length (aref l1 i))))
				  (grout-princ #\space))
		       		(return nil))
		       	    (incf i))
		       ;; rest of the line
		       (loop :for n :from i :below (length l2)
			  :do
			    (grout-format "~a~a" (aref l2 n)
					  *directory-separator*))
		       (terpri))
		     (grout-format "~a~%" line))
		 (setf last-line line))))
	(loop :for d :in (load-dirs)
	   :do
	     (if (not (nos:file-exists d))
		 (progn
		   (grout-set-color :red :black) (pr d) (grout-set-normal))
		 (pr d)))))))

(defcommand ldirs ()
  "List maybe some directories where things are loaded from. Missing directories
are printed in red on some terminals. Redundant prefixes are omitted."
  (ldirs))

;; EOF
