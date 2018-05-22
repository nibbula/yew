;;
;; man.lisp - Show a manual entry.
;;

;; Note that this just uses an already existing system "man" command.
;; Otherwise we would have to implement a lot of crap, like groff.
;; This is just so we can have our own command with completion and
;; use our pager.

(defpackage :man
  (:documentation "Show a manual entry.")
  (:use :cl :dlib :dlib-misc :opsys :glob :grout :lish :pager)
  (:export
   #:*man-sections*
   #:get-manual-sections
   #:arg-manual-section
   #:*man-entries*
   #:get-manual-entries
   #:arg-manual-entry
   #:!man
   ))
(in-package :man)

(defvar *man-sections* nil
  "Cached manual sections. Set it to NIL again to recalculate them.")

;; @@@ resolve vs. lish::manpath
(defun manpath ()
  "Return the manual path."
  (or (nos:environment-variable "MANPATH")
      (!$ "manpath")
      (error "Can't figure out the where the manuals are.")))

(defun get-manual-sections ()
  "Approximate and flawed method for finding manual sections."
  (or *man-sections*
      (setf *man-sections*
	    (remove-duplicates
	     (loop :for d :in (split-sequence #\: (manpath))
		;; :do (format t "  ~a " d) (finish-output)
		:append
		(mapcar (_ (remove-prefix (nos:basename _) "man"))
			(glob (s+ d "/man*/"))))
	     :test #'equal))))

(defclass arg-manual-section (arg-lenient-choice)
  ()
  (:default-initargs
   :choice-func #'get-manual-sections)
  (:documentation "Manual section."))

(defparameter *suffixes* #(".gz" ".bz2" ".z" ".Z" ".F" ".Y" ".bz" ".xz" ".7z"))

(defun remove-man-page-suffixes (name)
  "Desperately try to get rid of man page suffixes on NAME."
  (let ((c (count #\. name)) matches)
    (case c
      (0 name)
      (1 (subseq name 0 (position #\. name)))
      (t
       (let ((result name))
	 (loop :for s :across *suffixes* :do
	    (when (ends-with s name)
	      (setf result (remove-suffix name s))))
	 (loop :for s :in (get-manual-sections) :do
	    (when (ends-with (s+ #\. s) result)
	      (setf result (remove-suffix result (s+ #\. s)))))
	 (when (setf matches (ppcre:all-matches "\\.[1-9][^.]*$" result))
	   (setf result (subseq result 0 (car matches))))
	 result)))))

(defvar *man-entries* nil
  "Cached manual entries. Set it to NIL again to recalculate them.")

(defun get-manual-entries ()
  "Approximate and flawed method for finding manual entries."
  (or *man-entries*
      (progn
	;; (format t "Getting manual entries...~%") (finish-output)
	(with-spin ()
	  (setf *man-entries*
		(loop :for d :in (split-sequence #\: (manpath))
		   ;; :do (format t "  ~s~%" d) (finish-output)
		   :append
		   (loop :with result
		      :for s :in (get-manual-sections)
		      :do
		      ;;(format t "   ~s ~%" s) (finish-output)
		      (spin)
		      (setf result
			    (loop :for ff :in
			       (glob (s+ d *directory-separator* "man"
					 s *directory-separator* "*"))
			       ;;:do (format t "  --> ~s~%" ff)
			       :collect
			       (remove-man-page-suffixes (basename ff))))
		      :append result)
		   ;; :do (terpri)
		   )))
	;;(format t "  Sorting...~%") (finish-output)
	(setf *man-entries* (sort *man-entries* #'string-lessp))
	;;(format t "  De-duping...~%") (finish-output)
	(setf *man-entries* (remove-duplicates *man-entries* :test #'equal))
	*man-entries*)))

(defclass arg-manual-entry (arg-lenient-choice)
  ()
  (:default-initargs
   :test #'arg-choice-compare
   :choice-func #'get-manual-entries)
  (:documentation "Manual entry."))

;; @@@ This needs work
(defcommand man
   ((apropos string  :short-arg #\k :help "Search page titles for a string.")
    (section manual-section  :short-arg #\s :help "Show page from section.")
    (all     boolean :short-arg #\a :help "Show all matching pages.")
    (rehash  boolean :short-arg #\r :help "Re-calculate caches.")
    (path    string  :short-arg #\M
	       :help "Colon separated path to search for pages.")
    (entry   manual-entry  :optional t :help "Name of manual entry."))
  "Display a manual page for a command. With -k search for a matching page."
  (when rehash
    (setf *man-sections* nil
	  *man-entries* nil
	  *man-entries* (get-manual-entries))
    (when (not (or entry apropos))
      (return-from !man (values))))
  (let* ((base "/usr/bin/man -P cat ")
	 (cmd (make-array (list (length base))
			  :element-type 'character
			  :adjustable t
			  :fill-pointer t))
	 (cols (princ-to-string (with-grout () (grout-width))))
	 stream)
    #-(or linux freebsd) (declare (ignorable cols))
    (setf (fill-pointer cmd) 0)
    (with-output-to-string (str cmd)
      #+(or linux freebsd)
      ;; (format str "env MANWIDTH=~d MAN_KEEP_FORMATTING=t ~a " cols base)
      (progn
	(setf (nos:environment-variable "MANWIDTH") cols
	      (nos:environment-variable "MAN_KEEP_FORMATTING") "t"
	      )
	(princ base str))
      #-(or linux freebsd) (princ base str)
      ;;#+linux (princ "-Tutf8 " str) ; force fancy text
      (when apropos (format str "-k ~a " apropos))
      (when all     (format str "-a "))
      (when section (format str "~a " section))
      (when path    (format str "-M ~a " path))
      ;; (format t "cmd = ~s entry = ~s~%" cmd entry)
      ;; (read-line)
      (unwind-protect
	(progn
	  (setf stream (!! cmd (or entry "")))
	  (pager:pager stream))
	(when stream
	  (close stream))))))

(defcommand crap
    (("crap" manual-entry :help "Name of the manual entry."))
  "Very crap"
  (let* ((cols (princ-to-string (with-grout () (grout-width))))
	 stream)
    ;;(setf (nos:environment-variable "COLUMNS") cols)
    #+(or linux freebsd) (setf (nos:environment-variable "MANWIDTH") cols)
    (unwind-protect
	 (progn
	   (setf stream (!! "/usr/bin/man -P cat " (or crap "")))
	   (pager:pager stream))
      (when stream
	(close stream)))))

;; EOF
