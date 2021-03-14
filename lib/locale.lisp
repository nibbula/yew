;;;
;;; locale.lisp - Localization
;;;

(defpackage :locale
  (:documentation "Localization")
  (:use :cl :dlib :opsys)
  (:export
   #:locale
   #:locale-language #:locale-region #:locale-time-format #:locale-calendar
   #:ensure-locale
   #:register-locale
   #:find-locale
   #:language-for-time
   #:language-month-names
   #:language-weekday-names
   ))
(in-package :locale)

;;; @@@ This should probably represent coarse user choices. The detailed data
;; can be loaded and put in the theme.
(defclass locale ()
  ((language
    :initarg :language :accessor locale-language
    :documentation "Language code.")
   (region
    :initarg :region :accessor locale-region
    :documentation "Region settings code.")
   (time-format
    :initarg :time-format :accessor locale-time-format
    :documentation "Time format settings code.")
   (calendar
    :initarg :calendar :accessor locale-calendar
    :documentation "Calendar to use.")
   ;; @@@ units? temperature units? monetary?
   )
  (:documentation "Generic user localizatin settings."))

(defvar *locale* nil
  "The current locale.")

(defvar *locales* nil
  "The list of regisetered locales.")

(defun register-locale (locale)
  "Add a locale to the locale register."
  (pushnew locale *locales*))

(defun decompose-locale-string (string)
  "Take apart a locale string like “fr_CA.UTF-8” into language, region, and
encoding, and return it as a plist."
  (let ((dot (position #\. string :from-end t))
	(under (position #\_ string))
	(lang string)
	region encoding result)
    (when dot
      (setf encoding (subseq string dot)
	    lang (subseq string 0 dot)))
    (when under
      (setf lang (subseq string 0 under)
	    region (subseq lang under)))
    (setf result (list :language lang))
    (when region
      (setf result (nconc result (list :region region))))
    (when encoding
      (setf result (nconc result (list :encoding region))))))

(defun find-locale (&key language region)
  "Find a locale with the given qualities."
  (find-if (lambda (x)
	     (let ((c (find-class x)))
	       (and c (and language
			   (equal language
				  (getf (mop:class-default-initargs c)
					:language)))
		    (and region
			   (equal region
				  (getf (mop:class-default-initargs c)
					:region))))
	       x))
	   *locales*))

(defun ensure-locale ()
  "Make sure there's a current locale."
  (or *locale*
      (let ((jink (decompose-locale-string (or (nos:setlocale :messages)
					       (nos:setlocale :time)
					       (nos:setlocale :numeric)))))
	;; @@@ We should just load the thing to the theme.
	(asdf:load-system :locale-data)
	(setf *locale*
	      (make-instance (find-locale
			      :language    (getf jink :language)
			      :region      (getf jink :region)))))))

;; @@@ get rid of this?
(defun language-for-time ()
  "Return the language to use for dates and time."
  (ensure-locale)
  (locale-language *locale*))

(defgeneric language-month-names (calendar language format context)
  (:documentation "Return a collection of month names."))

(defgeneric language-weekday-names (calendar language format context)
  (:documentation "Return a collection of month names."))

(defgeneric locale-decimal-point (locale)
  (:documentation
   "Return a character which should be used to separate integer and fractional
part of decimal number."))

(defgeneric locale-thousands-seperator (locale)
  (:documentation
   "Return a character which should be used to inserted before the thousand
places in numbers."))

;; End
