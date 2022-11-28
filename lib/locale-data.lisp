;;;
;;; locale-data.lisp - Localization data.
;;;

(defpackage :locale-data
  (:documentation "Localization data.")
  (:use :cl :calendar :locale)
  (:export
   ))
(in-package :locale-data)

(defclass english-us (locale)
  ()
  (:default-initargs
   :language :en
   :region :us
   :time-format nil ;; @@@
   :calendar (make-instance 'gregorian))
  (:documentation "Locale for English in the US."))

(register-locale 'english-us)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Temporary data for EN

;;;;;;;;;;;;;
;; Months

(defmethod language-month-names ((calendar gregorian) (language (eql :en))
				 format context)
  (declare (ignore format context))
  #("January" "February" "March" "April""May" "June" "July" "August"
    "September" "October" "November" "December"))

(defmethod language-month-names ((calendar gregorian) (language (eql :en))
				 (format (eql :abbreviated))
				 (context (eql :format)))
  (declare (ignore format context))
  #("Jan" "Feb" "Mar" "Apr""May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defmethod language-month-names ((calendar gregorian) (language (eql :en))
				 (format (eql :narrow))
				 (context (eql :standalone)))
  (declare (ignore format context))
  #("J" "F" "M" "A" "M" "J" "J" "A" "S" "O" "N" "D"))

;;;;;;;;;;;;;
;; Weekdays

;; @@@ Note that the order is different than the Common Lisp weekday numbering
;; which starts at 0 = Monday.

(defmethod language-weekday-names ((calendar gregorian)
				   (language (eql :en))
				   format context)
  (declare (ignore format context))
  #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday""Saturday"))

(defmethod language-weekday-names ((calendar gregorian)
				   (language (eql :en))
				   (format (eql :abbreviated))
				   context)
  (declare (ignore format context))
  #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

(defmethod language-weekday-names ((calendar gregorian)
				   (language (eql :en))
				   (format (eql :short))
				   context)
  (declare (ignore format context))
  #("Su" "Mo" "Tu" "We" "Th" "Fr" "Sa"))


(defmethod language-weekday-names ((calendar gregorian)
				   (language (eql :en))
				   (format (eql :short))
				   (context (eql :standalone)))
  (declare (ignore format context))
  #("S" "M" "T" "W" "T" "F" "S"))

;; End
