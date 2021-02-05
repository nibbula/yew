;;;
;;; chart.lisp - Draw charts.
;;;

(defpackage :chart
  (:documentation "Draw charts.")
  (:use :cl :dlib :collections :char-util :table :terminal :keymap :inator
	:terminal-inator)
  (:export
   #:chart
   #:bar-chart
   #:horizontal-bar
   #:vertical-bar
   #:do-bars
   #:draw-chart
   #:make-chart-from-table
   #:chart-viewer
   #:view-chart
   #:!view-chart
   ))
(in-package :chart)

(defclass chart ()
  ((title
    :initarg :title :accessor chart-title :initform ""
    :documentation "The title of the chart."))
  (:documentation "A generic chart."))

(defclass bar-chart (chart)
  ((labels :initarg :labels :accessor chart-labels :initform nil
    :documentation "Sequence of labels for the bars.")
   (values
    :initarg :values :accessor chart-values :initform nil
    :documentation "Sequence of values.")
   (bar-fill
    :initarg :bar-fill :accessor bar-fill
    :documentation "A thing to fill the bar with.")
   (vertical-separator
    :initarg :vertical-separator :accessor vertical-separator
    :documentation "A thing to separate the labels.")
   (horizontal-separator
    :initarg :horizontal-separator :accessor horizontal-separator
    :documentation "A thing to separate the labels."))
  (:documentation "A bar chart."))

(defclass horizontal-bar (bar-chart)
  ()
  (:default-initargs
   :vertical-separator "─"
   :horizontal-separator " │ "
   :bar-fill #\▒)
  (:documentation "An horizontal bar chart."))

(defclass vertical-bar (bar-chart)
  ()
  (:default-initargs
   :vertical-separator #\─
   :horizontal-separator " │ "
   :bar-fill #\▒)
  (:documentation "A vertical bar chart."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro do-bars ((label-var value-var chart) &body body)
    (with-names (c label-iter value-iter)
      `(progn
	 (loop :with ,c = ,chart
	    :with ,label-var
	    :and ,value-var
	    :and ,label-iter = (oiterator (chart-labels ,c))
	    :and ,value-iter = (oiterator (chart-values ,c))
	    :while (not (and (oend-p ,label-iter) (oend-p ,value-iter)))
	    :do
	      (setf ,label-var (oiter-ref ,label-iter)
		    ,value-var (oiter-ref ,value-iter))
	      ,@body
	      (when (not (oend-p ,label-iter))
		(oincr ,label-iter))
	      (when (not (oend-p ,value-iter))
		(oincr ,value-iter)))))))

;; @@@ Specialize for output device? This is only for terminals.
(defmethod draw-chart ((chart horizontal-bar))
  (with-slots (labels values bar-fill horizontal-separator) chart
    (let* ((labels-width (loop :for l :in labels :maximize (display-length l)))
	   (max-value    (loop :for v :in values :maximize v))
	   (width (- (tt-width) (display-length horizontal-separator)
		     labels-width)))
      (do-bars (label value chart)
	(tt-format "~v@a" labels-width (or label ""))
	(tt-write-string horizontal-separator)
	(tt-format "~v,,,va"
		   (round (* value width) max-value) bar-fill bar-fill)
	(tt-fresh-line)))))

(defmethod draw-chart ((chart vertical-bar))
  (with-slots (labels values bar-fill horizontal-separator vertical-separator)
      chart
    (let* ((max-value    (loop :for v :in values :maximize v))
	   (height (- (tt-height) 2))
	   (x 0))
      (do-bars (label value chart)
	(loop :for y :from height
	   :downto (- height (round (* value height) max-value))
	   :do
	     (tt-move-to y x)
	     (tt-write-char bar-fill))
	(incf x))
      (tt-move-to (- (tt-height) 2) 0)
      (tt-format "~v,,,va" (1- (tt-width))
		 vertical-separator vertical-separator))))

(defun make-chart-from-table (type table &key (label-column 0) (value-column 1))
  "Make a chart of TYPE from TABLE. Get the labels from LABEL-COLUMN or the
first column, and the values from VALUE-COLUMN or the second column. The columns
can be specified as column names or numbers."
  (flet ((get-col (col default)
	   (if (numberp col)
	       col
	       (or (table-column-number col table :test #'equalp) default))))
    (let ((label-col (get-col (or label-column "label") 0))
	  (value-col (get-col (or value-column "value") 1))
	  labels values)
      (omap (lambda (row)
	      (push (oelt row label-col) labels)
	      (push (oelt row value-col) values))
	    table)
      (setf labels (nreverse labels)
	    values (nreverse values))
      (make-instance type :labels labels :values values))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chart viewer

(defvar *chart-viewer* nil
  "The current chart viewer.")

(defkeymap *chart-viewer-keymap* ()
  `((#\q		. quit)
    (#\?		. help)
    (#\s		. sort-chart)
    (#\i		. record-info)
    (:home		. move-to-top)
    (:end		. move-to-bottom)
    (:up		. previous)
    (:down		. next)
    (:right		. scroll-right)
    (:left		. scroll-left)
    ;;(,(meta-char #\i)	. table-info)
    ;; (#\escape		. *chart-viewer-escape-keymap*)
    ))

(defclass chart-viewer (terminal-inator)
  ((chart
    :initarg :chart :accessor chart-viewer-chart :initform nil
    :documentation "The chart to view."))
  (:default-initargs
   :keymap `(,*chart-viewer-keymap* ,*default-inator-keymap*))
  (:documentation "View a chart."))

(defmethod update-display ((o chart-viewer))
  (tt-move-to 0 0)
  (tt-erase-below)
  (draw-chart (chart-viewer-chart o)))

;; (defgeneric sort-chart (inator)
;;   (:documentation "Sort the chart.")
;;   (:method ((i chart-viewer))
;;     (osort (chart-viewer)))

(defun view-chart (chart)
  "View a chart."
  (with-terminal-inator (*chart-viewer* 'chart-viewer :chart chart)
    (event-loop *chart-viewer*)))

(defparameter *chart-type-names* '(horizontal-bar vertical-bar)
  "The different chart types.")

#+lish
(lish:defcommand view-chart
  ((chart object :optional t
    :help "The chart to draw or a table to make it from.")
   (type choice :short-arg #\t :default "horizontal-bar"
    :choices *chart-type-names*
    :help "The type of chart."))
  "Draw a chart."
  (when (not chart)
    (when (not lish:*input*)
      (error "View what chart?"))
    (setf chart lish:*input*))
  (let ((cc
	 (typecase chart
	   (null lish:*input*)
	   (table (make-chart-from-table
		   (symbolify (string type) :package (find-package :chart))
		   chart))
	   (chart chart)
	   (t (error "I don't know how to view a ~s.~%" (type-of chart))))))
    (view-chart cc)))

;; End
