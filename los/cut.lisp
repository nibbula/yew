;;
;; cut.lisp - Cut pieces from lines.
;;

(defpackage :cut
  (:documentation "Cut pieces from lines.")
  (:use :cl :dlib :lish :stretchy :char-util :unicode :table)
  (:export
   #:cut-lines
   #:!cut
   ))
(in-package :cut)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

(defun overlap-p (a b)
  "Return true if A an B overlap, given that A < B."
  (destructuring-bind ((start-a . end-a) (start-b . end-b)) (list a b)
    (declare (ignore end-b))
    (or (and end-a (or (eq end-a :max) (<= start-b end-a)))
	(= start-a start-b))
    #| that's all since they're sorted |#))

(defun join-pair (a b)
  "Return a pair that is the maximizes the size of A and B."
  (destructuring-bind ((start-a . end-a) (start-b . end-b)) (list a b)
    (cons (min start-a start-b)
	  (if (or (and end-a (eq end-a :max))
		  (and end-b (eq end-b :max)))
	      :max
	      (cond
		((not end-a) end-b)
		((not end-b) end-a)
		(t (max end-a end-b)))))))

(defun coalesce (ranges)
  "Return the ranges with overlapping ranges joined."
  (let ((result ranges))
    (loop
       :with range = result
       :until (null (cdr range))
       :if (overlap-p (car range) (cadr range)) :do
	 (setf (car range) (join-pair (car range) (cadr range))
	       (cdr range) (cddr range)) ;; delete the merged pair
       :else :do
	 (setf range (cdr range)))
    result))

(defun get-regions (string)
  "Return a list of regions, which are (start . end) pairs. End can be NIL to
indicate only a start number, or :max to indicate it extends to the end."
  (let ((ranges (split-sequence #\, string :omit-empty t))
	result)
    (loop :with start :and end :and n :and pos
       :for r :in ranges
       :do
	 (setf start 1 end nil pos 0)
	 ;; Possibly get the start number, or it defaults to 1.
	 (when (char/= (char r 0) #\-)
	   (setf (values n pos) (parse-integer r :junk-allowed t))
	   (when (not n)
	     (error "Malformed range: ~s." string))
	   (setf start n))
	 ;; Possibly get the end number.
	 (cond
	   ((= pos (1- (length r))) ;; Only one more char.
	    ;; It must be a trailing dash.
	    (when (char/= (char r pos) #\-)
	      (error "Malformed range: ~s." string))
	    ;; Which indicates the maximum.
	    (setf end :max))
	   ((< pos (1- (length r))) ;; at least 2 more chars
	    ;; must be a dash
	    (when (char/= (char r pos) #\-)
	      (error "Malformed range: ~s." string))
	    (setf (values n pos) (parse-integer r :junk-allowed t
						:start (1+ pos)))
	    (when (not n)
	      (error "Malformed range: ~s." string))
	    (setf end n)))
	 ;; flip ranges
	 (when (and end (not (eq end :max)) (< end start))
	   (warn "FTFY: flipped the decreasing range ~a-~a" start end)
	   (rotatef start end))
	 (push (cons start end) result))
    ;; Sort ranges in ascending order, and coalesce.
    (coalesce (sort-muffled result #'< :key #'car))))

;; @@@ Perhaps the args the be after get-regions is done?
(defun cut-lines (stream &key bytes characters fields delimiter output-delimiter
			   only-delimited collect re-delimiter quiet)
  "Write lines from stream, while cutting, bytes, characters, or fields."
  (when (> (+ (if bytes 1 0) (if characters 1 0) (if fields 1 0)) 1)
    (error "Only one of bytes, characters, or fields can be specified."))
  (when (not output-delimiter)
    (setf output-delimiter (string delimiter)))
  (when bytes
    (setf output-delimiter (string-to-utf8-bytes output-delimiter)))
  (with-open-file-or-stream (str stream
				 :element-type
				 (if bytes '(unsigned-byte 8) 'character))
    (let ((regions (get-regions (or bytes characters fields)))
	  (line-vec (when bytes (make-stretchy-vector 20)))
	  (newline-code (char-code #\newline))
	  snip read-a-line write-newline write-a-string
	  start end result)
      (labels ((fix-char-region (s e len)
		 "Make start and end reflect the real region for LEN."
		 ;; (setf start (clamp (1- s) 0 (max 0 (1- len)))
		 (setf start (max 0 (1- s))
		       end   (cond
			       ((null e) (1+ start))
			       ((eq e :max) len)
			       (t (clamp e 0 len)))))
	       (fix-region (s e len)
		 "Make start and end reflect the real region for LEN."
		 (setf start (max (1- s) 0)
		       end   (cond
			       ((null e) start)
			       ((eq e :max) (1- len))
			       (t (max (1- e) 0)))))
	       (cut-regions (line)
		 "Write the regions of the line."
		 ;;(dbugf :cut "regions ~s~%" regions)
		 (let ((first t) data)
		   (loop :for (s . e) :in regions
		      :do
			(fix-char-region s e (length line))
			(when (not first)
			  (funcall write-a-string output-delimiter))
			(setf first nil)
		      ;; (format t "(~s . ~s) start ~s end ~s~%" s e start end)
			(when (< start (length line))
			  (funcall write-a-string (subseq line start end))
			  (when collect
			    (push (subseq line start end) data))))
		   (funcall write-newline)
		   (when collect
		     (push (nreverse data) result))))
	       (print-fields (line)
		 "Print the fields."
		 ;;(dbugf :cut "delimiter ~s line ~s~%" delimiter line)
		 (let* ((fs (if re-delimiter
				(ppcre:split re-delimiter line)
				(split-sequence delimiter line)))
			(len (length fs))
			(fn regions)
			(i 0)
			(region (car fn))
			(first t)
			data)
		   (if (= (length fs) 1)
		       (when (not only-delimited)
			 (funcall write-a-string line)
			 (funcall write-newline))
		       (progn
			 (fix-region (car region) (cdr region) len)
			 (loop
			    :for f :in fs
			    :do
			      ;; (format t "i ~s start ~s end ~s~%" i start end)
			      (when (and (>= i start) (<= i end))
				(when (not first)
				  (funcall write-a-string output-delimiter))
				(funcall write-a-string f)
				(when collect
				  (push f data))
				(setf first nil))
			      (incf i)
			      (when (> i end)
				;; go to the next region
				(setf fn (cdr fn))
				(when fn
				  (setf region (car fn))
				  (fix-region (car region) (cdr region) len))))
			 (funcall write-newline)
			 (when collect
			   (push (nreverse data) result))))))
	       (read-byte-line (stream bogo)
		 "Read a line of bytes."
		 (declare (ignore bogo))
		 (stretchy-truncate line-vec)
		 (loop :with byte
		    :while (and (setf byte (read-byte stream nil))
				(/= byte newline-code))
		    :do
		      (stretchy-append line-vec byte))
		 (when (not (zerop (length line-vec)))
		   line-vec)))
	;; Set up the appropriate functions.
	(setf snip (if fields #'print-fields #'cut-regions))
	(setf read-a-line (if bytes #'read-byte-line #'read-line))
	(setf write-newline
	      (cond
		(quiet #'values)
		(bytes #'(lambda () (write-byte newline-code *standard-output*)))
		(t #'terpri)))
	(setf write-a-string
	      (cond
		(quiet #'values)
		(bytes #'(lambda (str) (write-sequence str *standard-output*)))
		(t #'write-string)))
	;;(dbugf :cut "fields ~s chars ~s bytes ~s~%" fields characters bytes)
	;;(dbugf :cut "snip ~s~%read-a-line ~s~%" snip read-a-line)
	(loop :with line
	   :while (setf line (funcall read-a-line str nil))
	   :do
	     (funcall snip line))
	(when collect
	  (nreverse result))))))

(defcommand cut
  ((bytes string :short-arg #\b #| :repeating t |#
    :help "Select bytes.")
   (characters string :short-arg #\c #| :repeating t |#
    :help "Select characters.")
   (fields string :short-arg #\f #| :repeating t |#
    :help "Print only specific fields.")
   (delimiter character :short-arg #\d :default #\tab
    :help "Character that separates fields.")
   (re-delimiter string :short-arg #\r
    :help "A Perl regular expression that separates fields.")
   (output-delimiter string :short-arg #\o
    :help "Character that separates fields.")
   (only-delimited boolean :short-arg #\s
    :help "True to omit lines without any delimiters with -f.")
   (collect boolean :short-arg #\C
    :help "True to collect output as lists.")
   (table boolean :short-arg #\t
    :help "True to collect output as a table.")
   (quiet boolean :short-arg #\q
    :help "True to suppress printing output lines.")
   (files input-stream-or-filename
    :default '(list *standard-input*) :repeating t
    :help "Files to read from."))
  :keys-as args
  "Remove sections of each line of input."
   (when (not files)
     (setf files (list *standard-input*)))
   (when table
     (remf args :table)
     (setf args (append args '(:collect t)))
     (setf collect t))
   (flet ((call-cut (f)
	    (apply #'cut-lines f args)))
     (let (results)
       (remf args :files)
       (loop :for f :in files :do
	    (if collect
		(push (call-cut f) results)
		(call-cut f)))
       (when collect
	 (setf *output* (nreverse results))
	 (when (= (length *output*) 1)
	   (setf *output* (first *output*)))
	 (if table
	     (setf *output* (make-table-from *output*))
	     *output*)))))

;; EOF
