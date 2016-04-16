;;
;; pipe-buffer.lisp - A pipe buffer of some sort
;;

(defpackage :pipe-buffer
  (:documentation "A pipe buffer of some sort.")
  (:use :cl)
  (:export
   #:make-pipe-buffer
   #:write-data
   #:read-data
   ;; settable options, if you really must
   #:*chunk-max-size*
   #:*chunk-min-size*
   #:*default-element-type*
   ))
(in-package :pipe-buffer)

;; A buffer of indefinite length that hopefully supports efficient writing
;; to one end and reading from the other, and can return memory to the system
;; after it's read.

;; @@@ Perhaps we could also have a maximum size of the whole pipe buffer,
;; after which write-data would get an error or hang or something.
;;
;; @@@ We should work out a "fast path" for single elements.

(defparameter *chunk-max-size* #.(* 1024 1024 4)
  "Limit on the size of chunks.")

(defparameter *chunk-min-size* #.(* 1024 4) ; @@@ should be system page size
  "Starting size of chunks.")

(defparameter *default-element-type* 'character
  "Default element type of data.")

(defstruct buffer-chunk
  "Storage block for a piece of the buffer."
  start					; First element number
  end					; Last element number
  data)					; The data

(defstruct (pipe-buffer (:constructor %make-pipe-buffer))
  element-type			      ; Type of elements
  length			      ; The total length of data in all chunks
  size				      ; The total size of all chunks
  read-point			      ; Where reading is done from
  write-point			      ; Where writing is done to
  chunks-head			      ; Start of the list of chunks
  chunks-tail)			      ; End of the list of chunks

(defun alloc-size (buf len)
  "Return the size a new chunk should be for BUF to fit LEN."
  ;; We double the previous chunk size or big enough to fit the write,
  ;; in multiples of *chunk-min-size*, but topping out at the maximum chunk
  ;; size. Everything should be multiples of *chunk-min-size* which should
  ;; be the page size, or a multiple of it.
  ;;
  ;; I have no idea if this is actually good or not!
  (min (max (* 2 (length (buffer-chunk-data
			  (car (pipe-buffer-chunks-tail buf)))))
	    (ceiling len *chunk-min-size*))
       *chunk-max-size*))

(defun alloc (buf size)
  "Add room for SIZE elements to BUF."
  (assert (typep buf 'pipe-buffer))
  (assert (typep size 'integer))
  (incf (pipe-buffer-size buf) size)
  (if (not (pipe-buffer-chunks-head buf))
      ;; First chunk
      (let ((new-chunk (make-buffer-chunk
			:start 0
			:end (1- size)
			:data (make-array
			       `(,size)
			       :element-type (pipe-buffer-element-type buf)
			       :adjustable nil))))
	(setf (pipe-buffer-chunks-head buf) (list new-chunk)
	      (pipe-buffer-chunks-tail buf) (pipe-buffer-chunks-head buf)))
      ;; Subsequent chunks
      (let* ((new-start (1+ (buffer-chunk-end
			     (car (pipe-buffer-chunks-tail buf)))))
	     (new-tail (list (make-buffer-chunk
			      :start new-start
			      :end (+ new-start (1- size))
			      :data
			      (make-array
			       `(,size)
			       :element-type (pipe-buffer-element-type buf)
			       :adjustable nil)))))
	(rplacd (pipe-buffer-chunks-tail buf) new-tail)
	(setf (pipe-buffer-chunks-tail buf) new-tail)))
  buf)

(defun make-pipe-buffer (&key
			   (element-type *default-element-type*)
			   (initial-size *chunk-min-size*)
			   (chunk-max-size *chunk-max-size*))
  (check-type initial-size integer)
  (check-type chunk-max-size integer)
  (let ((result
	 (%make-pipe-buffer
	  :element-type element-type
	  :length 0
	  :size 0
	  :read-point 0
	  :write-point 0
	  :chunks-head nil
	  :chunks-tail nil)))
    (alloc result initial-size)
    result))

(defun write-data (buf data &optional (start 0) end)
  ;; (check-type buf pipe-buffer)
  ;; (check-type data sequence)
  ;; (check-type start integer)
  ;; (check-type end integer)
  (with-slots (length write-point chunks-tail) buf
    (let* ((data-len (length data))
	   (chunk (car chunks-tail))
	   (saved-tail chunks-tail)
	   (chunk-point (- write-point (buffer-chunk-start chunk)))
	   (len (- (or end (length data)) start)))

      ;; Check parameters
      (when (or (< start 0) (and end (> start end)) (> start data-len))
	(error "START is not in the bounds [0 ~a]"
	       (min data-len end)))
      (when (and end (or (< end 0) (< end start) (> end data-len)))
	(error "END is not in the bounds [~a ~a]" start data-len))

      ;; If the data is too big for the last chunk
      (if (> (+ chunk-point len) (length (buffer-chunk-data chunk)))
	  (progn
	    ;;(format t "Have to alloc~%")
	    (setf (subseq (buffer-chunk-data chunk) chunk-point)
		  (subseq data start
			  (+ start (- (length (buffer-chunk-data chunk))
				      chunk-point))))
	    ;;(format t "First piece ~s~%"
	    ;;	    (subseq data start
	    ;;		    (+ start (- (length (buffer-chunk-data chunk))
	    ;;				chunk-point))))
	    ;; Allocate new chunks, until we have enough space
	    (loop
	       :with done = (- (length (buffer-chunk-data chunk))
			       (- write-point (buffer-chunk-start chunk)))
	       :and size
	       :while (< done len)
	       :do
	       (setf size (alloc-size buf (- len done)))
	       (alloc buf size)
	       ;;(format t "Allocated ~d~%" size)
	       (setf chunk (cadr saved-tail)
		     saved-tail (cdr saved-tail))
	       ;;(format t "New chunk ~s~%" chunk)
	       (setf size (min (length (buffer-chunk-data chunk)) (- len done)))
	       ;;(format t "size to copy ~a~%" size)
	       ;;(format t "start=~d done=~a~%" start done)
	       (setf (subseq (buffer-chunk-data chunk) 0 size)
		     (subseq data (+ start done) (+ start done size)))
	       ;;(format t "Copied ~s~%"
	       ;;       (subseq data (+ start done) (+ start done size)))
	       (incf done size)
	       )
	    )
	  ;; It fits, so just copy it.
	  (progn
	    ;;(format t "It fits~%chunk-point=~d~%" chunk-point)
	    (setf (subseq (buffer-chunk-data chunk)
			  chunk-point (+ chunk-point len))
		  (subseq data start end))
	    ;;(format t "Copied ~s~%" (subseq (buffer-chunk-data chunk)
	    ;;				    chunk-point (+ chunk-point len)))
	    ))
      (incf write-point len)
      (incf length len))))

(defun slice (array start end)
  "Return a displaced piece of an array."
  (make-array (list (1+ (- end start)))
	      :element-type (array-element-type array)
	      :displaced-to array
	      :displaced-index-offset start))

;; @@@ It should probably reset to the beginning of the buffer when everything
;; is read. Also perhaps there should be an option to be able to pick where
;; you want to be able to ‘unread’ from.

(defun read-data (buf &optional len (eof-error-p t) (eof-value nil))
  ;; (check-type buf pipe-buffer)
  (with-slots (length size read-point write-point chunks-head chunks-tail) buf
    (let (result
	  (read-len (or len length))
	  (read-count 0)
	  result-list start end chunk-size chunk-read-len
	  (chunk (car chunks-head)))
      (loop :while (and chunk (< read-count read-len) (> length 0))
	 :do
	 (setf chunk-size (length (buffer-chunk-data chunk)))
	 ;;(format t "chunk-size=~d~%" chunk-size)
	 (if (> read-point (buffer-chunk-end chunk))
	     ;; The whole chunk has been read, so we can discard it.
	     (progn
	       (setf (buffer-chunk-data chunk) nil) ;; just to be sure
	       ;; @@@ this should probably be an error since it indicates
	       ;; an inconsistency
	       ;;(format t "already read chunk?~%"))
	     ;; read out of the chunk
	     (progn
	       (setf start (- read-point
			      (buffer-chunk-start chunk))
		     ;; Smaller of:
		     ;;   how much we want to read (read-len)
		     ;;   how much is before the write point
		     ;;   how much is left in the chunk
		     end (min (+ start read-len)
			      (- (min (buffer-chunk-end chunk)
				      (1- write-point))
				 (buffer-chunk-start chunk)))
		     chunk-read-len (1+ (- end start)))
	       ;;(format t "start=~d end=~d chunk-read-len=~d~%"
	       ;;       start end chunk-read-len)
	       ;; Hopefully "slicing" is more efficient than copying.
	       (push (slice (buffer-chunk-data chunk) start end)
		     result-list)
	       ;;(format t "-> ~s~%" (car result-list))
	       (incf read-point chunk-read-len)
	       (incf read-count chunk-read-len)
	       (decf length chunk-read-len)))
	 ;; if we read the whole chunk
	 (when (= end (1- chunk-size))
	   (decf size chunk-size)
	   (setf chunks-head (cdr chunks-head))
	   (when (not chunks-head)
	     ;; we read all the chunks
	     (setf chunks-tail nil)))
	 ;;(format t "length = ~d size = ~d read-point = ~d write-point = ~d~%"
	 ;;	 length size read-point write-point)
	 ;; go to the next chunk
	 (setf chunk (and chunks-head (car chunks-head))))
      (setf result-list (nreverse result-list)
	    result (if (= 1 (length result-list))
		       (copy-seq (first result-list))
		       (apply #'concatenate
			      `(vector ,(pipe-buffer-element-type buf) *)
			      result-list)))
      (if (= read-count 0)
	  (if eof-error-p
	      (error "End of buffer")
	      eof-value)
	  result))))

;; This is just for testing, since it assumes strings and is inefficient.
(defun whole (buf)
  "Return the whole buffera as a string, for testing."
  (with-slots (chunks-head) buf
    (subseq (with-output-to-string (str)
	      (loop :for c :in chunks-head :do
		 (princ (buffer-chunk-data c) str)))
	    0 (pipe-buffer-length buf))))

;; EOF
