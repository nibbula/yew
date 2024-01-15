;;;
;;; free.lisp - Show free memory.
;;;

(defpackage :free
  (:documentation "Show free memory.")
  (:use :cl :dlib :opsys :dlib-misc :table :grout)
  (:export
   #:*bar-char*
   #:describe-free-memory
   #:get-free
   #:memory-total
   #:memory-free
   #:memory-shared
   #:memory-buffer
   #:memory-cached
   #:memory-reclaimable
   #:memory-avail
   #:memory-total-swap
   #:memory-free-swap
   #:memory-lisp-space
   #:memory-lisp-used
   #:!free
   ))
(in-package :free)

(defstruct memory
  total
  free
  shared
  buffer
  cached
  reclaimable
  avail
  total-swap
  free-swap
  lisp-space
  lisp-used)

;; for comparing to linux free
(defun bo ()
  (with-open-file (in "/proc/meminfo")
    (let (total free used shared buffers cached avail reclaimable
	  swap-total swap-used swap-free)
      (macrolet
	  ((bongo (name val)
             `(when (begins-with ,name line)
		(let ((start (length ,name)))
		  (multiple-value-bind (i pos)
		      (parse-integer line :start start :junk-allowed t)
                    (when (and i (/= pos start))
                      (setf ,val i))))
		(return))))
	(loop :with line
	  :while (setf line (read-line in nil nil))
	  :do
	  (block nil
	    (bongo "MemTotal:" total)
	    (bongo "MemFree:" free)
	    (bongo "MemShared:" shared)
	    (bongo "MemAvailable:" avail)
	    (bongo "Shmem:" shared)
	    (bongo "Buffers:" buffers)
	    (bongo "Cached:" cached)
	    (bongo "SwapTotal:" swap-total)
	    (bongo "SwapFree:" swap-free)
	    (bongo "SwapUsed:" swap-used)
	    (bongo "SReclaimable:" reclaimable)
	    ))
	(setf total   (* total 1024)
	      free    (* free 1024)
	      shared  (* shared 1024)
	      avail   (* avail 1024)
	      buffers (* buffers 1024)
	      reclaimable (* reclaimable 1024)
	      cached  (+ (* cached 1024) reclaimable)
	      swap-total (* swap-total 1024)
	      swap-free (* swap-free 1024)
	      swap-used (- swap-total swap-free)
	      )
        (setf used (- total free cached buffers))
	(format t "
               total        used        free      shared  buff/cache   available
Mem:     ~11d ~11d ~11d ~11d ~11d ~11d
Swap:    ~11d ~11d ~11d~%"
			 total used free shared (+ buffers cached) avail
			 swap-total swap-used swap-free)))))

(defun lisp-space ()
  #+sbcl (sb-ext:dynamic-space-size)
  #-sbcl nil)

(defun lisp-used ()
  #+sbcl (sb-kernel:dynamic-usage)
  #-sbcl nil)

(defun get-free ()
  (let* ((memory-unit-bytes   (nos:get-system-info :memory-unit-bytes))
	 (total
	   (* (nos:get-system-info :total-memory) memory-unit-bytes))
	 (free
	   (* (nos:get-system-info :free-memory) memory-unit-bytes))
	 (shared
	   (* (nos:get-system-info :shared-memory) memory-unit-bytes))
	 (buffer
	    (* (nos:get-system-info :buffer-memory) memory-unit-bytes))
	 (cached
	    (* (nos:get-system-info :cached-memory) memory-unit-bytes))
	 (reclaimable
	    (* (nos:get-system-info :reclaimable-memory) memory-unit-bytes))
	 (avail
	    (* (nos:get-system-info :available-memory) memory-unit-bytes))
	 (total-swap
	   (* (nos:get-system-info :total-swap) memory-unit-bytes))
	 (free-swap
	   (* (nos:get-system-info :free-swap) memory-unit-bytes))
	 ;; (processes           (nos:get-system-info :processes))
	 ;; (total-high-memory   (nos:get-system-info :total-high-memory))
	 ;; (free-high-memory    (nos:get-system-info :free-high-memory))
	 )
    (make-memory :total total
		 :free free
		 :shared shared
		 :buffer buffer
		 :cached cached
		 :reclaimable reclaimable
		 :avail avail
		 :total-swap total-swap
		 :free-swap free-swap
		 :lisp-space (lisp-space)
		 :lisp-used (lisp-used)
		 )))

(defun format-compact-size (n width)
  (format nil "~v@a" width
	  (remove #\space
		  (dlib-misc:print-size n :traditional t
					  :stream nil :unit ""))))
(defun format-bytes (n width)
  (format nil "~vd" width n))

(defun free-memory-table (&key in-bytes lisp)
  (let* ((format-func (if in-bytes #'format-bytes #'format-compact-size))
	 (m (get-free))
	 (table
	   (make-table-from
	    `(("Mem" ,(memory-total m)
		     ,(- (memory-total m)
			 (memory-free m)
			 (memory-cached m)
			 ;; (memory-reclaimable m)
			 (memory-buffer m))
		     ,(memory-free m)
		     ,(memory-shared m)
		     ,(+ (memory-buffer m)
			 (memory-cached m)
			 (memory-reclaimable m))
		     ,(memory-avail m))
	      ("Swap" ,(memory-total-swap m)
		      ,(- (memory-total-swap m) (memory-free-swap m))
		      ,(memory-free-swap m)
		      0 0
		      ,(- (memory-total m) (memory-free m)))
	      ,@(when lisp
		  `(("Lisp" ,(memory-lisp-space m)
			   ,(memory-lisp-used m)
			   ,(- (memory-lisp-space m) (memory-lisp-used m))
			   0 0
			   ,(- (memory-lisp-space m) (memory-lisp-used m))))))
	    :columns
	    `((:name "")
	      (:name "Total"      :align :right :type number
	       :format ,format-func)
	      (:name "Used"       :align :right :type number
	       :format ,format-func)
	      (:name "Free"       :align :right :type number
	       :format ,format-func)
	      (:name "Shared"     :align :right :type number
	       :format ,format-func)
	      (:name "Buff/Cache" :align :right :type number
	       :format ,format-func)
	      (:name "Available"  :align :right :type number
	       :format ,format-func)
	      ))))
    table))

(defparameter *bar-char* (code-char #x2592)) ; ▒
;; (defparameter *bar-char* (code-char #x2588)) ; █

(defun describe-free-memory (&key bytes table (lisp t))
  "Describe free system memory. Keyword arguments are:
 ‘bytes’  True to show sizes in bytes.
 ‘table’  Don't output but return a table with the data.
 ‘lisp’   Include Lisp implementation limits if possible."
  (cond
    (table
     (let ((table (free-memory-table :in-bytes bytes :lisp lisp)))
       (with-grout ()
         (grout-print-table table))
       table))
    (t
    ;; Like htop
    (with-grout ()
      (let* ((bar-width (- (grout-width) (+ 1 3 1 1 1)))
	     (m (get-free))
	     (used-bar
	       (round (* (- (memory-total m)
			    (memory-free m)
			    (memory-cached m)
			    (memory-buffer m))
			 bar-width)
		      (memory-total m)))
	     (buf-used-bar
	       (round (* (memory-buffer m)
			  bar-width)
		       (memory-total m)))
	     (shared-used-bar
	       (round (* (+ (memory-shared m)
			    (memory-cached m))
			 bar-width)
		      (memory-total m)))
	     (unused-bar
	       (max 0 (- bar-width used-bar buf-used-bar shared-used-bar)))
	     (swap-bar
	       (if (zerop (memory-total-swap m))
		   0
		   (round (* (- (memory-total-swap m)
				(memory-free-swap m)) bar-width)
			  (memory-total-swap m))))
	     (swap-unused-bar
	       (- bar-width swap-bar)
	       ;; (round (* (memory-free-swap m) bar-width)
	       ;; 	      (memory-total-swap m))
	       )
	     (lisp-bar
	       (when (and lisp (memory-lisp-space m))
		 (if (zerop (memory-lisp-space m))
		     0
		     (round (* (memory-lisp-used m) bar-width)
			    (memory-lisp-space m)))))
	     (lisp-unused-bar
	       (when (and lisp (memory-lisp-space m))
		 (- bar-width lisp-bar))))
	(grout-span `((:cyan "Used: ")
		      ,(format-compact-size (- (memory-total m)
					      (memory-free m)
					      (memory-cached m)
					      (memory-buffer m)) 4)
		      " / " (:cyan "Total: " )
		      ,(format-compact-size (memory-total m) 4)
		      ,(format nil "~v,,,' a"
			       (- (grout-width)
				  (length "Used: XX.XG Total: XX.XG")
				  (length "used/buffers/cache/free")
				  2)
			       #\space)
		      (:bg-green (:fg-black "used/"))
		      (:bg-blue  "buffers/")
		      (:bg-yellow (:fg-black "cache/"))
		      (:bg-white (:fg-black (:bold (:inverse "free"))))
		      #\newline))
	;; (print-values* (used-bar buf-used-bar shared-used-bar unused-bar))
	(grout-span
	 `((:cyan "Mem")
	   (:bold :fg-green #\[)
	   (:fg-green
	    ,(make-string used-bar :initial-element *bar-char*))
	   (:fg-blue
	    ,(make-string buf-used-bar :initial-element *bar-char*))
	   (:fg-yellow
	    ,(make-string shared-used-bar
			  :initial-element *bar-char*))
	   (:bold :fg-black
		  ,(make-string unused-bar
				:initial-element *bar-char*))
	   (:bold :fg-green #\]) #\newline))
	(grout-span
	 `((:cyan "Swp")
	   (:bold :fg-green #\[)
	   (:fg-red
	    ,(make-string swap-bar :initial-element *bar-char*))
	   (:bold :fg-black
		  ,(make-string swap-unused-bar
				:initial-element *bar-char*))
	   (:bold :fg-green #\]) #\newline))
	(when (and lisp lisp-bar)
	  (grout-span
	   `((:cyan "Lsp")
	     (:bold :fg-green #\[)
	     (:fg-magenta
	      ,(make-string lisp-bar :initial-element *bar-char*))
	     (:bold :fg-black
		    ,(make-string lisp-unused-bar
				  :initial-element *bar-char*))
	     (:bold :fg-green #\]) #\newline)))
	))
    (values))))

;; End
