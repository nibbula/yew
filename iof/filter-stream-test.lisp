;;;
;;; filter-stream-test.lisp - Test filter streams.
;;;

(defpackage :filter-stream-test
  (:documentation "Test filter streams.")
  (:use :cl :filter-stream)
  (:export
   #:test
   #:run
   ))
(in-package :filter-stream-test)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

;; (defparameter *whitespace* #(#\space #\newline #\tab #\return))
;;
;; (defun complex-fnorder (stream thing)
;;   (when (integerp thing)
;;     (setf thing (code-char thing)))
;;   (etypecase thing
;;     (character
;;      (cond
;;        ((eql thing #\.)
;; 	(setf (filter-stream-state stream) :got-dot))
;;        ((and (eql (filter-stream-state stream) :got-dot)
;; 	     (position thing *whitespace*))
;; 	(setf (setf (filter-stream-state stream) #\F)))))
;;     (string
;;      (let ((pos (position #\. thing)))
;;        (when (and pos (> (length thing) 1)
;; 		  (position (char thing (1+ pos)) *whitespace*))
;; 	 (

(defparameter *fnord-text*
  "Hello. How are you? This is the central scrutinizer. You will be refrobnicated. Continue on as usual. Pay no attention to the man behind the curtain.~%")

(defparameter *b1ff-text*
  "Hello d00d!!! This is b1ff. how r u? ~%")

(defparameter *frob-text*
   "
               ___          ______                                   
              /__/\\     ___/_____/\\          FrobTech, Inc.          
              \\  \\ \\   /         /\\\\                                 
               \\  \\ \\_/__       /  \\         \"If you've got the job, 
               _\\  \\ \\  /\\_____/___ \\         we've got the frob.\"   
              // \\__\\/ /  \\       /\\ \\                               
      _______//_______/    \\     / _\\/______                         
     /      / \\       \\    /    / /        /\\                        
  __/      /   \\       \\  /    / /        / _\\__                     
 / /      /     \\_______\\/    / /        / /   /\\                    
/_/______/___________________/ /________/ /___/  \\                   
\\ \\      \\    ___________    \\ \\        \\ \\   \\  /                   
 \\_\\      \\  /          /\\    \\ \\        \\ \\___\\/                    
    \\      \\/          /  \\    \\ \\        \\  /                       
     \\_____/          /    \\    \\ \\________\\/                        
          /__________/      \\    \\  /                                
          \\   _____  \\      /_____\\/       This .signature gratuitously
           \\ /    /\\  \\    / \\  \\ \\        refers to k               
            /____/  \\  \\  /   \\  \\ \\                   i             
            \\    \\  /___\\/     \\  \\ \\                    b           
             \\____\\/            \\__\\/                      o         ")

(defparameter *sword*
   "
                      /\\                              
            _         )( _____________________________ 
           (_)///////(**)_____________________________>
                      )(                               
                      \\/                              ")

(defun fnord (stream)
  (make-filter-stream
   stream
   #'(lambda (s x)
       (declare (ignore s))
       (ppcre:regex-replace-all "\\.\\s" x ". Fnord\\&"))))

(defun b1ff (stream)
  (make-filter-stream
   stream
   #'(lambda (s x)
       (declare (ignore s))
       (if (lower-case-p x) (char-upcase x) x))
   :unit :character))

(defun reverser (stream)
  (make-filter-stream
   stream #'(lambda (s x) (declare (ignore s)) (reverse x))))

(defun recoder (stream)
  (make-filter-stream
   stream #'(lambda (s x)
	      (declare (ignore s))
	      (if (and (graphic-char-p x) (not (char= x #\space)))
		  (code-char (1+ (char-code x)))
		  x))
   :unit :character))

(defun spacer (stream)
  (make-filter-stream
   stream #'(lambda (s x)
	      (declare (ignore s))
	      (if (char= x #\space) "   " x))
   :unit :character))

(defun sponk (stream)
  (make-filter-stream
   stream #'(lambda (s x)
	      (with-slots ((state filter-stream::state)) s
		(case x
		  (#\space (if state "." x))
		  ((#\/ #\_ #\\) (setf state t) x)
		  (#\newline (setf state nil) x)
		  (otherwise x))))
   :state nil
   :unit :character))

(defun kaka (stream)
  (make-filter-stream
   stream #'(lambda (s x)
	      (declare (ignore s))
	      (let ((pos (position x "_/\\kFj")))
		(if pos (char "~\\/nGg" pos) x)))
   :state nil
   :unit :character))

(defun liner (n stream)
  (make-filter-stream
   stream
   #'(lambda (s x)
       (with-slots ((state filter-stream::state)) s
	 (prog1 (when (> state n) x)
	   (incf state))))
   :state 0))

(defun test-output ()
  (let ((s (fnord *standard-output*)))		(format s *fnord-text*))
  (let ((s (b1ff *standard-output*)))    	(format s *b1ff-text*))
  (let ((s (b1ff (fnord *standard-output*))))	(format s *b1ff-text*))
  (let ((s (fnord (b1ff *standard-output*))))	(format s *b1ff-text*))
  (let ((s (reverser *standard-output*)))
    (format s *sword*) (finish-output s))
  (let ((s (reverser *standard-output*)))	(format s *fnord-text*))
  (let ((s (recoder *standard-output*)))	(format s *sword*))
  (let ((s (spacer *standard-output*)))		(format s *fnord-text*))
  (let ((s (sponk *standard-output*)))		(format s *frob-text*))
  (let ((s (kaka *standard-output*)))		(format s *frob-text*))
  (let ((s (liner 10 *standard-output*)))
    (format s *frob-text*) (finish-output s)))

(defun test ()
  (test-output))

(defun run ()
  (format t "filter-stream-test needs to be fixed~%"))

;; End
