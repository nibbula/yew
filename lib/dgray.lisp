;;;
;;; dgray.lisp - Thinnest portability wrapper around Gray streams.
;;;

#-(or sbcl allegro cmu clisp ecl mocl clasp openmcl lispworks abcl genera
      mezzano)
(error "We don't know how to use gray streams on ~s." (lisp-implementation-type))

#+(or ecl clasp)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (gray::redefine-cl-functions))

(macrolet
    ((stupid ()
       (let ((symbols
	       `(;; types
		 #:fundamental-stream
		 #:fundamental-input-stream #:fundamental-output-stream
		 #:fundamental-character-stream #:fundamental-binary-stream
		 #:fundamental-character-input-stream
		 #:fundamental-character-output-stream
		 #:fundamental-binary-input-stream
		 #:fundamental-binary-output-stream
		 ;; functions
		 #:stream-read-char
		 #:stream-unread-char #:stream-read-char-no-hang
		 #:stream-peek-char #:stream-listen #:stream-read-line
		 #:stream-clear-input #:stream-write-char #:stream-line-column
		 #:stream-start-line-p #:stream-write-string #:stream-terpri
		 #:stream-fresh-line #:stream-finish-output
		 #:stream-force-output
		 #:stream-clear-output #:stream-advance-to-column
		 #:stream-read-byte #:stream-write-byte)))
	 `(defpackage :dgray
	    (:import-from #+sbcl :sb-gray
	     #+allegro :excl
	     #+cmu :ext
	     #+(or clisp ecl mocl clasp) :gray
	     #+openmcl :ccl
	     #+lispworks :stream
	     #+(or abcl genera) :gray-streams
	     #+mezzano :mezzano.gray
	     ,@symbols)
	    (:export ,@symbols)
	    (:documentation
	     "Thinnest portability wrapper around Gray streams.")))))
  (stupid))

;; End
