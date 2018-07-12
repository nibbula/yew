;;
;; dlib-test.lisp - Tests for dlib.
;;

(defpackage :dlib-test
  (:documentation "Tests for DLIB.")
  (:use :cl :test :dlib)
  (:export
   #:run
   ))
(in-package :dlib-test)

;; (test 
;;   (ensure-no-warning
;;    (without-warning (warn "The without warning macro doesn't work!"))))

(deftests (dlib-1 :doc "Test things that are external requirements.")
  "It's possible, but I've yet to come across a system that has getenv
  and doesn't have PATH."
  (stringp (d-getenv "PATH"))
  "This test could actually fail if you tried to make it fail."
  (not (d-getenv "A_RRREALLY_UNLIKELY_ENV_VAR_Z9823223_X"))
  (not (null *mop-package*))
  (and (find-package *mop-package*)
       (packagep (find-package *mop-package*)))
  ;; What about (slot-documentation) ?
  (let ((str (system-command-stream "echo" '("hello"))))
    (prog1 (equal (read-line str) "hello")
      (close str)))
  (equal (shell-line "echo" "goodbye" "walrus") "goodbye walrus")
  ;;(shell-lines)
  (or (listp (system-args))
      (vectorp (system-args)))
  ;; How can we test these? It seems like we would have to run a sub-image.
  ;;(exit-system)
  ;;(save-image-and-exit)
  (typep (overwhelming-permission) 'boolean))

(deftests (dlib-2 :doc "Just minimal stuff in order from the source.")
    "Version numbers"
  (not (null *lisp-version-number*))
  (numberp *lisp-version-number*)
  (integerp *lisp-version-number*)
  "Features"
  (progn (d-add-feature :floop)
	 (has-feature :floop))
  (progn
    (d-remove-feature :floop)
    (not (has-feature :floop)))
  (with-unique-names (foo bar)
    (and (not (eq foo 'foo))
	 (not (eq bar 'bar))))
  "Warnings and notes"
  (without-warning (warn "You shouldn't see this!") t)
  ;; Does this really test anything?
  ;;(without-notes (signal 'compiler-note "You shouldn't see this!") t)
  "initial-span"
  (equal "fooo" (initial-span "fooo the bar" " "))
  (equal "flip" (initial-span "flip23out234" "1234"))
  (equal '(5 6 7 8 9 10 11)
	 (initial-span '(5 6 7 8 9 10 11 1 1 1 2 2 1 1 2) '(1 2)))
  (equal '(5 6 7 8 9 10 11 1 1 1)
	 (initial-span '(5 6 7 8 9 10 11 1 1 1 2 2 1 1 2) '(2)))
  "replace-subseq"
  (equal "barbar" (replace-subseq "foo" "bar" "foobar"))
  (equal "__very__lame__" (replace-subseq " " "__" " very lame "))
  (equal "yyyyx" (replace-subseq "xx" "y" "xxxxxxxxx"))
  (equal "ZeepZeepZeep" (replace-subseq "." "Zeep" "..."))
  (equal "StudlyCapFailure" (replace-subseq " " "" "Studly Cap  Failure"))
  (equal '(_ :FAIRY _ :FLAME _)
	 (replace-subseq '(1 1) '(_) '(1 1 :fairy 1 1 :flame 1 1)))
  "begins-with"
  (begins-with "duh" "duhluth")
  (not (begins-with "duhx" "duhluth"))
  (not (begins-with "snarbflackula" "snarb"))
  (begins-with "Yo" "yolandi" :test #'equalp)
  (not (begins-with '(1 2) '(1.0 2.0 3.0 4.0)))
  (begins-with '(1 2) '(1.0 2.0 3.0 4.0) :test #'=)
  "ends-with"
  (ends-with "ump" "heffalump")
  (not (ends-with "lump" "bugbump"))
  (not (ends-with "weasel" "easel"))
  (ends-with "Go" "indigo" :test #'equalp)
  (not (ends-with '(3 4) '(1.0 2.0 3.0 4.0)))
  (ends-with '(3 4) '(1.0 2.0 3.0 4.0) :test #'=)
  "remove-prefix"
  (equal "y" (remove-prefix "fishy" "fish"))
  (equal "Hat" (remove-prefix "TopHat" "top" :test #'equalp))
  "remove-suffix"
  (equal "fish" (remove-suffix "fishily" "ily"))
  (equal "Top" (remove-suffix "TopHat" "Hat" :test #'equalp))
  "s+"
  (equal "foobarbaz" (s+ "foo" "bar" "baz"))
  (equal "foobarbaz" (s+ "foo" '|bar| "baz"))
  (equal "foo:1bar:2baz:3" (s+ "foo:" 1 "bar:" 2 "baz:" 3))
  (or (equal "-#C(0.0 3.0)=" (s+ #\- (sqrt -9) #\=))
      (equal "-#C(0 3)=" (s+ #\- (sqrt -9) #\=)))
  (equal "NIL" (s+ nil))
  "trim"
  (equalp #(#\a #\b #\c #\space #\space)
	  (ltrim #(#\space #\space #\a #\b #\c #\space #\space)))
  (equalp #(#\space #\space  #\a #\b #\c)
	  (rtrim #(#\space #\space #\a #\b #\c #\space #\space)))
  (equalp #(#\a #\b #\c)
	 (trim #(#\space #\space #\a #\b #\c #\space #\space)))
  (equalp '(#\y #\e #\s)
	  (trim '(#\space #\tab #\y #\e #\s #\space #\tab #\newline)))
  (equal "zorp" (trim "120938zorp982321" "0123456789"))
  (equal "zo   rp" (trim "120zo   rp321" "0123456789"))
  (equal "    " (trim "31415926    53589793" "0123456789"))
  "join-by-string"
  (equal "foo-bar-baz" (join-by-string '("foo" "bar" "baz") "-"))
  (equal "foo-bar-baz" (join-by-string '("foo" "bar" "baz") #\-))
  (equal "f.u.b.a.r" (join-by-string #("f" "u" "b" #\a #\r) #\.))
  (equal "192.168.0.63" (join-by-string '(192 168 0 63) #\.))
  (equal "i·c·a·n·d·o·i·t" (join-by-string "icandoit" "·"))
  "delete-nth"
  (equalp '(2 3 4 5) (let ((l (list 1 2 3 4 5))) (delete-nth 0 l)))
  (equalp '(1 3 4 5) (let ((l (list 1 2 3 4 5))) (delete-nth 1 l)))
  (equalp '(1 2 4 5) (let ((l (list 1 2 3 4 5))) (delete-nth 2 l)))
  (equalp '(1 2 3 5) (let ((l (list 1 2 3 4 5))) (delete-nth 3 l)))
  (equalp '(1 2 3 4) (let ((l (list 1 2 3 4 5))) (delete-nth 4 l)))
  (equalp '(((2 3 4 5) (1 2 3 4 5))
	    ((1 3 4 5) (1 3 4 5))
	    ((1 2 4 5) (1 2 4 5))
	    ((1 2 3 5) (1 2 3 5))
	    ((1 2 3 4) (1 2 3 4)))
	  (loop :for i :from 0 :to 4 :collect
	     (let ((l (list 1 2 3 4 5))) (list (delete-nth i l) l))))
  ;; "alist-to-hash-table"
  ;; (alist-to-hash-table )
  "flatten"
  (tree-equal
   '(1 2 3 4 5 6 7 8 9 10)
   (flatten '((1 2 3) (4 5 6) ((7 8) (9 (10))))))
  (tree-equal
   '(1 2 3 4 5 6 7 8 9 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
   (flatten '((1 2 3) (4 5 6) (7 8 9) (1 1 1)
	      ((((1 1) (1 1 (1 1 1)) 1 1 1)
		1 1 1 1 1 1 (((1 1 1))) 1)))))
  (tree-equal
   '(1 2 3 4 5 6 7 8 9 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
   (flatten '((1 2 3) (4 5 6) (7 8 9) (1 nil 1)
	      ((((1 1) (1 1 (1 1 1)) 1 1 1) 1 1 1 1 1 1 (((1 1 1))) 1)))))
  (tree-equal
   '(1 2 3 4 5 6 7 8 9 1 NIL 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
   (dlib:flatten '((1 2 3) (4 5 6) (7 8 9) (1 nil 1)
		   ((((1 1) (1 1 (1 1 1)) 1 1 1) 1 1 1 1 1 1
		     (((1 1 1))) 1))) :preserve-nils t))
  (tree-equal (flatten '(() () ())) nil)
  (tree-equal (flatten '(() () ()) :preserve-nils t)
	      '(nil nil nil))
  (tree-equal (flatten 1) '(1))
  (tree-equal (flatten '(1 . 2)) '(1 2))
  (tree-equal (flatten '(nil . 3)) '(3))
  ;; ranges?
  ;; (shallow-copy-object)
  ;; (doseq)
  ;; (exe-in-path-p)
  ;; (try-things)
  )

(deftests (dlib-io-1
	   :doc "Things related to input and output.")
  (equal "Howdy." (with-input-from-string (str "Howdy.")
		    (resilient-read-line str)))
  ;; (with-open-file-or-stream)
  ;; (with-lines)
  ;; (get-ilnes)
  ;; (safe-read-from-string)
  ;; (clean-read-from-string)
  ;; (package-robust-read-from-string)
  ;; (package-robust-read)
  ;; (copy-stream)
  ;; (quote-format)
  )

(deftests (dlib-language-1
	   :doc "Things relating to basic programming language expression.")
  ;; (define-constant)
  ;; (defalias)
  "λ"
  (functionp (λ (x) (+ x 2)))
  (equal "luuhrg" ;; I suppose this could fail for weird encodings.
	 (map 'string (λ (x) (code-char (+ 6 (char-code x)))) "foobla"))
  (equal 23 (let ((f (λ (&rest args) (apply #'+ 2 args))))
	      (funcall f 1 2 3 4 5 6)))
  ;; (_)
  ;; (symbolify)
  ;; (keywordify)
  ;; (likely-callable)
  ;; (lambda-list)
  ;; (lambda-list-vars)
  ;; (with-unique-names)
  ;; (with-package)
  ;; (shortest-package-nick)
  ;; (not-so-funcall)
  ;; (@)
  )

(deftests (dlib-debug-1
	   :doc "Things relating to debugging.")
  "dbug"
  (equal "Foo" (with-dbug
		 (with-output-to-string (*debug-io*)
		   (dbug "Foo"))))
  (equal "Foobar 23" (with-dbug
			 (with-output-to-string (*debug-io*)
			   (dbug "Foo~a ~d" "bar" 23))))
  (equal "" (with-output-to-string (*debug-io*)
	      (dbug "Nope")))
  (equal "" (with-dbug (without-dbug 
			   (with-output-to-string (*debug-io*)
			     (dbug "Nope")))))
  ;; (with-dbug-package)
  (equal (format nil "A=1 B=2 C=3 D=4 ~%")
	 (with-output-to-string (*debug-io*)
	   (let ((a 1) (b 2) (c 3) (d 4)) (dump-values a b c d)))))

(deftests (dlib-environment-1
	   :doc "Things relating to the environment we are running in.")
  (not (null *host*))
  (not (null *arch*))
  (not (null *arch-nickname*))
  (not (null *os*))
  (not (null *lisp-implementation-nickname*))
  (not (null *lisp-version*))
  (not (null *lisp-version-number*))
  (not (null *platform-nickname*)))

(deftests (split-sequence-1 :doc "Test split-sequence.")
   (equal '("usr" "local" "bin")
	  (split-sequence #\/ "/usr/local/bin/" :omit-empty t))
   (equal '("" "usr" "local" "bin" "")
	  (split-sequence #\/ "/usr/local/bin/" :omit-empty nil))
   (equal '("usr" "local" "bin")
	  (split-sequence "/" "/usr/local/bin/" :omit-empty t))
   (equal '("" "usr" "local" "bin" "")
	  (split-sequence "/" "/usr/local/bin/" :omit-empty nil))
   (equal '("www" "common-lisp" "net")
	  (split-sequence #\. "www.common-lisp.net" :omit-empty t))
   (equal
    '("giant" "pig" "wizzle")
    (split-sequence #\space " giant  pig wizzle  " :omit-empty t))
   (equal '("flip" "out" "4")
	  (split-sequence "23" "flip23out234"))
   (equalp (split-sequence #\; "a;;b;c") '("a" "" "b" "c"))
   ;;(equalp (split-sequence #\; "a;;b;c" :from-end t) '("a" "" "b" "c"))
   ;;(equalp (split-sequence #\; "a;;b;c" :from-end t :count 1) '("c"))
   (equalp (split-sequence #\; "a;;b;c" :remove-empty-subseqs t) '("a" "b" "c"))
   (equalp (split-sequence #\; ";oo;bar;ba;" :start 1 :end 9) '("oo" "bar" "b"))
   (equalp (split-sequence-if (lambda (x) (member x '(#\a #\b))) "abracadabra")
	   '("" "" "r" "c" "d" "" "r" "")))

(deftests (split-sequence-by-range-1 :doc "Test split-sequence-by-range.")
  "normal usage with lists"
  (equal '("foo" "the" "bar")
	 (split-sequence-by-range '((0 2) (4 6) (8 10)) "foo the bar"))
  (equal '("foo" "the" "bar")
	 (split-sequence-by-range '((nil 2) (4 6) (8 10)) "foo the bar"))
  (equal '("foo" "the" "bar")
	 (split-sequence-by-range '((0 2) (4 6) (8 nil)) "foo the bar"))
  (equal '("foo" "the" "bar")
	 (split-sequence-by-range '((nil 2) (4 6) (8 nil)) "foo the bar"))
  "normal usage with pairs"
  (equal '("foo" "the" "bar")
	 (split-sequence-by-range '((0 . 2) (4 . 6) (8 . 10)) "foo the bar"))
  (equal '("foo" "the" "bar")
	 (split-sequence-by-range '((nil . 2) (4 . 6) (8 . nil))
				  "foo the bar"))
  "overlapping"
  (equal '("foo" "foo the" "bar")
	 (split-sequence-by-range '((nil . 2) (nil . 6) (8 . nil))
				  "foo the bar"))
  (equal '("foo" "foo the bar" "bar")
	 (split-sequence-by-range '((nil . 2) (nil . nil) (8 . nil))
				  "foo the bar"))
  "unusual usage"
  (equal nil (split-sequence-by-range nil nil))
  (equal nil (split-sequence-by-range nil "foo the bar"))
  (equal '("foo the bar") (split-sequence-by-range '(()) "foo the bar"))
  (equalp '("") (split-sequence-by-range '(()) ""))
  "non-string sequences"
  (equalp '((f o o) (t h e) (b a r))
	 (split-sequence-by-range '((0 2) (4 6) (8 10))
				  '(f o o nil t h e nil b a r)))
  (equalp '(#(#\f #\o #\o) #(#\t #\h #\e) #(#\b #\a #\r))
	  (split-sequence-by-range '((0 2) (4 6) (8 10))
				   #(#\f #\o #\o 0 #\t #\h #\e 0 #\b #\a #\r))))

(deftests (dlib-all :doc "Test :dlib and :dlib-misc.")
  dlib-1 split-sequence-1 split-sequence-by-range-1 dlib-2 dlib-language-1
  dlib-io-1 dlib-environment-1 dlib-debug-1)

(defun run ()
  (run-group-name 'dlib-all :verbose t))

;; EOF
