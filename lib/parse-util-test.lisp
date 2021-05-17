;;;
;;; parse-util-test.lisp - Tests for parse-util
;;;

(defpackage :parse-util-test
  (:documentation "Tests for parse-util")
  (:use :cl :test :parse-util)
  (:export
   #:run
   ))
(in-package :parse-util-test)

(defmacro parse-eq (value expr)
  `(multiple-value-bind (status result) ,expr
     (and status (equal result ,value))))

(defun parse-path (path)
  "Parse a unix path."
  (with-parsing (path :track-next t)
    ;; This is so we can distinguish between relative and absolute paths.
    ;; Absolute paths will have a "/" as the first element.
    (optional
     (note ("/") (is-element #\/)))
    (zero-or-more
     (one-of
      (one-or-more (is-element #\/))
      (with-sub-sequence (element)
        (note (element)
	  (one-or-more
	   (is-not-element #\/))))))))

(deftests (parse-util-path)
  (parse-eq '() (parse-path ""))
  (parse-eq '("/") (parse-path "/"))
  (parse-eq '("foo" "bar") (parse-path "foo/bar"))
  (parse-eq '("/" "foo" "bar") (parse-path "/foo/bar"))
  (parse-eq '("/" "foo" "bar") (parse-path "///foo///bar///"))
  (parse-eq '("asdfjk\\" "zurp.aj;l" "d")
	    (parse-path "asdfjk\\/zurp.aj;l/d")))

(defun parse-command (line)
  (with-parsing (line)
    (sequence-of
     (one-of (note ("corge") (is-sequence "corge"))
	     (note ("gralt") (is-sequence "gralt"))
	     (note ("egg") (is-sequence "egg"))
	     (note ("cat") (is-sequence "cat")))
     (zero-or-more
      (one-or-more (is-element #\space))
      (with-sub-sequence (word)
	(note (word) (one-or-more (is-not-element #\space))))))))

(deftests (parse-util-command)
  (not (parse-command ""))
  (parse-eq '("corge") (parse-command "corge"))
  (parse-eq '("cat") (parse-command "cat"))
  (parse-eq '("cat" "food" "in" "cans") (parse-command "cat food in cans"))
  (parse-eq '("egg" "is" "good") (parse-command "egg      is        good"))
  (not (parse-command "can cat?")))

(defun parse-url (url)
  (with-parsing (url :track-next t)
    (optional
     (sequence-of
      (with-sub-sequence (scheme)
	(note ((list :scheme scheme))
	      (one-or-more (and (not (in-sequence ":/?#")) (next-element)))))
      (is-element #\:)))
    (optional
     (sequence-of
      (is-sequence "//")
      (with-sub-sequence (host)
    	(note (`(:host ,host))
    	      (zero-or-more (and (not (in-sequence "/?#")) (next-element)))))))
    (let (path)
      (note (`(:path ,@(nreverse path)))
        (zero-or-more
         (one-of
          (one-or-more (is-element #\/))
          (with-sub-sequence (element)
            (and
              (one-or-more
	        (and (not (in-sequence "/?#")) (next-element)))
              (push element path)))))))
    (optional
     (sequence-of
      (is-element #\?)
      (with-sub-sequence (query)
    	(note (`(:query ,query))
    	      (zero-or-more
    	       (is-not-element #\#))))))
    (optional
     (sequence-of
      (is-element #\#)
      (with-sub-sequence (fragment)
    	(note (`(:fragment ,fragment))
    	      (zero-or-more
    	       (is-not-element #\#))))))))

(deftests (parse-util-url)
  (parse-eq '((:scheme "nibs") (:host "") (:path)) (parse-url "nibs://"))
  (parse-eq '((:scheme "ftp") (:host "example.org") (:path "ding" "bats"))
	    (parse-url "ftp://example.org/ding/bats/"))
  (parse-eq '((:scheme "http") (:host "example.com") (:path "hi" "there")
	      (:query "fish") (:fragment "food"))
            (parse-url "http://example.com/hi/there?fish#food"))
  (not (parse-command "hoop:dupe/scoop&loops")))

(deftests (parse-util-all :doc "Test :parse-util.")
  parse-util-path
  parse-util-command
  parse-util-url)

(defun run ()
  (run-group-name 'parse-util-all :verbose t))

;; End
