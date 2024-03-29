;;;
;;; glob-test.lisp - Test glob package.
;;;

(defpackage :glob-test
  (:documentation "Test glob package")
  (:use :cl :test :glob :opsys :dlib)
  (:export
   ;; Main entry point
   #:run
   ))
(in-package :glob-test)

(deftests (fnmatch-strings)
  ;; Non pattern strings
  (fnmatch "hello" "hello")
  (not (fnmatch "hello" "hellx"))
  (not (fnmatch "hello" "hellx"))
  (not (fnmatch "hello" "hellox"))
  (fnmatch "" ""))

(deftests (fnmatch-qmark)
  ;; Question mark patterns
  (fnmatch "h?y" "hey")
  (fnmatch "??y" "hey")
  (fnmatch "???" "hey")
  (not (fnmatch "???" "heyy"))
  (not (fnmatch "????" "hey"))
  (not (fnmatch "z???" "hey"))
  (fnmatch "?hey" "They"))

(defun test-class (class string)
  (every (lambda (c) (fnmatch class (string c))) string))

(deftests (fmatch-charset)
  ;; Character sets
  (not (fnmatch "??[abc]" "hey"))
  (fnmatch "??[xyz]" "hey")
  (not (fnmatch "[a-z]" "hey"))
  (fnmatch "[a-z][a-z][a-z]" "hey")
  (fnmatch "[a-z][A-z][xyz]" "hey")
  (not (fnmatch "[a-z][A-z][xyz]" "h2y"))
  (fnmatch "[a-z][0-9][xyz]" "h2y")
  (fnmatch "?[^a-z]?" "h2y")
  (not (fnmatch "?[^a-z]?" "hey"))
  (not (fnmatch "[!a-z]??" "hey"))
  (not (fnmatch "[a-z]?[^y]" "hey"))
  (fnmatch "[A-Z][^A-Z][a-xy-z]" "Hey")
  (fnmatch "[X-]" "-")
  (fnmatch "[X-]" "X")
  (fnmatch "[-Z]" "-")
  (fnmatch "[-Z]" "Z")
;  (fnmatch "[[]" "[")
  (fnmatch "[]]" "]")
  (fnmatch "[]-]" "-")
  (fnmatch "[]-]" "]")
  (fnmatch "[" "[")
  (fnmatch "[\\]" "\\")
  ;; Character equivalence classes
  (fnmatch "[[=A=]]ey" "Hey")
  (fnmatch "Hey[[= =]]Ho" "Hey	Ho")
  (fnmatch "Hey[[=,=]]Now" "Hey/Now")
  (fnmatch "Hey[[==]]Now" "HeyNow")
  (fnmatch "[[=1=]] more" "2 more")
  (fnmatch "O[[=v=]]O" "OwO")
  ;; Named character classes
  (test-class "[[:xdigit:]]" "deadbeef")
  (not (test-class "[[:xdigit:]]" "dorkblaster"))
  (not (test-class "[[:xdigit:]]" "#x48fe9a29FF"))
  (test-class "[[:xdigit:]]" "CAFEBABE")
  (test-class "[[:xdigit:]]"
	      "0950002009380924094d091a093f0924093e0928093e0928094d0926093e")
  (test-class "[[:ascii:]]" "asd;lfkjzxc.v,mnkeiqr;lkjfa")
  (test-class "[[:word:]]" "woordington")
  (test-class "[[:word:]]" "shrubbleby_florpington_shnoog")
  (not (test-class "[[:word:]]" " grob/gob/glob/grod "))
  (test-class "[[:print:]]" "This is totally not printable.")
  (not (test-class "[[:print:]]" " <Print-Me> "))
  (test-class "[[:graph:]]" "I-don't-even-know-what-you-mean?")
  (not (test-class "[[:graph:]]" "Oh. No I see."))
  (test-class "[[:alnum:]]"
	      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
  (test-class "[[:alpha:]]"
	      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
  (not (test-class "[[:alpha:]]" "Sod off!"))
  (test-class "[[:lower:]]" "iamnotanasciianymore")
  (not (test-class "[[:lower:]]" "‘Literally’"))
  (test-class "[[:upper:]]" "DISOBEYMYEVERYCOMMAND")
  (not (test-class "[[:upper:]]" "Actually: Obey"))
  (test-class "[[:digit:]]" "0123456789")
  (not (test-class "[[:digit:]]" "2D293D92EF0"))
  (test-class "[[:cntrl:]]" (with-output-to-string (str)
			      (loop :for i :from 0 :below 31
				 :do (princ (code-char i) str))))
  (not (test-class "[[:cntrl:]]" "Why even test forwhat you don't want?"))
  (test-class "[[:punct:]]" "~`!@#$%^&*()_-+={}|[]\;':\"<>?,./")
  (not (test-class "[[:punct:]]" "scissors tape telephone bowling-ball"))
  (test-class "[[:space:]]" " 	
")
  (not (test-class "[[:space:]]" "Let him have time of Time's help to despair"))
  (test-class "[[:blank:]]" " 	    		    ")
  (not (test-class "[[:blank:]]" "[[:blank:]]"))
  )

(deftests (fnmatch-star)
  (fnmatch "foo*" "foo")
  (fnmatch "foo*" "foo*")
  (fnmatch "foo*" "foobar")
  (fnmatch "*bar" "foobar")
  (fnmatch "fo*ar" "foobar")
  (fnmatch "f*b*r" "foobar")
  (fnmatch "????????????????????" "anythingthats20chars")
  (fnmatch "*?*?*?*?*?*?*?*?*?*?*?*?*?*?*?*?*?*?*?*?" "anythingthats20chars")
  (fnmatch
   "*p*l*e*a*s*e*d*o*n*t*l*e*t*s*o*m*e*o*n*e*b*l*o*w*o*u*t*y*o*u*r*s*t*a*c*k*"
   "pleasedontletsomeoneblowoutyourstack")
  (fnmatch "a**?**cd**?**??k" "abcdecdhjk")
  (fnmatch "w*****p" "whoop")
  (fnmatch "w*****?p" "whoop")
  (fnmatch "w****??p" "whoop")
  (fnmatch "??**??***?" "whoop")
  (fnmatch "*****??" "whoop")
  (fnmatch "***??p" "whoop")
  (fnmatch "**??p" "whoop")
  (fnmatch "*?p" "whoop")
  (not (fnmatch "w**oo***?**p" "whoop"))
  (fnmatch "?**oo***?" "whoop")
  (fnmatch "0**?**23**?**??9" "0123456789")
  (fnmatch "**??**4**??8**9" "0123456789")
  (fnmatch "0**?**23**?**??9" "0123456789")
  (fnmatch "*0**?3**??***6*?8?" "0123456789")
  (fnmatch "*0**?3**??**?7*" "0123456789")
  (fnmatch "*??**??**??**?*??9" "0123456789")
  (fnmatch "*?1**??**??**?*?8?" "0123456789")
  (not (fnmatch "0**?**23**?**?????9" "0123456789"))
  (not (fnmatch "**??2**???*5**8**9" "0123456789"))
  (fnmatch "\\" "\\")
  (fnmatch "\\\\" "\\\\")
  (fnmatch "\\\\\\" "\\\\\\")
  (fnmatch "[*" "[foo")
  (fnmatch "[/\\]*" "/foo")
  )

(deftests (fnmatch-escape)
  (fnmatch "foo*bar" "foo\\*bar")
  (fnmatch "foo\\*bar" "foo*bar")
  (not (fnmatch "foo\\*bar" "foo*bar" :escape nil))
  (fnmatch "foo\\?bar" "foo?bar")
  (not (fnmatch "foo\\?bar" "foo?bar" :escape nil))
  (fnmatch "foo\\?bar" "foo\\?bar" :escape nil)
  )

#+unix (d-add-feature :t-has-system-glob)

#+t-has-system-glob
(progn
  (cffi:defcstruct foreign-glob-t
    (gl_pathc nos:size-t)
    (gl_matchc :int)
    (gl_offs nos:size-t)
    (gl_flags :int)
    (gl_pathv (:pointer :string)) ;; char **
    ;; We don't really care about all those callback functions.
    (gl_errfunc :pointer) ;; int (*gl_errfunc)(const char *, int);
    (gl_closedir :pointer) ;; void (*gl_closedir)(void *);
    (gl_readdir :pointer)  ;; struct dirent *(*gl_readdir)(void *);
    (gl_opendir :pointer)  ;; void *(*gl_opendir)(const char *);
    (gl_lstat :pointer)	   ;; int (*gl_lstat)(const char *, struct stat *);
    (gl_stat :pointer))	   ;; int (*gl_stat)(const char *, struct stat *);

  ;; Reference the C functions
  (cffi:defcfun ("fnmatch" real-fnmatch) :int (pattern :string) (string :string)
	        (flags :int))
  (cffi:defcallback glob-error :int ((epath :string) (eerrno :int))
    (format *error-output* "Glob error ~d for path ~s.~%" eerrno epath))
  (cffi:defcfun ("glob" real-glob) :int (pattern :string) (flags :int)
    (errfunc :pointer) (pglob (:pointer (:struct foreign-glob-t))))
  (cffi:defcfun globfree :void (pglob (:pointer (:struct foreign-glob-t))))
  (defun system-glob (pattern &optional (flags 0))
    (cffi:with-foreign-object (pglob '(:struct foreign-glob-t))
      (real-glob pattern flags (cffi:callback glob-error) pglob)
      (cffi:with-foreign-slots ((gl_pathc gl_pathv) pglob
				(:struct foreign-glob-t))
	(loop :for i :from 0 :below gl_pathc
	      :collect (cffi:mem-aref gl_pathv :string i))))))

(defparameter *test-dir* "zxcv")

(defun touch (name)
  (with-open-file (stm name :direction :output)
    (format stm "foo~%")))

(defun trailing-directory-p (name)
  (char= *directory-separator* (char name (1- (length name)))))

(defun make-dir-tree (tree)
  (loop :for f :in tree :do
    (if (trailing-directory-p f)
	(make-directory f)
	(touch f))))

(defun delete-dir-tree (tree)
  (loop :for f :in (reverse tree) :do
    (if (trailing-directory-p f)
	(delete-directory f)
	(delete-file f))))

(defun fix-path (path)
  "Fix the path so it's right for the OS."
  (substitute *directory-separator* #\/ path))

(defparameter *tree-1*
  (mapcar #'fix-path
	  '("foo"
	    "one"
	    "two"
	    "boo/"
	    "boo/baz"
	    "boo/foo"
	    "boo/bar/"
	    "boo/bar/corge"
	    "boo/bar/gralt.c"
	    "zoo/"
	    "zoo/bar"
	    "zoo/foo"
	    "zoo/baz/"
	    "zoo/baz/lemon"
	    "zoo/baz/foo"
	    "zoo/baz/bark.c"
	    "zoo/baz/pippy.c/"
	    "zoo/baz/pippy.c/fuzz"
	    "zoo/quux/"
	    "zoo/quux/pidge.c"
	    "zoo/quux/snoo")))

(defun glob-setup ()
  (make-directory *test-dir*)
  (in-directory (*test-dir*)
    (make-dir-tree *tree-1*))
  (change-directory *test-dir*))

(defun glob-takedown ()
  (change-directory "..")
  (in-directory (*test-dir*)
    (delete-dir-tree *tree-1*))
  (delete-directory *test-dir*))

;; @@@ !!! Need tests for:
;; symbolic links, e.g. (glob "/var/*")
;; unreadable and/or unsearchable dir and subdirs
;; trailing slash: e.g. (glob "*/")

(defun muffle-sort (x)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (sort x #'string<))

(defun p-equal (p1 p2)
  "Test that fixes the paths in the second list."
  (equal p1 (mapcar #'fix-path p2)))

(defun p-glob (string)
  "Glob that fixes the paths."
  (glob (fix-path string)))

(deftests (glob :setup glob-setup
		:takedown glob-takedown
		:doc "Tests of globbing.")
  (p-equal (muffle-sort (p-glob "*oo/b*/*.c"))
	   '("boo/bar/gralt.c" "zoo/baz/bark.c" "zoo/baz/pippy.c"))
  (p-equal (muffle-sort (p-glob "*/*/*.c"))
	   '("boo/bar/gralt.c" "zoo/baz/bark.c" "zoo/baz/pippy.c"
	     "zoo/quux/pidge.c"))
  (p-equal (muffle-sort (p-glob "*/*/*"))
	   '("boo/bar/corge" "boo/bar/gralt.c" "zoo/baz/bark.c"
	     "zoo/baz/foo" "zoo/baz/lemon" "zoo/baz/pippy.c" "zoo/quux/pidge.c"
	     "zoo/quux/snoo"))
  (p-equal (muffle-sort (p-glob "*/*/*[^c]"))
	   '("boo/bar/corge" "zoo/baz/foo" "zoo/baz/lemon" "zoo/quux/snoo"))
  (p-equal (in-directory ("boo") (muffle-sort (p-glob "../*/*/*.c")))
	   '("../boo/bar/gralt.c" "../zoo/baz/bark.c" "../zoo/baz/pippy.c"
	     "../zoo/quux/pidge.c")))

(deftests (globstar :setup glob-setup
		   :takedown glob-takedown
		   :doc "Tests of recursive globbing.")
  (p-equal (muffle-sort (p-glob "z*/**/*.c"))
	   '("zoo/baz/bark.c" "zoo/baz/pippy.c" "zoo/quux/pidge.c"))
  (p-equal (muffle-sort (p-glob "**/*.c"))
	   '("boo/bar/gralt.c" "zoo/baz/bark.c" "zoo/baz/pippy.c"
	     "zoo/quux/pidge.c"))
  ;; (p-equal (muffle-sort (p-glob "**"))
  ;; 	 '("boo/bar/corge" "boo/bar/gralt.c" "zoo/baz/bark.c"
  ;; 	   "zoo/baz/foo" "zoo/baz/lemon" "zoo/baz/pippy.c" "zoo/quux/pidge.c"
  ;; 	   "zoo/quux/snoo"))
  (p-equal (muffle-sort (p-glob "**/*[^c]"))
	   '("boo" "boo/bar" "boo/bar/corge" "boo/baz" "boo/foo" "foo" "one"
	     "two" "zoo" "zoo/bar" "zoo/baz" "zoo/baz/foo" "zoo/baz/lemon"
	     "zoo/baz/pippy.c/fuzz" "zoo/foo" "zoo/quux" "zoo/quux/snoo"))
  (p-equal (in-directory ("boo") (muffle-sort (p-glob "../**/*.c")))
	   '("../boo/bar/gralt.c" "../zoo/baz/bark.c" "../zoo/baz/pippy.c"
	     "../zoo/quux/pidge.c")))

(deftests (glob-all)
  fnmatch-strings fnmatch-qmark fmatch-charset fnmatch-star fnmatch-escape
  glob globstar)

(defun run ()
  (run-group-name 'glob-all :verbose t))

#+unix (d-remove-feature :t-has-system-glob)

;; EOF
