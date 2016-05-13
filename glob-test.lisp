;;
;; glob-test.lisp - Test glob package.
;;

(defpackage :glob-test
  (:documentation "Test glob package")
  (:use :cl :test :glob :opsys)
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
  "This one was suggested by a user."
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

(cffi:defcstruct foreign-glob-t
  (gl_pathc nos:size-t)
  (gl_matchc :int)
  (gl_offs nos:size-t)
  (gl_flags :int)
  (gl_pathv (:pointer :string)) ;; char **
  ;; We don't really care about all those callback functions.
  (gl_errfunc :pointer)   ;; int (*gl_errfunc)(const char *, int);
  (gl_closedir :pointer)  ;; void (*gl_closedir)(void *);
  (gl_readdir :pointer)   ;; struct dirent *(*gl_readdir)(void *);
  (gl_opendir :pointer)   ;; void *(*gl_opendir)(const char *);
  (gl_lstat :pointer)     ;; int (*gl_lstat)(const char *, struct stat *);
  (gl_stat :pointer)      ;; int (*gl_stat)(const char *, struct stat *);
  )

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
	 :collect (cffi:mem-aref gl_pathv :string i)))))

(defparameter *test-dir* "zxcv")

(defun touch (name)
  (with-open-file (stm name :direction :output)
    (format stm "foo~%")))

(defun glob-setup ()
  (make-directory *test-dir*)
  (in-directory (*test-dir*)
    (mapcar #'touch '("foo" "one" "two"))
    (mapcar #'make-directory '("boo" "zoo"))
    (in-directory ("boo")
      (mapcar #'touch '("baz" "foo"))
      (make-directory "bar")
      (in-directory ("bar")
	(mapcar #'touch '("corge" "gralt.c"))))
    (in-directory ("zoo")
      (mapcar #'touch '("bar" "foo"))
      (mapcar #'make-directory '("baz" "quux"))
      (in-directory ("baz")
	(mapcar #'touch '("lemon" "foo" "bark.c")))
      (in-directory ("quux")
	(mapcar #'touch '("pidge.c" "snoo")))))
  (change-directory *test-dir*))

(defun glob-takedown ()
  (change-directory "..")
  (in-directory (*test-dir*)
    (mapcar #'delete-file '("boo/bar/corge" "boo/bar/gralt.c" "boo/baz"
			    "boo/foo" "zoo/bar" "zoo/foo" "zoo/baz/lemon"
			    "zoo/baz/foo" "zoo/baz/bark.c" "zoo/quux/pidge.c"
			    "zoo/quux/snoo" "foo" "one" "two"))
    (mapcar #'delete-directory '("boo/bar" "boo" "zoo/baz" "zoo/quux" "zoo")))
  (delete-directory *test-dir*))

;; @@@ !!! Need tests for:
;; symbolic links, e.g. (glob "/var/*")
;; unreadable and/or unsearchable dir and subdirs
;; trailing slash: e.g. (glob "*/")

(deftests (glob :setup glob-setup
		:takedown glob-takedown
		:doc "Tests of globbing.")
  (equal (glob "*oo/b*/*.c") '("boo/bar/gralt.c" "zoo/baz/bark.c"))
  (equal (glob "*/*/*.c") '("boo/bar/gralt.c" "zoo/baz/bark.c"
			    "zoo/quux/pidge.c"))
  (equal (glob "*/*/*") '("boo/bar/corge" "boo/bar/gralt.c" "zoo/baz/bark.c"
			  "zoo/baz/foo" "zoo/baz/lemon" "zoo/quux/pidge.c"
			  "zoo/quux/snoo"))
  (equal (glob "*/*/*[^c]")
	 '("boo/bar/corge" "zoo/baz/foo" "zoo/baz/lemon" "zoo/quux/snoo"))
  (equal (in-directory ("boo") (glob "../*/*/*.c"))
	 '("../boo/bar/gralt.c" "../zoo/baz/bark.c" "../zoo/quux/pidge.c"))
)

(deftests (glob-all)
  fnmatch-strings fnmatch-qmark fmatch-charset fnmatch-star fnmatch-escape
  glob)

(defun run ()
  (run-group-name 'glob-all :verbose t))

;; EOF
