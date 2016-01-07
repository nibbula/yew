;;;								-*- Lisp -*-
;;; glob.asd -- System definition for glob
;;;

(defpackage :glob-system
    (:use :common-lisp :asdf))

(in-package :glob-system)

(defsystem glob
    :name               "glob"
    :description        "Glops of globby blobby blob glob."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :long-description
    "Shell style file name pattern matching.
Main functions are:
 FNMATCH      - Checks whether a string matches a pattern.
 GLOB         - Returns a list of pathnames that match a pattern.
 PATTERN-P    - Returns true if the string might have pattern characters.
 EXPAND-TILDE - Returns the pathname with ~ home directories expanded.

The documentation for FNMATCH describes the pattern syntax a little.

Another great example of how to poorly reinvent something stupid."
    :depends-on (:opsys :dlib)
    :components
    ((:file "glob"))
    :in-order-to ((asdf:test-op (asdf:test-op :glob-test))))
