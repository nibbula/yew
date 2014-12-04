;;							-*- Lisp -*- (for now)
;; .lishrc - Lisp shell initialization
;;

;; $Revision: 1.12 $

;; Aliases

; (let* ((ls (getenv "LS"))
;        (color-term (getenv "COLOR_TERM"))
;        (ls-version (shell-line "ls" "--version"))
;        (is-gnu (and ls-version
; 		    (loop named poo
; 		      for l in (shell-lines "ps")
; 		      do (if (search "richard" l :test #'equalp)
; 			     (return-from poo t)))))
;        (ls-opt (if (and color-term
; 			(or is-gnu (equal ls "gls")))
; 		   " --color=auto"))
;        (vi (getenv "VI")))
;   (when vi
;     {alias vi }))

(defvar *use-gnu-ls* t "True to use the GNU version of ls.")
(defvar *ls* "ls" "The name of the ls executable.")

(cond
  ((equalp *os* "Darwin")
   (setf *use-gnu-ls* (not (null (command-pathname "gls"))))
   (setf *ls* "gls")
   (!alias "ldd"  "otool -L")
   (!alias "top" "top -u"))
  ((equalp *os* "solaris")
   (setf *use-gnu-ls* t)
   (setf *ls* "gls"))
  ((equalp *os* "Linux")
   (setf *use-gnu-ls* t)
   (setf *ls* "ls")))

(cond
  (*use-gnu-ls*
   (!alias "ls"   (s+ *ls* " -CF --color=auto"))
   (!alias "lss"  (s+ *ls* " -CFs --color=auto"))
   (!alias "ll"   (s+ *ls* " -CFlg --color=auto"))
   (!alias "lla"  (s+ *ls* " -CFlag --color=auto"))
   (!alias "lld"  (s+ *ls* " -CFlgd --color=auto"))
   (!alias "lldd" (s+ *ls* " -CFlg --color=auto | grep '^d'")))
  (t
   (!alias "ls"   "ls -GF")
   (!alias "lss"  "ls -GFs")
   (!alias "ll"   "ls -GFl")
   (!alias "lla"  "ls -GFla")
   (!alias "lld"  "ls -GFld")
   (!alias "lldd" "ls -GFl | grep '^d'")))

;alias m   less
alias m   pager
alias zm  zmore
alias cx  "chmod +x"
;alias df  "df -k"
alias ns  "netstat -a -f inet | pager"
alias nsn "netstat -an -f inet | pager"
alias pst "pstree -g3 -w | pager"
alias ta  "type -a"
alias tp  "type -p"
alias j   "jobs"
alias fg  "resume"

alias pd pushd
alias up  "cd .."
alias uup "cd ../.."
alias uuup "cd ../../.."
alias uuuup "cd ../../../.."
alias uuuuup "cd ../../../../.."
alias back "cd !*old-pwd*"
alias down "cd !*old-pwd*"

alias t  "date +%r"
alias dt "date \"+%a %h %d (%D) %T\""
alias du "du -k"
alias q  "quota -v"
alias ng "unset DISPLAY"
alias muca "cvs -n -q update"
alias st "stty everything"
alias xvv "xv -vsgeometry 610x796+660+12 -geometry +0+10"

;; Stuff for customized prompt.

(defun fixed-homedir ()
  "(user-homedir-pathname) with a trailing slash removed if there was one."
  (let ((h (namestring (user-homedir-pathname))))
    (if (equal #\/ (aref h (1- (length h))))
	(subseq h 0 (1- (length h)))
	h)))

(defun twiddlify (name)
  "Turn (user-homedir-pathname) occuring in name into a tilde."
  (replace-subseq (namestring (fixed-homedir)) "~"
		  (namestring name) :count 1))

(defun my-prompt (sh)
  (declare (special lish::*lish-level*))
  (format nil "~a:~a~a " *host* (twiddlify (nos:current-directory))
 	  (make-string (+ 1 lish::*lish-level*)
 		       :initial-element
		       (if (= 0 (opsys:getuid))
			   #\#
			   (lish::lish-prompt-char sh)))))

(setf (lish:lish-prompt-function lish::*shell*) #'my-prompt)

;; Shell convenience functions


;; Actually useful commands

(defclass arg-system-designator (arg-keyword)
  ()
  (:documentation
   "A system designator, either a keyword or an ASDF/SYSTEM:SYSTEM"))

(defmethod convert-arg ((arg arg-system-designator) value)
  (format t "Converting a system-designator ~s~%" value)
  (if (and (stringp value) (char= (char value 0) #\:))
      (intern (string-upcase (subseq value 1)) (find-package :keyword))
      value))

(defcommand l (("system" system-designator :optional nil))
  "Load a system."
  ;; @@@ it's a string not a keyword since we haven't implemented the 
  ;; asdf-system argument type.
  (asdf:oos 'asdf:load-op system))

(defun title (&optional string)
  "Set the title of a terminal window. The terminal is assumed to work like
XTerm or something."
  (format t "~c]0;~a~c" #\escape
	  (or string (progn (princ "Title? ") (finish-output) (read-line)))
	  #\^G))

(defcommand title (("string" string))
  "Change the title of an xterm compatible terminal window."
  (title string))

(defun snip-ext (file)
  (let ((pos (position #\. file :from-end t)))
    (if (and pos (/= pos 0)) (subseq file 0 pos) file)))

(defcommand yuto (("file" filename))
  "Convert mp4 downloaded from youtube (with youtubedown) to audio only."
  (! "ffmpeg -i \"" file "\" -acodec copy -vn \"" (snip-ext file) ".m4a\""))

(defcommand tt (("hours" integer :default 0))
  (princ (format-date "~2,'0d:~2,'0d ~a~%" (:12hr :min :pm)
		      :time (+ (get-universal-time) (hours hours)))))

;; @@@ This needs work
(defcommand man
   (("apropos" string  :short-arg #\k)
    ("section" string  :short-arg #\s)
    ("all"     boolean :short-arg #\a)
    ("entry"   string  :optional t))
  (let ((cmd "/usr/bin/man -P cat "))
    (when apropos (setf cmd (s+ cmd " -k " apropos)))
    (when all     (setf cmd (s+ cmd " -a ")))
    (when section (setf cmd (s+ cmd section " ")))
    (pager:pager (!! cmd (or entry "")))))

;; EOF
