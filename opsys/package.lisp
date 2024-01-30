;;;
;;; package.lisp - Package definition for OPSYS
;;;

;; The without-warning is overkill, so be careful. Comment it out to check for
;; real problems. Otherwise, certain complainy implementatations, don't take
;; kindly to us programatically re-exporting things from opsys-base.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Of course we first have to make it be only a warning, before we can
  ;; suppress it.
  (let (#+sbcl (*on-package-variance* '(:warn (:opsys) :error t)))
    (fake-dlib:without-warning
(defpackage :opsys
  (:documentation "Generic interface to operating system functionality.")
  (:nicknames :nos)
  (:use :cl :cffi :fake-dlib :dgray :opsys-base :opsys-generic
        :libc
	#+unix :os-unix
	#+(and windows (not unix)) :os-ms)
  (:export
   ;; errors
   #:error-message
   
   ;; info
   #:environment
   #:environment-variable
   #:env
   #:unsetenv
   #:lisp-args
   #:memory-page-size
   #:processor-count
   #:system-info-names
   #:system-info-description
   #:get-system-info
   #:os-machine-instance
   #:os-machine-type
   #:os-machine-version
   #:os-software-type
   #:os-software-version
   #:os-software-release

   #:get-user-info
   #:user-home
   #:user-name-char-p
   #:valid-user-name
   #:user-name
   #:user-id
   #:user-full-name
   #:get-next-user
   #:user-list
   #:refresh-user-list
   #:is-administrator
   #:users-logged-in

   #:group-name
   #:group-id
   #:get-next-group
   #:group-list
   #:refresh-group-list

   ;; directories
   #:change-directory
   #:current-directory
   #:in-directory
   #:with-working-directory
   #:*default-directory-mode*
   #:make-directory
   #:delete-directory
   #:read-directory
   #:map-directory
   #:dir-entry
   #:dir-entry-p
   #:make-dir-entry
   #:dir-entry-name
   #:dir-entry-type
   #:dir-entry-inode
   #:without-access-errors
   #:probe-directory
   #:directory-p
   #:ensure-directory
   #:split-path
   #:path-to-absolute #:abspath
   #:path-absolute-p #:absolute-path-p
   #:path-directory-name #:dirname
   #:path-file-name #:basename
   #:path-root
   #:path-append #:p+
   #:path-snip-ext
   #:path-extension
   #:path-parent
   #:parse-path
   #:os-pathname
   #:os-pathname-namestring
   #:os-pathname-pathname
   #:hidden-file-name-p
   #:superfluous-file-name-p
   #:command-test
   #:*command-pathname-cache*
   #:command-pathname
   #:command-pathname-cache-remove
   #:command-pathname-cache-clear
   #:command-path-list
   #:list-to-command-path
   #:set-command-path-list
   #:quote-filename
   #:unquote-filename
   #:safe-namestring

   ;; files
   #:file-info
   #:get-file-info ;; Deprecated
   #:file-accessible-p
   #:file-executable-p
   #:stream-system-handle
   #:stream-handle-direction
   #:system-handle-close
   #:file-exists
   #:os-delete-file
   #:os-rename-file
   #:with-os-file
   #:set-file-time
   #:make-symbolic-link
   #:symbolic-link-target

   ;; locking
   #:with-locked-file

   ;; Application paths
   #:data-dir
   #:config-dir
   #:data-path
   #:config-path
   #:cache-dir
   #:runtime-dir
   
   ;; processes
   #:system-command
   #:run-program
   #:pipe-program
   #:with-process-output
   #:suspend-process
   #:resume-process
   #:terminate-process
   #:is-executable
   #:command-pathname
   #:process-times
   #:process-list
   #:process-info
   #:current-process-id
   #:wait-and-chill
   #:check-jobs
   #:os-process-priority
   #:*os-process-most-favorable-priority*
   #:*os-process-least-favorable-priority*
   #:*system-process-type*
   #:system-process-list
   #:system-process-info

   ;; time
   #:get-time
   #:os-time
   #:get-os-time ;; Deprecated
   #:set-time
   #:timezone-name
   #:timezone-offset
   #:fake-time
   #:*time-result*

   ;; events
   #:listen-for
   #:*event-set*
   #:create-event-set
   #:destroy-event-set
   #:with-event-set
   #:add-event
   #:delete-event
   #:clear-triggers
   #:await-events
   #:pick-events
   #:map-events
   #:events-pending-p

   ;; administration
   #:system-power-off
   #:system-restart
   #:system-suspend

   ;; filesystems
   #:mounted-filesystems
   #:get-filesystem-info
   #:mount-point-of-file

   ;; terminals
   #:file-handle-terminal-p
   #:file-handle-terminal-name
   #:open-terminal
   #:close-terminal
   #:slurp-terminal
   #:read-terminal-char
   #:read-terminal-byte
   #:read-until
   #:write-terminal-char
   #:write-terminal-string
   #:set-terminal-mode
   #:get-terminal-mode
   #:get-window-size
   #:reset-terminal-modes
   #:drain-terminal
   #:flush-terminal
   #:terminal-time
   #:terminal-query #:query-terminal
   #:with-terminal-mode
   #:with-terminal-signals
   #:*default-console-device-name*
   
   ;; network
   #:network-host-name
   #:network-domain-name

   ;; character coding / localization (or similar)
   #:char-width
   #:locale-categories
   #:setlocale
   #:setup-locale-from-environment
   #:language

   ;; misc
   #:exit-lisp
   #:missing-implementation

   ;; os-stream
   #:os-stream
   #:os-stream-handle
   #:make-os-stream
   #:make-os-stream-from-handle
   #:with-os-stream

   ;; re-export the junk from libc
   #:*stdin* #:*stdout* #:*stderr*
   #-(and windows (not unix)) #:fileno
   #:fopen #:fclose #:fflush
   #:fgetc #:getc #:getchar #:fgets #:gets
   #:printf #:fprintf #:sprintf #:snprintf
   #:fputc #:putc #:putchar #:fputs #:puts
   #:fread #:fwrite
   #:fscanf #:scanf #:sscanf
   #:fsetpos #:fgetpos #:fseek #:ftell
   #:perror #:setbuf #:ungetc

   ;; ctype
   #-(and windows (not unix)) #:iswblank
   #:iswalnum #:iswalpha #:iswascii #:iswcntrl #:iswdigit
   #:iswgraph #:iswhexnumber #:iswideogram #:iswlower #:iswnumber
   #:iswphonogram #:iswprint #:iswpunct #:iswrune #:iswspace #:iswspecial
   #:iswupper #:iswxdigit

   #-(and windows (not unix)) #:isascii
   #-(and windows (not unix)) #:isblank
   #:isalnum #:isalpha #:iscntrl #:isdigit #:isgraph
   #:ishexnumber #:isideogram #:islower #:isnumber #:isphonogram #:isprint
   #:ispunct #:isrune #:isspace #:isspecial #:isupper #:isxdigit

   ;; i18n
   #:locale-categories
   #:lc-category
   #:+LC-ALL+
   #:+LC-COLLATE+
   #:+LC-CTYPE+
   #:+LC-MONETARY+
   #:+LC-NUMERIC+
   #:+LC-TIME+
   #:+LC-MESSAGES+
   #:+LC-COLLATE+
   #:+LC-MESSAGES+
   #:+LC-PAPER+
   #:+LC-NAME+
   #:+LC-ADDRESS+
   #:+LC-TELEPHONE+
   #:+LC-MEASUREMENT+
   #:+LC-IDENTIFICATION+
   #:+LC-LAST+
   #:setlocale

   ;; stdlib
   #:system
   ))
))) ;; without-warning

;; Re-export things from opsys-base
(do-external-symbols (sym :opsys-base)
  (export sym :opsys))

;; Re-export things from opsys-generic
(do-external-symbols (sym :opsys-generic)
  (export sym :opsys))

;; End
