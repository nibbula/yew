;;;
;;; package.lisp - Package definition for OPSYS
;;;

;; The without-warning is overkill, so be careful. Comment it out to check for
;; real problems. Otherwise, certain complainy implementatations, don't take
;; kindly to us re-exporting things from opsys-base.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (dlib:without-warning
(defpackage :opsys
  (:documentation "Generic interface to operating system functionality.")
  (:nicknames :nos)
  (:use :cl :cffi :dlib :opsys-base
	#+unix :os-unix
	#+(and windows (not unix)) :os-ms)
  (:export
   ;; errors
   #:error-message
   
   ;; info
   #:environment
   #:environment-variable
   #:lisp-args
   #:memory-page-size
   #:processor-count
   #:system-info-names
   #:system-info-description
   #:get-system-info

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
   #:split-path
   #:path-to-absolute #:abspath
   #:path-absolute-p #:absolute-path-p
   #:path-directory-name #:dirname
   #:path-file-name #:basename
   #:path-append
   #:path-snip-ext
   #:path-extension
   #:hidden-file-name-p
   #:superfluous-file-name-p
   #:command-pathname
   #:command-path-list
   #:list-to-command-path
   #:set-command-path-list
   #:quote-filename
   #:safe-namestring

   ;; files
   #:get-file-info
   #:stream-system-handle
   #:file-exists
   #:simple-delete-file
   #:with-os-file
   #:set-file-time

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
   #:*system-process-type*
   #:system-process-list
   #:system-process-info

   ;; time
   #:get-time
   #:get-os-time
   #:set-time

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

   ;; filesystems
   #:mounted-filesystems
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
   #:terminal-query
   #:with-terminal-mode
   #:with-terminal-signals
   #:*default-console-device-name*
   
   ;; character coding / localization (or similar)
   #:char-width
   #:setlocale
   #:setup-locale-from-environment

   ;; misc
   #:exit-lisp
   #:missing-implementation

   ;; stdio
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

   ;; stdlib
   #:system
   ))
)) ;; without-warning

;; Re-export things from opsys-base

(do-external-symbols (sym :opsys-base)
  (export sym :opsys))

;; End
