;;
;; ms/package.lisp - Interface to Microsoft systems.
;;

;; Notes:
;;  - Don't try to apdapt Unix concepts to windows (e.g. signals), or vice
;;    versa. Find a generic concept that works on both systems, or if the
;;    faciliity really doesn't exist, only provide system specific versions.
;;
;;  - Imagine you are creating a hypothetical new operating system that has
;;    the features of both, but is better, and has a better lispy API.
;;    Especially don't be biased by Unix's (or Windows') historical
;;    crumminess. Accept that sometimes, Windows (or Unix, or neither) has the
;;    better way. This really applies to the whole OPSYS system and not just
;;    this package.

;; Conventions:
;;   - Names that conflict should be given the prefix "MS-".
;;   - Type names should try to follow Microsoft style, because they're quite
;;     terse, complicated and could be very confusing otherwise.
;;   - Slot, function, constant, and variable names are much nicer to deal with
;;     when converted to Lisp hyphenated and earmuffed style.
;;   - We generally convert Windows interface function names from StudlyCaps to
;;     %hyphenated-identifier-style, and perhaps provide a function without
;;     the '%' for calling from other Lisp code.

(defpackage :opsys-ms
  (:documentation "Interface to Microsoft systems.")
  (:use :cl :cffi :dlib :opsys-base)
  (:nicknames :os-ms :wos)
  (:export
   ;; Things that opsys imports:
   #:error-message
   #:environment
   #:environment-variable
   #:env
   #:memory-page-size
   #:processor-count
   #:get-user-info
   #:user-name
   #:user-home
   #:user-id
   #:user-full-name
   #:user-name-char-p
   #:valid-user-name
   #:get-next-user
   #:user-list
   #:refresh-user-list
   #:is-administrator
   #:users-logged-in
   #:get-file-info
   #:file-exists
   #:os-delete-file
   #:with-os-file
   #:set-file-time
   #:make-symbolic-link
   #:read-directory
   #:map-directory
   #:change-directory
   #:current-directory
   #:make-directory
   #:delete-directory
   #:probe-directory
   #:directory-p
   #:without-access-errors
   #:hidden-file-name-p
   #:superfluous-file-name-p
   #:%path-absolute-p
   #:lock-file
   #:unlock-file
   #:with-locked-file
   #:is-executable
   #:config-dir
   #:data-path
   #:data-dir
   #:config-path
   #:cache-dir
   #:runtime-dir
   #:suspend-process
   #:resume-process
   #:terminate-process
   #:process-times
   #:process-list
   #:system-process-list
   #:system-process-type
   #:wait-and-chill
   #:check-jobs
   #:get-time
   #:set-time
   #:listen-for
   #:%create-event-set
   #:%destroy-event-set
   #:%add-event
   #:%delete-event
   #:%clear-triggers
   #:await-events
   #:pick-events
   #:map-events
   #:events-pending-p
   #:mounted-filesystems
   #:mount-point-of-file
   #:file-handle-terminal-p
   #:file-handle-terminal-name
   #:*default-console-device-name*
   #:open-terminal
   #:close-terminal
   #:read-terminal-char
   #:read-terminal-byte
   #:read-until
   #:listen-for-terminal
   #:write-terminal-char
   #:write-terminal-string
   #:slurp-terminal
   #:set-terminal-mode
   #:get-terminal-mode
   #:reset-terminal-modes
   #:terminal-query
   #:with-terminal-signals
   ;; Extra Windows specific stuff:
   #:windows-error
   #:+ERROR-FILE-NOT-FOUND+
   #:+ERROR-PATH-NOT-FOUND+
   #:+ERROR-NOT-READY+
   #:+ERROR-ENVVAR-NOT-FOUND+
   #:get-binary-type #:*binary-types*
   #:binary-type-description
   #:ms-process-handle
   #:get-command-line
   #:get-computer-name
   ;; Console stuff:
   #:get-console-info
   #:get-window-size
   #:get-cursor-position
   #:get-cursor-info
   #:set-cursor-state
   #:set-cursor-position
   #:scroll-console
   #:fill-console-char
   #:fill-console-attribute
   #:get-attributes
   #:set-console-attribute
   #:+FOREGROUND-BLUE+ #:+FOREGROUND-GREEN+ #:+FOREGROUND-RED+
   #:+FOREGROUND-INTENSITY+ #:+BACKGROUND-BLUE+ #:+BACKGROUND-GREEN+
   #:+BACKGROUND-RED+ #:+BACKGROUND-INTENSITY+
   #:get-console-title #:set-console-title
   #:get-computer-name
   #:os-machine-instance
   #:os-machine-type
   #:os-machine-version
   #:os-software-type
   #:os-software-version
   ))
(in-package :opsys-ms)

;; End
