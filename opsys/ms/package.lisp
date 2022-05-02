;;;
;;; ms/package.lisp - Interface to Microsoft systems.
;;;

;; Notes:
;;
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
;;
;; Conventions:
;;
;;   - Names that conflict should be given the prefix "MS-".
;;   - Type names should try to follow Microsoft style, because they're quite
;;     terse, complicated and could be very confusing otherwise.
;;   - Slot, function, constant, and variable names are converted to Lisp
;;     hyphenated and earmuffed style. Note that this is a different convention 
;;     from the Unix code, but that's how much I hate CamelCase.
;;   - We generally convert Windows interface function names from CamelCase to
;;     %hyphenated-identifier-style, and perhaps provide a function without
;;     the '%' for calling from other Lisp code.

(defpackage :opsys-ms
  (:documentation "Interface to Microsoft systems.")
  (:use :cl :cffi :dlib :opsys-base :opsys-generic)
  (:nicknames :os-ms :wos)
  (:export
   ;; Things that opsys imports:
   #:error-message
   #:environment
   #:environment-variable
   #:env
   #:memory-page-size
   #:processor-count
   #:system-info-names
   #:system-info-description
   #:get-system-info

   ;; users
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
   #:file-info
   #:file-exists
   #:os-delete-file
   #:with-os-file
   #:set-file-time
   #:make-symbolic-link
   #:symbolic-link-target
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
   #:path-to-absolute
   #:parse-path
   #:os-pathname-namestring
   #:os-pathname-pathname
   #:lock-file
   #:unlock-file
   #:with-locked-file
   #:is-executable
   #:command-test
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
   #:get-time-zone-information
   #:timezone-name
   #:timezone-offset
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
   #:get-filesystem-info
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
   #:with-nonblocking-io
   #:terminal-time
   #:terminal-query
   #:terminal-read-timeout
   #:with-terminal-signals
   ;; Extra Windows specific stuff:
   #:windows-error
   #:+ERROR-FILE-NOT-FOUND+
   #:+ERROR-PATH-NOT-FOUND+
   #:+ERROR-NOT-READY+
   #:+ERROR-SHARING-VIOLATION+
   #:+ERROR-ENVVAR-NOT-FOUND+
   #:get-binary-type #:*binary-types*
   #:binary-type-description
   #:ms-process-handle
   #:get-command-line
   #:get-computer-name
   ;; Console stuff:
   #:get-console-info
   #:get-console-extended-info
   #:get-console-all-extended-info
   #:set-console-all-extended-info
   #:get-console-colors
   #:set-console-colors
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
   #:os-software-release
   #:os-software-version
   ;; os-stream
   #:ms-stream
   #:ms-input-stream
   #:ms-output-stream
   #:ms-io-stream
   #:ms-character-input-stream
   #:ms-character-output-stream
   #:ms-character-io-stream
   #:ms-binary-input-stream
   #:ms-binary-output-stream
   #:ms-binary-io-stream
   #:fill-buffer
   #:flush-buffer
   #:stream-handle-direction
   ))
(in-package :opsys-ms)

;; End
