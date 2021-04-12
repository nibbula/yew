;;;
;;; unix/i18n.lisp - Internationalization
;;;

(in-package :opsys-unix)

(declaim #.`(optimize ,.(getf opsys-config::*config* :optimization-settings)))

(defun language ()
  "Return a the system language."
  (let (lang)
    ;; This is lame. I'd like to turn these into something more readable
    ;; to humans, but that seems likely to bitrot and error.
    (cond
      ((setf lang (env "LC_MESSAGES"))
       lang)
      ((setf lang (env "LC_ALL"))
       lang)
      ((setf lang (env "LANG"))
       lang)
      ;; But this is really just for look messages.
      ;; On one hand, the user having more control over what language messages
      ;; are in is good. On the other hand, then we don't have a concept of
      ;; what the system default language is set to.
      ;; Anyway we just take the first one.
      ((setf lang (env "LANGUAGE"))
       (setf lang (initial-span lang ":")))
      ;; (t "Unknown") ;; Or should we just return NIL?
      )))

;; Natural Language info

(defctype nl-item :int
  "Type of items requested from nl_langinfo.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun nl-item-value (category index)
    "Return a nl-item value for INDEX of CATEGORY."
    (logior (ash category 16) index)))

;; From POSIX
(defparameter *nl-items* nil
  "Natural language items available from nl-langinfo.")

;; The order here is important and for GNU libc on linux.
(define-enum-list *nl-items*
  #(#(+ABDAY-1+     "Abbreviated name of the first day of the week.")
    #(+ABDAY-2+     "Abbreviated name of the second day of the week.")
    #(+ABDAY-3+     "Abbreviated name of the third day of the week.")
    #(+ABDAY-4+     "Abbreviated name of the fourth day of the week.")
    #(+ABDAY-5+     "Abbreviated name of the fifth day of the week.")
    #(+ABDAY-6+     "Abbreviated name of the sixth day of the week.")
    #(+ABDAY-7+     "Abbreviated name of the seventh day of the week.")

    #(+DAY-1+       "Name of the first day of the week (for example, Sunday).")
    #(+DAY-2+       "Name of the second day of the week (for example, Monday).")
    #(+DAY-3+       "Name of the third day of the week (for example, Tuesday).")
    #(+DAY-4+       "Name of the fourth day of the week (for example, Wednesday).")
    #(+DAY-5+       "Name of the fifth day of the week (for example, Thursday).")
    #(+DAY-6+       "Name of the sixth day of the week (for example, Friday).")
    #(+DAY-7+       "Name of the seventh day of the week example, Saturday).")

    #(+ABMON-1+     "Abbreviated name of the first month.")
    #(+ABMON-2+     "Abbreviated name of the second month.")
    #(+ABMON-3+     "Abbreviated name of the third month.")
    #(+ABMON-4+     "Abbreviated name of the fourth month.")
    #(+ABMON-5+     "Abbreviated name of the fifth month.")
    #(+ABMON-6+     "Abbreviated name of the sixth month.")
    #(+ABMON-7+     "Abbreviated name of the seventh month.")
    #(+ABMON-8+     "Abbreviated name of the eighth month.")
    #(+ABMON-9+     "Abbreviated name of the ninth month.")
    #(+ABMON-10+    "Abbreviated name of the tenth month.")
    #(+ABMON-11+    "Abbreviated name of the eleventh month.")
    #(+ABMON-12+    "Abbreviated name of the twelfth month.")

    #(+MON-1+       "Name of the first month of the year.")
    #(+MON-2+       "Name of the second month.")
    #(+MON-3+       "Name of the third month.")
    #(+MON-4+       "Name of the fourth month.")
    #(+MON-5+       "Name of the fifth month.")
    #(+MON-6+       "Name of the sixth month.")
    #(+MON-7+       "Name of the seventh month.")
    #(+MON-8+       "Name of the eighth month.")
    #(+MON-9+       "Name of the ninth month.")
    #(+MON-10+      "Name of the tenth month.")
    #(+MON-11+      "Name of the eleventh month.")
    #(+MON-12+      "Name of the twelfth month.")

    #(+AM-STR+      "Ante-meridiem affix.")
    #(+PM-STR+      "Post-meridiem affix.")

    #(+D-T-FMT+     "String for formatting date and time.")
    #(+D-FMT+       "Date format string.")
    #(+T-FMT+       "Time format string.")
    #(+T-FMT-AMPM+  "a.m. or p.m. time format string.")

    #(+ERA+         "Era description segments.")
    #| USE_GNU has an ERA-YEAR in here |#
    #(+ERA-D-FMT+   "Era date format string.")
    #(+ALT-DIGITS+  "Alternative symbols for digits.")
    #(+ERA-D-T-FMT+ "Era date and time format string.")
    #(+ERA-T-FMT+   "Era time format string.")
    )
  :start #.(nl-item-value +LC-TIME+ 0))

(define-enum-list *nl-items*
  #(#(+DECIMAL-POINT+            "Decimal point character.")
    #(+RADIXCHAR+                "Radix character.")
    #(+THOUSEP+                  "Separator for thousands.")
    #(+GROUPING+                 "Grouping")
    #(+NUMERIC-DECIMAL-POINT-WC+ "Decimal point wide character")
    #(+NUMERIC-THOUSANDS-SEP-WC+ "Thousands separator wide character")
    #(+NUMERIC-CODESET+          "Code set")
    )
  :start #.(nl-item-value +LC-NUMERIC+ 0))
  
(define-enum-list *nl-items*
  #(#(+YESEXPR+     "Affirmative response expression.")
    #(+NOEXPR+      "Negative response expression.")
    #(+YESSTR+      "Output string for ‘yes’.")
    #(+NOSTR+       "Output string for ‘no’."))
  :start #.(nl-item-value +LC-MESSAGES+ 0))

(define-enum-list *nl-items*
  #(#(+CRNCYSTR+
    "Local currency symbol, preceded by '-' if the symbol should appear
before the value, '+' if the symbol should appear after the value, or '.'
if the symbol should replace the radix character. If the local currency symbol
is the empty string, implementations may return the empty string ("")."))
  :start #.(nl-item-value +LC-MONETARY+ 0))

;; We treat locale like an opaque pointer, even though it's not.
(defctype locale-t :pointer
  "Type of a unix locale.")

(defcfun ("nl_langinfo_l" nl-langinfo-l) :string
  "Return the string for ITEM from the language catalog for LOCALE."
  (item nl-item) (locale locale-t))

(defcfun ("nl_langinfo" nl-langinfo) :string
  "Return the string for ITEM from the language catalog for the current locale."
  (item nl-item))

;; End
