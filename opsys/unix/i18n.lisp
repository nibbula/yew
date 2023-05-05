;;;
;;; unix/i18n.lisp - Internationalization
;;;

(in-package :opsys-unix)

(declaim #.`(optimize ,.(getf opsys-config::*config* :optimization-settings)))

(defun language ()
  "Return the system language."
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

;; The order quite here is important and for GNU libc on linux.
;; Constants starting with a #\- seem to be trouble or wrong.

#+linux
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
    #(+ERA-YEAR+    "Era date format string.")
    #(+ERA-D-FMT+   "Era date format string.")
    #(+ALT-DIGITS+  "Alternative symbols for digits.")
    #(+ERA-D-T-FMT+ "Era date and time format string.")
    #(+ERA-T-FMT+   "Era time format string.")

    #(+NL-TIME-ERA-NUM-ENTRIES+ "")
    #(+NL-TIME-ERAENTRIES+ "")

    #(+NL-WABDAY-1+ "")
    #(+NL-WABDAY-2+ "")
    #(+NL-WABDAY-3+ "")
    #(+NL-WABDAY-4+ "")
    #(+NL-WABDAY-5+ "")
    #(+NL-WABDAY-6+ "")
    #(+NL-WABDAY-7+ "")

    ;; Long-named days of the week.
    #(+NL-WDAY-1+ "Sunday")
    #(+NL-WDAY-2+ "Monday")
    #(+NL-WDAY-3+ "Tuesday")
    #(+NL-WDAY-4+ "Wednesday")
    #(+NL-WDAY-5+ "Thursday")
    #(+NL-WDAY-6+ "Friday")
    #(+NL-WDAY-7+ "Saturday")

    #(+NL-WABMON-1+ "")
    #(+NL-WABMON-2+ "")
    #(+NL-WABMON-3+ "")
    #(+NL-WABMON-4+ "")
    #(+NL-WABMON-5+ "")
    #(+NL-WABMON-6+ "")
    #(+NL-WABMON-7+ "")
    #(+NL-WABMON-8+ "")
    #(+NL-WABMON-9+ "")
    #(+NL-WABMON-10+ "")
    #(+NL-WABMON-11+ "")
    #(+NL-WABMON-12+ "")

    #(+NL-WMON-1+ "")
    #(+NL-WMON-2+ "")
    #(+NL-WMON-3+ "")
    #(+NL-WMON-4+ "")
    #(+NL-WMON-5+ "")
    #(+NL-WMON-6+ "")
    #(+NL-WMON-7+ "")
    #(+NL-WMON-8+ "")
    #(+NL-WMON-9+ "")
    #(+NL-WMON-10+ "")
    #(+NL-WMON-11+ "")
    #(+NL-WMON-12+ "")

    #(+NL-WAM-STR+      "Ante meridiem string.")
    #(+NL-WPM-STR+      "Post meridiem string.")

    #(+NL-WD-T-FMT+     "Date and time format for strftime.")
    #(+NL-WD-FMT+       "Date format for strftime.")
    #(+NL-WT-FMT+       "Time format for strftime.")
    #(+NL-WT-FMT-AMPM+  "12-hour time format for strftime.")

    #(+NL-WERA-YEAR+	"Year in alternate era format.")
    #(+NL-WERA-D-FMT+	"Date in alternate era format.")
    #(+NL-WALT-DIGITS+	"Alternate symbols for digits.")
    #(+NL-WERA-D-T-FMT+	"Date and time in alternate era format.")
    #(+NL-WERA-T-FMT+	"Time in alternate era format.")

    #(+NL-TIME-WEEK-NDAYS+ "")
    #(-NL-TIME-WEEK-1STDAY+ "")
    #(+NL-TIME-WEEK-1STWEEK+ "")
    #(+NL-TIME-FIRST-WEEKDAY+ "")
    #(+NL-TIME-FIRST-WORKDAY+ "")
    #(+NL-TIME-CAL-DIRECTION+ "")
    #(+NL-TIME-TIMEZONE+ "")

    #(+DATE-FMT+	"strftime format for date.")
    #(+NL-W-DATE-FMT+ "")
    #(+NL-TIME-CODESET+ "")

    #(+ALTMON-1+ "")
    #(+ALTMON-2+ "")
    #(+ALTMON-3+ "")
    #(+ALTMON-4+ "")
    #(+ALTMON-5+ "")
    #(+ALTMON-6+ "")
    #(+ALTMON-7+ "")
    #(+ALTMON-8+ "")
    #(+ALTMON-9+ "")
    #(+ALTMON-10+ "")
    #(+ALTMON-11+ "")
    #(+ALTMON-12+ "")

    #(+NL-WALTMON-1+ "")
    #(+NL-WALTMON-2+ "")
    #(+NL-WALTMON-3+ "")
    #(+NL-WALTMON-4+ "")
    #(+NL-WALTMON-5+ "")
    #(+NL-WALTMON-6+ "")
    #(+NL-WALTMON-7+ "")
    #(+NL-WALTMON-8+ "")
    #(+NL-WALTMON-9+ "")
    #(+NL-WALTMON-10+ "")
    #(+NL-WALTMON-11+ "")
    #(+NL-WALTMON-12+ "")

    #(+NL-ABALTMON-1+ "")
    #(+NL-ABALTMON-2+ "")
    #(+NL-ABALTMON-3+ "")
    #(+NL-ABALTMON-4+ "")
    #(+NL-ABALTMON-5+ "")
    #(+NL-ABALTMON-6+ "")
    #(+NL-ABALTMON-7+ "")
    #(+NL-ABALTMON-8+ "")
    #(+NL-ABALTMON-9+ "")
    #(+NL-ABALTMON-10+ "")
    #(+NL-ABALTMON-11+ "")
    #(+NL-ABALTMON-12+ "")

    #(+NL-WABALTMON-1+ "")
    #(+NL-WABALTMON-2+ "")
    #(+NL-WABALTMON-3+ "")
    #(+NL-WABALTMON-4+ "")
    #(+NL-WABALTMON-5+ "")
    #(+NL-WABALTMON-6+ "")
    #(+NL-WABALTMON-7+ "")
    #(+NL-WABALTMON-8+ "")
    #(+NL-WABALTMON-9+ "")
    #(+NL-WABALTMON-10+ "")
    #(+NL-WABALTMON-11+ "")
    #(+NL-WABALTMON-12+ "")
    )
  :start #.(nl-item-value +LC-TIME+ 0))

#+linux
(define-enum-list *nl-items*
  #(
   #(+NL-COLLATE-NRULES+ "")
   #(+NL-COLLATE-RULESETS+ "")
   #(+NL-COLLATE-TABLEMB+ "")
   #(+NL-COLLATE-WEIGHTMB+ "")
   #(+NL-COLLATE-EXTRAMB+ "")
   #(+NL-COLLATE-INDIRECTMB+ "")
   #(+NL-COLLATE-GAP1+ "")
   #(+NL-COLLATE-GAP2+ "")
   #(+NL-COLLATE-GAP3+ "")
   #(+NL-COLLATE-TABLEWC+ "")
   #(+NL-COLLATE-WEIGHTWC+ "")
   #(+NL-COLLATE-EXTRAWC+ "")
   #(+NL-COLLATE-INDIRECTWC+ "")
   #(+NL-COLLATE-SYMB-HASH-SIZEMB+ "")
   #(+NL-COLLATE-SYMB-TABLEMB+ "")
   #(+NL-COLLATE-SYMB-EXTRAMB+ "")
   #(+NL-COLLATE-COLLSEQMB+ "")
   #(+NL-COLLATE-COLLSEQWC+ "")
   #(+NL-COLLATE-CODESET+ "")
    )
  :start #.(nl-item-value +LC-COLLATE+ 0))

#+linux
(define-enum-list *nl-items*
  #(#(+DECIMAL-POINT+            "Decimal point character.")
    #(+RADIXCHAR+                "Radix character.")
    #(+THOUSEP+                  "Separator for thousands.")
    #(-GROUPING+                 "Grouping")
    #(+NUMERIC-DECIMAL-POINT-WC+ "Decimal point wide character")
    #(+NUMERIC-THOUSANDS-SEP-WC+ "Thousands separator wide character")
    #(+NUMERIC-CODESET+          "Code set")
    )
  :start #.(nl-item-value +LC-NUMERIC+ 0))
  
#+linux
(define-enum-list *nl-items*
  #(#(+YESEXPR+     "Affirmative response expression.")
    #(+NOEXPR+      "Negative response expression.")
    #(+YESSTR+      "Output string for ‘yes’.")
    #(+NOSTR+       "Output string for ‘no’.")
    #(+NL-MESSAGES-CODESET+ "")
    )
  :start #.(nl-item-value +LC-MESSAGES+ 0))

#+linux
(define-enum-list *nl-items*
  #(#(+CRNCYSTR+
      "Local currency symbol, preceded by '-' if the symbol should appear
before the value, '+' if the symbol should appear after the value, or '.'
if the symbol should replace the radix character. If the local currency symbol
is the empty string, implementations may return the empty string ("").")
    #(+INT-CURR-SYMBO+ "")
    #(+CURRENCY-SYMBOL+ "")
    #(+MON-DECIMAL-POINT+ "")
    #(+MON-THOUSANDS-SEP+ "")
    #(+MON-GROUPING+ "")
    #(+POSITIVE-SIGN+ "")
    #(-NEGATIVE-SIGN+ "")
    #(-INT-FRAC-DIGITS+ "")
    #(-FRAC-DIGITS+ "")
    #(-P-CS-PRECEDES+ "")
    #(-P-SEP-BY-SPACE+ "")
    #(-N-CS-PRECEDES+ "")
    #(-N-SEP-BY-SPACE+ "")
    #(-P-SIGN-POSN+ "")
    #(-N-SIGN-POSN+ "")
    #(-NL-MONETARY-CRNCYSTR+ "")
    #(-INT-P-CS-PRECEDES+ "")
    #(-INT-P-SEP-BY-SPACE+ "")
    #(-INT-N-CS-PRECEDES+ "")
    #(-INT-N-SEP-BY-SPACE+ "")
    #(-INT-P-SIGN-POSN+ "")
    #(-INT-N-SIGN-POSN+ "")
    #(-NL-MONETARY-DUO-INT-CURR-SYMBOL+ "")
    #(-NL-MONETARY-DUO-CURRENCY-SYMBOL+ "")
    #(-NL-MONETARY-DUO-INT-FRAC-DIGITS+ "")
    #(-NL-MONETARY-DUO-FRAC-DIGITS+ "")
    #(-NL-MONETARY-DUO-P-CS-PRECEDES+ "")
    #(-NL-MONETARY-DUO-P-SEP-BY-SPACE+ "")
    #(-NL-MONETARY-DUO-N-CS-PRECEDES+ "")
    #(-NL-MONETARY-DUO-N-SEP-BY-SPACE+ "")
    #(-NL-MONETARY-DUO-INT-P-CS-PRECEDES+ "")
    #(-NL-MONETARY-DUO-INT-P-SEP-BY-SPACE+ "")
    #(-NL-MONETARY-DUO-INT-N-CS-PRECEDES+ "")
    #(-NL-MONETARY-DUO-INT-N-SEP-BY-SPACE+ "")
    #(-NL-MONETARY-DUO-P-SIGN-POSN+ "")
    #(-NL-MONETARY-DUO-N-SIGN-POSN+ "")
    #(-NL-MONETARY-DUO-INT-P-SIGN-POSN+ "")
    #(-NL-MONETARY-DUO-INT-N-SIGN-POSN+ "")
    #(-NL-MONETARY-UNO-VALID-FROM+ "")
    #(-NL-MONETARY-UNO-VALID-TO+ "")
    #(-NL-MONETARY-DUO-VALID-FROM+ "")
    #(-NL-MONETARY-DUO-VALID-TO+ "")
    #(-NL-MONETARY-CONVERSION-RATE+ "")
    #(-NL-MONETARY-DECIMAL-POINT-WC+ "")
    #(-NL-MONETARY-THOUSANDS-SEP-WC+ "")
    ;; #(+NL-MONETARY-CODESET+ "")
    ;; #(+NL-NUM-LC-MONETARY+ "")
    )
  :start #.(nl-item-value +LC-MONETARY+ 0))

#+linux
(define-enum-list *nl-items*
  #(#(-PAPER-HEIGHT+     "")
    #(-PAPER-WIDTH+      "")
    #(-PAPER-CODESET+    ""))
  :start #.(nl-item-value +LC-PAPER+ 0))

#+linux
(define-enum-list *nl-items*
  #(#(+NL-NAME-NAME-FMT+ "")
    #(+NL-NAME-NAME-GEN+ "")
    #(+NL-NAME-NAME-MR+ "")
    #(+NL-NAME-NAME-MRS+ "")
    #(+NL-NAME-NAME-MISS+ "")
    #(+NL-NAME-NAME-MS+ "")
    #(+NL-NAME-CODESET+ ""))
  :start #.(nl-item-value +LC-NAME+ 0))

#+linux
(define-enum-list *nl-items*
    #(#(+NL-ADDRESS-POSTAL-FMT+ "")
      #(+NL-ADDRESS-COUNTRY-NAME+ "")
      #(+NL-ADDRESS-COUNTRY-POST+ "")
      #(+NL-ADDRESS-COUNTRY-AB2+ "")
      #(+NL-ADDRESS-COUNTRY-AB3+ "")
      #(+NL-ADDRESS-COUNTRY-CAR+ "")
      #(+NL-ADDRESS-COUNTRY-NUM+ "")
      #(+NL-ADDRESS-COUNTRY-ISBN+ "")
      #(+NL-ADDRESS-LANG-NAME+ "")
      #(+NL-ADDRESS-LANG-AB+ "")
      #(+NL-ADDRESS-LANG-TERM+ "")
      #(+NL-ADDRESS-LANG-LIB+ "")
      #(+NL-ADDRESS-CODESET+ "")
      ;; #(+NL-NUM-LC-ADDRESS+ "")
      )
  :start #.(nl-item-value +LC-ADDRESS+ 0))

#+linux
(define-enum-list *nl-items*
    #(#(+NL-TELEPHONE-TEL-INT-FMT+ "")
      #(+NL-TELEPHONE-TEL-DOM-FMT+ "")
      #(+NL-TELEPHONE-INT-SELECT+ "")
      #(+NL-TELEPHONE-INT-PREFIX+ "")
      #(+NL-TELEPHONE-CODESET+ "")
      ;; #(+NL-NUM-LC-TELEPHONE+ "")
      )
  :start #.(nl-item-value +LC-TELEPHONE+ 0))

#+linux
(define-enum-list *nl-items*
    #(#(+NL-MEASUREMENT-MEASUREMENT+ "")
      #(+NL-MEASUREMENT-CODESET+ "")
      ;; #(+NL-NUM-LC-MEASUREMENT+ "")
      )
  :start #.(nl-item-value +LC-MEASUREMENT+ 0))

#+linux
(define-enum-list *nl-items*
  #(#(+NL-IDENTIFICATION-TITLE+ "")
    #(+NL-IDENTIFICATION-SOURCE+ "")
    #(+NL-IDENTIFICATION-ADDRESS+ "")
    #(+NL-IDENTIFICATION-CONTACT+ "")
    #(+NL-IDENTIFICATION-EMAIL+ "")
    #(+NL-IDENTIFICATION-TEL+ "")
    #(+NL-IDENTIFICATION-FAX+ "")
    #(+NL-IDENTIFICATION-LANGUAGE+ "")
    #(+NL-IDENTIFICATION-TERRITORY+ "")
    #(+NL-IDENTIFICATION-AUDIENCE+ "")
    #(+NL-IDENTIFICATION-APPLICATION+ "")
    #(+NL-IDENTIFICATION-ABBREVIATION+ "")
    #(+NL-IDENTIFICATION-REVISION+ "")
    #(+NL-IDENTIFICATION-DATE+ "")
    #(+NL-IDENTIFICATION-CATEGORY+ "")
    #(+NL-IDENTIFICATION-CODESET+ "")
    ;; #(+NL-NUM-LC-IDENTIFICATION+ "")
    )
  :start #.(nl-item-value +LC-IDENTIFICATION+ 0))

;; #+linux
;; (setf *nl-items* (nreverse *nl-items*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; darwin

;; order is important
#+darwin
(define-enum-list *nl-items*
  #(#(+D-T-FMT+     "String for formatting date and time.")
    #(+D-FMT+       "Date format string.")
    #(+T-FMT+       "Time format string.")
    #(+T-FMT-AMPM+  "a.m. or p.m. time format string.")
    #(+AM-STR+      "Ante-meridiem affix.")
    #(+PM-STR+      "Post-meridiem affix.")

    #(+DAY-1+       "Name of the first day of the week (for example, Sunday).")
    #(+DAY-2+       "Name of the second day of the week (for example, Monday).")
    #(+DAY-3+       "Name of the third day of the week (for example, Tuesday).")
    #(+DAY-4+       "Name of the fourth day of the week (for example, Wednesday).")
    #(+DAY-5+       "Name of the fifth day of the week (for example, Thursday).")
    #(+DAY-6+       "Name of the sixth day of the week (for example, Friday).")
    #(+DAY-7+       "Name of the seventh day of the week example, Saturday).")

    #(+ABDAY-1+     "Abbreviated name of the first day of the week.")
    #(+ABDAY-2+     "Abbreviated name of the second day of the week.")
    #(+ABDAY-3+     "Abbreviated name of the third day of the week.")
    #(+ABDAY-4+     "Abbreviated name of the fourth day of the week.")
    #(+ABDAY-5+     "Abbreviated name of the fifth day of the week.")
    #(+ABDAY-6+     "Abbreviated name of the sixth day of the week.")
    #(+ABDAY-7+     "Abbreviated name of the seventh day of the week.")

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

    #(+ERA+         "Era description segments.")
    #(+ERA-D-FMT+   "Era date format string.")
    #(+ERA-D-T-FMT+ "Era date and time format string.")
    #(+ERA-T-FMT+   "Era time format string.")
    #(+ALT-DIGITS+  "Alternative symbols for digits.")

    #(+RADIXCHAR+   "Radix character.")
    #(+THOUSEP+     "Separator for thousands.")

    #(+YESEXPR+     "Affirmative response expression.")
    #(+NOEXPR+      "Negative response expression.")
    #(+YESSTR+      "Output string for ‘yes’.")
    #(+NOSTR+       "Output string for ‘no’.")
    #(+CRNCYSTR+
    "Local currency symbol, preceded by '-' if the symbol should appear
before the value, '+' if the symbol should appear after the value, or '.'
if the symbol should replace the radix character. If the local currency symbol
is the empty string, implementations may return the empty string ("").")
    )
  :start 1)

;; We treat locale like an opaque pointer, even though it's not.
(defctype locale-t :pointer
  "Type of a unix locale.")

(defcfun ("nl_langinfo_l" nl-langinfo-l) :string
  "Return the string for ITEM from the language catalog for LOCALE."
  (item nl-item) (locale locale-t))

(defcfun ("nl_langinfo" nl-langinfo) :string
  "Return the string for ITEM from the language catalog for the current locale."
  (item nl-item))

(defcfun ("nl_langinfo" nl-langinfo-raw) :pointer
  "Return the string for ITEM from the language catalog for the current locale."
  (item nl-item))

;; End
