(defpackage #:cl-telegram-bot/utils
  (:use #:cl)
  (:import-from #:arrows
                #:->)
  (:import-from #:cl-ppcre
                #:regex-replace)
  (:import-from #:cl-strings
                #:replace-all)
  (:import-from #:kebab
                #:to-snake-case)

  (:export #:make-keyword
           #:obfuscate
           #:slot-names
           #:symbol-to-keyword
           #:get-property-values
           #:raw-data-to-initargs))
(in-package cl-telegram-bot/utils)


(defun make-keyword (text)
  (-> text
      (replace-all "_" "-")
      (nstring-upcase)
      (alexandria:make-keyword)))


(defun obfuscate (url)
  (regex-replace "/bot.*?/"
                 url
                 "/bot<token>/"))


(defun make-json-keyword (arg)
  (check-type arg symbol)
  (-> arg
      (symbol-name)
      (to-snake-case)
      (alexandria:make-keyword)))


(defun slot-names (class)
  "Return the slot names of CLASS."
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots class)))
#+nil
(slot-names (find-class 'user))

(defun symbol-to-keyword (symbol)
  (intern (symbol-name symbol) :keyword))
#+nil
(symbol-to-keyword 'first-name)

(defun get-property-values (data property-indicators)
  "Return property values from the plist DATA using PROPERTY-INDICATORS.
The order of values are the same as the order of indicators."
  (mapcar (lambda (x) (getf data x)) property-indicators))
#+nil
(get-property-values '(:a 1 :b 2 :c 3) '(:a :c))


(defun raw-data-to-initargs (class raw-data &optional
                                              (remove-slot-names nil))
  "Convert RAW-DATA from Telegram into an initialization argument list of
the class CLASS. RAW-DATA will be set as the value of the slot RAW-DATA.
REMOVE-SLOT-NAMES is the list of slot names that will be removed from
the output."
  (closer-mop:ensure-finalized class)
  (let* ((slot-names (set-difference (slot-names class)
                                     remove-slot-names))
         (slot-keywords (mapcar #'symbol-to-keyword slot-names))
         (property-indicators (mapcar #'to-snake-case slot-keywords))
         (values (get-property-values raw-data property-indicators))
         (initargs (mapcan #'list slot-keywords values)))
    (setf (getf initargs :raw-data) raw-data)
    initargs))
#+nil
(raw-data-to-initargs
 (find-class 'cl-telegram-bot/user:user)
 '(:|language_code| "en" :|last_name| "A" :|first_name| "B" :|is_bot| NIL
   :|id| 123))

