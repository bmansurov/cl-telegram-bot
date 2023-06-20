(defpackage #:cl-telegram-bot/user
    (:use #:cl)
  (:import-from :kebab
                #:to-snake-case)
  (:import-from :cl-telegram-bot/utils
                #:slot-names
                #:symbol-to-keyword
                #:get-property-values
                #:raw-data-to-initargs)
  (:export #:user
           #:make-user
           #:get-user-id
           #:get-user-is-bot
           #:get-user-first-name
           #:get-user-last-name
           #:get-user-username
           #:get-user-language-code
           #:get-user-is-premium
           #:get-user-added-to-attachment-menu
           #:get-user-can-join-groups
           #:get-user-can-read-all-group-messages
           #:get-user-supports-inline-queries
           #:get-raw-data))
(in-package cl-telegram-bot/user)

(defclass user ()
  ((id :initarg :id
       :reader get-user-id)
   (is-bot :initarg :is-bot
           :reader get-user-is-bot)
   (first-name :initarg :first-name
               :reader get-user-first-name)
   (last-name :initarg :last-name
              :reader get-user-last-name)
   (username :initarg :username
             :reader get-user-username)
   (language-code :initarg :language-code
                  :reader get-user-language-code)
   (is-premium :initarg :is-premium
               :reader get-user-is-premium)
   (added-to-attachment-menu :initarg :added-to-attachment-menu
                             :reader get-user-added-to-attachment-menu)
   (can-join-groups :initarg :can-join-groups
                    :reader get-user-can-join-groups)
   (can-read-all-group-messages
    :initarg :can-read-all-group-messages
    :reader get-user-can-read-all-group-messages)
   (supports-inline-queries :initarg :supports-inline-queries
                            :reader get-user-supports-inline-queries)
   (raw-data :initarg :raw-data
             :reader get-raw-data))
  (:documentation "https://core.telegram.org/bots/api#user"))


(defun make-user (data)
  (when data
    (apply #'make-instance
           'user
           (raw-data-to-initargs (find-class 'user) data))))
#+nil
(inspect
 (make-user
  '(:|language_code| "en" :|last_name| "A" :|first_name| "B"
    :|is_bot| NIL :|id| 123)))

