(defpackage #:cl-telegram-bot/user
    (:use #:cl)
    (:nicknames :ctb/user)
  (:import-from :kebab
                #:to-snake-case)
  (:import-from :cl-telegram-bot/utils
                #:slot-names
                #:symbol-to-keyword
                #:get-property-values
                #:raw-data-to-initargs)
  (:export #:user
           #:make-user
           #:user-id
           #:user-is-bot
           #:user-first-name
           #:user-last-name
           #:user-username
           #:user-language-code
           #:user-is-premium
           #:user-added-to-attachment-menu
           #:user-can-join-groups
           #:user-can-read-all-group-messages
           #:user-supports-inline-queries
           #:user-raw-data))
(in-package cl-telegram-bot/user)

(defclass user ()
  ((id :initarg :id
       :reader user-id)
   (is-bot :initarg :is-bot
           :reader user-is-bot)
   (first-name :initarg :first-name
               :reader user-first-name)
   (last-name :initarg :last-name
              :reader user-last-name)
   (username :initarg :username
             :reader user-username)
   (language-code :initarg :language-code
                  :reader user-language-code)
   (is-premium :initarg :is-premium
               :reader user-is-premium)
   (added-to-attachment-menu :initarg :added-to-attachment-menu
                             :reader user-added-to-attachment-menu)
   (can-join-groups :initarg :can-join-groups
                    :reader user-can-join-groups)
   (can-read-all-group-messages
    :initarg :can-read-all-group-messages
    :reader user-can-read-all-group-messages)
   (supports-inline-queries :initarg :supports-inline-queries
                            :reader user-supports-inline-queries)
   (raw-data :initarg :raw-data
             :reader user-raw-data))
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

