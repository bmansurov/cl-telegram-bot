(defpackage #:cl-telegram-bot/message-entity
  (:use #:cl)
  (:export
   #:message-entity
   #:message-entity-type
   #:message-entity-type
   #:message-entity-offset
   #:message-entity-length
   #:message-entity-url
   #:message-entity-user
   #:message-entity-language
   #:message-entity-custom-emoji-id
   #:message-entity-raw-data
   #:make-message-entity))
(in-package cl-telegram-bot/message-entity)

(defclass message-entity ()
  ((type :reader message-entity-type
         :initarg :type
         :type string)
   (offset :reader message-entity-offset
           :initarg :offset
           :type integer)
   (length :reader message-entity-length
           :initarg :length
           :type integer)
   (url :reader message-entity-url
        :initarg :url
        :type (or null string))
   (user :reader message-entity-user
         :initarg :user
         :type (or null cl-telegram-bot/user:user))
   (language :reader message-entity-language
             :initarg :language
             :type (or null string))
   (custom-emoji-id :reader message-entity-custom-emoji-id
                    :initarg :custom-emoji-id
                    :type (or null string))
   (raw-data :initarg :raw-data
             :reader message-entity-raw-data))
  (:documentation "<https://core.telegram.org/bots/api#messageentity>"))

(defun make-message-entity (data)
  (when data
    (let* ((user (cl-telegram-bot/user:make-user (getf data :|user|)))
           (message-entity
             (apply #'make-instance
                    'message-entity
                    (cl-telegram-bot/utils:raw-data-to-initargs
                     (find-class 'message-entity)
                     data
                     '(user)))))
      (setf (slot-value message-entity 'user) user)
      message-entity)))

