(defpackage #:cl-telegram-bot/chat-member
    (:use #:cl)
    (:nicknames #:ctb/chat-member)
  (:import-from #:cl-telegram-bot/chat
                #:chat
                #:make-chat)
  (:import-from #:cl-telegram-bot/pipeline
                #:process)
  (:import-from #:cl-telegram-bot/user
                #:make-user)
  (:import-from :cl-telegram-bot/utils
                #:slot-names
                #:symbol-to-keyword
                #:get-property-values
                #:raw-data-to-initargs)
  (:export #:chat-member-meber
           #:chat-member-member-status
           #:chat-member-member-user
           #:chat-member-member-raw-data
           #:make-chat-member-member
           #:chat-member-updated
           #:make-chat-member-updated
           #:chat-member-updated-chat
           #:chat-member-updated-from
           #:chat-member-updated-date
           #:chat-member-updated-old-chat-member
           #:chat-member-updated-new-chat-member
           #:chat-member-updated-invite-link
           #:chat-member-updated-via-chat-folder-invite-link
           #:chat-member-updated-raw-data
           #:on-chat-member-updated))
(in-package cl-telegram-bot/chat-member)


(defclass chat-member-member ()
  ((status :initarg :status
           :reader chat-member-member-status)
   (user :initarg :from
         :reader chat-member-member-user)
   (raw-data :initarg :raw-data
             :reader chat-member-member-raw-data))
  (:documentation "https://core.telegram.org/bots/api#chatmembermember"))

(defun make-chat-member-member (data)
  (when data
    (let ((user (make-user (getf data :|user|)))
          (chat-member-member
            (apply #'make-instance
                   'chat-member-member
                   (raw-data-to-initargs
                    (find-class 'chat-member-member)
                    data
                    '(user)))))
      (setf (slot-value chat-member-member 'user) user)
      chat-member-member)))
#+nil
(inspect
 (make-chat-member-member
  '(:|status| "member" :|user|
    (:|username| "oliboli2_bot" :|first_name| "Oli Boli 2" :|is_bot| T
                                :|id| 5838283419))))

(defclass chat-member-updated ()
  ((chat :initarg :chat
         :reader chat-member-updated-chat)
   (from :initarg :from
         :reader chat-member-updated-from)
   (date :initarg :date
         :reader chat-member-updated-date)
   (old-chat-member :initarg :old-chat-member
                    :reader chat-member-updated-old-chat-member)
   (new-chat-member :initarg :new-chat-member
                    :reader chat-member-updated-new-chat-member)
   ;; TODO create a class for invite-link
   (invite-link :initarg :invite-link
                :reader chat-member-updated-invite-link)
   (via-chat-folder-invite-link
    :initarg :via-chat-folder-invite-link
    :reader chat-member-updated-via-chat-folder-invite-link)
   (raw-data :initarg :raw-data
             :reader chat-member-updated-raw-data))
  (:documentation "https://core.telegram.org/bots/api#chatmemberupdated"))

(defun make-chat-member-updated (data)
  (when data
    (let ((chat (make-chat (getf data :|chat|)))
          (from (make-user (getf data :|from|)))
          (date (getf data :|date|))
          ;; TODO: MEMBER can be other types of chat members:
          ;; https://core.telegram.org/bots/api#chatmember
          (old-chat-member (make-chat-member-member
                            (getf data :|old_chat_member|)))
          (new-chat-member (make-chat-member-member
                            (getf data :|new_chat_member|)))
          ;; TODO this needs to be a class
          (invite-link (getf data :|invite_link|))
          (via-chat-folder-invite-link
            (getf data :|via-chat-folder-invite-link|)))
      (make-instance 'chat-member-updated
                     :chat chat
                     :from from
                     :date date
                     :old-chat-member old-chat-member
                     :new-chat-member new-chat-member
                     :invite-link invite-link
                     :via-chat-folder-invite-link
                     via-chat-folder-invite-link
                     :raw-data data))))
#+nil
(inspect
 (make-chat-member-updated
  '(:|new_chat_member|
    (:|status| "member" :|user|
     (:|username| "mybot" :|first_name| "My Bot" :|is_bot| T :|id| 123))
    :|old_chat_member|
    (:|until_date| 0 :|status| "kicked" :|user|
     (:|username| "mybot" :|first_name| "My Bot" :|is_bot| T :|id| 123))
    :|date| 1687221822 :|from|
    (:|language_code| "en" :|last_name| "A" :|first_name| "B"
                           :|is_bot| NIL :|id| 456)
    :|chat|
    (:|type| "private" :|last_name| "A" :|first_name| "B" :|id| 456))))


(defgeneric on-chat-member-updated (bot chat-member-updated)
  (:documentation
   "Handle CHAT-MEMBER-UPDATED. More info:
<https://core.telegram.org/bots/api#chatmemberupdated>."))

(defmethod process ((bot t) (chat-member-updated chat-member-updated))
  "Process CHAT-MEMBER-UPDATED."
  (log:debug "Processing chat-member-updated." bot chat-member-updated)
  (on-chat-member-updated bot chat-member-updated)
  (values))
