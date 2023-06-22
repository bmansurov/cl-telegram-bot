(defpackage #:cl-telegram-bot/update
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:cl-telegram-bot/chat-member
                #:make-chat-member-updated)
  (:import-from #:cl-telegram-bot/message
                #:make-message)
  (:import-from #:cl-telegram-bot/network
                #:make-request)
  (:import-from #:cl-telegram-bot/bot
                #:get-last-update-id
                #:bot)
  (:import-from #:cl-telegram-bot/pipeline
                #:process)
  (:export #:make-update
           #:get-raw-data
           #:get-update-id
           #:process-updates
           #:update
           #:get-payload))
(in-package cl-telegram-bot/update)


(defclass update ()
  ((id :initarg :id
       :reader get-update-id)
   (payload :initarg :payload
            :reader get-payload)
   (raw-data :initarg :raw-data
             :reader get-raw-data)))



(defun make-update (data)
  (let* ((data-str (format nil "~S" data))
         (payload
           (cond ((getf data :|message|)
                  ;; https://core.telegram.org/bots/api#message
                  (make-message (getf data :|message|)))
                 ((getf data :|my_chat_member|)
                  ;; https://core.telegram.org/bots/api#chatmemberupdated
                  (make-chat-member-updated
                   (getf data :|my_chat_member|)))
                 (t
                  (log:warn "Received not supported update." data-str)
                  nil))))
    (log:debug "make-update" data-str)
    (make-instance 'update
                   :id (getf data :|update_id|)
                   :payload payload
                   :raw-data data)))


(defun get-updates (bot &key limit timeout)
  "https://core.telegram.org/bots/api#getupdates"
  (let* ((current-id (get-last-update-id bot))
         (results (make-request bot "getUpdates"
                                :|offset| current-id
                                :|limit| limit
                                :|timeout| timeout
                                :streamp t
                                :timeout timeout)))

    (let ((updates (mapcar 'make-update results)))
      (when updates
        (let ((max-id (reduce #'max updates :key #'get-update-id)))
          ;; In original cl-telegram-bot a bug was here, because
          ;; it saved update's id only the first time, and after that,
          ;; just incremented that value
          (log:debug "Setting new" max-id)
          (setf (get-last-update-id bot) (+ max-id 1))))
      (values updates))))


;; Generics

(defgeneric process-updates (bot)
  (:documentation "By default, this method starts an infinite loop and fetching new updates using long polling."))


(defmethod process-updates ((bot t))
  "Starts inifinite loop to process updates using long polling."
  (loop
    do (loop for update
               in (restart-case
                      (get-updates bot :timeout 30)
                    (continue-processing (&optional delay)
                      :report "Continue processing updates from Telegram"
                      (when delay (sleep delay))
                      ;; Return no updates
                      (values)))
             do (restart-case
                    (process bot update)
                  (continue-processing (&optional delay)
                    :report "Continue processing updates from Telegram"
                    (when delay (sleep delay)))))))


(defmethod process ((bot t) (update update))
  "By default, just calls `process' on the payload."
  (log:debug "Processing update: ~S" update)
  (let ((payload (get-payload update)))
    (log:debug "Processing payload ~S for bot ~S." payload bot)
    (process bot payload)))
