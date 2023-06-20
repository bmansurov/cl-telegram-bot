(defpackage #:cl-telegram-bot/network
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:cl-telegram-bot/utils
                #:obfuscate)
  (:import-from #:cl-telegram-bot/bot
                #:get-endpoint)
  (:export
   #:make-request
   #:request-error
   #:set-proxy
   #:what))
(in-package cl-telegram-bot/network)

(defvar *proxy* nil)

(defun set-proxy (proxy)
  (setf *proxy* proxy))

(define-condition request-error (error)
  ((what :initarg :what
         :reader what))
  (:report (lambda (condition stream)
             (format stream "Request error: ~A" (what condition)))))


(defun make-request (bot name &rest options &key (streamp nil) (timeout 3) &allow-other-keys)
  "Perform HTTP request to 'name API method with 'options JSON-encoded object."
  (declare (ignore streamp))

  (let ((url (concatenate 'string (get-endpoint bot) name))
        (options-str (format nil "~S" options)))
    (log:debug "Posting data to" (obfuscate url) options-str)
    (let* ((max-timeout (* timeout 10))
           (processed-options (loop for (key value)
                                      on (alexandria:remove-from-plist options :timeout :streamp)
                                        by #'cddr
                                    when value
                                      collect (kebab:to-snake-case key)
                                      and
                                        collect value))
           (response
             (if *proxy*
                 (dexador:post url
                               :headers '(("Content-Type" . "application/json"))
                               :content (jonathan:to-json processed-options)
                               :read-timeout max-timeout
                               :connect-timeout max-timeout
                               :proxy *proxy*)
                 (dexador:post url
                               :headers '(("Content-Type" . "application/json"))
                               :content (jonathan:to-json processed-options)
                               :read-timeout max-timeout
                               :connect-timeout max-timeout)))
           (data (jonathan:parse response)))
      (unless (getf data :|ok|)
        (let ((data-str (format nil "~S" data)))
          (log:error "Wrong data received from the server" data-str))
        (error 'request-error :what data))

      (getf data :|result|))))
