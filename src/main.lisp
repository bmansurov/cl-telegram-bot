(defpackage #:cl-telegram-bot
  (:use #:cl)
  (:nicknames :ctb)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:destroy-thread)
  (:import-from #:log4cl)
  (:import-from #:cl-telegram-bot/update
                #:process-updates)
  (:import-from #:cl-telegram-bot/bot
                #:defbot)
  (:import-from #:cl-telegram-bot/message
                #:on-message
                #:reply)
  (:import-from #:trivial-backtrace
                #:print-backtrace)
  ;; This package exports only essential symbols, needed
  ;; in 80% cases.
  (:export #:defbot
           #:on-message
           #:reply
           #:start-processing
           #:stop-processing
           #:on-command))
(in-package cl-telegram-bot)



(defvar *threads* nil)


(defun start-processing (bot &key debug (delay-between-retries 10))
  (when (getf *threads* bot)
    (error "Processing already started."))

  (log:info "Starting thread to process updates for" bot)
  (flet ((continue-processing-if-not-debug (condition)
           (let ((restart (find-restart
                           'cl-telegram-bot/update::continue-processing
                           condition)))
             (when restart
               (let ((traceback (print-backtrace condition :output nil)))
                 (log:error "Unable to process Telegram updates" traceback))
               (unless debug
                 (invoke-restart restart delay-between-retries))))))
    (setf (getf *threads* bot)
          (make-thread
           (lambda ()
             (handler-bind ((error #'continue-processing-if-not-debug))
               (process-updates bot)))
           :name "telegram-bot"))))


(defun stop-processing (bot)
  (let ((thread (getf *threads* bot)))
    (when thread
      (when (bt:thread-alive-p thread)
        (log:info "Stopping thread for" bot)
        (destroy-thread thread))
      (setf (getf *threads* bot)
            nil))))
