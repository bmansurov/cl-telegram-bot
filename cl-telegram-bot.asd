(defsystem "cl-telegram-bot"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("alexandria"
               "arrows"
               "cl-strings"
               "closer-mop"
               "dexador"
               "jonathan"
               "kebab"
               "log4cl"
               "serapeum"
               "trivial-backtrace")
  :components ((:module "src"
                :components
                ((:file "bot")
                 (:file "chat" :depends-on ("network"
                                            "telegram-call"))
                 (:file "chat-member" :depends-on ("chat"
                                                   "pipeline"
                                                   "user"
                                                   "utils"))
                 (:file "files")
                 (:file "game")
                 (:file "inline")
                 (:file "inline-keyboard")
                 (:file "main"  :depends-on ("message-entity"
                                             "message"
                                             "update"))
                 (:file "media")
                 (:file "message" :depends-on ("chat"
                                               "message-entity"
                                               "network"
                                               "pipeline"
                                               "telegram-call"
                                               "user"
                                               "utils"))
                 (:file "message-entity" :depends-on ("pipeline"
                                                      "user"
                                                      "utils"))
                 (:file "network" :depends-on ("utils"))
                 (:file "payments")
                 (:file "pipeline")
                 (:file "profile")
                 (:file "stickers")
                 (:file "telegram-call" :depends-on ("network"
                                                     "utils"))
                 (:file "update" :depends-on ("chat-member"
                                              "message"
                                              "network"
                                              "pipeline"))
                 (:file "user" :depends-on ("utils"))
                 (:file "utils")
                 (:file "webhooks"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-telegram-bot/tests"))))

(defsystem "cl-telegram-bot/tests"
  :author ""
  :license ""
  :depends-on ("cl-telegram-bot"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-telegram-bot"
  :perform (test-op (op c) (symbol-call :rove :run c)))
