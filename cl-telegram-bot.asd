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
                                                   "user"
                                                   "utils"))
                 (:file "entities/command" :depends-on ("entities/core"
                                                        "message"
                                                        "pipeline"
                                                        "utils"))
                 (:file "entities/core" :depends-on ("utils"))
                 (:file "files")
                 (:file "game")
                 (:file "inline")
                 (:file "inline-keyboard")
                 (:file "main"  :depends-on ("entities/command"
                                             "entities/core"
                                             "message"
                                             "update"))
                 (:file "media")
                 (:file "message" :depends-on ("entities/core"
                                               "network"
                                               "pipeline"
                                               "telegram-call"
                                               "utils"))
                 (:file "network" :depends-on ("utils"))
                 (:file "payments")
                 (:file "pipeline")
                 (:file "profile")
                 (:file "stickers")
                 (:file "telegram-call" :depends-on ("network"
                                                     "utils"))
                 (:file "update" :depends-on ("message"
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
