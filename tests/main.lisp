(defpackage cl-telegram-bot/tests/main
  (:use :cl
        :cl-telegram-bot
        :rove))
(in-package :cl-telegram-bot/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-telegram-bot)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
