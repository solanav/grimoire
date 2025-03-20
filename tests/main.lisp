(defpackage grimoire/tests/main
  (:use :cl
        :grimoire
        :rove))
(in-package :grimoire/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :grimoire)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
