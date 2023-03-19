(defpackage clog-notebook/tests/main
  (:use :cl
        :clog-notebook
        :rove))
(in-package :clog-notebook/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :clog-notebook)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
