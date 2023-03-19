(defsystem "clog-notebook"
  :version "0.1.0"
  :author "Satoshi Imai"
  :license "MIT"
  :depends-on (:clog
               :clog-ace
               :cl-debug-print
               :clcm
               )
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "clog-notebook/tests"))))

(defsystem "clog-notebook/tests"
  :author ""
  :license ""
  :depends-on ("clog-notebook"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for clog-notebook"
  :perform (test-op (op c) (symbol-call :rove :run c)))
