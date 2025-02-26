(defsystem "grimoire"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "grimoire/tests"))))

(defsystem "grimoire/tests"
  :author ""
  :license ""
  :depends-on ("grimoire"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for grimoire"
  :perform (test-op (op c) (symbol-call :rove :run c)))
