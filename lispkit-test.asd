(defsystem lispkit-test
  :version "0.0.1"
  :description "Lispy browser"
  :licence "BSD"
  :components ((:module "test"
                        :components
                        ((:file "test-suites")
			 (:file "commands")
			 (:file "events")
			 (:file "jumps")
			 (:file "keys")
			 (:file "main"))))
  :depends-on (:lispkit :fiveam :alexandria))
