(defsystem lispkit-test
  :version "0.0.1"
  :description "Lispy browser"
  :licence "BSD"
  :depends-on (:lispkit :prove :alexandria)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "test"
                        :components
                        ((:file "package")
			 (:test-file "commands")
			 (:test-file "events")
			 (:test-file "jumps")
			 (:test-file "keys")
			 (:test-file "main"))))
  :perform (asdf:test-op :after (op c)
                         (funcall (intern #.(string :run) :prove) c)))
