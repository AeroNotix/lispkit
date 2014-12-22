;; -*- mode: common-lisp -*-
(defsystem lispkit
  :version "0.0.1"
  :description "Lispy browser"
  :licence "BSD"
  :serial t
  :components ((:file "packages")
               (:file "user")
               (:file "settings")
               (:file "commands")
               (:file "commands/inspector")
               (:file "macros")
               (:file "modeline")
               (:file "jumps")
               (:file "events")
               (:file "browser")
               (:file "keys")
               (:file "lispkit"))
  :depends-on (:asdf
               :cl-cffi-gtk
               :cl-ppcre
               :cl-webkit2
               :cl-xkeysym
               :djula
               :parenscript
               :purl
               :split-sequence
               :lparallel)
  :in-order-to ((test-op (test-op :lispkit-test))))

(defsystem lispkit-test
  :version "0.0.1"
  :description "Lispy browser"
  :licence "BSD"
  :components ((:module "test"
                        :components
                        ((:file "tests"))))
  :depends-on (:lispkit :lisp-unit :alexandria)
  :perform (test-op (o s)
                    ;; LISP-UNIT:RUN-ALL-TESTS is a macro, so it can't be called
                    ;; like a function.
                    (eval `(,(intern "RUN-ALL-TESTS" :lisp-unit)
                            :lispkit-test))))
