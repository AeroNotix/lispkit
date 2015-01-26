(in-package #:lispkit-test)


(5am:in-suite commands)

(5am:test test-command-p
  (5am:is-true (eq (lispkit::command-p "reload-config") t)))
