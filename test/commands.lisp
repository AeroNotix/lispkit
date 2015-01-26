(in-package #:lispkit-test)


(5am:in-suite commands)

(5am:test test-command-p
  (lispkit:defcommand test-command ()
    "Test command")
  (multiple-value-bind (value presentp)
      (lispkit::command-p "test-command")
    (5am:is-true (eq presentp t))))

(5am:test test-run-named-command
  (lispkit:defcommand test-command (browser)
    "Test command"
    (eq browser "fixture"))
  (5am:is-true (eq (lispkit::run-named-command "test-command" "fixture") t)))
