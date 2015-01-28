(in-package #:lispkit-test)


(5am:in-suite commands)

(lispkit:defcommand test-command (browser)
  "Test command"
  (string= browser "fixture"))

(5am:test test-command-p
  (multiple-value-bind (value presentp)
      (lispkit::command-p "test-command")
    (declare (ignore value))
    (5am:is-true (eq presentp t))))

(5am:test test-run-named-command
  (5am:is-true (eq (lispkit::run-named-command "test-command" "fixture") t)))

(5am:test test-load-rc-file
  ;; Create the ~/.lispkitrc file
  (open (merge-pathnames (user-homedir-pathname) #p".lispkitrc") :direction :probe :if-does-not-exist :create)
  (5am:is-true (eq (lispkit::load-rc-file) t)))

(5am:test test-reload-config
  ;; Create the ~/.lispkitrc file
  (open (merge-pathnames (user-homedir-pathname) #p".lispkitrc") :direction :probe :if-does-not-exist :create)
  (multiple-value-bind (value presentp)
      (gethash "reload-config" lispkit::*available-commands*)
    (declare (ignore value))
    (5am:is-true (eq presentp t))
    (5am:is-true (eq (lispkit::reload-config nil) t))))
