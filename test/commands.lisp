(in-package #:lispkit-test)


(lispkit:defcommand test-command (browser)
  "Test command"
  (string= browser "fixture"))

(plan 5)

(multiple-value-bind (value presentp)
    (lispkit::command-p "test-command")
  (declare (ignore value))
  (ok (eq presentp t)))

(ok (eq (lispkit::run-named-command "test-command" "fixture") t))

;; Create the ~/.lispkitrc file
(open (merge-pathnames (user-homedir-pathname) #p".lispkitrc") :direction :probe :if-does-not-exist :create)
(ok (eq (lispkit::load-rc-file) t))

;; Create the ~/.lispkitrc file
(open (merge-pathnames (user-homedir-pathname) #p".lispkitrc") :direction :probe :if-does-not-exist :create)
(multiple-value-bind (value presentp)
    (gethash "reload-config" lispkit::*available-commands*)
  (declare (ignore value))
  (ok (eq presentp t))
  (ok (eq (lispkit::reload-config nil) t)))

(finalize)
