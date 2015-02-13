(in-package #:lispkit-test)


(lispkit:defcommand test-command (browser)
  "Test command"
  (string= browser "fixture"))

(plan 5)

(multiple-value-bind (value presentp)
    (lispkit::command-p "test-command")
  (declare (ignore value))
  (ok presentp))

(ok (lispkit::run-named-command "test-command" "fixture"))

;; Create the ~/.lispkitrc file
(open (merge-pathnames (user-homedir-pathname) #p".lispkitrc") :direction :probe :if-does-not-exist :create)
(ok (lispkit::load-rc-file))

;; Create the ~/.lispkitrc file
(open (merge-pathnames (user-homedir-pathname) #p".lispkitrc") :direction :probe :if-does-not-exist :create)
(multiple-value-bind (value presentp)
    (gethash "reload-config" lispkit::*available-commands*)
  (declare (ignore value))
  (ok presentp)
  (ok (lispkit::reload-config nil)))

(finalize)
