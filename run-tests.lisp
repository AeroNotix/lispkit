(in-package   :cl-user)


(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload :lispkit))
(use-package  :lisp-unit)
(in-package   :lispkit)

(let* ((tr (lisp-unit:run-tests :all))
       (failed (lisp-unit:failed-tests tr)))
  (when failed
    (error (format nil "tests failed: ~{~a~^, ~}" failed))))
