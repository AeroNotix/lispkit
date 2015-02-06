(defpackage #:lispkit-test
  (:use :cl :lispkit))

(in-package #:lispkit-test)


;; If the debugger is fired, it means something went wrong.
(setf fiveam:*debug-on-error* t
      fiveam:*debug-on-failure* t)
(setf *debugger-hook*
      (lambda (c h)
	(declare (ignore c h))
	(uiop:quit -1)))

(5am:def-suite main)
(5am:def-suite commands)
(5am:def-suite events)
(5am:def-suite jumps)
(5am:def-suite keys)

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :lispkit-test))))
  (5am:run! 'main)
  (5am:run! 'commands)
  (5am:run! 'events)
  (5am:run! 'jumps)
  (5am:run! 'keys))
