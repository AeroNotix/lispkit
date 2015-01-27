(in-package #:cl-user)


(ql:quickload :lispkit)
(ql:quickload :lispkit-test)

;; If the debugger is fired, it means something went wrong.
(setf fiveam:*debug-on-error* t
      fiveam:*debug-on-failure* t)
(setf *debugger-hook*
      (lambda (c h)
	(declare (ignore c h))
	(uiop:quit -1)))

;; Run tests
(in-package :lispkit-test)

(5am:run! 'main)
(5am:run! 'commands)
(5am:run! 'events)

;; Everything went fine
(uiop:quit 0)
