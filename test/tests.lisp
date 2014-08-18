(defpackage #:lispkit-test
  (:use :cl :lispkit :lisp-unit))

(in-package #:lispkit-test)

(define-test test-lookup-jumps
  (let* ((jumps (make-hash-table :test #'equal))
         (prefix "g")
         (url    "http://google.com/search?=~a"))
    (defjump jumps prefix url)
    (assert-equal 1 (hash-table-count jumps))
    (assert-equal url (lookup-jump prefix jumps))))
