(in-package :lispkit)


(define-test test-lookup-jumps
  (let* ((jumps nil)
         (prefix "g")
         (url    "http://google.com/search?=~a"))
    (defjump jumps prefix url)
    (assert-equal 1 (length jumps))
    (assert-equal `(,prefix . ,url) (lookup-jump prefix))))
