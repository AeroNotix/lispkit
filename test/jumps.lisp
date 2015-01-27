(in-package #:lispkit-test)


(5am:in-suite jumps)

(5am:test test-jumps
  (5am:is-true (string= "https://google.com/search?q=~a" (gethash "g" lispkit::*jumps*)))
  (5am:is-true (string= "https://github.com/search?&q=~a" (gethash "gh" lispkit::*jumps*)))
  (5am:is-true (string= "https://www.youtube.com/results?search_query=~a" (gethash "y" lispkit::*jumps*)))
  (5am:is-true (string= "https://www.reddit.com/r/~a/" (gethash "r" lispkit::*jumps*))))

(5am:test test-defjump
  (let ((table (make-hash-table :test #'equal)))
    (lispkit::defjump table "foo" "bar")
    (5am:is-true (string= (gethash "foo" table)
			  "bar"))))

(5am:test test-lookup-jump
  (let ((table (make-hash-table :test #'equal)))
    (lispkit::defjump table "foo" "bar")
    (5am:is-true (string= (lispkit::lookup-jump "foo" table)
			  "bar"))))

(5am:test test-extract-jump-str
  (5am:is-true (equal (list "foo" "bar")
		      (lispkit::extract-jump-str "foo bar"))))

;; apply-jumps is not testable since load-url uses webkit
