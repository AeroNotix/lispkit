(in-package #:lispkit-test)


(plan 7)

(is "https://google.com/search?q=~a" (gethash "g" lispkit::*jumps*))
(is "https://github.com/search?&q=~a" (gethash "gh" lispkit::*jumps*))
(is "https://www.youtube.com/results?search_query=~a" (gethash "y" lispkit::*jumps*))
(is "https://www.reddit.com/r/~a/" (gethash "r" lispkit::*jumps*))

(let ((table (make-hash-table :test #'equal)))
  (lispkit::defjump table "foo" "bar")
  (is (gethash "foo" table) "bar"))

(let ((table (make-hash-table :test #'equal)))
  (lispkit::defjump table "foo" "bar")
  (is (lispkit::lookup-jump "foo" table) "bar"))

(is (list "foo" "bar")
    (lispkit::extract-jump-str "foo bar"))

;; apply-jumps is not testable since load-url uses webkit

(finalize)
