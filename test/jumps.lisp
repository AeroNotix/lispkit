(in-package #:lispkit-test)


(plan 7)

(ok (string= "https://google.com/search?q=~a" (gethash "g" lispkit::*jumps*)))
(ok (string= "https://github.com/search?&q=~a" (gethash "gh" lispkit::*jumps*)))
(ok (string= "https://www.youtube.com/results?search_query=~a" (gethash "y" lispkit::*jumps*)))
(ok (string= "https://www.reddit.com/r/~a/" (gethash "r" lispkit::*jumps*)))

(let ((table (make-hash-table :test #'equal)))
  (lispkit::defjump table "foo" "bar")
  (ok (string= (gethash "foo" table)
               "bar")))

(let ((table (make-hash-table :test #'equal)))
  (lispkit::defjump table "foo" "bar")
  (ok (string= (lispkit::lookup-jump "foo" table)
               "bar")))

(ok (equal (list "foo" "bar")
           (lispkit::extract-jump-str "foo bar")))

;; apply-jumps is not testable since load-url uses webkit

(finalize)
