(in-package :lispkit)


(defparameter *jumps* nil)

(defun defjump (prefix url)
  (push `(,prefix . ,url) *jumps*))

(defjump "g" "http://google.com/search?=%s")

(defun prefix-of? (s1 s2)
  (= (mismatch s1 s2) (length s2)))

(defun lookup-jump (s)
  (assoc s *jumps* :test #'prefix-of?))

(defun extract-jump-str (s)
  )

(defun apply-jumps (s)
  (let ((jump? (lookup-jump s)))
    (when jump?
      (print jump?))))
