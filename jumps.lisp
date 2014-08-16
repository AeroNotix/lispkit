(in-package :lispkit)


(defparameter *jumps* nil)

(defun defjump (prefix url)
  (push `(,prefix . ,url) *jumps*))

(defjump "g" "http://google.com/search?=~a")

(defun lookup-jump (s)
  (assoc s *jumps* :test #'string=))

(defun extract-jump-str (s)
  (split-sequence " " s :test #'string=))

(defmacro multiple-value-destructuring-bind (lambda-list value-list &body body)
  (let ((ignore (gensym)))
    `(destructuring-bind (,@lambda-list &rest ,ignore)
         (multiple-value-list ,value-list)
       (declare (ignore ,ignore))
       ,@body)))

(defun apply-jumps (s browser)
  (multiple-value-destructuring-bind ((jump args)) (extract-jump-str s)
    (let ((jump (lookup-jump jump)))
      (when jump
        (print (format nil (cdr jump) args))
        t))))
