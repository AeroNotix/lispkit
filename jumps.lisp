(in-package :lispkit)


(defparameter *jumps* (make-hash-table :test #'equal))

(defun defjump (place prefix url)
  (setf (gethash prefix place) url))

(defjump *jumps* "g" "http://google.com/search?=~a")

(defun lookup-jump (s table)
  (gethash s table))

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
    (let ((jump? (lookup-jump jump *jumps*)))
      (when jump?
        (load-url (format nil jump? args) browser)
        t))))
