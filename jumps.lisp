(in-package :lispkit)


(defparameter *jumps* (make-hash-table :test #'equal))

(defun defjump (place prefix url)
  (setf (gethash prefix place) url))

(defjump *jumps* "g" "https://google.com/search?q=~a")
(defjump *jumps* "gh" "https://github.com/search?&q=~a")
(defjump *jumps* "y" "https://www.youtube.com/results?search_query=~a")
(defjump *jumps* "r" "https://www.reddit.com/r/~a/")


(defun lookup-jump (s table)
  (gethash s table))

(defun jump-p (url)
  (lookup-jump (subseq url 0 (position #\Space url)) *jumps*))

(defun extract-jump-str (s)
  (let ((splits (split-sequence #\Space s)))
    (list (first splits) (format nil "~{~a~^ ~}" (rest splits)))))

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
