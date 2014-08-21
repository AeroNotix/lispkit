(in-package :lispkit)


(defparameter *available-commands* nil)

(defclass command ()
  ((name :initarg :name :accessor name)
   (implementation :initarg :impl :accessor impl)
   (documentation :initarg :doc :accessor doc)))

(defmacro defcommand (name documentation arglist &body body)
  `(make-instance 'command
                  :name (symbol-name ',name)
                  :impl #'(lambda ,arglist
                            ,@body)
                  :doc ,documentation))

(defmethod initialize-instance :after ((command command) &key)
  (with-slots (name) command
    (setq *available-commands*
          (remove-if #'(lambda (other-command)
                         (equalp name (name other-command))) *available-commands*)))
  (push command *available-commands*))

(defun command-match (name? command)
  (with-slots (name) command
    (equalp name name?)))

(defun command-p (name)
  (member name *available-commands* :test #'command-match))

(defun run-named-command (name browser)
  (let ((command (command-p name)))
    (when command
      (with-slots (implementation) (first command)
        (funcall implementation browser)))))
