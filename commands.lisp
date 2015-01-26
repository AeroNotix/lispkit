(in-package :lispkit)

(defparameter *available-commands* (make-hash-table :test #'equalp))
(defparameter *cancel-functions* (make-hash-table))

(defclass command ()
  ((name :initarg :name :accessor name)
   (implementation :initarg :impl :accessor impl)
   (documentation :initarg :doc :accessor doc)))

(define-condition documentation-style-warning (style-warning)
  ((name :initarg :name :reader name)
   (subject-type :initarg :subject-type :reader subject-type))
  (:report
   (lambda (condition stream)
     (format stream
             "~:(~A~) ~A doesn't have a documentation string"
             (subject-type condition)
             (name condition)))))

(define-condition command-documentation-style-warning
    (documentation-style-warning)
  ((subject-type :initform 'command)))

(define-condition cancel-documentation-style-warning
    (documentation-style-warning)
  ((subject-type :initform 'cancel)))

(defmacro defcommand (name arglist &body body)
  (let ((documentation (if (stringp (first body))
                           (first body)
                           (warn (make-condition
                                  'command-documentation-style-warning
                                  :name name))))
        (body (if (stringp (first body))
                  (rest body)
                  body)))
    `(progn
       (defun ,name ,arglist
         ,@body)
       (make-instance 'command
                      :name (symbol-name ',name)
                      :impl #',name
                      :doc ,documentation))))

(defmacro defcancel (name arglist &body body)
  (unless (stringp (first body))
    (warn (make-condition 'cancel-documentation-style-warning
                          :name name)))
  `(progn
     (defun ,name ,arglist ,@body)
     (setf (gethash ',name *cancel-functions*) #',name)))

(defmethod initialize-instance :after ((command command) &key)
  (setf (gethash (name command) *available-commands*) command))

(defun command-match (name? command)
  (with-slots (name) command
    (equalp name name?)))

(defun command-p (name)
  (multiple-value-bind (_ presencep)
      (gethash name *available-commands*)
    (declare (ignore _))
    presencep))

(defun run-named-command (name browser)
  (let ((command (command-p name)))
    (when command
      (with-slots (implementation) command
        (funcall implementation browser)))))

(defun load-rc-file ()
  (let* ((rc (get-rc-file)))
    (when rc (load rc))))

(defcommand reload-config (browser)
  "Reloads the configuration file."
  (declare (ignore browser))
  (load-rc-file))
