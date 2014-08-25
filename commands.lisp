(in-package :lispkit)

(defparameter *available-commands* (make-hash-table :test #'equalp))
(defparameter *cancel-functions* (make-hash-table))

(defclass command ()
  ((name :initarg :name :accessor name)
   (implementation :initarg :impl :accessor impl)
   (documentation :initarg :doc :accessor doc)))

(define-condition command-documentation-warning (style-warning)
  ((name :initarg :name :reader name))
  (:report
   (lambda (condition stream)
     (format stream
             "Command ~A doesn't have a documentation string"
             (name condition)))))

(defmacro defcommand (name arglist &body body)
  (let ((documentation (if (stringp (first body))
                           (first body)
                           (warn (make-condition 'command-documentation-warning
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
  `(progn
     (defun ,name ,arglist ,@body)
     (setf (gethash ',name *cancel-functions*) #',name)))

(defmethod initialize-instance :after ((command command) &key)
  (setf (gethash (name command) *available-commands*) command))

(defun command-match (name? command)
  (with-slots (name) command
    (equalp name name?)))

(defun command-p (name)
  (gethash name *available-commands*))

(defun run-named-command (name browser)
  (let ((command (command-p name)))
    (when command
      (with-slots (implementation) command
        (funcall implementation browser)))))

(defun get-rc-file ()
  (let* ((xdg-config-dir
           (let ((dir (uiop:getenv "XDG_CONFIG_HOME")))
             (if (or (not dir) (string= dir ""))
                 (merge-pathnames  #p".config/" (user-homedir-pathname))
                 dir)))
         (user-rc (probe-file (merge-pathnames (user-homedir-pathname) #p".lispkitrc")))
         (conf-rc (probe-file (merge-pathnames #P"lispkit/config" xdg-config-dir)))
         (etc-rc  (probe-file #p"/etc/lispkit")))
    (or user-rc conf-rc etc-rc)))

(defun load-rc-file ()
  (let* ((rc (get-rc-file)))
    (load rc)))

(defcommand reload-config (browser)
  (declare (ignore browser))
  (load-rc-file))
