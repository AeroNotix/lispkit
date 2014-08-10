(in-package :lispkit)

(defparameter *insert-mode* nil)
(defparameter *grabbing-keys* nil)
(defparameter *key-event-handlers* nil)
(defparameter *emacs-key-handler* nil)

(defclass key-event-handler nil
  ((key-map     :initarg :key-map :initform (make-hash-table :test #'equal))
   (key-events  :initform nil)
   (prefix-keys :initform nil)
   (ungrab-keys :initform nil)))

(defun strip-mod2 (keys)
  (remove-if
   (lambda (elt) (equal :mod2-mask elt))
   keys))

(defun event-as-string (event)
  (with-gdk-event-slots (state string) event
    (print (list (strip-mod2 state) string))))

(defun events-as-string (events)
  (mapcar #'event-as-string events))

(defun handle-key (window event)
  (declare (ignore window))
  (if (or *insert-mode* *grabbing-keys*)
      (dolist (handler *key-event-handlers*)
        (handle-key-event handler event))
      nil))

(defun dispatch-keypress (window event)
  (declare (ignore window))
  (dolist (handler *key-event-handlers*)
    (handle-key-event handler event)))

(defgeneric handle-key-event (handler event))

(defmethod handle-key-event ((handler key-event-handler) event)
  (with-gdk-event-slots (state string) event
    (print (list (strip-mod2 state) string))))

(setf *emacs-key-handler* (make-instance 'key-event-handler))
(push *emacs-key-handler* *key-event-handlers*)
