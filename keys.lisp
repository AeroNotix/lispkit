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

(defun strip-irrelevant-mods (keys)
  (remove-if
   (lambda (elt) (member elt '(:mod2-mask :shift-mask)))
   keys))

(defun keyval->string (keyval)
  (string (code-char keyval)))

(let ((mod-map '((:mod1-mask . "M")
                 (:control-mask . "C"))))
  (defun mod->string (mod)
    (cdr (assoc mod mod-map :test #'equal))))

(defun mods->string (s)
  (mapcar #'mod->string (strip-irrelevant-mods s)))

(defun event-as-string (event)
  (with-gdk-event-slots (state keyval) event
    (if (< keyval 255)
        (let ((key     (keyval->string keyval))
              (mod-str (mods->string   state)))
          (if (consp mod-str)
              (format t "~{~a~^-~}-~a~%" mod-str key)
              (format t "~a~%" key))
          (finish-output nil)))))

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

(defun handle-key-event (handler event)
  (declare (ignore handler))
  (event-as-string event))

(setf *emacs-key-handler* (make-instance 'key-event-handler))
(push *emacs-key-handler* *key-event-handlers*)
