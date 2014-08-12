(in-package :lispkit)

(defparameter *grabbing-keys* nil)
(defparameter *key-event-handlers* nil)
(defparameter *emacs-key-handler* nil)
(defparameter *emacs-map* (make-hash-table :test #'equal))
(defparameter *previous-event* nil)


(defclass key-event-handler nil
  ((key-map     :initarg :key-map :accessor key-map :initform (make-hash-table :test #'equal))
   (key-events  :initform nil)
   (recent-keys :initform nil :accessor recent-keys)
   (prefix-keys :initarg :prefix-keys :initform nil :accessor prefix-keys)
   (ungrab-keys :initform nil :initarg :ungrab-keys :accessor ungrab-keys)))

(defun define-key (map key function)
  (when (functionp function)
    (setf (gethash key map) function)))

(define-key *emacs-map* "C-x F5"      #'reload-page)
(define-key *emacs-map* "C-x C-Left"  #'backwards-page)
(define-key *emacs-map* "C-x C-Right" #'forwards-page)

(defun strip-irrelevant-mods (keys)
  (remove-if
   (lambda (elt) (member elt '(:mod2-mask :shift-mask)))
   keys))

(let ((mod-map '((:mod1-mask . "M")
                 (:control-mask . "C"))))
  (defun mod->string (mod)
    (cdr (assoc mod mod-map :test #'equal))))

(defun mods->string (s)
  (mapcar #'mod->string (strip-irrelevant-mods s)))

(defun duplicated-event? (event)
  (when (and *previous-event* event)
    (= (gdk:GDK-EVENT-KEY-TIME event)
       (gdk:GDK-EVENT-KEY-TIME *previous-event*))))

(defun event-as-string (event)
  (with-gdk-event-slots (state keyval) event
    (let ((key     (keysym->keysym-name keyval))
          (mod-str (mods->string   state)))
      (unwind-protect
           (when (and (not (duplicated-event? event))
                      (not (modifier? keyval)))
             (if (consp mod-str)
                 (format nil "~{~a~^-~}-~a" mod-str key)
                 (format nil "~a" key)))
        (setq *previous-event* event)))))

(defun grabbing-keys? () *grabbing-keys*)

(defun if-any-handled? (responses)
  (member :handled responses :test #'equal))

(defun dispatch-keypress (window event)
  (let* ((key-str (event-as-string event))
         (handler-responses
          (when key-str
            (dolist (handler *key-event-handlers*)
              (handle-key-event handler key-str window)))))
    (if-any-handled? handler-responses)
    nil))

(defun execute-pending-commands (handler window)
  (let* ((full-str (format nil "~{~a~^ ~}" (reverse (recent-keys handler))))
         (key-func (gethash full-str (key-map handler))))
    (if (functionp key-func)
      (progn
        (funcall key-func window)
        (reset-key-state handler)
        :handled)
      :unhanled)))

(defun reset-key-state (handler)
  (setf *grabbing-keys* nil)
  (setf (recent-keys handler) nil))

(defun handle-key-event (handler key-str window)
  (if (ungrab-key? handler key-str)
      (reset-key-state handler)
      (progn
        (when (is-prefix-key? handler key-str)
          (setf *grabbing-keys* t))
        (when (grabbing-keys?)
          (push key-str (recent-keys handler))
          (execute-pending-commands handler window)))))

(defun ungrab-key? (handler key-str)
  (member key-str (ungrab-keys handler) :test #'equal))

(defun is-prefix-key? (handler key-str)
  (member key-str (prefix-keys handler) :test #'equal))

(let ((prefix-keys '("C-x" "C-c"))
      (ungrab-keys '("C-g")))
  (setf *emacs-key-handler* (make-instance 'key-event-handler
                                           :key-map *emacs-map*
                                           :prefix-keys prefix-keys
                                           :ungrab-keys ungrab-keys))
  (push *emacs-key-handler* *key-event-handlers*))
