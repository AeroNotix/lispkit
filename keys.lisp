(in-package :lispkit)

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
(define-key *emacs-map* "C-x C-f"     #'browse-url)
(define-key *emacs-map* "C-x plus"    #'zoom)
(define-key *emacs-map* "C-x minus"   #'unzoom)
(define-key *emacs-map* "C-x n"       #'next-tab)
(define-key *emacs-map* "C-x p"       #'prev-tab)
(define-key *emacs-map* "C-x k"       #'new-tab)


(defun if-any-handled? (responses)
  (member :handled responses :test #'equal))

(defun new-key-dispatcher (browser)
  (lambda (window event)
    (declare (ignore window))
    (let* ((key-str (event-as-string event))
           (handler-responses
            (when key-str
              (loop for handler in *key-event-handlers*
                 collect (handle-key-event handler key-str browser)))))
      (or (if-any-handled? handler-responses)
          (grabbing-keys? browser)))))

(defun execute-pending-commands (handler browser)
  (let* ((full-str (format nil "~{~a~^ ~}" (reverse (recent-keys handler))))
         (key-func (gethash full-str (key-map handler))))
    (if (functionp key-func)
        (progn
          (funcall key-func browser)
          (reset-key-state handler browser)
          :handled)
        :unhandled)))

(defun reset-key-state (handler browser)
  (setf (grabbing-keys? browser) nil)
  (setf (recent-keys handler) nil))

(defun handle-key-event (handler key-str browser)
  (if (ungrab-key? handler key-str)
      (reset-key-state handler browser)
      (progn
        (when (is-prefix-key? handler key-str)
          (setf (grabbing-keys? browser) t))
        (when (grabbing-keys? browser)
          (push key-str (recent-keys handler))
          (execute-pending-commands handler browser)))))

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
