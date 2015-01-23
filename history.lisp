(in-package :lispkit)


(defparameter *history-path* :default)
(defparameter *history-appender* :default)


(defclass base-history-appender ()
  ((output-path
    :initform (error "Must include an output-path")
    :initarg  :output-path
    :accessor output-path)
   (history
    :initform nil
    :initarg :history
    :accessor history)))

(defclass base-entry ()
  ((name
    :initform (error "Must include an entry name")
    :initarg :name
    :accessor name)
   (value
    :initform (error "Must include an entry value")
    :initarg :value
    :accessor value)))

(defgeneric create-entry (value)
  (:documentation "Creates an entry from some other value"))

(defgeneric entry= (a b)
  (:documentation "Compares two entries"))

(defgeneric entry-to-string (value)
  (:documentation "Converts a value to string"))

(defmethod create-entry ((value string))
  (make-instance 'base-entry :name value :value value))

(defmethod entry= ((a base-entry) (b base-entry))
  (and (string=
        (name a)
        (name b))
       (string=
        (value a)
        (value b))))

(defmethod entry-to-string ((value base-entry))
  (value value))

(defun create-history (&optional (path *history-path*))
  (if (eq path :default)
      (create-history (get-history-file))
      (if (eq *history-appender* :default)
          (make-instance 'base-history-appender :output-path path)
          (make-instance *history-appender* :output-path path))))

(defgeneric hydrate (appender)
  (:documentation "Deserializes the history from disk"))

(defgeneric dehydrate (appender)
  (:documentation "Serializes the history to-disk"))

(defgeneric add-entry (appender entry)
  (:documentation "Adds a history entry to the appender"))

(defgeneric to-list (appender)
  (:documentation "Returns a list representation of the appender's history"))

(defmethod hydrate ((a base-history-appender))
  (let ((history (remove-duplicates
                  (with-open-file (stream (output-path a) :if-does-not-exist nil)
                    (when stream
                      (loop for line = (read-line stream nil 'eof)
                         until (eq line 'eof)
                         collect (create-entry line)))) :test #'entry=)))
    (setf (history a) history)))

(defmethod dehydrate ((a base-history-appender))
  (with-open-file (stream (output-path a)
                          :direction :output
                          :if-exists :supersede)
    (loop for entry in (history a)
       do
         (write-line (entry-to-string entry) stream))))

(defmethod add-entry ((a base-history-appender) (e string))
  (push (create-entry e) (history a)))

(defmethod to-list ((a base-history-appender))
  (mapcar #'entry-to-string (history a)))
