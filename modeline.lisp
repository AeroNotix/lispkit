(in-package :lispkit)


(defparameter *modeline-tick-seconds* 0.1)
(defparameter *modeline-quit* nil)
(defparameter *modeline-format* "~url")


(defun try-pop-queue (queue)
  (when (lparallel.queue:peek-queue queue)
    (lparallel.queue:pop-queue queue)))

(defun render-modeline (lbl modeline-state)
  (when-let* ((url (gethash :url modeline-state)))
    (let ((rendered-modeline (cl-ppcre:regex-replace-all "~url" *modeline-format* url)))
      (gtk:gtk-label-set-text lbl (format nil "~A" rendered-modeline)))))

(defun start-modeloop (lbl)
  (let ((queue    (lparallel.queue:make-queue))
        (ml-state (make-hash-table :test #'equalp)))
    (bordeaux-threads:make-thread
     (lambda ()
       (loop
          while (not *modeline-quit*)
          do
            (when-let ((msg (try-pop-queue queue)))
              (setf (gethash (first msg) ml-state) (second msg)))
            (sleep *modeline-tick-seconds*)
            (render-modeline lbl ml-state))))
    queue))

(defun stop-modeline ()
  (setf *modeline-quit* t))
