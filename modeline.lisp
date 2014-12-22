(in-package :lispkit)


(defparameter *modeline-tick-seconds* 0.1)
(defparameter *modeline-quit* nil)
(defparameter *modeline-format* "~url")
(defparameter *modeline-expander*
  (lambda (browser modeline-state)
    (declare (ignore modeline-state))
    (let* ((lbl (get-widget browser "message-area"))
           (url (current-uri browser))
           (rendered-modeline (cl-ppcre:regex-replace-all "~url" *modeline-format* url)))
      (gtk:gtk-label-set-text lbl (format nil "~A" rendered-modeline)))))

(defun try-pop-queue (queue)
  (when (lparallel.queue:peek-queue queue)
    (lparallel.queue:pop-queue queue)))

(defun render-modeline (lbl modeline-state)
  (funcall *modeline-expander* lbl modeline-state))

(defun start-modeloop (browser)
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
            (render-modeline browser ml-state))))
    (setf (modeline browser) queue)))

(defun stop-modeline ()
  (setf *modeline-quit* t))
