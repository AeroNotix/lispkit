(in-package :lispkit)


(defparameter *modeline-tick-seconds* 0.1)
(defparameter *modeline-quit* nil)
(defparameter *modeline-format* "~url")
(defparameter *modeline-workers* nil)
(defparameter *modeline-expander*
  (lambda (browser modeline-state)
    (declare (ignore modeline-state))
    (let* ((url (current-uri browser))
           (rendered-modeline (cl-ppcre:regex-replace-all "~url" *modeline-format* (or url ""))))
      (ui-set-text browser "message-area" (format nil "~A" rendered-modeline)))))

(defun try-pop-queue (queue)
  (when (lparallel.queue:peek-queue queue)
    (lparallel.queue:pop-queue queue)))

(defun render-modeline (lbl modeline-state)
  (funcall *modeline-expander* lbl modeline-state))

(defcommand start-modeline (browser)
  "Starts and displays the modeline"
  (ui-show browser "infobar1")
  (let ((queue    (lparallel.queue:make-queue))
        (ml-state (make-hash-table :test #'equalp)))
    (push (bordeaux-threads:make-thread
           (lambda ()
             (loop
                while (not *modeline-quit*)
                do
                  (when-let ((msg (try-pop-queue queue)))
                    (setf (gethash (first msg) ml-state) (second msg)))
                  (sleep *modeline-tick-seconds*)
                  (render-modeline browser ml-state))))
          *modeline-workers*)
    (setf (modeline browser) queue)))

(defcommand stop-modeline (browser)
  "Stops and destroys the modeline"
  (setf *modeline-quit* t)
  (ui-hide browser "infobar1")
  (mapcar #'bordeaux-threads:join-thread *modeline-workers*))
