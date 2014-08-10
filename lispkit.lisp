(in-package :lispkit)

(defparameter *current-tab* nil)

(defun load-url (url &optional view)
  (webkit.foreign:webkit-web-view-load-uri
   (or view *current-tab*) url))

(defun event-as-string (event)
  (with-gdk-event-slots (state string) event
    (print (list state string))))

(let ((key-events nil))

  (defun all-events ()
    key-events)

  (defun events-as-string (events)
    (mapcar #'event-as-string events))

  (defun handle-key (window event)
    (declare (ignore window))
    (push event key-events)
    (events-as-string key-events)))

(defun main (&rest args)
  (declare (ignore args))
  (within-main-loop
    (let* ((ui     (load-ui-from-file "main.ui"))
           (window (gtk:gtk-builder-get-object ui "mainwindow"))
           (frame  (gtk:gtk-builder-get-object ui "webkit-frame"))
           (view   (setq *current-tab*
                         (webkit.foreign:webkit-web-view-new))))
      (gtk-container-add frame view)
      (g-signal-connect window "key_press_event"
                        #'handle-key)
      (load-url "http://www.github.com/AeroNotix/lispkit")
      (gtk-widget-show-all window))))
