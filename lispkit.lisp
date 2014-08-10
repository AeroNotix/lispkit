(in-package :lispkit)

(defparameter *current-tab* nil)

(defun load-url (url &optional view)
  (webkit.foreign:webkit-web-view-load-uri
   (or view *current-tab*) url))

(defun load-ui-from-file (path)
  (if (probe-file path)
      (let ((builder (gtk:gtk-builder-new)))
        (gtk:gtk-builder-add-from-file builder path)
        builder)
      (error (format nil "non existent path: ~s" path))))

(defun main (&rest args)
  (declare (ignore args))
  (within-main-loop
    (let* ((ui     (load-ui-from-file "main.ui"))
           (window (gtk:gtk-builder-get-object ui "mainwindow"))
           (frame  (gtk:gtk-builder-get-object ui "scrolledwindow"))
           (view   (setq *current-tab*
                         (webkit.foreign:webkit-web-view-new))))
      (gtk-container-add frame view)
      (g-signal-connect window "key_press_event"
                        #'handle-key)
      (load-url "http://www.github.com/AeroNotix/lispkit")
      (gtk-widget-show-all window))))
