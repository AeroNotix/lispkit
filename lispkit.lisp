(in-package :lispkit)

(defparameter *current-tab* nil)

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
           (entry  (gtk:gtk-builder-get-object ui "entry_box"))
           (view   (setq *current-tab*
                         (webkit.foreign:webkit-web-view-new)))
           (browser (new-browser ui)))
      (print ui)
      (gtk-container-add frame view)
      (g-signal-connect window "key_press_event"
                        (new-key-dispatcher browser))
      (load-url "http://www.github.com/AeroNotix/lispkit")
      (gtk-widget-hide entry)
      (dolist (widget (list window frame view))
        (gtk-widget-show widget)))))
