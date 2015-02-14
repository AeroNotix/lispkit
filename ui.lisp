(in-package :lispkit)


(defun ui-build-browser (destroy?)
  "Builds the browser UI."
  (let* ((ui      (ui-load-from-string +ui-builder-file+))
         (window  (gtk:gtk-builder-get-object ui "mainwindow"))
         (frame   (gtk:gtk-builder-get-object ui "scrolledwindow"))
         (entry   (gtk:gtk-builder-get-object ui "entry_box"))
         (ib      (gtk:gtk-builder-get-object ui "infobar1"))
         (c-area  (gtk:gtk-info-bar-get-content-area ib))
         (view    (make-webview))
         (nb      (gtk:gtk-builder-get-object ui "webviewcontainer"))
         (lbl     (gtk:gtk-builder-get-object ui "message-area"))
         (browser (make-browser ui view)))
    (setf (gtk:gtk-notebook-show-tabs nb) nil)
    (gtk-container-add frame view)
    (gtk-widget-hide entry)
    (gtk:gtk-container-add c-area lbl)
    (mapcar #'gtk-widget-show (list window frame view ib lbl))
    (gtk-window-maximize window)
    (ui-connect-signals destroy? browser window nb)
    (values browser)))

(defun ui-connect-signals (destroy? browser window nb)
  "Connects the UI signals."
  (g-signal-connect window "key_press_event"
                    (make-key-dispatcher browser))
  (g-signal-connect nb "switch-page"
                    (make-page-listener browser))
  (when destroy?
    (g-signal-connect window "destroy"
                      (lambda (widget)
                        (declare (ignore widget))
                        (stop-modeline browser)
                        (leave-gtk-main)))))

(defun ui-load-from-string (contents)
  "Loads the UI from a string template."
  (let ((builder (gtk:gtk-builder-new)))
    (gtk:gtk-builder-add-from-string builder contents)
    builder))

(defun ui-start-loop ()
  "Starts the UI loop."
  (join-gtk-main))

(defun ui-set-text (browser id text)
  "Sets the text on a label."
  (gtk:gtk-label-set-text (get-widget browser id) text))

(defun ui-show (browser id)
  "Shows a widget."
  (gtk:gtk-widget-show (get-widget browser id)))

(defun ui-hide (browser id)
  "Hides a widget."
  (gtk:gtk-widget-hide (get-widget browser id)))
