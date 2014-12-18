(in-package :lispkit)


(defparameter *default-page*
  "http://www.this-page-intentionally-left-blank.org/")

(djula:add-template-directory (asdf:system-relative-pathname :lispkit "templates/"))
(defvar +helppage+ (djula:compile-template* "helppage.dtl"))
(defvar +command-info+ (djula:compile-template* "command-info.dtl"))
(defvar +ui-builder-file+
  (read-file-into-string (asdf:system-relative-pathname :lispkit "main.ui")))

(defun load-ui-from-string (contents)
  (let ((builder (gtk:gtk-builder-new)))
    (gtk:gtk-builder-add-from-string builder contents)
    builder))


(defparameter *default-browser* nil)

(defun main (&optional (destroy? nil))
  "Main exists separately from do-main so that during development we
  can easily separate killing the main gtk loop from stopping and
  starting applications within that main loop"
  (within-main-loop
    (let* ((ui      (load-ui-from-string +ui-builder-file+))
           (window  (gtk:gtk-builder-get-object ui "mainwindow"))
           (frame   (gtk:gtk-builder-get-object ui "scrolledwindow"))
           (entry   (gtk:gtk-builder-get-object ui "entry_box"))
           (ib      (gtk:gtk-builder-get-object ui "infobar1"))
           (c-area  (gtk:gtk-info-bar-get-content-area ib))
           (view    (make-webview))
           (nb      (gtk:gtk-builder-get-object ui "webviewcontainer"))
           (lbl     (gtk:gtk-builder-get-object ui "message-area"))
           (browser (make-browser ui view)))
      (gtk-notebook-set-show-tabs nb nil)
      (gtk-container-add frame view)
      (g-signal-connect window "key_press_event"
                        (make-key-dispatcher browser))
      (g-signal-connect nb "switch-page"
                        (make-page-listener browser))
      (when destroy?
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main))))
      (load-url *default-page* browser)
      (setf *default-browser* browser)
      (gtk-widget-hide entry)
      (ensure-cookies-folder-exists *cookie-path-dir*)
      ;; TODO - Add error handling to this.
      (load-rc-file)
      (gtk-window-maximize window)
      (gtk:gtk-container-add c-area lbl)
      (dolist (widget (list window frame view ib lbl))
        (gtk-widget-show widget)))))

(defun do-main (&rest args)
  "The main entry point when running as an executable. This should not
   be run directly but only indirectly when an image has been built."
  (declare (ignore args))
  (main t)
  (join-gtk-main))
