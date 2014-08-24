(in-package :lispkit)


(defparameter *default-page*
  "http://www.this-page-intentionally-left-blank.org/")

(djula:add-template-directory (asdf:system-relative-pathname :lispkit "templates/"))
(defvar +helppage+ (djula:compile-template* "helppage.dtl"))
(defvar +command-info+ (djula:compile-template* "command-info.dtl"))

(defun load-ui-from-file (path)
  (if (probe-file path)
      (let ((builder (gtk:gtk-builder-new)))
        (gtk:gtk-builder-add-from-file builder (namestring path))
        builder)
      (error (format nil "non existent path: ~s" path))))

(defun get-rc-file ()
  (let* ((user-rc (probe-file (merge-pathnames (user-homedir-pathname) #p".lispkitrc")))
         (conf-rc (probe-file (merge-pathnames #P".config/lispkit/config" (user-homedir-pathname))))
         (etc-rc  (probe-file #p"/etc/lispkit")))
    (or user-rc conf-rc etc-rc)))

(defun load-rc-file ()
  (let* ((rc (get-rc-file)))
    (load rc)))

(defun main (&rest args)
  (declare (ignore args))
  (within-main-loop
    (let* ((ui      (load-ui-from-file
                     (asdf:system-relative-pathname :lispkit "main.ui")))
           (window  (gtk:gtk-builder-get-object ui "mainwindow"))
           (frame   (gtk:gtk-builder-get-object ui "scrolledwindow"))
           (entry   (gtk:gtk-builder-get-object ui "entry_box"))
           (view    (webkit.foreign:webkit-web-view-new))
           (nb      (gtk:gtk-builder-get-object ui "webviewcontainer"))
           (browser (make-browser ui view)))
      (gtk-notebook-set-show-tabs nb nil)
      (gtk-container-add frame view)
      (g-signal-connect window "key_press_event"
                        (make-key-dispatcher browser))
      (g-signal-connect nb "switch-page"
                        (make-page-listener browser))
      (load-url *default-page* browser)
      (gtk-widget-hide entry)
      ;; TODO - Add error handling to this.
      (load-rc-file)
      (dolist (widget (list window frame view))
        (gtk-widget-show widget)))))
