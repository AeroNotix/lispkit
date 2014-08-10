(defpackage lispkit
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :drakma :cl-webkit
        :glib :gio :pango :cairo :common-lisp)
  (:export #:main))

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
    (let ((window (make-instance 'gtk:gtk-window :title "lispkit!"))
          (view (setq *current-tab*
                      (webkit.foreign:webkit-web-view-new))))
      (gtk-container-add window view)
      (gtk-container-add window (make-instance 'gtk-scrolled-window))
      (g-signal-connect window "key_press_event"
                        #'handle-key)
      (load-url "http://www.github.com/AeroNotix/lispkit")
      (gtk-widget-show-all window))))
