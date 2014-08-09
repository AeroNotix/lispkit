(defpackage lispkit
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :drakma :cl-webkit
        :glib :gio :pango :cairo :common-lisp)
  (:export #:main))

(in-package :lispkit)

(defparameter *current-tab* nil)

(defun handle-key (window event)
  (declare (ignore window))
  (print event)
  nil)

(defun main (&rest args)
  (declare (ignore args))
  (within-main-loop
    (let ((window (gtk-window-new :toplevel))
          (view (webkit.foreign:webkit-web-view-new)))
      (gtk-container-add window view)
      (gtk-container-add window (make-instance 'gtk-scrolled-window))
      (webkit.foreign:webkit-web-view-load-uri
       view "http://www.github.com/AeroNotix/lispkit")
      (g-signal-connect window "key_press_event"
                        #'handle-key)
      (gtk-widget-show-all window))))
