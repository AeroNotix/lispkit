(defpackage lispkit
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :drakma :cl-webkit
        :glib :gio :pango :cairo :common-lisp)
  (:export #:main))

(in-package :lispkit)

(defun main (&rest args)
  (declare (ignore args))
  (within-main-loop
    (let ((window (gtk-window-new :toplevel))
          (view (webkit.foreign:webkit-web-view-new)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-widget-destroy view)
                          (leave-gtk-main)))
      (gtk-container-add window view)
      (webkit.foreign:webkit-web-view-load-uri view "http://www.example.com")
      (gtk-widget-show-all window))))
