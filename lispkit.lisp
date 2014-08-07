(defpackage lispkit
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :drakma
        :glib :gio :pango :cairo :common-lisp)
  (:export #:main))

(in-package :lispkit)

(defun main (&rest args)
  (declare (ignore args))

  (within-main-loop
    (let ((window (gtk-window-new :toplevel)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-widget-show-all window))))
