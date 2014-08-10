(in-package :lispkit)

(defun gdk-event-slot (slot)
  (intern (format nil "GDK-EVENT-KEY-~a" slot) 'gdk))

(defmacro with-gdk-event-slots (slots gdk-event &body body)
  `(let (,@(mapcar
            #'(lambda (slot)
                (let ((slot-name (gdk-event-slot slot)))
                  `(,slot (,slot-name ,gdk-event))))
            slots))
     ,@body))

