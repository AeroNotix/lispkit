(in-package :lispkit)


(defmacro destructuring-dolist (vars list &body body)
  (let ((var (gensym))
        (l   (gensym)))
    `(let ((,l ,list))
       (dolist (,var ,l)
         (destructuring-bind ,vars ,var
           ,@body)))))

(defun gdk-event-slot (slot)
  (intern (format nil "GDK-EVENT-KEY-~a" slot) 'gdk))

(defmacro with-gdk-event-slots (slots gdk-event &body body)
  `(let (,@(mapcar
            #'(lambda (slot)
                (let ((slot-name (gdk-event-slot slot)))
                  `(,slot (,slot-name ,gdk-event))))
            slots))
     ,@body))

