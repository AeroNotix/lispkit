(in-package :lispkit)


(defun strip-irrelevant-mods (keys)
  (remove-if
   (lambda (elt) (member elt '(:mod2-mask :shift-mask)))
   keys))

(let ((mod-map '((:mod1-mask . "M")
                 (:control-mask . "C"))))
  (defun mod->string (mod)
    (cdr (assoc mod mod-map :test #'equal))))

(defun mods->string (s)
  (mapcar #'mod->string (strip-irrelevant-mods s)))

(defun duplicated-event? (event)
  (when (and *previous-event* event)
    (= (gdk:GDK-EVENT-KEY-TIME event)
       (gdk:GDK-EVENT-KEY-TIME *previous-event*))))

(defun parse-event (event)
  (with-gdk-event-slots (state keyval) event
    (let ((key     (keysym->keysym-name keyval))
          (mod-str (mods->string   state)))
      (values key mod-str))))

(defun event-as-string (event)
  (with-gdk-event-slots (state keyval) event
    (let ((key     (keysym->keysym-name keyval))
          (mod-str (mods->string   state)))
      (unwind-protect
           (when (and (not (duplicated-event? event))
                      (not (modifier? keyval)))
             (if (consp mod-str)
                 (format nil "~{~a~^-~}-~a" mod-str key)
                 (format nil "~a" key)))
        (setq *previous-event* event)))))
