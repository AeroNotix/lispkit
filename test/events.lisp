(in-package #:lispkit-test)


(plan 8)

(ok (equal (list :foo :bar)
           (lispkit::strip-irrelevant-mods '(:mod2-mask :foo :shift-mask :bar))))

(ok (string= "M"
             (lispkit::mod->string :mod1-mask)))
(ok (string= "C"
             (lispkit::mod->string :control-mask)))

(ok (equal (list "M" "C")
           (lispkit::mods->string '(:mod2-mask :mod1-mask :shift-mask :control-mask))))

(let ((lispkit::*previous-event* nil))
  (isnt (lispkit::duplicated-event? t) t))

(let ((event (make-instance 'gdk:gdk-event-key)))
  (multiple-value-bind (key mod-str)
      (lispkit::parse-event event)
    (ok (eq key nil))
    (ok (eq mod-str nil))))

(let ((lispkit::*previous-event* nil))
  (ok (string= (lispkit::event-as-string (make-instance 'gdk:gdk-event-key)) "NIL")))

(finalize)
