(in-package #:lispkit-test)


(plan 8)

(is (list :foo :bar)
    (lispkit::strip-irrelevant-mods '(:mod2-mask :foo :shift-mask :bar)))

(is "M" (lispkit::mod->string :mod1-mask))
(is "C" (lispkit::mod->string :control-mask))

(is (list "M" "C")
    (lispkit::mods->string '(:mod2-mask :mod1-mask :shift-mask :control-mask)))

(let ((lispkit::*previous-event* nil))
  (is (lispkit::duplicated-event? t) nil))

(let ((event (make-instance 'gdk:gdk-event-key)))
  (multiple-value-bind (key mod-str)
      (lispkit::parse-event event)
    (is key nil)
    (is mod-str nil)))

(let ((lispkit::*previous-event* nil))
  (is (lispkit::event-as-string (make-instance 'gdk:gdk-event-key))
      "NIL"))

(finalize)
