(in-package #:lispkit-test)


(5am:in-suite events)

(5am:test test-strip-irrelevant-mods
  (5am:is-true (equal (list :foo :bar)
		      (lispkit::strip-irrelevant-mods '(:mod2-mask :foo :shift-mask :bar)))))

(5am:test test-mod->string
  (5am:is-true (string= "M"
			(lispkit::mod->string :mod1-mask)))
  (5am:is-true (string= "C"
			(lispkit::mod->string :control-mask))))

(5am:test test-mods->string
  (5am:is-true (equal (list "M" "C")
		      (lispkit::mods->string '(:mod2-mask :mod1-mask :shift-mask :control-mask)))))

(5am:test test-duplicated-event?
  (let ((lispkit::*previous-event* nil))
    (5am:is-false (eq (lispkit::duplicated-event? t) t))))

(5am:test test-parse-event
  (let ((event (make-instance 'gdk:gdk-event-key)))
    (multiple-value-bind (key mod-str)
	(lispkit::parse-event event)
      (5am:is-true (eq key nil))
      (5am:is-true (eq mod-str nil)))))

(5am:test test-event-as-string
  (let ((lispkit::*previous-event* nil))
    (5am:is-true (string= (lispkit::event-as-string (make-instance 'gdk:gdk-event-key)) "NIL"))))
