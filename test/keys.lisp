(in-package #:lispkit-test)


(5am:in-suite keys)

(5am:test test-make-keymap
  (let ((keymap (lispkit::make-keymap)))
    (5am:is-true (eq 'lispkit::keymap (type-of keymap)))
    (5am:is-true (eq 'hash-table (type-of (lispkit::bindings keymap))))))

(5am:test test-keymap->keydescs
  "Tests keymap->keydesc* and keymap->keydesc"
  (let ((keybind (lispkit::keymap->keydesc* nil "foo" nil)))
    (5am:is-true (eq 'lispkit::keybind (type-of keybind)))
    (5am:is-true (string= "foo" (lispkit::key keybind))))
  (let ((keymap (make-instance 'lispkit::keymap)))
    (5am:is-true (string= "foo foo"
			  (lispkit::key
			   (first
			    (let ((keymap (make-instance 'lispkit::keymap)))
			      (setf (gethash
				     "foo"
				     (lispkit::bindings keymap))
				    "bar")
			      (lispkit::keymap->keydesc* nil "foo" keymap))))))))

(5am:test test-define-key
  (let ((map (make-instance 'lispkit::keymap)))
    (lispkit::define-key map "foo" "bar")
    (5am:is-true (string= "bar"
			  (gethash "foo" (lispkit::bindings map))))))
