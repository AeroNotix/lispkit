(in-package #:lispkit-test)


(plan 6)

(let ((keymap (lispkit::make-keymap)))
  (ok (eq 'lispkit::keymap (type-of keymap)))
  (ok (eq 'hash-table (type-of (lispkit::bindings keymap)))))

(let ((keybind (lispkit::keymap->keydesc* nil "foo" nil)))
  (ok (eq 'lispkit::keybind (type-of keybind)))
  (ok (string= "foo" (lispkit::key keybind))))
(ok (string= "foo foo"
             (lispkit::key
              (car
               (let ((keymap (make-instance 'lispkit::keymap)))
                 (setf (gethash
                        "foo"
                        (lispkit::bindings keymap))
                       "bar")
                 (lispkit::keymap->keydesc* nil "foo" keymap))))))

(let ((map (make-instance 'lispkit::keymap)))
  (lispkit::define-key map "foo" "bar")
  (ok (string= "bar"
               (gethash "foo" (lispkit::bindings map)))))

(finalize)
