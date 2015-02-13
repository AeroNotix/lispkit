(in-package #:lispkit-test)


(plan 6)

(let ((keymap (lispkit::make-keymap)))
  (is-type keymap 'lispkit::keymap)
  (is-type (lispkit::bindings keymap) 'hash-table))

(let ((keybind (lispkit::keymap->keydesc* nil "foo" nil)))
  (is-type keybind 'lispkit::keybind)
  (is "foo" (lispkit::key keybind)))
(is "foo foo"
    (lispkit::key
     (car
      (let ((keymap (make-instance 'lispkit::keymap)))
        (setf (gethash
               "foo"
               (lispkit::bindings keymap))
              "bar")
        (lispkit::keymap->keydesc* nil "foo" keymap)))))

(let ((map (make-instance 'lispkit::keymap)))
  (lispkit::define-key map "foo" "bar")
  (is "bar" (gethash "foo" (lispkit::bindings map))))

(finalize)
