(in-package #:lispkit-test)

(5am:in-suite main)

(5am:test test-lookup-jumps
  (let* ((jumps (make-hash-table :test #'equal))
         (prefix "g")
         (url    "http://google.com/search?=~a"))
    (defjump jumps prefix url)
    (5am:is (equal 1 (hash-table-count jumps)))
    (5am:is (equal url (lookup-jump prefix jumps)))))

(5am:test make-simple-browser
    (let ((browser (make-browser (make-ui-builder) (make-webview))))
      (5am:is (equal (type-of browser) 'lispkit::browser))))

(5am:test key-map-traversal
    (let ((top-map (make-keymap))
          (second-map (make-keymap))
          (third-map (make-keymap)))
      (define-key top-map "C-x" second-map)
      (define-key second-map "C-x" third-map)
      (let ((browser (make-browser (make-ui-builder) (make-webview) (list top-map))))
        (handle-key browser "C-x")
        (5am:is (equal (keymaps browser) (list second-map)))
        (handle-key browser "C-x")
        (5am:is (equal (keymaps browser) (list third-map)))
        (handle-key browser "oops")
        (5am:is (equal (keymaps browser) (list top-map))))))

(defparameter pressed? nil)
(defcommand foobar (_)
  "Sets pressed? to t so we can ensure that this function was activated."
  (declare (ignore _))
  (setf pressed? t))

(5am:test key-map-functional-call
    (let* ((top-map (make-keymap))
           (browser (make-browser (make-ui-builder) (make-webview) (list top-map))))
      (5am:is-false pressed?)
      (define-key top-map "C-x" "foobar")
      (handle-key browser "C-x")
      (5am:is-true pressed?)))
