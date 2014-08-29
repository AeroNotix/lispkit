(defpackage #:lispkit-test
  (:use :cl :lispkit :lisp-unit))

(in-package #:lispkit-test)

(define-test test-lookup-jumps
  (let* ((jumps (make-hash-table :test #'equal))
         (prefix "g")
         (url    "http://google.com/search?=~a"))
    (defjump jumps prefix url)
    (assert-equal 1 (hash-table-count jumps))
    (assert-equal url (lookup-jump prefix jumps))))

(define-test make-simple-browser
    (let ((browser (make-browser (make-ui-builder) (make-webview))))
      browser))

(define-test key-map-traversal
    (let ((top-map (make-keymap))
          (second-map (make-keymap))
          (third-map (make-keymap)))
      (define-key top-map "C-x" second-map)
      (define-key second-map "C-x" third-map)
      (let ((browser (make-browser (make-ui-builder) (make-webview) (list top-map))))
        (handle-key browser "C-x")
        (assert-equal (keymaps browser) (list second-map))
        (handle-key browser "C-x")
        (assert-equal (keymaps browser) (list third-map))
        (handle-key browser "oops")
        (assert-equal (keymaps browser) (list top-map)))))
