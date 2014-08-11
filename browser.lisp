(in-package :lispkit)


(defun load-url (url &optional view)
  (webkit.foreign:webkit-web-view-load-uri
   (or view *current-tab*) url))

(defun reload-page (window)
  (declare (ignore window))
  (webkit.foreign:webkit-web-view-reload *current-tab*))

(defun forwards-page (window)
  (declare (ignore window))
  (webkit.foreign:webkit-web-view-go-forward *current-tab*))

(defun backwards-page (window)
  (declare (ignore window))
  (webkit.foreign:webkit-web-view-go-back *current-tab*))
