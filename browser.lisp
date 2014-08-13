(in-package :lispkit)


(defclass browser ()
  ((tabs    :initarg :tabs)
   (ui      :initarg :ui
            :initform (error "Cannot instantiate a browser without a UI object")
            :accessor ui)
   (url-bar :initarg :url-bar)
   (grabbing-keys? :initform nil :accessor grabbing-keys?)))

(defmethod initialize-instance :after ((browser browser) &key)
  (check-type (ui browser) gtk:gtk-builder))

(defun new-browser (ui)
  (make-instance 'browser :ui ui))

(defun load-url (url &optional view)
  (webkit.foreign:webkit-web-view-load-uri
   (or view *current-tab*) url))

(defun reload-page (browser)
  (webkit.foreign:webkit-web-view-reload *current-tab*))

(defun forwards-page (browser)
  (declare (ignore browser))
  (webkit.foreign:webkit-web-view-go-forward *current-tab*))

(defun backwards-page (browser)
  (declare (ignore browser))
  (webkit.foreign:webkit-web-view-go-back *current-tab*))

(defun browse-url (browser)
  (let* ((ui (ui browser))
         (entry-box (gtk:gtk-builder-get-object ui "entry_box")))
    (g-signal-connect entry-box "key_press_event"
                      (lambda (window event)
                        (declare (ignore window))
                        (when (string= (parse-event event) "Return")
                          (let* ((buf (gtk:gtk-entry-buffer entry-box))
                                (url (gtk:gtk-entry-buffer-get-text buf)))
                            (load-url url)
                            (gtk-widget-hide entry-box)))))
    (gtk:gtk-widget-grab-focus entry-box)
    (gtk:gtk-widget-show entry-box)))
