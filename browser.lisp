(in-package :lispkit)


(defclass browser ()
  ((tabs
    :initarg :tabs
    :accessor tabs)
   (ui
    :initarg :ui
    :initform (error "Cannot instantiate a browser without a UI object")
    :accessor ui)
   (webview
    :accessor webview
    :initarg :webview
    :initform (error "Cannot instantiate a browser without a webview object"))
   (url-bar
    :initarg
    :url-bar)
   (grabbing-keys?
    :initform nil
    :accessor grabbing-keys?)))

(defmacro with-webview (var browser &body body)
  `(let ((,var (webview ,browser)))
     ,@body))

(defmethod initialize-instance :after ((browser browser) &key)
  (check-type (ui browser) gtk:gtk-builder))

(defun new-browser (ui webview)
  (let ((tabs (list webview)))
    (make-instance 'browser
                   :ui ui
                   :webview webview
                   :tabs tabs)))

(defun load-url (url browser)
  (webkit.foreign:webkit-web-view-load-uri (webview browser) url))

(defun reload-page (browser)
  (webkit.foreign:webkit-web-view-reload (webview browser)))

(defun forwards-page (browser)
  (webkit.foreign:webkit-web-view-go-forward (webview browser)))

(defun backwards-page (browser)
  (webkit.foreign:webkit-web-view-go-back (webview browser)))

(defun browse-url (browser)
  (let* ((ui (ui browser))
         (entry-box (gtk:gtk-builder-get-object ui "entry_box")))
    (g-signal-connect entry-box "key_press_event"
                      (lambda (window event)
                        (declare (ignore window))
                        (when (string= (parse-event event) "Return")
                          (let* ((buf (gtk:gtk-entry-buffer entry-box))
                                 (url (gtk:gtk-entry-buffer-get-text buf)))
                            (if (purl:url-p url)
                                (load-url url browser)
                                (apply-jumps url browser))
                            (gtk-widget-hide entry-box)))))
    (gtk:gtk-widget-grab-focus entry-box)
    (gtk:gtk-widget-show entry-box)))

(defun zoom (browser)
  (with-webview wv browser
    (webkit.foreign:webkit-web-view-zoom-in wv)))

(defun unzoom (browser)
  (with-webview wv browser
    (webkit.foreign:webkit-web-view-zoom-out wv)))

(defun next-tab (browser)
  )

(defun prev-tab (browser)
  )

(defun new-tab (browser)
  )
