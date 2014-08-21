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

(defgeneric create-new-tab (browser))
(defgeneric get-widget (browser widget-name))

(defmacro with-webview (var browser &body body)
  `(let ((,var (webview ,browser)))
     ,@body))

(defmacro with-browser-input (browser buf-contents &body body )
  (with-gensyms (window event buf stop-key entry-box)
    `(let* ((,entry-box (get-widget ,browser "entry_box"))
            (,stop-key  "Return"))
       (g-signal-connect ,entry-box "key_press_event"
                         (lambda (,window ,event)
                           (declare (ignore ,window))
                           (when (string= (parse-event ,event) ,stop-key)
                             (let* ((,buf (gtk:gtk-entry-buffer ,entry-box))
                                    (,buf-contents
                                     (gtk:gtk-entry-buffer-get-text ,buf)))
                               ,@body
                               (gtk-widget-hide ,entry-box)))))
       (gtk:gtk-widget-grab-focus ,entry-box)
       (gtk:gtk-widget-show ,entry-box))))

(defmethod get-widget ((browser browser) widget-name)
  (gtk:gtk-builder-get-object (ui browser) widget-name))

(defmethod initialize-instance :after ((browser browser) &key)
  (check-type (ui browser) gtk:gtk-builder))

(defun new-browser (ui webview)
  (let ((tabs (list webview)))
    (make-instance 'browser
                   :ui ui
                   :webview webview
                   :tabs tabs)))

(defun new-page-listener (browser)
  (lambda (notebook page page-num)
    (declare (ignore notebook page))
    (setf (webview browser) (elt (tabs browser) page-num))))

(defun goto-last-tab (browser)
  (let ((notebook (get-widget browser "webviewcontainer")))
    (gtk-notebook-set-current-page
     notebook (1- (gtk-notebook-get-n-pages notebook)))))

(defmacro add-tab (browser tab)
  (with-gensyms (br)
    `(let ((,br ,browser))
       (setf (tabs ,br) (append (tabs ,br) (list ,tab))))))

(defmethod create-new-tab ((browser browser))
  (let* ((notebook   (get-widget browser "webviewcontainer"))
         (scrollview (gtk-scrolled-window-new))
         (webview    (webkit.foreign:webkit-web-view-new)))
    (gtk-container-add scrollview webview)
    (gtk-notebook-append-page notebook scrollview (cffi:null-pointer))
    (setf (webview browser) webview)
    (add-tab browser webview)
    (load-url *default-page* browser)
    (dolist (widget (list scrollview webview))
      (gtk-widget-show widget))
    (goto-last-tab browser)
    (values)))

(defun load-url (url browser)
  (webkit.foreign:webkit-web-view-load-uri (webview browser) url))

(defcommand reload-page "Reload the current page." (browser)
  (webkit.foreign:webkit-web-view-reload (webview browser)))

(defcommand forwards-page "Move forwards a page" (browser)
  (webkit.foreign:webkit-web-view-go-forward (webview browser)))

(defcommand backwards-page "Move backwards a page." (browser)
  (webkit.foreign:webkit-web-view-go-back (webview browser)))

(defcommand browse-url "Browse the the named URL." (browser)
  (with-browser-input browser url
    (if (purl:url-p url)
        (load-url url browser)
        (apply-jumps url browser))))

(defcommand zoom "Zoom the browser view in." (browser)
  (with-webview wv browser
    (webkit.foreign:webkit-web-view-zoom-in wv)))

(defcommand unzoom "Unzoom the browser view." (browser)
  (with-webview wv browser
    (webkit.foreign:webkit-web-view-zoom-out wv)))

(defun move-tabs (browser op)
  (let ((notebook (get-widget browser "webviewcontainer")))
    (funcall op notebook)))

(defcommand next-tab "Move to the next tab." (browser)
  (move-tabs browser #'gtk-notebook-next-page))

(defcommand prev-tab "Move to the next tab." (browser)
  (move-tabs browser #'gtk-notebook-prev-page))

(defcommand new-tab "Create a new tab." (browser)
  (create-new-tab browser))
