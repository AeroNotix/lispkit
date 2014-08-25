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
    :initarg :url-bar)
   (grabbing-keys?
    :initform nil
    :accessor grabbing-keys?)
   (default-keymaps
    :initarg :default-keymaps
    :initform nil
    :accessor default-keymaps)
   (keymaps
    :initarg :keymaps
    :initform nil
    :accessor keymaps)))

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

(defun make-browser (ui webview &optional (keymaps (list *emacs-map* *help-map* *top-map*)))
  (let ((tabs (list webview)))
    (make-instance 'browser
                   :ui ui
                   :webview webview
                   :tabs tabs
                   :default-keymaps keymaps
                   :keymaps keymaps)))

(defun make-page-listener (browser)
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

(defun close-tab-at (browser i)
  (gtk-widget-destroy (elt (tabs browser) i)))

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

(defcommand reload-page (browser)
  "Reload the current page."
  (webkit.foreign:webkit-web-view-reload (webview browser)))

(defcommand forwards-page (browser)
  "Move forwards a page"
  (webkit.foreign:webkit-web-view-go-forward (webview browser)))

(defcommand backwards-page (browser)
  "Move backwards a page."
  (webkit.foreign:webkit-web-view-go-back (webview browser)))

(defcommand browse-url (browser)
  "Browse the the named URL."
  (with-browser-input browser url
    (if (purl:url-p url)
        (load-url url browser)
        (apply-jumps url browser))))

(defcommand zoom (browser)
  "Zoom the browser view in."
  (with-webview wv browser
    (webkit.foreign:webkit-web-view-zoom-in wv)))

(defcommand unzoom (browser)
  "Unzoom the browser view."
  (with-webview wv browser
    (webkit.foreign:webkit-web-view-zoom-out wv)))

(defun move-tabs (browser op)
  (let ((notebook (get-widget browser "webviewcontainer")))
    (funcall op notebook)))

(defcommand next-tab (browser)
  "Move to the next tab."
  (move-tabs browser #'gtk-notebook-next-page))

(defcommand prev-tab (browser)
  "Move to the next tab."
  (move-tabs browser #'gtk-notebook-prev-page))

(defcommand new-tab (browser)
  "Create a new tab."
  (create-new-tab browser))

(defun remove-nth (list n)
  (if (> n (length list))
      list
      (remove-if (constantly t) list :start (max (1- n) 0) :end n)))

(defcommand close-tab (browser)
  "Closes the current tab."
  (let* ((notebook (get-widget browser "webviewcontainer"))
         (current-tab (gtk-notebook-get-current-page notebook))
         (tabs (remove-nth (tabs browser) current-tab)))
    (gtk-notebook-remove-page notebook current-tab)
    (close-tab-at browser current-tab)
    (setf (tabs browser) tabs)))

(defcommand open-manual (browser)
  "Open a help page describing all commands."
  (create-new-tab browser)
  (let* ((keydescs (apply #'keymap->keydesc (default-keymaps browser)))
         (html     (djula:render-template* +helppage+ nil :keymaps keydescs)))
    (webkit.foreign:webkit-web-view-load-html (webview browser) html "")))

(defcommand describe-command (browser)
  "Describes what a command does."
  (create-new-tab browser)
  (with-browser-input browser command-name
    (when-let* ((command (first (command-p command-name)))
                (html    (djula:render-template* +command-info+
                                                 nil
                                                 :command-name command-name
                                                 :command-desc (doc command))))
      (webkit.foreign:webkit-web-view-load-html (webview browser) html ""))))

(defcommand i-search (browser) "Executes a search on the current webview."
  (with-browser-input browser search-term
    (let ((fc (webkit.foreign:webkit-web-view-get-find-controller (webview browser))))
      (webkit.foreign:webkit-find-controller-search fc search-term 1 1))))

(defun search-with-direction (browser op)
  (let ((fc (webkit.foreign:webkit-web-view-get-find-controller (webview browser))))
    (funcall op fc)))

(defcommand search-next (browser) "Finds the next instance of the search term."
  (search-with-direction browser #'webkit.foreign:webkit-find-controller-search-next))

(defcommand search-previous (browser) "Finds the previous instance of the search term."
  (search-with-direction browser #'webkit.foreign:webkit-find-controller-search-previous))

(defcommand cancel (browser) "Cancels any pending commands or contextual UI elements."
  (maphash #'(lambda (name fn)
               (declare (ignore name))
               (funcall fn browser)) *cancel-functions*))

(defcancel hide-input-bar (browser)
  (let ((entry-box (get-widget browser "entry_box")))
    (gtk-widget-hide entry-box)
    (reset-key-state browser)))
