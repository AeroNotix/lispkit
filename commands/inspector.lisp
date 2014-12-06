(in-package :lispkit)


(defun inspector-get (browser)
  "Gets the webkit inspector of a browser instance."
  (webkit2:webkit-web-view-get-inspector (webview browser)))

(defcommand inspector-open (browser)
  "Open the webkit inspector tools."
  (let* ((web-view (webview browser))
         (inspector (webkit2:webkit-web-view-get-inspector web-view)))
    (setf (slot-value (webkit2:webkit-web-view-get-settings web-view) 'webkit2::enable-developer-extras) t)
    (webkit2:webkit-web-inspector-show inspector)))

(defcommand inspector-close (browser)
  "Closes the webkit inspector tools."
  (webkit2:webkit-web-inspector-close (inspector-get browser)))

(defcommand inspector-attach (browser)
  "Attaches the webkit inspector tools."
  (webkit2:webkit-web-inspector-attach (inspector-get browser)))

(defcommand inspector-detach (browser)
  "Detaches the webkit inspector tools."
  (webkit2:webkit-web-inspector-detach (inspector-get browser)))

(defcommand inspector-toggle (browser)
  "Toggles the state of webkit inspector tools."
  (if (gtk-widget-is-visible (webkit2:webkit-web-inspector-get-web-view (inspector-get browser)))
      (run-named-command "inspector-close" browser)
      (run-named-command "inspector-open" browser)))

(defcommand inspector-toggle-attachment (browser)
  "Toggles the attachment state of webkit inspector tools."
  (if (webkit2:webkit-web-inspector-is-attached (inspector-get browser))
      (run-named-command "inspector-detach" browser)
      (run-named-command "inspector-attach" browser)))
