(in-package :lispkit)

(defparameter *cookie-path-dir* (merge-pathnames  #P"lispkit/" (get-xdg-config-dir)))
(defparameter *cookie-type* :webkit-cookie-persistent-storage-text)
(defparameter *cookie-accept-policy* :webkit-cookie-policy-accept-always)

(defun ensure-cookies-folder-exists (path)
  "Ensures that the cookies folder exists."
  (handler-case (ensure-directories-exist path)
    (file-error () (format *error-output* "Unable to ensure that the cookies folder ~s exists." path))))
