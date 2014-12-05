(in-package :lispkit)

(defparameter *cookie-path-dir* (merge-pathnames  #P"lispkit/" (get-xdg-config-dir)))
(defparameter *cookie-type* :webkit-cookie-persistent-storage-text)
(defparameter *cookie-accept-policy* :webkit-cookie-policy-accept-always)

(ensure-directories-exist *cookie-path-dir*)
