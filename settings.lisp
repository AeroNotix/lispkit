(in-package :lispkit)

(defparameter *cookie-path-dir* (merge-pathnames  #P"lispkit/" (get-xdg-config-dir)))
(defparameter *cookie-type* cl-webkit2:webkit-cookie-persistent-storage-text)
(defparameter *cookie-accept-policy* cl-webkit2:webkit-cookie-policy-accept-always)
