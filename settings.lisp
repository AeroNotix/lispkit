(in-package :lispkit)

(defparameter *cookie-path-dir* (merge-pathnames  #P"lispkit/" (get-xdg-config-dir)))
(defparameter *cookie-type* cl-webkit.foreign:webkit-cookie-persistent-storage-text)
(defparameter *cookie-accept-policy* cl-webkit.foreign:webkit-cookie-policy-accept-always)
