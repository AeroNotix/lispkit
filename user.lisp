(in-package :lispkit)


(defun get-xdg-config-dir ()
  (let ((dir (uiop:getenv "XDG_CONFIG_HOME")))
    (if (or (not dir) (string= dir ""))
        (merge-pathnames  #p".config/" (user-homedir-pathname))
        dir)))

(defun get-rc-file ()
  (let* ((xdg-config-dir (get-xdg-config-dir))
         (user-rc (probe-file (merge-pathnames (user-homedir-pathname) #p".lispkitrc")))
         (conf-rc (probe-file (merge-pathnames #P"lispkit/config" xdg-config-dir)))
         (etc-rc  (probe-file #p"/etc/lispkit")))
    (or user-rc conf-rc etc-rc)))
