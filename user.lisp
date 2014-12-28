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

(defun get-history-file ()
  (let* ((xdg-config-dir (get-xdg-config-dir))
         (user-history
          (merge-pathnames (user-homedir-pathname) #p".lispkithistory"))
         (conf-history
          (merge-pathnames #P"lispkit/history" xdg-config-dir)))
    (or
     (or (probe-file user-history)
         (probe-file conf-history))
     (progn
       (with-open-file (stream conf-history :direction :probe
                               :if-does-not-exist :create))
       (probe-file conf-history)))))
