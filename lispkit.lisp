(in-package :lispkit)


(defparameter *default-page*
  "http://www.this-page-intentionally-left-blank.org/")

(djula:add-template-directory (asdf:system-relative-pathname :lispkit "templates/"))
(defvar +helppage+ (djula:compile-template* "helppage.dtl"))
(defvar +command-info+ (djula:compile-template* "command-info.dtl"))
(defvar +ui-builder-file+
  (read-file-into-string (asdf:system-relative-pathname :lispkit "main.ui")))

(defparameter *default-browser* nil)

(defun main (&optional (destroy? nil))
  "Main exists separately from do-main so that during development we
  can easily separate killing the main gtk loop from stopping and
  starting applications within that main loop"
  (within-main-loop
    (let ((browser (ui-build-browser destroy?)))
      (load-url *default-page* browser)
      (setf *default-browser* browser)
      (ensure-cookies-folder-exists *cookie-path-dir*)
      (setup-link-hints)
      ;; TODO - Add error handling to this.
      (load-rc-file))))


(defun do-main (&rest args)
  "The main entry point when running as an executable. This should not
   be run directly but only indirectly when an image has been built."
  (declare (ignore args))
  (main t)
  (ui-start-loop))
