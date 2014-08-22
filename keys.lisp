(in-package :lispkit)

(defclass keymap ()
  ((bindings :initarg :bindings
             :initform (make-hash-table :test #'equal)
             :accessor bindings)))

(defun handle-key (browser key)
  (let ((binding (find-if #'identity
                          (mapcar (lambda (keymap)
                                    (gethash key (bindings keymap)))
                                  (keymaps browser)))))
    (cond
      ((typep binding 'keymap)
       (setf (keymaps browser) (list binding)))
      ((consp binding)
       ;; We assume it's a list of keymaps
       (setf (keymaps browser) binding))
      ((stringp binding)
       (run-named-command binding browser)
       (setf (keymaps browser) (default-keymaps browser))))))

(defun make-key-dispatcher (browser)
  (lambda (window event)
    (declare (ignore window))
    (handle-key browser (event-as-string event))))

(defun define-key (map key function-name)
  (setf (gethash key (bindings map)) function-name))

(defvar *emacs-map* (make-instance 'keymap))
(defvar *emacs-c-x-map* (make-instance 'keymap))

(define-key *emacs-map* "C-x" *emacs-c-x-map*)
(define-key *emacs-c-x-map* "F5"      "reload-page")
(define-key *emacs-c-x-map* "C-Left"  "backwards-page")
(define-key *emacs-c-x-map* "C-Right" "forwards-page")
(define-key *emacs-c-x-map* "C-f"     "browse-url")
(define-key *emacs-c-x-map* "plus"    "zoom")
(define-key *emacs-c-x-map* "minus"   "unzoom")
(define-key *emacs-c-x-map* "n"       "next-tab")
(define-key *emacs-c-x-map* "p"       "prev-tab")
(define-key *emacs-c-x-map* "k"       "new-tab")
