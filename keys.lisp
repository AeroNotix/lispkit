(in-package :lispkit)

(defclass keymap ()
  ((bindings :initarg :bindings
             :initform (make-hash-table :test #'equal)
             :accessor bindings)))

(defun make-keymap ()
  (make-instance 'keymap))

(defun keymap->keydesc* (name entry)
  (if (typep entry 'keymap)
      (list name (keymap->keydesc entry))
      (list name entry (doc (first (command-p entry))))))

(defun keymap->keydesc (&rest entries)
  (apply
   #'append
   (mapcar #'(lambda (entry)
               (loop for key being the hash-keys of (bindings entry)
                       using (hash-value value)
                     collect (keymap->keydesc* key value)))
           entries)))

(defun reset-key-state (browser)
  (setf (keymaps browser) (default-keymaps browser))
  (setf (grabbing-keys? browser) nil))

(defmethod (setf keymaps) :after (keymaps (browser browser))
  (unless (equal keymaps (default-keymaps browser))
    (setf (grabbing-keys? browser) t)))

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
       (reset-key-state browser))
      ((grabbing-keys? browser) (reset-key-state browser))
      (t (return-from handle-key nil)))
    ;; If we reached this, we've handled the key in some way.
    t))

(defun make-key-dispatcher (browser)
  (lambda (window event)
    (declare (ignore window))
    (handle-key browser (event-as-string event))))

(defun define-key (map key function-name)
  (setf (gethash key (bindings map)) function-name))

(defvar *emacs-map* (make-keymap))
(defvar *help-map* (make-keymap))
(defvar *emacs-c-x-map* (make-keymap))
(defvar *help-c-h-map* (make-keymap))
(defvar *top-map* (make-keymap))

(define-key *emacs-map* "C-x" *emacs-c-x-map*)
(define-key *help-map* "C-h" *help-c-h-map*)

(define-key *top-map* "C-s" "search-next")
(define-key *top-map* "C-r" "search-previous")
(define-key *top-map* "C-SunPageUp" "next-tab")
(define-key *top-map* "C-SunPageDown" "prev-tab")
(define-key *top-map* "F5" "reload-page")

(define-key *emacs-c-x-map* "C-Left" "backwards-page")
(define-key *emacs-c-x-map* "C-Right" "forwards-page")
(define-key *emacs-c-x-map* "C-f" "browse-url")
(define-key *emacs-c-x-map* "plus" "zoom")
(define-key *emacs-c-x-map* "minus" "unzoom")
(define-key *emacs-c-x-map* "n" "next-tab")
(define-key *emacs-c-x-map* "p" "prev-tab")
(define-key *emacs-c-x-map* "k" "new-tab")
(define-key *emacs-c-x-map* "w" "close-tab")
(define-key *emacs-c-x-map* "s" "i-search")

(define-key *help-c-h-map*  "m" "open-manual")
(define-key *help-c-h-map*  "f" "describe-command")
