(in-package #:cl-user)

(ql:quickload :lispkit)

#+sbcl
(sb-ext:save-lisp-and-die "lispkit" :toplevel #'lispkit:do-main :executable t)

#+clisp
(ext:saveinitmem "lispkit" :init-function (lambda ()
                                            (lispkit:main)
                                            (ext:quit))
                 :executable t :keep-global-handlers t :norc t)

