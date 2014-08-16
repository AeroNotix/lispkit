;; -*- mode: common-lisp -*-
(defsystem lispkit
  :version "0.0.1"
  :description "Lispy browser"
  :licence "BSD"
  :components ((:file "lispkit" :depends-on ("macros" "packages" "keys"))
               (:file "macros"  :depends-on ("packages"))
               (:file "keys"    :depends-on ("packages" "browser" "events"))
               (:file "browser" :depends-on ("packages" "events" "jumps"))
               (:file "events"  :depends-on ("packages"))
               (:file "jumps"   :depends-on ("packages"))
               (:file "packages"))
  :depends-on (:cl-cffi-gtk :cl-webkit :cl-xkeysym :split-sequence))
