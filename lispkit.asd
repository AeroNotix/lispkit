;; -*- mode: common-lisp -*-
(defsystem lispkit
  :version "0.0.1"
  :description "Lispy browser"
  :licence "BSD"
  :serial t
  :components ((:file "packages")
               (:file "user")
               (:file "settings")
               (:file "commands")
               (:file "commands/inspector")
               (:file "macros")
               (:file "modeline")
               (:file "jumps")
               (:file "events")
               (:file "browser")
               (:file "keys")
	       (:file "ui")
               (:file "lispkit"))
  :depends-on (:asdf
               :alexandria
               :cl-cffi-gtk
               :cl-ppcre
               :cl-webkit2
               :cl-xkeysym
               :djula
               :parenscript
               :purl
               :split-sequence
               :lparallel
               :bordeaux-threads
               :usocket)
  :in-order-to ((test-op (test-op :lispkit-test))))
