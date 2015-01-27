(defpackage #:lispkit-test
  (:use :cl :lispkit))

(in-package #:lispkit-test)

(5am:def-suite main)
(5am:def-suite commands)
(5am:def-suite events)
(5am:def-suite jumps)
(5am:def-suite keys)
(5am:def-suite settings)
(5am:def-suite user)
