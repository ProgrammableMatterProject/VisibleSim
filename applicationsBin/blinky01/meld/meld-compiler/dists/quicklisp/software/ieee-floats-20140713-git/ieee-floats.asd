(defpackage :ieee-floats-system
  (:use :common-lisp :asdf))
(in-package :ieee-floats-system)

(defsystem :ieee-floats
  :components ((:file "ieee-floats")))

(defsystem :ieee-floats-tests
  :depends-on (:ieee-floats :eos)
  :components ((:file "tests")))
