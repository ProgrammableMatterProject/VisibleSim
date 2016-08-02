
(in-package :cl-user)

(defpackage :cl-meld
 (:use :cl :cl-lex :yacc :ieee-floats)
 (:import-from :flexi-streams :make-in-memory-output-stream)
 (:import-from :arnesi :sharpl-reader)
 (:export :comp :meld-compile :meld-compile-exit :meld-compile-list))

(arnesi:enable-sharp-l)
