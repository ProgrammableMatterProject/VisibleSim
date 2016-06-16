
(in-package :cl-user)

(defpackage :cl-meld
 (:use :cl :cl-lex :yacc :arnesi :ieee-floats)
 (:import-from :alexandria :format-symbol :with-gensyms :when-let)
 (:import-from :flexi-streams :make-in-memory-output-stream)
 (:export :comp :meld-compile :meld-compile-list))

(arnesi:enable-sharp-l)
