;;;; Loads all files needed to run cl-lex code.
;;;; Copyright (C) 2009 David J. Rosenbaum, email: davidjrosenbaum@comcast.net
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, under version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; Copyright (C) 2009 David J. Rosenbaum
;;;; This program comes with ABSOLUTELY NO WARRANTY; for details see COPYING.
;;;; This is free software, and you are welcome to redistribute it
;;;; under certain conditions; for details see COPYING.

(in-package :cl-user)

(let ((cl-lex-directory (make-pathname :name nil :type nil :defaults *load-truename*)))
  (with-compilation-unit ()
    (require :asdf)
    (asdf:operate 'asdf:load-op :cl-ppcre)
    (dolist (file '("packages" "lex"))
      (load (make-pathname :name file :type "lisp" :defaults cl-lex-directory)))))

(provide :cl-lex)
