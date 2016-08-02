(in-package :cl-meld)

(define-condition external-invalid-error (error)
   ((text :initarg :text :reader text)))

(defparameter *external-functions* (make-hash-table :test #'equal))
(defparameter *external-functions-counter* 0)

(defun lookup-custom-external-function (name)
	(find-if (lambda (x) (string-equal name (extern-name x))) *externs*))
	
(defun lookup-custom-external-function-id (name)
	(extern-id (lookup-custom-external-function name)))
	
(defun lookup-standard-external-function (name)
	(multiple-value-bind (extern found-p) (gethash name *external-functions*)
		(if found-p
			extern)))

(defun lookup-external-definition (name)
	;; lookup pre-defined externals first
	(multiple-value-bind (extern found-p) (gethash name *external-functions*)
		(unless found-p
			;; if not, look for user defined
			(let ((item (lookup-custom-external-function name)))
				(when item
					(setf found-p t
							extern item))))
		(unless found-p
			(error 'external-invalid-error :text (tostring "invalid external function: ~a" name)))
		extern))
		
(defun lookup-external-function-id (name)
	(extern-id (lookup-external-definition name)))

(defmacro define-external-function (name ret-type types)
   `(progn
      (setf (gethash ,name *external-functions*) (make-extern ,name ,ret-type ,types *external-functions-counter*))
      (incf *external-functions-counter*)))

(define-external-function "sigmoid" :type-float '(:type-float))
(define-external-function "randint" :type-int '(:type-int))
(define-external-function "normalize" '(:type-list :type-float) '((:type-list :type-float)))
(define-external-function "damp" '(:type-list :type-float) '((:type-list :type-float) (:type-list :type-float) :type-float))
(define-external-function "divide" '(:type-list :type-float) '((:type-list :type-float) (:type-list :type-float)))
(define-external-function "convolve" '(:type-list :type-float) '((:type-list :type-float) (:type-list :type-float)))
(define-external-function "addfloatlists" '(:type-list :type-float) '((:type-list :type-float) (:type-list :type-float)))
(define-external-function "intlistlength" :type-int '((:type-list :type-int)))
(define-external-function "intlistdiff" '(:type-list :type-int) '((:type-list :type-int) (:type-list :type-int)))
(define-external-function "intlistnth" :type-int '((:type-list :type-int) :type-int))
(define-external-function "concatenate" :type-string '(:type-string :type-string))
(define-external-function "str2float" :type-float '(:type-string))
(define-external-function "str2int" :type-int '(:type-string))
(define-external-function "nodelistremove" :type-list-addr '(:type-list-addr :type-addr))
(define-external-function "wastetime" :type-int '(:type-int))
(define-external-function "truncate" :type-float '(:type-float :type-int))
(define-external-function "float2int" :type-int '(:type-float))
(define-external-function "int2str" :type-string '(:type-int))
(define-external-function "float2str" :type-string '(:type-float))
(define-external-function "intlistsub" '(:type-list :type-int) '((:type-list :type-int) :type-int :type-int))
(define-external-function "intlistappend" '(:type-list :type-int) '((:type-list :type-int) (:type-list :type-int)))
(define-external-function "str2intlist" '(:type-list :type-int) '(:type-string))
(define-external-function "filecountwords" :type-int '(:type-string :type-int))
(define-external-function "residual" :type-float '((:type-list :type-float) (:type-list :type-float)))
(define-external-function "nodelistlength" :type-int '((:type-list :type-addr)))
(define-external-function "nodelistcount" :type-int '((:type-list :type-addr) :type-addr))
(define-external-function "nodelistappend" '(:type-list :type-addr) '((:type-list :type-addr) (:type-list :type-addr)))
(define-external-function "nodepriority" :type-float '(:type-addr))
(define-external-function "nodelistreverse" '(:type-list :type-addr) '((:type-list :type-addr)))
(define-external-function "nodelistlast" :type-addr '((:type-list :type-addr)))
(define-external-function "cpu-id" :type-int '(:type-addr))
(define-external-function "node2int" :type-int '(:type-addr))
(define-external-function "intpower" :type-int '(:type-int :type-int))
(define-external-function "intlistsort" '(:type-list :type-int) '((:type-list :type-int)))
(define-external-function "intlistremoveduplicates" '(:type-list :type-int) '((:type-list :type-int)))
(define-external-function "degeneratevector" '(:type-list :type-int) '(:type-int :type-int))
(define-external-function "demergemessages" '(:type-list :type-int) '((:type-list :type-int) (:type-list :type-int)))
(define-external-function "intlistequal" :type-int '((:type-list :type-int) (:type-list :type-int)))
