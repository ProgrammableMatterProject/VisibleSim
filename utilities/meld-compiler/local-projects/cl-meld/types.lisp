(in-package :cl-meld)

(defun type-list-p (x) (tagged-p x :type-list))
(defun make-list-type (ty) `(:type-list ,ty))
(defun type-list-element (x) (second x))

(defun type-array-p (x) (tagged-p x :type-array))
(defun make-array-type (ty) `(:type-array ,ty))
(defun type-array-element (x) (second x))

(defun type-struct-p (x) (tagged-p x :type-struct))
(defun make-struct-type (ls) `(:type-struct ,ls))
(defun type-struct-list (x) (second x))

(defun make-type-node (x) `(:type-node ,x))
(defun type-node-type (x) (second x))
(defun type-node-p (x) (tagged-p x :type-node))

(defparameter *number-types* '(:type-int :type-float))
(defparameter *list-number-types* (mapcar #'make-list-type *number-types*))
(defparameter *list-types* '((:type-list :all)))
(defparameter *all-types* '(:all))

(defun is-all-type-p (x)
	(or (and (listp x) (one-elem-p x) (eq :all (first x)))
		 (and x (eq (first x) :all))))

(defun has-all-type-p (x) (find-anywhere :all x))

(defmacro deftype-p (&rest types)
   `(on-top-level
         ,@(mapcar #'(lambda (x) `(defun ,(alexandria:format-symbol t "TYPE-~A-P" (symbol-name x)) (ty)
                                       (eq ,(alexandria:format-symbol "KEYWORD" "TYPE-~A" (symbol-name x)) ty)))
                  types)))

(deftype-p int addr bool string float thread)

(defun valid-type-p (typ)
	(cond
		((or (type-int-p typ) (type-addr-p typ)
			  (type-bool-p typ) (type-string-p typ)
           (type-node-p typ)
			  (type-float-p typ))
			t)
		((type-list-p typ)
			(valid-type-p (type-list-element typ)))
		((type-struct-p typ)
			(every #'valid-type-p (type-struct-list typ)))
      ((type-array-p typ)
         (valid-type-p (type-array-element typ)))
      (t
       (assert nil)
       nil)))

(defun type-to-string (typ)
   (cond
    ((type-addr-p typ) "addr")
    ((type-int-p typ) "int")
    ((type-float-p typ) "float")
    ((type-bool-p typ) "bool")
    ((type-string-p typ) "string")
    ((type-node-p typ) (tostring "node ~a" (type-node-type typ)))
    ((type-list-p typ)
     (tostring "list ~a" (type-to-string (type-list-element typ))))
    ((type-array-p typ)
     (tostring "array ~a" (type-to-string (type-array-element typ))))
    ((type-struct-p typ)
     (let ((str "struct "))
      (loop for ty in (type-struct-list typ)
            for i from 0
            do (setf str (concatenate 'string (concatenate 'string str (if (= i 0) "[" ", "))
                                 (type-to-string ty))))
      str))
    (t
     (assert nil)
     "")))

(defun type-operands (op &optional forced-types)
   (cond
      ((eq-arith-p op)
			(if (is-all-type-p forced-types)
				*number-types*
         	(if forced-types
            	(intersection forced-types *number-types*)
            	*number-types*)))
		((eq-num-cmp-p op)
			(if (or (has-elem-p forced-types :type-bool)
						(is-all-type-p forced-types))
				*number-types*))
      ((eq-cmp-p op)
         (if (or forced-types
                 (not (has-elem-p forced-types :type-bool)))
				*all-types*))
		(t (warn "not valid operands") nil)))

(defun type-op (op &optional forced-types)
   (cond
      ((eq-arith-p op)
			(if (is-all-type-p forced-types)
				*number-types*
         	(if forced-types
            	(intersection *number-types* forced-types)
            	'*number-types*)))
      ((eq-cmp-p op)
			(if (is-all-type-p forced-types)
				'(:type-bool)
         	(if forced-types
            	(intersection '(:type-bool) forced-types)
            	'(:type-bool))))))

(defun type-oper-op (op forced-types)
   (cond
      ((eq-arith-p op)
         (intersection *number-types* forced-types))
      ((eq-cmp-p op) '(:type-bool))))
      
(defun expr-type (expr)
   (cond
      ((or (nil-p expr) (host-id-p expr) (cpus-p expr) (world-p expr)) (second expr))
      ((or (var-p expr) (int-p expr) (bool-p expr) (float-p expr) (addr-p expr) (tail-p expr)
           (head-p expr) (not-p expr) (test-nil-p expr)
           (convert-float-p expr)
			  (get-constant-p expr)
           (argument-p expr)
			  (struct-p expr))
         (third expr))
      ((or (op-p expr) (struct-val-p expr) (call-p expr) (callf-p expr) (cons-p expr))
         (fourth expr))
      ((or (let-p expr) (if-p expr)) (fifth expr))
      (t (assert nil) (error 'type-invalid-error :text (tostring "expr-type: cannot deduce type of expression ~a" expr)))))
      
(defun typed-var-p (var) (and (= (length var) 3)))
(defun single-typed-var-p (var)
	(when (typed-var-p var)
		(if (valid-type-p (third var))
			t
			(and (one-elem-p (third var))
					(valid-type-p (first (third var)))))))
(defun typed-op-p (op) (= (length op) 4))
(defun typed-int-p (i) (= (length i) 3))

(defun same-types-p (types1 types2)
	(set-equal-p types1 types2))

(defun type-eq-p (ty1 ty2) (equal ty1 ty2))
(defun simple-type-eq-p (ty1 ty2) (or (eq ty1 ty2)
                                       (and (listp ty1) (listp ty2) (eq (first ty1) (first ty2)))))

(defun recursive-type-p (typ)
	(or (type-struct-p typ) (type-list-p typ)
       (type-array-p typ)))

(defun reference-type-p (typ)
	(or (eq typ :all) (type-string-p typ)
      (type-node-p typ)
      (type-addr-p typ) (recursive-type-p typ)))

(defparameter *program-types* nil)

(defun add-type-to-typelist (types new)
   (assert new)
	(if (member new types :test #'equal)
		types
		(cond
			((type-list-p new)
            (push-end new (add-type-to-typelist types (type-list-element new))))
			((type-struct-p new)
				(dolist (x (type-struct-list new))
					(setf types (add-type-to-typelist types x)))
            (push-end new types))
         ((type-array-p new)
            (push-end new (add-type-to-typelist types (type-array-element new))))
			(t (push-end new types)))))
		
(defun lookup-type-id (typ)
	(let ((ret (position typ *program-types* :test #'equal)))
		(assert (integerp ret))
		ret))

