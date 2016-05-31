(in-package :cl-meld)

(define-condition type-invalid-error (error)
   ((text :initarg :text :reader text)))

(defun check-home-argument (name typs)
   (when (null typs)
      (error 'type-invalid-error :text (concatenate 'string name " has no arguments")))
   (let ((fst (first typs)))
      (unless (or (type-addr-p fst) (type-thread-p fst) (type-node-p fst))
         (error 'type-invalid-error
            :text (concatenate 'string "first argument of tuple " name " must be of type 'node'")))))
         
(defun no-types-p (ls) (null ls))

(defun merge-type (t1 t2)
	(assert (not (null t1)))
	(assert (not (null t2)))
	(if (equal t1 t2)
		(return-from merge-type t1))
	(cond
		((eq t1 :all) t2)
		((eq t2 :all) t1)
		((and (listp t1) (not (listp t2)) (eq t2 (first t1))) t2)
		((and (listp t2) (not (listp t1)) (eq t1 (first t2))) t1)
		((and (listp t1) (not (listp t2))) nil)
		((and (not (listp t2)) (listp t2)) nil)
      ((and (type-node-p t1) (type-node-p t2))
        (let ((a (type-node-type t1))
              (b (type-node-type t2)))
         (if (string-equal a b)
          t1 nil)))
      ((and (type-addr-p t1) (type-node-p t2)) t2)
      ((and (type-node-p t1) (type-addr-p t2)) t1)
		((and (type-list-p t1) (type-list-p t2))
		 (let* ((sub1 (type-list-element t1))
				  (sub2 (type-list-element t2))
				  (merged (merge-type sub1 sub2)))
			(if merged
				(make-list-type merged))))
      ((and (type-array-p t1) (type-array-p t2))
       (let* ((sub1 (type-array-element t1))
              (sub2 (type-array-element t2))
              (merged (merge-type sub1 sub2)))
        (if merged
         (make-array-type merged))))
		((and (type-struct-p t1) (type-struct-p t2))
			(let ((l1 (type-struct-list t1))
					(l2 (type-struct-list t2)))
				(cond
               ((eq l2 :all) t1)
               ((eq l1 :all) t2)
					((not (= (length l1) (length l2))) nil)
					(t
						(let ((result
									(loop for t1 in l1
											for t2 in l2
											collect (merge-types t1 t2))))
							(if (find-if #'null result)
								nil
								(make-struct-type result)))))))
		((and (listp t1) (listp t2))
			(merge-types t1 t2))
		((and (not (listp t1)) (not (listp t2)))
			(if (eq t1 t2)
				t1
				nil))
		(t nil)))
		
(defun merge-with-suitable-list-type (types subtype)
	(let ((ls (filter #'type-list-p types)))
		(if (eq subtype :all)
			ls
			(let ((f (find-if #L(eq (type-list-element !1) subtype) ls)))
				(when f
					`(,f))))))
		
(defun merge-types (ls types)
	(cond
		((= (length ls) (length types) 1)
			(let ((t1 (first ls))
					(t2 (first types)))
				(cond
					((and (type-list-p t1)
							(type-list-p t2))
						(let ((t11 (type-list-element t1))
								(t22 (type-list-element t2)))
							(let ((merged-type (merge-type t11 t22)))
								(when merged-type
									`(,(make-list-type merged-type))))))
					(t
                (let ((merged (merge-type t1 t2)))
                  (when merged
                   `(,merged)))))))
		((= (length ls) 1)
			(let ((t1 (first ls)))
				(if (eq t1 :all)
					types
					(if (type-list-p t1)
						(let ((sub (type-list-element t1)))
							(merge-with-suitable-list-type types sub))
						(intersection ls types)))))
		((= (length types) 1)
			(let ((t2 (first types)))
            (cond
             ((eq t2 :all) ls)
             ((type-list-p t2)
              (let* ((t21 (type-list-element t2))
                     (list-types (filter #'type-list-p ls))
                     (elements (mapcar #'type-list-element list-types))
                     (merged (loop for element in elements
                                   append (let ((m (merge-types (list element) (list t21))))
                                                (when m
                                                   (list (make-list-type (first m))))))))
               merged))
             (t
					(intersection ls types)))))
		(t (intersection ls types :test #'equal))))
   
(defparameter *constraints* nil)
(defparameter *defined* nil)
(defparameter *defined-in-context* nil)
(defparameter *node-constraints* nil)

(defmacro with-node-type-context (&body body)
   `(let ((*node-constraints* (make-hash-table)))
      ,@body))

(defun add-node-constraint (addr typ)
   (multiple-value-bind (type-found found-p) (gethash (addr-num addr) *node-constraints*)
      (cond
       ((not (or (type-addr-p typ) (type-node-p typ)))
         (error 'type-invalid-error :text (tostring "add-node-constraint: invalid node type ~a" typ)))
       (found-p
         (let ((result (merge-type type-found typ)))
          (unless result
           (error 'type-invalid-error :text (tostring "add-node-constraint: could not merge types '~a' and '~a' for node ~a"
                                             (type-to-string type-found) (type-to-string typ) (addr-num addr))))
          (setf (gethash (addr-num addr) *node-constraints*) result)
          result))
       (t
        (setf (gethash (addr-num addr) *node-constraints*) typ)
        typ))))

(defmacro with-typecheck-context (&body body)
   `(let ((*defined* nil)
          (*defined-in-context* nil)
          (*constraints* (make-hash-table)))
      ,@body))

(defmacro extend-typecheck-context (&body body)
   `(let ((*defined* (copy-list *defined*))
          (*defined-in-context* nil)
          (*constraints* (copy-hash-table *constraints*)))
      ,@body))

(defun reset-typecheck-context ()
	(setf *defined* nil)
	(setf *defined-in-context* nil)
	(setf *constraints* (make-hash-table))) 

(defun variable-is-defined (var)
   (unless (has-elem-p *defined* (var-name var))
      (push (var-name var) *defined-in-context*)
      (push (var-name var) *defined*)))
(defun variable-defined-p (var) (has-elem-p *defined* (var-name var)))
(defun has-variables-defined (expr) (every #'variable-defined-p (all-variables expr)))

(defun set-type (expr typs)
   (let ((typ (list (try-one typs))))
      (when (and (one-elem-p typs)
                  (not (has-all-type-p typs)))
         (setf *program-types* (add-type-to-typelist *program-types* (first typs))))
      (cond
         ((or (nil-p expr) (host-p expr) (cpus-p expr) (world-p expr) (host-id-p expr)) (setf (cdr expr) typ))
         ((or (var-p expr) (bool-p expr) (int-p expr) (float-p expr) (string-constant-p expr) (tail-p expr) (head-p expr)
               (not-p expr) (test-nil-p expr) (addr-p expr) (convert-float-p expr)
					(get-constant-p expr) (struct-p expr))
            (setf (cddr expr) typ))
         ((or (call-p expr) (op-p expr) (callf-p expr)
				  (cons-p expr) (struct-val-p expr))
				(setf (cdddr expr) typ))
         ((or (let-p expr) (if-p expr)) (setf (cddddr expr) typ))
			((or (argument-p expr))) ; do nothing
         (t (error 'type-invalid-error :text (tostring "set-type: Unknown expression ~a" expr))))))
      
(defun force-constraint (var new-types)
   (multiple-value-bind (types ok) (gethash var *constraints*)
      (when ok
         (let ((old-types new-types))
            (setf new-types (merge-types types new-types))
            (when (no-types-p new-types)
               (error 'type-invalid-error :text
                     (tostring "Type error in variable ~a: new constraint are types '~a' but variable is set as '~a'"
                         var (type-to-string (first old-types)) (type-to-string (first types)))))))
      (set-var-constraint var new-types)))

(defun set-var-constraint (var new-types)
	(assert (listp new-types))
	(setf (gethash var *constraints*) new-types))
(defun get-var-constraint (var)
   (gethash var *constraints*))
   
(defun select-simpler-types (types)
	(cond
		((= (length types) 1) types)
		((= (length types) 2)
			(if (and (has-elem-p types :type-int)
						(has-elem-p types :type-float))
				'(:type-int)
				(if (every #'type-list-p types)
					(let* ((list-types (mapcar #'type-list-element types))
							 (simplified (select-simpler-types list-types)))
						(if (= (length simplified) 1)
							(make-list-type (first simplified))
							types))
					types)))
		(t types)))

(defun get-list-elements (forced-types)
	(if (is-all-type-p forced-types)
		forced-types
		(mapcar #'type-list-element (filter #'type-list-p forced-types))))
		
(defun get-struct-lists (forced-types)
	(if (is-all-type-p forced-types)
		forced-types
		(mapcar #'type-struct-list (filter #'type-struct-p forced-types))))

(defun unify-types (typ concrete-type concrete-template)
   "Unify 'typ' by matching the concrete-template with concrete-type."
   (cond
    ((eq concrete-template :all)
     (subst concrete-type :all typ :test #'equal))
    ((type-list-p concrete-template)
     (unify-types typ (type-list-element concrete-type) (type-list-element concrete-template)))
    ((type-array-p concrete-template)
     (unify-types typ (type-array-element concrete-type) (type-array-element concrete-template)))
    ((type-struct-p concrete-template)
     (cond
      ((eq (type-struct-list concrete-template) :all)
       (subst (type-struct-list concrete-type) :all typ :test #'equal))
      (t
        (loop for conc in (type-struct-list concrete-type)
              for temp in (type-struct-list concrete-template)
              do (setf typ (unify-types typ conc temp)))
        typ)))
    (t typ)))

(defun unify-arg-types (arg-types template-ret ret-types)
   (unless (has-all-type-p template-ret)
      (return-from unify-arg-types arg-types))
   (cond
    ((and (one-elem-p ret-types)
         (not (has-all-type-p ret-types)))
     (let ((f (first ret-types)))
        (loop for arg in arg-types
               collect (unify-types arg f template-ret))))
    (t arg-types)))

(defun find-all-type (template concrete)
   (cond
    ((eq template :all) concrete)
    ((type-list-p template) (find-all-type (type-list-element template) (type-list-element concrete)))
    ((type-array-p template) (find-all-type (type-array-element template) (type-array-element concrete)))
    ((type-struct-p template)
     (when (eq (type-struct-list template) :all)
      (return-from find-all-type concrete))
     (loop for temp in (type-struct-list template)
           for conc in (type-struct-list concrete)
           do (let ((x (find-all-type temp conc)))
               (when x
                (return-from find-all-type x))))
     (assert nil))
    (t
     nil)))
         
(defun get-type (expr forced-types body-p)
	(assert (not (null forced-types)))
	(assert (not (null (first forced-types))))
	(labels ((do-get-type (expr forced-types)
            (cond
					((string-constant-p expr) (merge-types forced-types '(:type-string)))
               ((addr-p expr)
                  (when (one-elem-p forced-types)
                     (let ((ty (first forced-types)))
                      (cond
                       ((is-all-type-p forced-types)
                        (add-node-constraint expr :type-addr)
                        '(:type-addr))
                       ((type-addr-p ty)
                        (add-node-constraint expr ty)
                        '(:type-addr))
                       ((type-node-p ty)
                        (add-node-constraint expr ty)
                        `(,ty))
                       (t nil)))))
					((or (host-p expr) (host-id-p expr))
                  (when (one-elem-p forced-types)
                     (let ((ty (first forced-types)))
                      (cond
                       ((is-all-type-p forced-types) '(:type-addr))
                       ((type-addr-p ty) '(:type-addr))
                       ((type-node-p ty) `(,ty))
                       (t nil)))))
               ((var-p expr)
						(cond
							(body-p
								(variable-is-defined expr)
								(let ((ret (force-constraint (var-name expr) forced-types)))
                         ret))
							(t
								(when (not (variable-defined-p expr))
									(error 'type-invalid-error :text
												(tostring "variable ~a is not defined" (var-name expr))))
                        (let ((ret (force-constraint (var-name expr) forced-types)))
                         ret))))
					((bool-p expr) (merge-types forced-types '(:type-bool)))
               ((int-p expr) (let ((new-types (merge-types forced-types '(:type-int :type-float))))
											(if (one-elem-this new-types :type-float)
												(transform-int-to-float expr))
											new-types))
               ((float-p expr) (merge-types forced-types '(:type-float)))
					((argument-p expr) (merge-types forced-types '(:type-string)))
					((get-constant-p expr)
						(with-get-constant expr (:name name)
							(let ((const (lookup-const name)))
								(unless const
									(error 'type-invalid-error :text
										(tostring "could not find constant ~a" name)))
								(merge-types forced-types (list (constant-type const))))))
               ((if-p expr)
                  (get-type (if-cmp expr) '(:type-bool) body-p)
                  (let ((t1 (get-type (if-e1 expr) forced-types body-p))
                        (t2 (get-type (if-e2 expr) forced-types body-p)))
                     (unless (equal t1 t2)
                        (error 'type-invalid-error :text
                              (tostring "expressions ~a and ~a must have equal types" (if-e1 expr) (if-e2 expr))))
                     t1))
					((callf-p expr)
						(let ((fun (lookup-function (callf-name expr))))
							(unless fun (error 'type-invalid-error :text (tostring "undefined call ~a" (callf-name expr))))
							(when (not (= (length (function-args expr)) (length (callf-args fun))))
								(error 'type-invalid-error :text
										(tostring "function call ~a has invalid number of arguments (should have ~a arguments)"
											expr (length (function-args fun)))))
							(loop for var in (function-args fun)
									for arg in (callf-args expr)
									do (progn
										(get-type arg `(,(var-type var)) body-p)))
							(merge-types forced-types `(,(function-ret-type fun)))))
               ((call-p expr)
                  (let ((extern (lookup-external-definition (call-name expr))))
                     (unless extern (error 'type-invalid-error :text (tostring "undefined call ~a" (call-name expr))))
                     (when (not (= (length (extern-types extern)) (length (call-args expr))))
								(error 'type-invalid-error :text
									(tostring "external call ~a has invalid number of arguments (should have ~a arguments)"
										extern (length (extern-types extern)))))
                     (let* ((ret-types (merge-types forced-types `(,(extern-ret-type extern))))
                            (arg-types (extern-types extern))
                            (unified-arg-types (unify-arg-types arg-types (extern-ret-type extern) ret-types)))
                        (loop for typ in unified-arg-types
                              for arg in (call-args expr)
                              do (get-type arg `(,typ) body-p))
                        (let (change (concrete-arg-types (mapcar #'expr-type (call-args expr))))
                           (loop for i from 0 upto (1- (length concrete-arg-types))
                                 for template-arg-type in arg-types
                                 do (let ((concrete-type (nth i concrete-arg-types)))
                                       (when (and (not (has-all-type-p concrete-type))
                                                   (has-all-type-p template-arg-type))
                                          (setf change t)
                                          (setf ret-types (loop for ret-type in ret-types
                                                                collect (unify-types ret-type concrete-type template-arg-type)))
                                          (setf concrete-arg-types (loop for ty in concrete-arg-types
                                                                         collect (unify-types ty concrete-type template-arg-type))))))
                           (when change
                              (loop for typ in concrete-arg-types
                                    for arg in (call-args expr)
                                    do (get-type arg `(,typ) body-p))))
                        ret-types)))
               ((let-p expr)
                  (if (variable-defined-p (let-var expr))
                     (error 'type-invalid-error :text (tostring "Variable ~a in LET is already defined" (let-var expr))))
                  (let* (ret
                         constraints
                         (var (let-var expr))
                         (typ-expr (get-type (let-expr expr) *all-types* body-p)))
                     (extend-typecheck-context
                        (force-constraint (var-name var) typ-expr)
                        (variable-is-defined var)
                        (setf ret (get-type (let-body expr) forced-types body-p))
                        (setf constraints (get-var-constraint (var-name var))))
                     (when (and (equal typ-expr constraints)
                                 (> (length constraints) 1))
                        
                        (error 'type-invalid-error :text
                              (tostring "Type of variable ~a cannot be properly defined. Maybe it is not being used in the LET?" var)))
                     (get-type (let-expr expr) constraints body-p) 
                     ret
                  ))
               ((convert-float-p expr)
                  (get-type (convert-float-expr expr) '(:type-int) body-p)
                  (merge-types forced-types '(:type-float)))
               ((nil-p expr) (merge-types forced-types *list-types*))
               ((world-p expr) (merge-types '(:type-int) forced-types))
               ((cpus-p expr) (merge-types '(:type-int) forced-types))
					((struct-val-p expr)
						(let* ((idx (struct-val-idx expr))
							    (var (struct-val-var expr)) ;; var is already typed
								 (vart (var-type var))
								 (ls (type-struct-list vart)))
							(merge-types forced-types `(,(nth idx ls)))))
					((struct-p expr)
						(let ((types (get-struct-lists forced-types)))
							(cond
								((is-all-type-p types)
									(list (make-struct-type (loop for subexpr in (struct-list expr)
																		collect (let ((ty (get-type subexpr types body-p)))
																						(if (and (listp ty) (one-elem-p ty))
																							(first ty)
																							ty))))))
								(t
									(loop for typ-list in types
											do (when (= (length typ-list) (length (struct-list expr)))
													(let ((new-types (loop for typ in typ-list
																				for subexpr in (struct-list expr)
																				collect (get-type subexpr `(,typ) body-p))))
														(when (every #'(lambda (x) (not (null x))) new-types)
															(return-from do-get-type `(,(make-struct-type (mapcar #'(lambda (x) (first x)) new-types))))))))))))
               ((cons-p expr)
                  (let* ((tail (cons-tail expr))
                         (head (cons-head expr))
                         (base-types (get-list-elements forced-types))
                         (head-types (get-type head base-types body-p))
								 (list-head-types (mapcar #'make-list-type head-types))
                         (new-types (merge-types list-head-types forced-types)))
                     ;(warn "types ~a base-types ~a head-types ~a list-head-types ~a new-types ~a" forced-types
                     ; base-types head-types list-head-types new-types)
							(let ((tail-type (get-type tail new-types body-p)))
									(when tail-type
										;; re-updated head-type
										(let* ((base-types (if (is-all-type-p tail-type) tail-type
																		(mapcar #'type-list-element tail-type)))
												 (head-types (get-type head base-types body-p)))
											tail-type)))))
               ((head-p expr)
                  (let ((ls (head-list expr))
                        (list-types (mapcar #'make-list-type forced-types)))
                     (mapcar #'type-list-element (get-type ls list-types body-p))))
               ((tail-p expr)
                  (get-type (tail-list expr) forced-types body-p))
               ((not-p expr)
                  (merge-types forced-types (get-type (not-expr expr) '(:type-bool) body-p))) 
               ((test-nil-p expr)
                  (get-type (test-nil-expr expr) *list-types* body-p)
                  (merge-types forced-types '(:type-bool)))
               ((op-p expr)
                  (let* ((op1 (op-op1 expr)) (op2 (op-op2 expr)) (op (op-op expr))
                         (typ-oper (type-operands op forced-types)) (typ-op (type-op op forced-types)))
							(when (no-types-p typ-op)
                        (error 'type-invalid-error :text (tostring "no types error for result ~a or operands ~a" typ-op typ-oper)))
                     (let ((t1 (get-type op1 typ-oper body-p)) (t2 (get-type op2 typ-oper body-p)))
                        (when (< (length t1) (length t2))
                           (setf t2 (get-type op2 t1 body-p)))
                        (when (< (length t2) (length t1))
                           (setf t1 (get-type op1 t2 body-p)))
								(let ((oper-type (merge-types t1 t2)))
									(unless oper-type
										(error 'type-invalid-error :text (tostring "can't merge operand types ~a ~a" t1 t2)))
									(setf oper-type (get-type op1 oper-type body-p))
									(setf oper-type (get-type op2 oper-type body-p))
									(setf t1 oper-type)
									(setf t2 oper-type)
									(when (and (= (length oper-type) 2) (one-elem-p forced-types) (eq (first forced-types) :type-bool))
										;; if having more than two types, select the simpler one
										(setf t1 (get-type op1 (select-simpler-types oper-type) body-p))
                           	(setf t2 (get-type op2 (select-simpler-types oper-type) body-p)))
									(unless (equal t1 t2)
										(error 'type-invalid-error :text (tostring "expressions ~a and ~a have different types: ~a ~a" op1 op2 (first t1) (first t2))))
                        	(type-oper-op op t1)))))
               (t (error 'type-invalid-error :text (tostring "get-type: Unknown expression ~a" expr))))))
      (let ((types (do-get-type expr forced-types)))
         (when (no-types-p types)
            (error 'type-invalid-error :text (tostring "Type error in expression ~a: wanted types ~a but got ~a" expr forced-types types)))
         (set-type expr types)
         types)))
      
(defun do-type-check-subgoal (name args options &key (body-p nil) (axiom-p nil))
	(let* ((def (lookup-definition name))
          (definition (definition-types def)))
      (unless def
         (error 'type-invalid-error :text (concatenate 'string "Definition " name " not found")))
      (when (not (= (length definition) (length args)))
         (error 'type-invalid-error :text (tostring "Invalid number of arguments in subgoal ~a~a" name args)))
      (cond
         ((is-linear-p def) ;; linear fact
            (dolist (opt options)
               (case opt
                  (:reused
                     (unless body-p
                        (error 'type-invalid-error :text (tostring "Linear reuse of facts must be used in the body, not the head: ~a" name))))
                  (:persistent
                     (error 'type-invalid-error :text (tostring "Only persistent facts may use !: ~a" name)))
						(:random)
						(:delay)
						(:linear)
                  (otherwise
							(cond
								((listp opt))
								(t
                     		(error 'type-invalid-error :text (tostring "Unrecognized option ~a for subgoal ~a" opt name))))))))
         (t ;; persistent fact
            (let ((has-persistent-p nil))
               (dolist (opt options)
                  (case opt
							(:linear (error 'type-invalid-error :text (tostring "Only linear facts may use ?: ~a" name)))
                     (:reused
                        (error 'type-invalid-error :text (tostring "Reuse option $ may only be used with linear facts: ~a" name)))
                     (:persistent
                        (setf has-persistent-p t))
							(:random)
							(:delay)
							(otherwise
								(cond
									((listp opt))
									(t
                        		(error 'type-invalid-error :text (tostring "Unrecognized option ~a for subgoal ~a" opt name)))))))
               (unless has-persistent-p
                  (warn (tostring "Subgoal ~a needs to have a !" name))))))
      (dolist2 (arg args) (forced-type (definition-arg-types definition))
			(assert arg)
         (let ((type-ret (get-type arg `(,forced-type) body-p)))
            (unless type-ret
               (error 'type-invalid-error :text (tostring "subgoal argument ~a from subgoal ~a~a has no type." arg name args)))
				(unless (one-elem-p type-ret)
            	(error 'type-invalid-error :text (tostring "type error ~a type ~a" arg type-ret)))))))

(defun do-type-check-agg-construct (c in-body-p clause)
	(let ((old-defined (copy-list *defined*))
         (types nil))
   	(extend-typecheck-context
			(do-subgoals (agg-construct-body c) (:name name :args args :options options)
		     	(do-type-check-subgoal name args options :body-p t)))
	 	(transform-agg-constructs-constants c)
		(extend-typecheck-context
			(do-subgoals (agg-construct-body c) (:args args)
				(dolist (arg args)
					(when (var-p arg)
						(variable-is-defined arg)
						(force-constraint (var-name arg) `(,(var-type arg))))))
			(create-assignments (agg-construct-body c))
			(assert-assignment-undefined (get-assignments (agg-construct-body c)))
			(do-type-check-assignments (agg-construct-body c))
			(do-constraints (agg-construct-body c) (:expr expr)
		     (do-type-check-constraints expr))
			(optimize-agg-construct-constraints c clause)
         (type-check-clause-head-assignments (agg-construct-head0 c))
			(type-check-all-subgoals-and-conditionals (agg-construct-head0 c))
         ;; replace spec variable's types.
			(do-agg-specs (agg-construct-specs c) (:op op :var to :args args)
				(case op
               (:custom
                  (let* ((fun (first args))
                         (start (second args))
                         (vtype (get-var-constraint (var-name to)))
                         (start-type (get-type start vtype nil))
                         (extern (lookup-external-definition fun))
                         (ret-type (extern-ret-type extern))
                         (args-type (extern-types extern)))
                   (assert (one-elem-p vtype))
                   (set-type to vtype)
                   (unless (and (every #L(equal !1 ret-type) args-type)
                                 (= (length args-type) 2))
                     (error 'type-invalid-error :text (tostring "External function ~a must have equal types." fun)))
                   (set-type start start-type)
                   (let ((res (merge-type (first vtype) ret-type)))
                     (unless res
                        (error 'type-invalid-error :text (tostring "Types ~a and ~a from external ~a do not match." (first vtype) ret-type fun))))))
					((:min :sum)
						(let* ((vtype (get-var-constraint (var-name to))))
							(assert (= 1 (length vtype)))
							(set-type to vtype)
							(set-var-constraint (var-name to) vtype)))
					(:collect
						(let* ((vtype (get-var-constraint (var-name to)))
							 	(vtype-list (mapcar #'make-list-type vtype)))
							(assert (= 1 (length vtype)))
							(set-type to vtype-list)
							(set-var-constraint (var-name to) vtype-list)))
					(:count
						(variable-is-defined to)
						(set-type to '(:type-int))
						(set-var-constraint (var-name to) '(:type-int)))
               (otherwise
                (error 'type-invalid-error :text (tostring "Not handling operator ~a" op)))))
         (type-check-clause-head-assignments (agg-construct-head c))
			(type-check-all-subgoals-and-conditionals (agg-construct-head c))
			(cleanup-assignments-from-agg-construct c)
			(optimize-subgoals (agg-construct-head c) (append (clause-body clause) (agg-construct-body c)))
			(optimize-subgoals (agg-construct-head0 c) (append (clause-body clause) (agg-construct-body c)))
			(cleanup-assignments-from-agg-construct c)
			(let ((new-ones *defined-in-context*)
					(target-variables (mapcar #'var-name (agg-construct-vlist c))))
              (do-agg-specs (agg-construct-specs c) (:op op :var to :args args)
					(when (or (eq op :sum)
				   			 (eq op :collect)
								 (eq op :count)
								 (eq op :min))
                  (push (var-name to) target-variables)))
               (unless (subsetp new-ones target-variables)
                  (error 'type-invalid-error :text (tostring "Aggregate ~a is using more variables than it specifies ~a -> ~a" c new-ones target-variables)))
               (unless (subsetp target-variables new-ones)
                  (error 'type-invalid-error :text (tostring "Aggregate ~a is not using enough variables ~a ~a" c target-variables new-ones)))))))

(defun do-type-check-constraints (expr)
	;; LET has problems with this
	;(unless (has-variables-defined expr)
   ;   (error 'type-invalid-error :text (tostring "all variables must be defined: ~a , ~a" expr (all-variables expr))))
   (let ((typs (get-type expr '(:type-bool) nil)))
      (unless (and (one-elem-p typs) (type-bool-p (first typs)))
         (error 'type-invalid-error :text "constraint must be of type bool"))))

(defun update-assignment (assignments assign)
	(let* ((var (assignment-var assign)) (var-name (var-name var)))
      (multiple-value-bind (forced-types ok) (gethash var-name *constraints*)
         (let ((ty (get-type (assignment-expr assign) (if ok forced-types *all-types*) t)))
            (variable-is-defined var)
				(force-constraint var-name ty)
            (set-type var ty)
				(dolist (used-var (all-variables (assignment-expr assign)))
               (alexandria:when-let ((other (find-if #'(lambda (a)
                                             (and (var-eq-p used-var (assignment-var a))
                                                   (not (one-elem-p (expr-type (assignment-var a))))))
                                    assignments)))
                  (update-assignment assignments other)))))))

(defun assert-assignment-undefined (assignments)
   (unless (every #'(lambda (a) (not (variable-defined-p a))) (get-assignment-vars assignments))
      (error 'type-invalid-error :text "some variables are already defined")))

(defun do-type-check-assignments (body)
	(let ((assignments (get-assignments body)))
      (loop while assignments
            for assign = (find-if #'(lambda (a)
                                       (has-variables-defined (assignment-expr a)))
                                 assignments)
            do (unless assign
                  (error 'type-invalid-error :text (tostring "undefined variables ~a" assignments)))
					(setf assignments (delete assign assignments :test #'equal))
               (when (< 1 (count-if #L(var-eq-p (assignment-var assign) !1) (get-assignment-vars assignments)))
                  (error 'type-invalid-error :text "cannot set multiple variables"))
					(update-assignment assignments assign))))

(defun create-assignments (body)
   "Turn undefined equal constraints to assignments"
   (let (vars)
      (do-constraints body (:expr expr :constraint orig)
         (let ((op1 (op-op1 expr)) (op2 (op-op2 expr)))
            (when (and (op-p expr) (equal-p expr) (var-p op1)
                        (not (variable-defined-p op1))
                        (not (has-elem-p vars (var-name op1))))
         		;; changes constraints to assignments
		        	(setf (first orig) :assign)
		        	(setf (second orig) op1)
		        	(setf (cddr orig) (list op2))
		        	(push (var-name op1) vars))))))

(defun unfold-cons (mangled-var cons)
   (let* ((tail-var (generate-random-var (expr-type cons)))
          (tail (cons-tail cons))
			 (head (cons-head cons))
			 (c1 (make-constraint (make-not (make-test-nil mangled-var)) 100)))
		(multiple-value-bind (new-head head-constraints head-vars) (transform-constant-to-constraint head)
			(let ((c2 (make-constraint (make-equal new-head '= (make-head mangled-var)))))
      		(cond
         		((cons-p tail)
						(multiple-value-bind (tail-constraints tail-vars)
								(unfold-cons tail-var tail)
							(values (append `(,c1 ,c2
														,(make-constraint (make-equal tail-var '= (make-tail mangled-var))))
											(append head-constraints tail-constraints))
								`(,tail-var ,@tail-vars ,@head-vars))))
         		(t
						(values `(,c1 ,c2 ,@head-constraints ,(make-constraint (make-equal tail '= (make-tail mangled-var))))
							`(,tail-var ,@head-vars))))))))
							
(defun unfold-struct (mangled-var struct)
	(let* (all-constraints all-vars)
		(let ((new-expr-list (loop for expr in (struct-list struct)
											for typ in (type-struct-list (expr-type struct))
											for i = 0 then (1+ i)
											collect (let ((sval (make-struct-val i mangled-var typ)))
															(multiple-value-bind (new-var constraints new-vars)
																		(transform-constant-to-constraint expr)
																(push (make-constraint (make-equal new-var '= sval)) all-constraints)
																(setf all-constraints (append all-constraints constraints))
																(setf all-vars (append all-vars new-vars))
																new-var)))))
			(setf (struct-list struct) new-expr-list)
			(values all-constraints all-vars))))

(defun transform-constant-to-constraint (arg &optional use-host-p)
   (cond
		((var-p arg)
			(values arg nil nil))
		((and (addr-p arg) use-host-p)
			(values (make-host-id) `(,(make-constraint (make-equal (make-host-id) '= arg))) nil))
		((and (get-constant-p arg) use-host-p)
			(values (make-host-id) `(,(make-constraint (make-equal (make-host-id) '= arg))) nil))
		((const-p arg)
			(let ((new-var (generate-random-var (expr-type arg))))
				(values new-var `(,(make-constraint (make-equal new-var '= arg))) `(,new-var))))
		((nil-p arg)
			(let ((new-var (generate-random-var (expr-type arg))))
				(values new-var `(,(make-constraint (make-test-nil new-var) 100)) `(,new-var))))
		((cons-p arg)
          (let ((new-var (generate-random-var (expr-type arg))))
				(multiple-value-bind (new-constraints new-vars)
						(unfold-cons new-var arg)
					(values new-var new-constraints `(,new-var ,@new-vars)))))
		((struct-p arg)
			(let ((new-var (generate-random-var (expr-type arg))))
				(multiple-value-bind (new-constraints new-vars)
					(unfold-struct new-var arg)
					(values new-var new-constraints `(,new-var ,@new-vars)))))
		((op-p arg)
			(let ((new-var (generate-random-var (expr-type arg))))
				(values new-var `(,(make-constraint (make-equal new-var '= arg))) `(,new-var))))
		(t (error 'type-invalid-error :text (tostring "subgoal argument ~a is invalid" arg)))))
				
(defun optimize-constraints-assignments (assigns constraints &optional constant-assigns constant-constraints)
	"Optimizes constraints by computing constant expressions.
	Returns new optimized set of constraints and assignments."
	(let ((new-constraints constraints) (new-assigns assigns))
		(loop for ass in assigns
				do (setf (assignment-expr ass) (optimize-expr (assignment-expr ass) (append constraints constant-constraints) (append assigns constant-assigns))))
		(loop for constr in constraints
				do (let ((result (optimize-expr (constraint-expr constr) (append assigns constant-assigns) (append (remove-tree constraints constr) constant-constraints))))
						(cond
							((and (bool-p result) (bool-val result))
								(warn "CONSTRAINT ~a is always true!" constr)
								;; remove this constraint because it is always true
								(delete-one new-constraints constr))
							((and (bool-p result) (not (bool-val result)))
								;; just a warning
								(warn "CONSTRAINT ~a is always false!" constr))
							(t
								(setf (constraint-expr constr) result)))))
		(append new-assigns new-constraints)))
	
(defun optimize-clause-constraints (clause)
	(let* ((assigns (get-assignments (clause-body clause)))
	  	 	 (constraints (get-constraints (clause-body clause)))
		 	 (new-body (remove-all (remove-all (clause-body clause) assigns) constraints))
		 	 (new-assigns-constraints (optimize-constraints-assignments assigns constraints)))
		(setf (clause-body clause) (append new-body new-assigns-constraints))))
		
(defun optimize-comprehension-constraints (compr clause)
	(let* ((assigns (get-assignments (comprehension-left compr)))
			 (constraints (get-constraints (comprehension-left compr)))
			 (new-body (remove-all (remove-all (comprehension-left compr) assigns) constraints))
			 (new-assigns-constraints (optimize-constraints-assignments assigns constraints
				(get-assignments (clause-body clause)) (get-constraints (clause-body clause)))))
		(setf (comprehension-left compr) (append new-body new-assigns-constraints))))
		
(defun optimize-agg-construct-constraints (agg clause)
	(let* ((assigns (get-assignments (agg-construct-body agg)))
			 (constraints (get-constraints (agg-construct-body agg)))
			 (new-body (remove-all (remove-all (agg-construct-body agg) assigns) constraints))
			 (new-assigns-constraints (optimize-constraints-assignments assigns constraints 
				(get-assignments (clause-body clause)) (get-constraints (clause-body clause)))))
		(setf (agg-construct-body agg) (append new-body new-assigns-constraints))))

(defun transform-clause-constants (clause)
	"Removes all constants from the subgoal arguments by creating constraints."
	(let ((found-variables (make-hash-table :test #'equal)))
   	(do-subgoals (clause-body clause) (:args args :subgoal sub)
			(let ((new-args (loop for arg in args
										 for i from 0
										collect (cond
														((and (var-p arg) (= i 0)) arg)
														((var-p arg)
															(multiple-value-bind (found found-p) (gethash (var-name arg) found-variables)
																(cond
																	(found-p
																		(let ((new-var (generate-random-var (var-type arg))))
																			(push-end (make-constraint (make-equal new-var '= arg)) (clause-body clause))
																			new-var))
																	(t (setf (gethash (var-name arg) found-variables) arg)))))
														(t
															(multiple-value-bind (new-arg new-constraints)
																(transform-constant-to-constraint arg nil)
																(dolist (new-c new-constraints)
																	(assert (constraint-p new-c))
																	(push-end new-c (clause-body clause)))
																new-arg))))))
      	(setf (subgoal-args sub) new-args)))))

(defun transform-constants-to-constraints-comprehension (comp args)
	(mapcar #'(lambda (arg)
						(multiple-value-bind (new-arg new-constraints)
							(transform-constant-to-constraint arg)
							(dolist (new-constraint new-constraints)
								(push-end new-constraint (comprehension-left comp)))
							new-arg))
				args))
				
(defun transform-comprehension-constants (comp)
	(do-subgoals (comprehension-left comp) (:args args :subgoal sub)
		(setf (subgoal-args sub) (transform-constants-to-constraints-comprehension comp args))))

(defun transform-constants-to-constraints-agg-construct (c args &optional only-addr-p)
   (mapcar #'(lambda (arg)
						(multiple-value-bind (new-arg new-constraints new-vars)
							(transform-constant-to-constraint arg only-addr-p)
							(when new-vars
								(setf (agg-construct-vlist c) (append (agg-construct-vlist c) new-vars)))
							(dolist (new-constraint new-constraints)
								(assert (constraint-p new-constraint))
								(push-end new-constraint (agg-construct-body c)))
							new-arg))
				args))
				
(defun transform-agg-constructs-constants (c)
	(do-subgoals (agg-construct-body c) (:args args :subgoal sub)
		(setf (subgoal-args sub) (transform-constants-to-constraints-agg-construct c args))))

(defun add-variable-head-clause (clause)
   (do-subgoals (clause-head clause) (:args args :subgoal sub)
		(multiple-value-bind (new-arg constraints)
			(transform-constant-to-constraint (first args) t)
			(dolist (constraint constraints)
				(push constraint (clause-body clause)))
			(setf (first (subgoal-args sub)) new-arg))))
                     
(defun add-variable-head ()
   (do-rules (:clause clause)
      (add-variable-head-clause clause))
   (do-all-var-axioms (:clause clause)
      (add-variable-head-clause clause)))
      
(defun do-type-check-comprehension (comp clause)
   (let ((target-variables (mapcar #'var-name (comprehension-variables comp))))
      (extend-typecheck-context
			(type-check-comprehension comp clause)
         ;; check if the set of new defined variables is identical to target-variables
         (let ((new-ones *defined-in-context*))
            (unless (subsetp new-ones target-variables)
               (error 'type-invalid-error :text (tostring "Comprehension ~a is using more variables than it specifies" comp)))
            (unless (subsetp target-variables new-ones)
               (error 'type-invalid-error :text (tostring "Comprehension ~a is not using enough variables ~a ~a" comp target-variables new-ones)))))))

(defun do-type-check-exists (vars body clause)
	(extend-typecheck-context
		(dolist (var vars)
			(force-constraint (var-name var) '(:type-addr))
      	(variable-is-defined var))
      (do-type-check-head body clause :axiom-p nil)))
      
(defun type-check-body (clause host thread axiom-p)
	(do-subgoals (clause-body clause) (:name name :args args :options options)
      (handler-case
         (do-type-check-subgoal name args options :body-p t)
         (type-invalid-error (c) (error 'type-invalid-error :text
                                  (tostring "In clause ~a~%~a" (clause-to-string clause) (text c))))))
   (do-agg-constructs (clause-body clause) (:agg-construct c)
      (do-type-check-agg-construct c t clause))
   (transform-clause-constants clause)
	(reset-typecheck-context)
	(when axiom-p
		(variable-is-defined host)
		(force-constraint (var-name host) '(:type-addr)))
	(do-subgoals (clause-body clause) (:args args)
		(dolist (arg args)
			(when (var-p arg)
				(variable-is-defined arg)
				(force-constraint (var-name arg) `(,(var-type arg))))))
   (create-assignments (clause-body clause))
	(assert-assignment-undefined (get-assignments (clause-body clause)))
	(do-type-check-assignments (clause-body clause))
	(do-constraints (clause-body clause) (:expr expr)
      (do-type-check-constraints expr))
	(optimize-clause-constraints clause))

(defun do-type-check-conditional (cond clause &key (axiom-p nil))
	(with-conditional cond (:cmp cmp :term1 term1 :term2 term2)
		(do-type-check-constraints cmp)
		(do-type-check-head term1 clause :axiom-p axiom-p)
		(do-type-check-head term2 clause :axiom-p axiom-p)))
	
(defun cleanup-assignments-from-clause (clause)
	(let ((new-body (remove-unneeded-assignments (clause-body clause) (clause-head clause))))
   	(do-type-check-assignments new-body)
		(setf (clause-body clause) new-body)))
		
(defun cleanup-assignments-from-comprehension (comp)
	(let ((new-left (remove-unneeded-assignments (comprehension-left comp) (comprehension-right comp))))
		(do-type-check-assignments new-left)
		(setf (comprehension-left comp) new-left)))
		
(defun cleanup-assignments-from-agg-construct (agg)
	(let ((new-body (remove-unneeded-assignments (agg-construct-body agg) (append (agg-construct-head0 agg) (agg-construct-head agg)))))
   	(do-type-check-assignments new-body)
		(setf (agg-construct-body agg) new-body)))
		
(defun type-check-clause-head-subgoals (clause-head &key axiom-p)
	(do-subgoals clause-head (:name name :args args :options options)
      (do-type-check-subgoal name args options :axiom-p axiom-p)))

(defun type-check-clause-head-assignments (clause-head)
   ;; transforms equal constraints to assignments
   (create-assignments clause-head)
	(do-type-check-assignments clause-head))

(defun do-type-check-head (head clause &key axiom-p)
	(type-check-clause-head-subgoals head :axiom-p axiom-p)
	(do-comprehensions head (:comprehension comp)
     	(do-type-check-comprehension comp clause))
	(do-agg-constructs head (:agg-construct c)
		(do-type-check-agg-construct c nil clause))
	(do-exists head (:var-list vars :body body)
		(do-type-check-exists vars body clause)
		(optimize-subgoals body (clause-body clause)))
	(do-conditionals head (:conditional cond)
      (optimize-conditional cond (clause-body clause))
		(do-type-check-conditional cond clause :axiom-p axiom-p)))

(defun type-check-all-except-body (clause host thread &key axiom-p)
	(when (and host axiom-p (not (variable-defined-p host)))
		(variable-is-defined host))
   (when (and thread axiom-p (not (variable-defined-p thread)))
       (variable-is-defined thread))
	(do-type-check-head (clause-head clause) clause :axiom-p axiom-p))
		
(defun optimize-subgoals (subgoals ass-constrs)
	(let ((ass (get-assignments ass-constrs))
			(constrs (get-constraints ass-constrs)))
		(do-subgoals subgoals (:args args :subgoal sub)
			(let ((new-args (mapcar #L(optimize-expr !1 ass constrs) args)))
				(setf (subgoal-args sub) new-args)))))

(defun optimize-conditional (cond body)
   (let ((ass (get-assignments body))
         (constrs (get-constraints body)))
    (setf (conditional-cmp cond) (optimize-expr (conditional-cmp cond) ass constrs))))
		
(defun type-check-body-and-head (clause host thread &key axiom-p)
	(type-check-body clause host thread axiom-p)
   (type-check-clause-head-assignments (clause-head clause))
	(type-check-all-except-body clause host thread :axiom-p axiom-p)
	(optimize-subgoals (clause-head clause) (clause-body clause))
	;; we may need to re-check subgoals again because of optimizations
	(type-check-clause-head-subgoals (clause-head clause) :axiom-p axiom-p)
	(cleanup-assignments-from-clause clause))
	
(defun type-check-all-subgoals-and-conditionals (head)
	(do-subgoals head (:name name :args args :options options)
   	(do-type-check-subgoal name args options))
	(do-conditionals head (:cmp cmp :term1 term1 :term2 term2)
		(do-type-check-constraints cmp)
		(type-check-all-subgoals-and-conditionals term1)
		(type-check-all-subgoals-and-conditionals term2)))

(defun type-check-comprehension (comp clause)
	(extend-typecheck-context
		(with-comprehension comp (:left left :right right)
			(do-subgoals left (:name name :args args :options options)
		   	(do-type-check-subgoal name args options :body-p t))))
	(transform-comprehension-constants comp)
	(with-comprehension comp (:left left :right right)
		(do-subgoals left (:args args)
			(dolist (arg args)
				(when (var-p arg)
					(variable-is-defined arg)
					(force-constraint (var-name arg) `(,(var-type arg))))))
	   (create-assignments left)
	   (assert-assignment-undefined (get-assignments left))
		(do-type-check-assignments left)
		(do-constraints left (:expr expr)
	      (do-type-check-constraints expr)))
	(optimize-comprehension-constraints comp clause)
	(with-comprehension comp (:right right)
      (type-check-clause-head-assignments right)
		(type-check-all-subgoals-and-conditionals right)
		(optimize-subgoals (recursively-get-subgoals right) (append (comprehension-left comp) (clause-body clause))))
	(cleanup-assignments-from-comprehension comp))
						
(defun type-check-clause (clause axiom-p)
	(with-typecheck-context
      (multiple-value-bind (host thread) (find-host-nodes clause)
			(type-check-body-and-head clause host thread :axiom-p axiom-p))
		;; add :random to every subgoal with such variable
		(when (clause-has-random-p clause)
			(let ((var (clause-get-random-variable clause)))
				(unless (variable-defined-p var)
					(error 'type-invalid-error :text
						(tostring "can't randomize variable ~a because such variable is not defined in the subgoal body" var)))
				(do-subgoals (clause-body clause) (:subgoal sub)
					(when (subgoal-has-var-p sub var)
						(subgoal-add-option sub :random)))))
		;; add :min to every subgoal with such variable
		(when (clause-has-min-p clause)
			(let ((var (clause-get-min-variable clause))
					(involved-variables nil))
				(unless (variable-defined-p var)
					(error 'type-invalid-error :text
						(tostring "can't minimize variable ~a because such variable is not defined in the subgoal body" var)))
				(do-subgoals (clause-body clause) (:subgoal sub)
					(when (subgoal-has-var-p sub var)
						(subgoal-add-min sub var)
						(with-subgoal sub (:args args)
							(dolist (arg (rest args))
								(unless (var-eq-p var arg)
									(push arg involved-variables))))))))))

(defun type-check-const (const)
	(with-constant const (:name name :expr expr)
		(let* ((first-types (get-type expr *all-types* nil))
				 (res (select-simpler-types first-types)))
			(unless (one-elem-p res)
				(error 'type-invalid-error :text (tostring "could not determine type of const ~a" name)))
			(unless (same-types-p first-types res)
				(get-type expr res nil))
			(unless (valid-type-p (first res))
				(error 'type-invalid-error :text (tostring "could not determine type of const ~a" name)))
			(setf (constant-type const) (first res)))))
			
(defun type-check-function (fun)
	(with-typecheck-context
		(with-function fun (:name name :args args :ret-type ret-type :body body)
			(dolist (arg args)
				(variable-is-defined arg)
				(set-var-constraint (var-name arg) `(,(var-type arg))))
			(get-type body `(,ret-type) nil))))

(defun check-repeated-definitions ()
	(do-definitions (:name name1 :definition def1)
		(do-definitions (:name name2 :definition def2)
			(unless (eq def1 def2)
				(if (string-equal name1 name2)
					(error 'type-invalid-error :text (tostring "multiple definitions of ~a: ~a ~a" name1 def1 def2)))))))
					
(defun test-same-arguments-p (args1 args2 constraints)
	(every #'(lambda (arg1 arg2)
					(cond
						((equal arg1 arg2) t)
						(t
							(multiple-value-bind (cs other-var) (find-first-assignment-constraint-to-var constraints arg1)
								(when cs
									(equal arg2 other-var))))))
				args1 args2))

(defun find-action-problems ()
   "Build rule dependency graph and check if an action
   can be derived in rules that dependent on each other.
   
   For example:

   a -o setColor, b.

   b, c -o setColor.

   This case is problematic because retraction will not work as intended when retracting c.
   "
   (let ((rgraph (make-hash-table)))
      (do-all-rules (:clause clause :head head)
         (do-subgoals head (:name name)
            (let ((fired-rules (find-clauses-with-subgoal-in-body name)))
             (push-dunion-all ;; add new rules affected by this head subgoal
                   (loop for rule in fired-rules
                         unless (eq clause rule)
                         collect rule)
                   (gethash clause rgraph)))))
     (iterate-hash (rgraph rule rules)
      ;; For each rule perform a DFS and check if there is any rule
      ;; that fires the same actions.
      (with-clause rule (:head head)
         (do-subgoals head (:name action-name)
            (let ((def (lookup-definition action-name)))
             ;; head subgoal must be an action.
             (when (is-action-p def)
               (let ((visited (make-hash-table)))
                (setf (gethash rule visited) t)
                (labels ((dfs (visited-rule)
                          (multiple-value-bind (in found-p) (gethash visited-rule visited)
                              (unless found-p
                                 (when (clause-head-matches-subgoal-p visited-rule action-name)
                                    (warn "Action ~a in rule ~a may be used incorrectly"
                                       action-name (clause-to-string rule)))
                                 (setf (gethash visited-rule visited) t)
                                 (multiple-value-bind (ls found-p) (gethash visited-rule rgraph)
                                    (dolist (new-rule ls)
                                     (dfs new-rule)))))))
                     (multiple-value-bind (ls found-p) (gethash rule rgraph)
                        (dolist (new-rule ls)
                           (dfs new-rule))))))))))))

(defun collect-basic-types ()
	(setf *program-types* nil)
	(do-definitions (:types types)
		(dolist (ty types)
			(setf *program-types* (add-type-to-typelist *program-types* (arg-type ty)))))
	(do-constant-list *consts* (:type typ)
		(setf *program-types* (add-type-to-typelist *program-types* typ)))
	(do-externs *externs* (:types types :ret-type ret)
		(setf *program-types* (add-type-to-typelist *program-types* ret))
		(dolist (typ types)
			(setf *program-types* (add-type-to-typelist *program-types* typ))))
	(do-functions *functions* (:ret-type typ :args args)
		(setf *program-types* (add-type-to-typelist *program-types* typ))
		(dolist (arg args)
			(setf *program-types* (add-type-to-typelist *program-types* (var-type arg))))))
					
(defun type-check ()
   (with-node-type-context
      (do-definitions (:name name :types typs :definition def)
         (check-home-argument name typs))
      (check-repeated-definitions)
      (do-index-list *directives* (:name name :field field)
       (let ((def (lookup-definition name)))
        (unless def
         (error 'type-invalid-error :text
          (tostring "Cannot find definition ~a from index directive" name)))
        (unless (and (>= (length (definition-types def)) field)
                     (not (= field 0)))
         (error 'type-invalid-error :text
          (tostring "Field ~a from index in ~a does not exist" field name)))))
      (dolist (const *consts*)
         (type-check-const const))
      (dolist (fun *functions*)
         (type-check-function fun))
      (do-externs *externs* (:name name :ret-type ret-type :types types)
         (let ((extern (lookup-external-definition name)))
            (unless extern
               (error 'type-invalid-error :text (tostring "could not found external definition ~a" name)))
            (unless (type-eq-p ret-type (extern-ret-type extern))
               (error 'type-invalid-error :text
                  (tostring "external function return types do not match: ~a and ~a"
                     ret-type (extern-ret-type extern))))
            (dolist2 (t1 types) (t2 (extern-types extern))
               (unless (type-eq-p t1 t2)
                  (error 'type-invalid-error :text
                     (tostring "external function argument types do not match: ~a and ~a"
                        t1 t2))))))
      (dolist (name *exported-predicates*)
         (let ((def (lookup-definition name)))
            (unless def
               (error 'type-invalid-error :text (tostring "exported predicate ~a was not found" name)))))
      (add-variable-head)
      (collect-basic-types) ;; collect all basic types from predicates, funs and constants.
      (do-all-rules (:clause clause)
         (handler-case
            (type-check-clause clause nil)
            (type-invalid-error (c) (error 'type-invalid-error :text
                                     (tostring "In clause ~a~%~a" (clause-to-string clause) (text c))))))
      (do-all-const-axioms (:subgoal sub)
         (with-subgoal sub (:name name :args args :options opts)
            (do-type-check-subgoal name args opts :axiom-p t)
            (optimize-subgoals (list sub) nil)))
      (do-all-var-axioms (:clause clause)
         (type-check-clause clause t))
      ;; remove unneeded constants
      (let (to-remove)
         ;; constants that are really constant do not need to be stored anymore since their values have been computed
         (dolist (const *consts*)
            (with-constant const (:name name :expr expr)
               (when (expr-is-constant-p expr nil nil)
                  (push const to-remove))))
         (delete-all *consts* to-remove))
      (find-action-problems)))
