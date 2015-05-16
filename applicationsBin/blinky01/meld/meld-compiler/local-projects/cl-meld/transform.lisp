
(in-package :cl-meld)

(defmacro transform-part-expression (part &optional (stop-here nil))
   (with-gensyms (x)
      `(with-symbol (,x ,part)
         (if (funcall test-fn ,x)
            (multiple-value-bind (new-val stop-p) (funcall transform-fn ,x)
               ,@(when stop-here
                     `((declare (ignore stop-p))))
               (when new-val (setf ,x new-val))
               ,@(unless stop-here
                  `((when (not (eq stop-p :stop))
                        (transform-expr test-fn transform-fn ,x)))))
               ,@(unless stop-here
                  `((transform-expr test-fn transform-fn ,x)))))))

(defun transform-expr (test-fn transform-fn expr)
   "Traverses the entire expression and changes it.
   For each sub-expression, test-fn is called.
   If the test returns T, we execute transform-fn for the sub-expression.
   The return value must be two-valued:
      1st - The new value for this sub-expression (if nil no change is done)
      2nd - The :stop symbol, which stops the iteration for deeper sub-expressions inside this sub-expression"
   (unless expr
      (return-from transform-expr nil))
   (cond
      ;; we do nothing for these
      ((var-p expr))
		((bool-p expr))
		((int-p expr))
      ((float-p expr))
		((host-id-p expr))
      ((nil-p expr))
		((world-p expr))
      ((addr-p expr))
		((string-constant-p expr))
		((argument-p expr))
		((get-constant-p expr))
		;; the real deal
		((constant-p expr)
			(transform-part-expression (constant-expr expr)))
      ((clause-p expr)
         (transform-expr test-fn transform-fn (clause-head expr))
         (transform-expr test-fn transform-fn (clause-body expr)))
      ((subgoal-p expr)
         (loop-cons-car (arg (subgoal-args expr))
            (transform-part-expression arg)))
      ((comprehension-p expr)
         (with-comprehension expr (:left left :right right)
            (transform-part-expression left)
            (transform-part-expression right)))
      ((agg-construct-p expr)
         (with-agg-construct expr (:body body :head head)
            (transform-part-expression body)
				(transform-part-expression head)
				))
      ((constraint-p expr) (transform-part-expression (constraint-expr expr)))
      ((assignment-p expr)
         (transform-part-expression (assignment-var expr) t)
         (transform-part-expression (assignment-expr expr)))
      ((call-p expr)
         (loop-cons-car (arg (call-args expr))
            (transform-part-expression arg)))
		((callf-p expr)
			(loop-cons-car (arg (callf-args expr))
				(transform-part-expression arg)))
		((struct-p expr)
			(loop-cons-car (el (struct-list expr))
				(transform-part-expression el)))
		((struct-val-p expr)
			(transform-part-expression (struct-val-var expr)))
      ((cons-p expr)
         (transform-part-expression (cons-head expr))
         (transform-part-expression (cons-tail expr)))
      ((head-p expr) (transform-part-expression (head-list expr)))
      ((tail-p expr) (transform-part-expression (tail-list expr)))
      ((let-p expr)
         (transform-part-expression (let-var expr))
         (transform-part-expression (let-expr expr))
         (transform-part-expression (let-body expr)))
      ((if-p expr)
         (transform-part-expression (if-cmp expr))
         (transform-part-expression (if-e1 expr))
         (transform-part-expression (if-e2 expr)))
      ((not-p expr) (transform-part-expression (not-expr expr)))
      ((test-nil-p expr) (transform-part-expression (test-nil-expr expr)))
      ((convert-float-p expr) (transform-part-expression (convert-float-expr expr)))
      ((colocated-p expr)
         (transform-part-expression (colocated-first expr))
         (transform-part-expression (colocated-second expr)))
      ((op-p expr)
         (transform-part-expression (op-op1 expr))
         (transform-part-expression (op-op2 expr)))
		((exist-p expr)
			(transform-part-expression (exist-body expr)))
      ((list-of-lists-p expr)
         (loop-cons-car (e expr)
            (transform-part-expression e)))
      (t (error 'expr-invalid-error
               :text (tostring "transform-expr: Invalid expression: ~a" expr))))
   expr)
   
(defmacro do-map-expr (expr)
   `(map-expr test-fn map-fn ,expr :go-down-fn go-down-fn))

(defmacro map-atom-expr ()
   `(if (funcall test-fn expr)
      (funcall map-fn expr)
      expr))
      
(defmacro with-mapped-expr (&body body)
   `(cond
      ((funcall test-fn expr)
       (funcall map-fn expr))
      ((funcall go-down-fn expr)
       ,@body)
      (t expr)))

(defun map-expr (test-fn map-fn expr &key (go-down-fn #'always-true))
   "Traverses the expression and creates a new expression.
   This is the functional counterpart of transform-expr.
   Parameters:
      - test-fn: called to check if map-fn is to be called upon the expression.
      - map-fn: called after test-fn succeeds. transforms the expression into something.
      - go-down-fn: called to check if we should go down into sub-expressions.
                    only called if test-fn returns nil.
      - expr: the expression to be mapped."
   (unless expr
      (return-from map-expr nil))
   (cond
      ((var-p expr) (map-atom-expr))
      ((int-p expr) (map-atom-expr))
      ((float-p expr) (map-atom-expr))
      ((host-id-p expr) (map-atom-expr))
      ((nil-p expr) (map-atom-expr))
      ((world-p expr) (map-atom-expr))
      ((addr-p expr) (map-atom-expr))
		((get-constant-p expr) (map-atom-expr))
      ((clause-p expr)
         (with-mapped-expr
            (make-clause (do-map-expr (clause-body expr))
                         (do-map-expr (clause-head expr))
                         (clause-options expr))))
      ((subgoal-p expr)
         (with-mapped-expr
            (make-subgoal (subgoal-name expr)
                          (mapcar #L(do-map-expr !1) (subgoal-args expr)))))
      ((constraint-p expr)
         (with-mapped-expr
            (make-constraint (do-map-expr (constraint-expr expr))
                             (constraint-priority expr))))
      ((assignment-p expr)
         (with-mapped-expr
            (make-assignment (do-map-expr (assignment-var expr))
                             (do-map-expr (assignment-expr expr)))))
      ((let-p expr)
         (with-mapped-expr
            (make-let (do-map-expr (let-var expr))
                      (do-map-expr (let-expr expr))
                      (do-map-expr (let-body expr)))))
      ((call-p expr)
         (with-mapped-expr
            (make-call (call-name expr)
                       (mapcar #L(do-map-expr !1) (call-args expr)))))
      ((cons-p expr)
         (with-mapped-expr
            (make-cons (do-map-expr (cons-head expr))
                       (do-map-expr (cons-tail expr)))))
      ((head-p expr)
         (with-mapped-expr
            (make-head (do-map-expr (head-list expr)))))
      ((tail-p expr)
         (with-mapped-expr
            (make-tail (do-map-expr (tail-list expr)))))
      ((not-p expr)
         (with-mapped-expr
            (make-not (do-map-expr (not-expr expr)))))
      ((test-nil-p expr)
         (with-mapped-expr
            (make-test-nil (do-map-expr (test-nil-expr expr)))))
      ((if-p expr)
         (with-mapped-expr
            (make-if (do-map-expr (if-cmp expr))
                     (do-map-expr (if-e1 expr))
                     (do-map-expr (if-e2 expr)))))
      ((convert-float-p expr)
         (with-mapped-expr
            (make-convert-float (do-map-expr (convert-float-expr expr)))))
      ((colocated-p expr)
         (with-mapped-expr
            (make-colocated (do-map-expr (colocated-first expr))
                            (do-map-expr (colocated-second expr)))))
      ((op-p expr)
         (with-mapped-expr
            (make-op (op-op expr)
                     (do-map-expr (op-op1 expr))
                     (do-map-expr (op-op2 expr)))))
      ((list-of-lists-p expr)
       (loop for item in expr
             collect (do-map-expr item)))
      (t (error 'expr-invalid-error
               :text (tostring "map-expr: Invalid expression: ~a" expr)))))

(defun transform-drop-subgoal-first-arg (clause host)
   (declare (ignore host))
	(assert (clause-p clause))
	(transform-expr #'subgoal-p
                        #'(lambda (x)
									; For some reason, cl-yacc may share subgoal structures
									(setf (subgoal-args x) (rest (subgoal-args x)))
                           (values nil :stop))
                  clause))

(defun transform-variable-to-host-id (clause old-var)
   (assert (clause-p clause))
	(let ((host-id (make-host-id))
			 new-constraint)
      (with-clause clause (:head head :body body)
			(transform-expr #'(lambda (x) (var-eq-p x old-var))
                      	#'(lambda (var)
                           	(declare (ignore var))
                           	(values host-id :stop))
                     head)
			(transform-expr #'(lambda (x) (var-eq-p x old-var))
									#'(lambda (var)
											(declare (ignore var))
											(values host-id :stop))
								 (remove-if #'subgoal-p body))
			(when (subgoals-in-list-have-var-p body old-var)
				(push-end (make-constraint (make-equal old-var '= host-id) :type-bool) (clause-body clause))))))

(defun map-one-variable-to-another (expr old-var new-var)
   (map-expr #'(lambda (x) (var-eq-p x old-var))
                   #'(lambda (var) (declare (ignore var)) new-var)
                  expr
            :go-down-fn
            #'(lambda (x)
               (cond
                  ((let-p x)
                     (if (var-eq-p (let-var x) old-var)
                           nil
                           t))
                  (t t)))
               ))