
(in-package :cl-meld)

(define-condition stratification-error (error)
   ((text :initarg :text :reader text)))
   
(defvar *strat-ctx* nil)
(defvar *strat-routes* nil)
(defvar *current-strat-level* 0)

(defun clause-edge-fact-p (routes)
   #L(let ((head-subgoal (first (clause-head !1))))
      (some #L(subgoal-matches-def-p head-subgoal !1) routes)))
      
(defun get-non-routes (defs)
   (remove-if #'is-route-p defs))

(defun make-stratification-ctx () (list))
(defun push-strata (def level)
   (definition-set-strata def level)
   (push def *strat-ctx*))
   
(defun subgoal-matches-any-def-p (sub &optional (ctx *strat-ctx*))
   (some #L(subgoal-matches-def-p sub !1) ctx))

(defun can-fire-clause-p (clause &optional (ctx *strat-ctx*))
   (when (every #'(lambda (sub) (subgoal-matches-any-def-p sub ctx))
            (get-subgoals (clause-body clause)))
      (clause-head clause)))

(defun is-aggregate-head-p (head)
   (when (one-elem-p head)
      (let ((subgoal (first head)))
         (let ((def (lookup-subgoal-definition subgoal)))
            (definition-aggregate-p def)))))
            
(defun is-aggregate-clause-p (clause)
   (let ((head (clause-head clause)))
      (is-aggregate-head-p head)))
      
(defun subgoal-not-generated-by-p (subgoal)
   #'(lambda (clause)
      (let ((head (clause-head clause)))
         (every #L(not (subgoal-match-p subgoal !1)) head))))
      
(defun heads-not-in-p (not-fire-clauses head)
   (every #L(every (subgoal-not-generated-by-p !1) not-fire-clauses) head))
   
(defun select-fired-rules (will-fire not-fire)
   (filter #'(lambda (fire-clause)
               (let ((head (clause-head fire-clause)))
                  (heads-not-in-p not-fire head)))
            will-fire))

(defun select-if-aggregate (clauses)
   (split-mult-return #'is-aggregate-clause-p clauses))
   
(defun get-head-subgoal (clause) (first (clause-head clause)))
(defun get-head-subgoal-name (clause) (subgoal-name (get-head-subgoal clause)))  
                  
(defun group-clauses-by-head (clauses)
   (let ((hash (make-hash-table :test #'equal)))
      ;(format t "group clauses ~a~%" clauses)
      (loop for clause in clauses
            do (let ((name (get-head-subgoal-name clause)))
                  (multiple-value-bind (ls found-p) (gethash name hash)
                     (declare (ignore found-p))
                     ;(format t "ADD ~a~%" name)
                     (setf (gethash name hash)
                              (cons clause ls)))))
      (iterate-hash (hash name clauses :op collect) clauses)))

(defun local-clause-p (clause) (not (clause-has-tagged-option-p clause :route)))

(defun is-init-subgoal-p (subgoal) (is-init-p (lookup-subgoal-definition subgoal)))

(defun is-init-clause-p (clause)
   "Given a clause tells you if that clause is generated during init."
   (with-clause clause (:body body)
      (and (one-elem-p body)
           (is-init-subgoal-p (first body)))))

(defun body-generated-by-all-p (clause)
   (with-clause clause (:body body)
      (every #L(definition-has-option-p (lookup-subgoal-definition !1) :generated-by-all)
             (get-subgoals body))))
      
(defun clause-generated-by-all-p (clause)
   "Tells you if a clause will be fired by all nodes."
   (or (is-init-clause-p clause)
       (body-generated-by-all-p clause)))
   
(defun generated-by-all-p (clauses)
   (some #'clause-generated-by-all-p clauses))
   
(defun set-generated-by-all (def)
   (definition-add-option def :generated-by-all))

(defun is-unique-aggregate-p (def)
   "Tells if aggregate has only one value (that is, argument with aggregate is the first one)"
   (assert (definition-p def))
   (with-definition def (:types typs)
      (assert (>= (length typs) 1))
      (aggregate-p (first typs))))
                        
(defun process-unrecursive-aggs (agg-clauses)
   (loop for clauses in agg-clauses
         for def = (lookup-definition (get-head-subgoal-name (first clauses)))
         do (push-strata def *current-strat-level*)
         do (when (every #'local-clause-p clauses)
               (definition-set-local-agg def))
         do (when (generated-by-all-p clauses)
               (set-generated-by-all def))))
                  
(defun process-unrecursive-non-agg-clause (clause)
   (let ((by-all (clause-generated-by-all-p clause)))
      (with-clause clause (:head head)
         (do-subgoals head (:subgoal sub)
            (let ((def (lookup-subgoal-definition sub)))
               (push-strata def *current-strat-level*)
               (if by-all
                  (set-generated-by-all def)))))))

(defun process-unrecursive-non-aggs (clauses)
   (loop for clause in clauses
         do (process-unrecursive-non-agg-clause clause)))
         
(defun get-head-definitions (clauses)
   (with-ret defs
      (do-clauses clauses (:head head)
         (do-subgoals head (:subgoal sub :name name)
            (let ((def (lookup-definition name defs)))
               (unless def
                  (push (lookup-subgoal-definition sub) defs)))))))
            
(defun definition-has-stage-argument (def)
   (with-definition def (:types typs)
      (and (>= (length typs) 1)
           (type-int-p (first typs)))))
            
(defun find-subgoals-by-definitions (body-head defs)
   (filter #'(lambda (sub)
               (some #L(subgoal-matches-def-p sub !1) defs))
         (get-subgoals body-head)))
         
(defun find-subgoals-by-definition (body-head def)
   (with-definition def (:name name)
      (filter (subgoal-by-name name) (get-subgoals body-head))))
            
(defun is-plus-1-arg-p (arg)
   (and (op-p arg)
      (let ((op1 (op-op1 arg))
            (op2 (op-op2 arg)))
         (or 
             (and (var-p op1)
                  (int-p op2)
                  (= (int-val op2) 1))
            (and (int-p op1)
                 (= (int-val op1) 1)
                 (var-p op2))))))
                 
(defun get-iteration-var (arg)
   (cond
      ((is-plus-1-arg-p arg)
         (cond
            ((var-p (op-op1 arg)) (op-op1 arg))
            ((var-p (op-op2 arg)) (op-op2 arg))
            (t nil)))
      ((var-p arg) arg)
      (t nil)))

(defun is-plus-1-arg-equal-p (arg var)
   (and (op-p arg)
        (let ((op1 (op-op1 arg))
              (op2 (op-op2 arg)))
            (or
               (and (var-p op1)
                    (var-eq-p op1 var)
                    (int-p op2)
                    (= (int-val op2) 1))
               (and (int-p op1)
                    (= (int-val op1) 1)
                    (var-p op2))))))

(defun uses-iter-var-p (arg var)
   (or (is-plus-1-arg-equal-p arg var)
       (and (var-p arg)
            (var-eq-p arg var))))
               
(defun find-assignment-or-constraint (body var)
   (let* ((assignments (get-assignments body))
          (found (find-if #L(var-eq-p (assignment-var !1) var) assignments)))
      (if found
         (assignment-expr found)
         (let ((op-expr (find-assignment-constraints-expr body var)))
            (when op-expr
               (op-op2 (first op-expr)))))))
   
(defun dereference-variable (arg body)
   (map-expr #'var-p
             #'(lambda (var)
                  (let ((expr (find-assignment-or-constraint body var)))
                     (if expr
                        (dereference-variable expr body)
                        var)))
            arg))

(defun dereference-variables (args body)
   (mapcar #L(dereference-variable !1 body) args))
   
(defun all-first-variables (subgoals)
   (mapcar #L(first (subgoal-args !1)) subgoals))

(defun get-stage-variables (subgoals body)
   (dereference-variables (all-first-variables subgoals) body))

(defun is-x-rule-p (defs)
   #'(lambda (clause)
      (with-clause clause (:body body :head head)
         (let* ((subs-head (find-subgoals-by-definitions head defs))
                (subs-body (find-subgoals-by-definitions body defs))
                (all (append subs-head subs-body))
                (vars-iter (get-stage-variables all body)))
            (and (every #'var-p vars-iter)
                 (all-equal-p vars-iter :test #'var-eq-p))))))
                    
(defun is-y-rule-p (defs)
   #'(lambda (clause)
      (with-clause clause (:body body :head head)
         (let* ((subs-head (find-subgoals-by-definitions head defs))
                (subs-body (find-subgoals-by-definitions body defs))
                (vars-head (get-stage-variables subs-head body))
                (vars-body (get-stage-variables subs-body body)))
            (when (and vars-head vars-body)
               (let ((iter-var (get-iteration-var (first vars-body))))
                  (and (every #L(is-plus-1-arg-equal-p !1 iter-var) vars-head)
                       (every #L(uses-iter-var-p !1 iter-var) vars-body))))))))
   
(defun is-start-rule-p (defs)
   #'(lambda (clause)
      (with-clause clause (:body body :head head)
         (let* ((subs-head (find-subgoals-by-definitions head defs))
                (subs-body (find-subgoals-by-definitions body defs))
                (stages-head (get-stage-variables subs-head body)))
            (and (null subs-body)
                 (every #'int-p stages-head))))))

(defun find-x-rules (clauses defs)
   (filter (is-x-rule-p defs) clauses))
   
(defun find-y-rules (clauses defs)
   (filter (is-y-rule-p defs) clauses))
   
(defun find-start-rules (clauses defs)
   (filter (is-start-rule-p defs) clauses))
   
(defun find-stable-arguments (def clause)
   "From the clause 'clause' finds the arguments that stay the same when a new 'def' fact is instantiated.
   This returns a list of arguments as integers."
   (with-definition def (:num-args n-args)
      (with-clause clause (:body body :head head)
         (let* ((subs-head (find-subgoals-by-definition head def))
                (subs-body (find-subgoals-by-definition body def))
                (all (append subs-head subs-body)))
            (if (= (length all) 1)
                (list 1)
               (let* ((args-except-first (mapcar #L(rest (subgoal-args !1)) all)) ;; remove stage argument
                      (args (mapcar #L(dereference-variables !1 body) args-except-first))
                      (ls (list 1))) ;; initial list of equal variables
                  (loop for i from 1 upto (1- n-args)
                        do (let ((this-args (mapcar #L(nth (1- i) !1) args)))
                              (when (all-equal-p this-args :test #'expr-eq-p)
                                 ;(format t "~a ARE EQUAL ~%" (1+ i))
                                 (push-end (1+ i) ls))))
                  ;(format t "~a all ~a~%" name args)
                  ls))))))
                  
(defun sort-clauses-by-occurrences (clauses name)
   (sort clauses #'> :key #L(clause-number-of-occurrences !1 name)))
   
(defun add-delete-options-to-clause (def clause args)
   (assert (ordered-p args))
   (with-definition def (:name name)
      (clause-add-delete clause name args)
      ;(format t "deleting ~a ~a using ~a~%" name args clause)
      ))

(defun add-delete-options (def clauses)
   (with-definition def (:name name)
      ;(format t "def ~a -> ~a~%" name clauses)
      (let* ((head-clauses (filter #L(and (clause-head-matches-subgoal-p !1 name)
                                          (not (clause-is-remote-p !1))) clauses))
             (body-clauses (filter #L(clause-body-matches-subgoal-p !1 name) clauses))
             (all-stables (mapcar #L(find-stable-arguments def !1) head-clauses))
             (inter-stables (intersection-all all-stables)))
         ;(format t "def ~a ~a~%" name inter-stables)
         (assert body-clauses)
         (add-delete-options-to-clause def
               (first (sort-clauses-by-occurrences body-clauses name))
               (if inter-stables inter-stables '(1))))))
   
(defun find-clause-clique (clauses)
   (let* ((defs (get-head-definitions clauses))
          (has-stage-argument (every #'definition-has-stage-argument defs)))
      (unless has-stage-argument
         (format t "Some definitions fail to have a stage argument!~%")
         (return-from find-clause-clique nil))
      (let* ((x-rules (find-x-rules clauses defs))
             (y-rules (find-y-rules clauses defs))
             (start-rules (find-start-rules clauses defs))
             (total-clique (+ (length x-rules) (length y-rules) (length start-rules))))
         (unless (= (length clauses) total-clique)
            (format t "Cannot partition clauses into x-rules and y-rules!~%")
            (return-from find-clause-clique nil))
         (when (or (null x-rules) (null y-rules))
            (return-from find-clause-clique nil))
         (printdbg "Found a XY-clique with ~a clauses!" total-clique)
         (loop for def in defs
               do (push-strata def *current-strat-level*))
         (incf *current-strat-level*)
         ;; Add delete stuff
         (loop for def in defs
               do (with-definition def (:name name)
                     (let ((affected-clauses (filter #L(and (clause-matches-subgoal-p !1 name))
                                                            (append x-rules y-rules))))
                        (add-delete-options def affected-clauses))))
         clauses)))

(defun stratification-loop (clauses)
   (multiple-value-bind (will-fire not-fire) (split-mult-return #'can-fire-clause-p clauses)
      (let ((will-really-fire (select-fired-rules will-fire not-fire)))
         (if (null will-really-fire)
            (progn
               (let ((clique (find-clause-clique clauses)))
                  (remove-all clauses clique)))
            (multiple-value-bind (agg-clauses not-agg) (select-if-aggregate will-really-fire)
               (when agg-clauses
                  (let ((grouped-agg (group-clauses-by-head agg-clauses)))
                     (process-unrecursive-aggs grouped-agg)))
               (process-unrecursive-non-aggs not-agg)
               (remove-all clauses will-really-fire))))))

(defun mark-unstratified-predicates ()
	; find new priorities before computing the priority list
	(find-priorities)
	(let ((priorities (assign-priorities *current-strat-level* (filter #'priority-p *directives*))))
		(dolist (prio priorities)
			(let ((name (first prio))
					(priority (rest prio)))
			(setf *current-strat-level* (max *current-strat-level* priority))
			(push-strata (lookup-definition name) priority))) 
		(do-definitions (:definition def)
      	(unless (definition-has-tagged-option-p def :strat)
         	(push-strata def *current-strat-level*)))))

(defun do-strat-loop (clauses)
   (incf *current-strat-level*)
   (when (null clauses)
      (mark-unstratified-predicates)
      (return-from do-strat-loop nil))
   (let ((remain (stratification-loop clauses)))
      (cond
         ((equal remain clauses)
            (warn "Could not stratify everything")
            (mark-unstratified-predicates))
         (t
            (do-strat-loop remain)))))
            
(defmacro with-stratification-context ((routes clauses) &body body)
   `(let* ((*strat-routes* (get-routes))
           (*current-strat-level* 0)
           (*strat-ctx* (make-stratification-ctx))
           (,routes *strat-routes*)
           (,clauses (remove-if (clause-edge-fact-p *strat-routes*) *clauses*)))
      ,@body))

(defun detect-cycles-table (visited edges n origin)
	(multiple-value-bind (ls found-p) (gethash n edges)
		(dolist (neighbor ls)
			(when (string-equal neighbor origin)
				(let ((def (lookup-definition origin)))
					(definition-set-cyclical def))
				(return-from detect-cycles-table nil))
			(multiple-value-bind (vis found-p) (gethash neighbor visited)
				(when (eq vis :unvisited)
					(setf (gethash neighbor visited) :visited)
					(detect-cycles-table visited edges neighbor origin)
					(setf (gethash neighbor visited) :unvisited))))))
	
(defun find-cycles ()
	(let ((table (make-hash-table :test 'equal)))
		(do-rules (:clause clause)
			(when (clause-is-persistent-p clause)
				(with-clause clause (:body body :head head)
					(do-subgoals body (:name body-name)
						(multiple-value-bind (value found-p) (gethash body-name table)
							(do-subgoals head (:name head-name)
								(unless (has-test-elem-p value head-name #'string-equal)
									(push head-name value)))
							(setf (gethash body-name table) value))))))
		(let ((visited (make-hash-table :test 'equal)))
			(loop for key being the hash-keys of table
				do (setf (gethash key visited) :unvisited))
			(loop for key being the hash-keys of table
				do (progn
						(loop for key being the hash-keys of table
							do (setf (gethash key visited) :unvisited))
						(detect-cycles-table visited table key key))))))
   
(defun stratify ()
	(find-cycles)
   (with-stratification-context (routes clauses)
      (dolist (rout routes)
         (set-generated-by-all rout)
         (push-strata rout *current-strat-level*))
      (let ((init-def (find-init-predicate *definitions*)))
         (set-generated-by-all init-def)
         (push-strata init-def *current-strat-level*))
      (if *use-stratification*
         (do-strat-loop clauses)
         (progn
            (incf *current-strat-level*)
            (mark-unstratified-predicates)))))
