(in-package :cl-meld)

(defclass ast ()
   ((definitions
      :initarg :definitions
      :initform (error "missing definitions.")
      :accessor definitions)
    (externs
      :initarg :externs
      :initform (error "missing externs.")
      :accessor externs)
    (clauses
      :initarg :clauses
      :initform (error "missing clauses.")
      :accessor clauses)
    (functions
      :initarg :functions
      :initform (error "missing functions.")
      :accessor functions)
    (nodes
      :initarg :nodes
      :initform (error "missing nodes.")
      :accessor nodes)
	(directives
		:initarg :directives
		:initform (error "missing directives.")
		:accessor directives)
	(consts
		:initarg :consts
		:initform (error "missing consts.")
		:accessor consts)
   (all-axioms
      :initarg :all-axioms
      :initform (error "missing all axioms.")
      :accessor all-axioms)
	(node-const-axioms
		:initarg :node-const-axioms
		:initform nil
		:accessor node-const-axioms)
   (node-var-axioms
      :initarg :node-var-axioms
      :initform nil
      :accessor node-var-axioms)
   (thread-var-axioms
      :initarg :thread-var-axioms
      :initform nil
      :accessor thread-var-axioms)
   (thread-const-axioms
      :initarg :thread-const-axioms
      :initform nil
      :accessor thread-const-axioms)
	(exported-predicates
		:initarg :exported-predicates
		:initform (error "missing exported predicates.")
		:accessor exported-predicates)
	(imported-predicates
		:initarg :imported-predicates
		:initform (error "missing imported predicates.")
		:accessor imported-predicates)
   (ast-has-thread-facts-p
      :initarg :ast-has-thread-facts-p
      :initform nil
      :accessor ast-has-thread-facts-p)
	(args-needed
		:initarg :args-needed
		:initform (error "missing args-needed.")
		:accessor args-needed)))

(defun make-ast (defs externs clauses axioms funs nodes directives consts exported-predicates imported-predicates args-needed)
   (do-definitions-list defs (:definition def :types typs)
      (when (type-thread-p (first typs))
         (definition-set-thread def)))
   (make-instance 'ast
      :definitions defs
      :externs externs
      :clauses clauses
      :all-axioms axioms
      :functions funs
      :nodes nodes
      :directives directives
      :consts consts
      :exported-predicates exported-predicates
      :imported-predicates imported-predicates
      :args-needed args-needed))

(defun merge-asts (ast1 ast2)
   "Merges two ASTs together. Note that ast1 is modified."
   (make-instance 'ast
         :definitions (nconc (definitions ast1) (definitions ast2))
         :externs (nconc (externs ast1) (externs ast2))
         :clauses (nconc (clauses ast1) (clauses ast2))
         :all-axioms (nconc (all-axioms ast1) (all-axioms ast2))
         :functions (nconc (functions ast1) (functions ast2))
         :nodes (union (nodes ast1) (nodes ast2))
			:directives (union (directives ast1) (directives ast2))
			:consts (append (consts ast1) (consts ast2))
			:exported-predicates (append (exported-predicates ast1) (exported-predicates ast2))
			:imported-predicates (append (imported-predicates ast1) (imported-predicates ast2))
			:args-needed (max (args-needed ast1) (args-needed ast2))))

(defun ast-prepare (ast seen-subgoals)
   (let ((threads (some #'definition-is-thread-p (definitions ast))))
      (ast-add-base-tuples ast threads seen-subgoals)
      (multiple-value-bind (const-axioms var-axioms) (split-mult-return #'is-constant-axiom-p (all-axioms ast))
         (multiple-value-bind (node-const-axioms thread-const-axioms)
                     (split-mult-return #L(is-node-axiom-p !1 (definitions ast)) const-axioms)
             (multiple-value-bind (node-var-axioms thread-var-axioms)
                     (split-mult-return #L(is-node-axiom-p !1 (definitions ast)) var-axioms)
                  (setf (node-var-axioms ast) node-var-axioms
                        (node-const-axioms ast) node-const-axioms
                        (thread-var-axioms ast) thread-var-axioms
                        (thread-const-axioms ast) thread-const-axioms
                        (ast-has-thread-facts-p ast) threads))))))

;;;;;;;;;;;;;;;;;;;
;; Clauses
;;;;;;;;;;;;;;;;;;;

(defun make-clause (perm conc &rest options) `(:clause ,perm ,conc ,options))
(defun make-axiom (conc &rest options) (make-clause nil conc options))
(defun clause-p (clause) (tagged-p clause :clause))
(defun clause-head (clause) (third clause))
(defun clause-body (clause) (second clause))
(defun set-clause-body (clause new-body)
   (setf (second clause) new-body))
(defsetf clause-body set-clause-body)
(defun set-clause-head (clause new-head)
	(setf (third clause) new-head))
(defsetf clause-head set-clause-head)

(defun clause-options (clause) (fourth clause))
(defun clause-add-option (clause opt) (push opt (fourth clause))) 
(defun clause-has-tagged-option-p (clause opt) (option-has-tag-p (clause-options clause) opt))
(defun clause-get-tagged-option (clause opt)
   (let ((res (find-if #L(tagged-p !1 opt) (clause-options clause))))
      (when res
         (rest res))))
(defun clause-get-all-tagged-options (clause opt)
   (mapfilter #'rest #L(tagged-p !1 opt) (clause-options clause)))
(defun clause-add-tagged-option (clause opt &rest rest)
   (clause-add-option clause `(,opt ,@rest)))
(defun clause-get-remote-dest (clause)
   (first (clause-get-tagged-option clause :route)))
(defun clause-is-remote-p (clause) (clause-has-tagged-option-p clause :route))
(defun clause-has-delete-p (clause) (clause-has-tagged-option-p clause :delete))
(defun clause-get-all-deletes (clause)
   (clause-get-all-tagged-options clause :delete))
(defun clause-get-delete (clause name)
   (find-if #L(string-equal (first !1) name) (clause-get-all-deletes clause)))
(defun clause-add-delete (clause name args)
   (clause-add-tagged-option clause :delete name args))
(defun clause-add-min (clause var)
	(clause-add-tagged-option clause :min var))
(defun clause-has-min-p (clause)
	(clause-has-tagged-option-p clause :min))
(defun clause-get-min-variable (clause)
	(first (clause-get-tagged-option clause :min)))
(defun clause-add-random (clause var)
	(clause-add-tagged-option clause :random var))
(defun clause-has-random-p (clause)
	(clause-has-tagged-option-p clause :random))
(defun clause-get-random-variable (clause)
	(first (clause-get-tagged-option clause :random)))
(defun clause-add-id (clause id)
	(clause-add-tagged-option clause :id id))
(defun clause-get-id (clause)
	(first (clause-get-tagged-option clause :id)))
(defun clause-set-persistent (clause)
	(clause-add-tagged-option clause :persistent))
(defun clause-is-persistent-p (clause)
	(clause-has-tagged-option-p clause :persistent))
(defun clause-head-is-recursive-p (clause-head)
	(and (not (null clause-head)) (every #'clause-p clause-head)))
(defun delete-option-args (delete-opt) (second delete-opt))
(defun delete-option-name (delete-opt) (first delete-opt))

(defun is-axiom-p (clause)
   (and (null (find-if #'subgoal-p (clause-body clause)))
         (null (find-if #'agg-construct-p (clause-body clause)))))

(defun is-constant-axiom-p (clause)
	(and (null (get-subgoals (clause-body clause)))
		(every #'subgoal-has-arbitrary-node-p (get-subgoals (clause-head clause)))))

(defun is-node-axiom-p (clause defs)
   (every #L(let ((def (lookup-subgoal-definition !1 defs)))
               (assert def (!1) "Could not retrieve definition of ~a." !1)
               (with-definition def (:types types)
                  (let ((typ (first types)))
                     (or (type-node-p typ) (type-addr-p typ)))))
      (get-subgoals (clause-head clause))))

(defun subgoal-has-arbitrary-node-p (sub)
   (with-subgoal sub (:args args)
      (const-p (first args))))

;;;;;;;;;;;;;;;;;;;
;; CONSTS
;;;;;;;;;;;;;;;;;;;

(defun make-constant (name expr &optional type) `(:constant ,name ,expr ,type))
(defun constant-p (c) (tagged-p c :constant))
(defun constant-name (c) (second c))
(defun constant-expr (c) (third c))
(defun constant-type (c) (fourth c))
(defun set-constant-expr (c expr)
	(setf (third c) expr))
(defsetf constant-expr set-constant-expr)

(defun set-constant-type (c newt)
	(setf (fourth c) newt))
(defsetf constant-type set-constant-type)

(defun lookup-const (name)
	(find-if #L(string-equal name (constant-name !1)) *consts*))

