(in-package :cl-meld)

(define-condition expr-invalid-error (error)
   ((text :initarg :text :reader text)))

(defmacro define-makes (&rest symbs)
   `(on-top-level
      ,@(mapcar #'(lambda (sym)
         `(defun ,(intern (concatenate 'string "MAKE-" (symbol-name sym))) (a b c)
               (declare (ignore b))
               (list ,sym a c)))
            symbs)))

;; available operations
(define-makes :plus :minus :mul :mod :div
      :lesser :lesser-equal :greater :greater-equal
      :equal :assign :not-equal :or :and)

(defmacro define-is-p (&rest symbs)
   `(on-top-level
      ,@(mapcar #'(lambda (sy)
            `(defun ,(intern (concatenate 'string (symbol-name sy) "-P")) (val)
                  (tagged-p val ,sy)))
         symbs)))
         
(define-is-p :bool :int :float :var :plus :minus :mul :div :mod
            :equal :not-equal
            :lesser :lesser-equal :greater :greater-equal
            :convert-float :world :cpus :colocated :host
            :constraint :extern :aggregate
            :true :false :not :head
            :tail :cons :call :callf :test-nil :addr
            :nil :host-id :thread-id :or :and)
      
(defun op-p (val)
   (any (plus-p minus-p mul-p div-p mod-p not-equal-p equal-p lesser-p lesser-equal-p greater-p greater-equal-p or-p and-p) val))

(defun make-call (name args) `(:call ,name ,args))
(defun call-name (call) (second call))
(defun call-args (call) (third call))

(defun set-call-args (call new-args) (setf (third call) new-args))
(defsetf call-args set-call-args)

(defun make-callf (name args) `(:callf ,name ,args))
(defun callf-name (call) (second call))
(defun callf-args (call) (third call))

(defun set-callf-args (call new-args) (setf (third call) new-args))
(defsetf callf-args set-callf-args)

(defun lookup-function (name)
	(let ((fun (find-if #'(lambda (x) (string-equal (callf-name x) name)) *functions*)))
		fun))

(defun make-function (name args ret-type body)
   `(:function ,name ,args ,ret-type ,body))
(defun function-name (fun) (second fun))
(defun function-args (fun) (third fun))
(defun function-ret-type (fun) (fourth fun))
(defun function-body (fun) (fifth fun))

(defun make-cons (h ts) `(:cons ,h ,ts))
(defun cons-head (c) (second c))
(defun cons-tail (c) (third c))

(defun set-cons-head (cons head)
   (setf (second cons) head))
(defsetf cons-head set-cons-head)

(defun set-cons-tail (cons tail)
   (setf (third cons) tail))
(defsetf cons-tail set-cons-tail)

(defun make-head (c &optional type) `(:head ,c ,type))
(defun head-list (c) (second c))

(defun set-head-list (head list)
   (setf (second head) list))
(defsetf head-list set-head-list)

(defun make-tail (c) `(:tail ,c))
(defun tail-list (c) (second c))

(defun set-tail-list (tail list)
   (setf (second tail) list))
(defsetf tail-list set-tail-list)

(defun make-true () '(:true))
(defun make-false () '(:false))

(defun make-not (expr &optional type) `(:not ,expr ,type))
(defun not-expr (not) (second not))

(defun set-not-expr (not expr)
   (setf (second not) expr))
(defsetf not-expr set-not-expr)

(defun make-test-nil (expr) `(:test-nil ,expr))
(defun test-nil-expr (tn) (second tn))

(defun set-test-nil-expr (test expr)
   (setf (second test) expr))
(defsetf test-nil-expr set-test-nil-expr)

(defun make-nil () (list :nil))

(defun make-addr (num) (list :addr num :type-addr))
(defun addr-num (addr) (second addr))
(defun set-addr-num (addr new-num)
   (setf (second addr) new-num))
(defsetf addr-num set-addr-num)

(defun option-has-tag-p (opts opt) (some #L(tagged-p !1 opt) opts))

;; to take the home node from the first subgoal or agg-construct
(defun find-host-nodes (clause)
   "Returns first node and thread (if any) from a clause."
   (let (first-thread first-node)
      (with-clause clause (:body body :head head)
         (do-subgoals (if (get-subgoals body) body head) (:name name :args args)
            (let ((def (lookup-definition name)))
               (with-definition def (:types types)
                (let ((fst (first types)))
                  (if (or (type-node-p fst) (type-addr-p fst))
                     (unless first-node
                      (setf first-node (first args)))
                     (unless first-thread
                      (when (type-thread-p fst)
                         (setf first-thread (first args)))))))))
         (unless first-node
            (dolist (a (get-assignments (clause-body clause)))
             (with-assignment a (:expr e :var var)
               (when (and (host-p e) (not first-node))
                  (setf first-node var))))))
      (values first-node first-thread)))

(defun find-host-nodes-head-only (head)
   "Returns first node and thread (if any) from a clause."
   (let (first-node first-thread)
      (do-subgoals head (:name name :args args)
         (let ((def (lookup-definition name)))
            (with-definition def (:types types)
             (let ((fst (first types)))
               (if (or (type-addr-p fst) (type-node-p fst))
                  (unless first-node
                   (setf first-node (first args))))
                  (unless first-thread
                   (when (type-thread-p fst)
                      (setf first-thread (first args))))))))
      (values first-node first-thread)))

(defun clause-host-thread (clause)
   (multiple-value-bind (host thread) (find-host-nodes clause)
      (declare (ignore host))
      thread))

(defun make-definition (name typs options) `(:definition ,name ,typs ,options))
(defun definition-p (def) (tagged-p def :definition))
(defun definition-name (def) (second def))
(defun definition-types (def) (third def))
(defun set-definition-types (def new-types)
   (setf (third def) new-types))
(defsetf definition-types set-definition-types)
(defun definition-num-args (def)
   (length (definition-types def)))
(defun definition-options (def) (fourth def))
(defun definition-add-option (def opt) (push opt (fourth def)))
(defun definition-has-option-p (def opt)
   (has-elem-p (definition-options def) opt))
(defun definition-has-tagged-option-p (def opt)
   (some #L(tagged-p !1 opt) (definition-options def)))
(defun definition-get-tagged-option (def opt)
   (let ((res (find-if #L(tagged-p !1 opt) (definition-options def))))
      (when res
         (second res))))
(defun definition-add-tagged-option (def name &rest rest)
   (definition-add-option def `(,name ,@rest)))
(defun definition-set-cyclical (def) (definition-add-option def :cycle))
(defun definition-is-cyclical-p (def) (definition-has-option-p def :cycle))
(defun definition-set-thread (def) (definition-add-option def :thread))
(defun definition-is-thread-p (def) (definition-has-option-p def :thread))
(defun definition-set-update (def updates count)
   (definition-add-tagged-option def :update (list updates count)))
(defun definition-is-update-p (def)
   (definition-get-tagged-option def :update))
(defun definition-get-update-count (def)
   (second (definition-get-tagged-option def :update)))
(defun definition-get-update-definition (def)
   (first (definition-get-tagged-option def :update)))

(defun definition-set-local-agg (def)
   (definition-add-option def :local-agg))
(defun definition-has-local-agg-p (def)
   (definition-has-option-p def :local-agg))
(defun definition-set-strata (def level)
   (definition-add-tagged-option def :strat level))
(defun definition-get-strata (def)
   (definition-get-tagged-option def :strat))
(defun definition-set-linear-id (def id)
   (definition-add-tagged-option def :id2 id))
(defun definition-set-persistent-id (def id)
   (definition-add-tagged-option def :id2 id))
(defun definition-get-linear-id (def)
   (definition-get-tagged-option def :id2))
(defun definition-get-persistent-id (def)
   (definition-get-tagged-option def :id2))

(defun is-init-p (def)
   (definition-has-option-p def :init-tuple))
(defun is-route-p (def)
   (definition-has-option-p def :route))
(defun is-linear-p (def)
   (definition-has-option-p def :linear))
(defun is-persistent-p (def)
   (not (is-linear-p def)))
(defun is-action-p (def)
   (definition-has-option-p def :action))
(defun is-reverse-route-p (def)
   (definition-has-tagged-option-p def :reverse-route))
(defun is-reused-p (def)
	(definition-has-option-p def :reused))
(defun definition-set-reused (def)
	(definition-add-option def :reused))
(defun find-init-predicate (defs) (find-if #'is-init-p defs))
(defun find-init-predicate-name (defs)
   (definition-name (find-init-predicate defs)))
(defun get-routes (&optional (code *ast*))
   (filter #'is-route-p (definitions code)))
   
(defun get-route-names (&optional (code *ast*))
   (mapcar #'definition-name (get-routes code)))
   
(defun subgoal-matches-def-p (sub def)
   (equal (subgoal-name sub) (definition-name def)))
(defun subgoal-match-p (sub1 sub2)
   (equal (subgoal-name sub1) (subgoal-name sub2)))

(defun make-aggregate (agg typ &optional mod) `(:aggregate ,agg ,typ ,mod))
(defun aggregate-agg (agg) (second agg))
(defun aggregate-type (agg) (third agg))

(defun aggregate-mod (agg) (fourth agg))
(defun aggregate-mod-is-input-p (aggmod) (tagged-p aggmod :input))
(defun aggregate-mod-is-output-p (aggmod) (tagged-p aggmod :output))
(defun aggregate-mod-is-immediate-p (aggmod) (eq aggmod :immediate))
(defun aggregate-mod-io-name (aggmod) (second aggmod))
(defun aggregate-mod-includes-home-p (aggmod)
   (and (> (length aggmod) 2)
      (eq (third aggmod) :home)))
(defun aggregate-mod-include-home (aggmod)
   (assert (= (length aggmod) 2))
   (push-end :home aggmod))

(defun definition-aggregate (def)
   (with-definition def (:types typs) (find-if #'aggregate-p typs)))

(defun arg-type (arg)
   (if (aggregate-p arg)
       (aggregate-type arg)
       arg))
(defun definition-arg-types (typs) (mapcar #'arg-type typs))

(defun definition-aggregate-p (def)
   (with-definition def (:types typs)
      (some #'aggregate-p typs)))

(defun make-extern (name ret-type types &optional id poly-p) `(:extern ,name ,ret-type ,types ,id ,poly-p))
(defun extern-name (ext) (second ext))
(defun extern-ret-type (ext) (third ext))
(defun extern-types (ext) (fourth ext))
(defun extern-id (ext) (fifth ext))
(defun extern-poly-p (ext) (sixth ext))

(defun make-constraint (expr &optional (priority 0)) (list :constraint expr priority))
(defun constraint-expr (ls) (second ls))
(defun constraint-priority (ls) (third ls))

(defun set-constraint-expr (constraint new-expr)
   (setf (second constraint) new-expr))
(defsetf constraint-expr set-constraint-expr)

(defun complex-const-p (s)
 (and (struct-p s)
  (every #'literal-p (struct-list s))))

(defun literal-p (s)
	(or (int-p s) (float-p s)
		(string-constant-p s)
		(addr-p s)))

(defun const-p (s)
   (or (literal-p s)
		(get-constant-p s)))
            
(defun make-op (op op1 op2)
   `(,op ,op1 ,op2))
(defun op-op (val) (tagged-tag val))
(defun op-op1 (val) (second val))
(defun op-op2 (val) (third val))

(defun set-op-op1 (o expr)
   (setf (second o) expr))
(defsetf op-op1 set-op-op1)

(defun set-op-op2 (o expr)
   (setf (third o) expr))
(defsetf op-op2 set-op-op2)

(defun make-let (var expr body &optional type) `(:let ,var ,expr ,body ,type))
(defun let-p (l) (tagged-p l :let))
(defun let-var (l) (second l))
(defun let-expr (l) (third l))
(defun let-body (l) (fourth l))
(defun set-let-var (l v)
   (setf (second l) v))
(defsetf let-var set-let-var)
(defun set-let-expr (l expr)
   (setf (third l) expr))
(defsetf let-expr set-let-expr)
(defun set-let-body (l body)
   (setf (fourth l) body))
(defsetf let-body set-let-body)

(defun make-argument (id)
	(assert (numberp id))
	(assert (and (>= id 1) (<= id 9)))
	`(:argument ,id :type-string))
(defun argument-p (x) (tagged-p x :argument))
(defun argument-id (x) (second x))

(defun make-get-constant (name &optional type) `(:get-constant ,name ,type))
(defun get-constant-p (c) (tagged-p c :get-constant))
(defun get-constant-name (c) (second c))
   
(defun make-if (cmp e1 e2 &optional type) `(:if ,cmp ,e1 ,e2 ,type))
(defun if-p (i) (tagged-p i :if))
(defun if-cmp (i) (second i))
(defun if-e1 (i) (third i))
(defun if-e2 (i) (fourth i))
(defun set-if-cmp (i cmp)
   (setf (second i) cmp))
(defsetf if-cmp set-if-cmp)
(defun set-if-e1 (i e1)
   (setf (third i) e1))
(defsetf if-e1 set-if-e1)
(defun set-if-e2 (i e2)
   (setf (fourth i) e2))
(defsetf if-e2 set-if-e2)

(defun make-conditional (cmp t1 t2) `(:conditional ,cmp ,t1 ,t2))
(defun conditional-p (x) (tagged-p x :conditional))
(defun conditional-cmp (x) (second x))
(defun conditional-term1 (x) (third x))
(defun conditional-term2 (x) (fourth x))
(defun set-conditional-cmp (x cmp)
	(setf (second x) cmp))
(defsetf conditional-cmp set-conditional-cmp)
(defun set-conditional-term1 (x t1)
	(setf (third x) t1))
(defsetf conditional-term1 set-conditional-term1)
(defun set-conditional-term2 (x t2)
	(setf (fourth x) t2))
(defsetf conditional-term2 set-conditional-term2)

(defun make-string-constant (v) `(:string ,v :type-string))
(defun string-constant-val (x) (second x))
(defun string-constant-p (x) (tagged-p x :string))

(defun make-bool (v) `(:bool ,v :type-bool))
(defun bool-val (b) (second b))
   
(defun int-val (val) (second val))
(defun make-int (int &optional typ)
   (if typ
      `(:int ,int ,typ)
      `(:int ,int)))
(defun make-forced-int (int) (make-int int :type-int))

(defun float-val (val) (second val))
(defun make-float (flt) `(:float ,flt :type-float))

(defun int-float-val (x) (second x))

(defun transform-int-to-float (expr)
	(setf (first expr) :float))

(defun make-host-id () '(:host-id :type-addr))
(defun make-thread-id () '(:thread-id :type-thread))

(defun make-convert-float (expr) `(:convert-float ,expr :type-float))
(defun convert-float-expr (flt) (second flt))

(defun set-convert-float-expr (c expr)
   (setf (second c) expr))
(defsetf convert-float-expr set-convert-float-expr)

(defun make-struct (ls) `(:struct ,ls))
(defun struct-p (x) (tagged-p x :struct))
(defun struct-list (x) (second x))
(defun set-struct-list (s ls)
	(setf (second s) ls))
(defsetf struct-list set-struct-list)

(defun make-struct-val (idx var &optional typ) (assert (var-p var)) `(:struct-val ,idx ,var ,typ))
(defun struct-val-p (x) (tagged-p x :struct-val))
(defun struct-val-idx (x) (second x))
(defun struct-val-var (x) (third x))
(defun set-struct-val-var (x v)
	(setf (third x) v))
(defsetf struct-val-var set-struct-val-var)

(defun make-world () (list :world))
(defun make-cpus () (list :cpus))
(defun make-host () (list :host))

(defun make-var (var &optional typ) `(:var ,(if (stringp var) (str->sym var) var) ,@(if typ `(,typ) nil)))      
(defun var-name (val) (second val))
(defun var-type (val) (third val))
(defun var-eq-p (v1 v2) (equal (var-name v1) (var-name v2)))
(defun set-var-type (var ty)
   (cond
      ((= (length var) 2)
         (push-end ty var))
      (t
         (setf (third var) ty))))
(defsetf var-type set-var-type)

(defparameter *var-counter* 0)
(defun generate-random-var-name ()
   (tostring "MV~a" (incf *var-counter*)))
(defun generate-random-var (&optional typ)
   "Generates a new variable name."
   (make-var (generate-random-var-name) typ))

;;;; ASSIGNMENTS

(defun make-assignment (var expr) (list :assign var expr))
(defun assignment-p (ls) (tagged-p ls :assign))
(defun assignment-var (ls) (second ls))
(defun assignment-expr (ls) (third ls))

(defun set-assignment-var (ass new-var)
   (setf (second ass) new-var))
(defsetf assignment-var set-assignment-var)

(defun set-assignment-expr (ass new-expr)
   (setf (third ass) new-expr))
(defsetf assignment-expr set-assignment-expr)

;;;; COMPREHENSIONS

(defun make-comprehension (left right variables)
   (list :comprehension left right variables))

(defun comprehension-p (comp) (tagged-p comp :comprehension))
(defun comprehension-left (comp) (second comp))
(defun comprehension-right (comp) (third comp))
(defun comprehension-variables (comp) (fourth comp))
(defun set-comprehension-left (comp left)
	(setf (second comp) left))
(defsetf comprehension-left set-comprehension-left)
(defun set-comprehension-right (comp right)
   (setf (third comp) right))
(defsetf comprehension-right set-comprehension-right)

;;;; AGGREGATES

(defun make-agg-spec (op var &optional args) `(:agg-spec ,op ,var ,args))
(defun agg-spec-p (x) (tagged-p x :agg-spec))
(defun agg-spec-op (x) (second x))
(defun agg-spec-var (x) (third x))
(defun agg-spec-args (x) (fourth x))

(defun make-agg-construct (spec vlist body &optional head head0)
   `(:agg-construct ,spec ,vlist ,body ,head ,head0))
(defun agg-construct-p (x) (tagged-p x :agg-construct))
(defun agg-construct-specs (a) (second a))
(defun agg-construct-vlist (a) (third a))
(defun agg-construct-body (a) (fourth a))
(defun agg-construct-head (a) (fifth a))
(defun agg-construct-head0 (a) (sixth a))
(defun agg-construct-spec-vars (a) (mapcar #'agg-spec-var (agg-construct-specs a)))

(defun set-agg-construct-vlist (c new)
	(setf (third c) new))
(defsetf agg-construct-vlist set-agg-construct-vlist)
(defun set-agg-construct-body (c body)
	(setf (fourth c) body))
(defsetf agg-construct-body set-agg-construct-body)
(defun set-agg-construct-head0 (c body)
	(setf (sixth c) body))
(defsetf agg-construct-head0 set-agg-construct-head0)

;;;; EXISTS

(defun make-exist (vars body)
	`(:exists ,vars ,body))
(defun exist-p (ex) (tagged-p ex :exists))
(defun exist-var-list (ex) (second ex))
(defun exist-body (ex) (third ex))

(defun set-exist-body (ex new-body)
	(setf (third ex) new-body))
(defsetf exist-body set-exist-body)

;;;; SUBGOALS

(defun make-subgoal (name args &optional options)
	(list :subgoal name args options))

(defun subgoal-p (ls) (tagged-p ls :subgoal))
(defun subgoal-name (subgoal) (second subgoal))
(defun set-subgoal-name (subgoal name)
	(setf (second subgoal) name))
(defsetf subgoal-name set-subgoal-name)
(defun subgoal-args (subgoal) (third subgoal))
(defun set-subgoal-args (subgoal new-args)
   (setf (third subgoal) new-args))
(defsetf subgoal-args set-subgoal-args)
(defun subgoal-options (subgoal) (fourth subgoal))
(defun subgoal-has-option-p (subgoal opt)
   (has-elem-p (subgoal-options subgoal) opt))
(defun subgoal-add-option (subgoal opt)
   (setf (fourth subgoal) (cons opt (fourth subgoal))))
(defun subgoal-add-tagged-option (subgoal opt arg)
	(subgoal-add-option subgoal `(,opt ,arg)))
(defun subgoal-add-route (sub route)
	(assert (var-p route))
   (subgoal-add-tagged-option sub :route route))
(defun subgoal-get-remote-dest (subgoal)
   (first (subgoal-get-tagged-option subgoal :route)))

(defun subgoal-set-thread (sub)
   (subgoal-add-option sub :thread))
(defun subgoal-is-thread-p (sub)
   (subgoal-has-option-p sub :thread))
	
(defun subgoal-get-tagged-option (subgoal opt)
   (let ((res (find-if #L(tagged-p !1 opt) (subgoal-options subgoal))))
      (when res
         (rest res))))
(defun subgoal-has-tagged-option-p (subgoal opt)
	(ensure-bool (subgoal-get-tagged-option subgoal opt)))
(defun subgoal-has-min-p (subgoal)
	(subgoal-has-tagged-option-p subgoal :min))
(defun subgoal-add-min (subgoal var)
	(subgoal-add-tagged-option subgoal :min var))
(defun subgoal-has-random-p (subgoal)
	(subgoal-has-option-p subgoal :random))
(defun subgoal-get-min-variable (subgoal)
	(let ((ret (subgoal-get-tagged-option subgoal :min)))
		(when ret
			(first ret))))
(defun subgoal-get-min-variable-position (subgoal)
	(let ((var (subgoal-get-min-variable subgoal)))
		(when var
			(with-subgoal subgoal (:args args)
				(let ((pos 0))
					(dolist (arg args)
						(when (var-eq-p arg var)
							(return-from subgoal-get-min-variable-position pos))
						(incf pos)))))))
(defun subgoal-is-remote-p (subgoal)
   (subgoal-get-remote-dest subgoal))
(defun subgoal-is-const-p (subgoal)
	(with-subgoal subgoal (:args args)
		; we want to ignore constants in this case (faster loading)
		(every #L(and (or (complex-const-p !1) (const-p !1)) (not (get-constant-p !1))) args)))
(defun subgoal-add-delay (subgoal delay)
	(assert (and (numberp delay) (> delay 0)))
	(subgoal-add-tagged-option subgoal :delay delay))
(defun subgoal-has-delay-p (subgoal)
	(subgoal-get-tagged-option subgoal :delay))
(defun subgoal-delay-value (subgoal)
	(first (subgoal-get-tagged-option subgoal :delay)))
(defun subgoal-set-reused (sub)
	(subgoal-add-option sub :reused))
(defun subgoal-is-reused-p (sub)
	(subgoal-has-option-p sub :reused))
(defun subgoal-will-reuse-other-p (sub)
	(subgoal-has-tagged-option-p sub :will-reuse))
(defun subgoal-will-reuse-other (sub sub2)
	(subgoal-add-tagged-option sub :will-reuse sub2))
(defun subgoal-get-reused (sub)
	(let ((opt (subgoal-get-tagged-option sub :will-reuse)))
		(when opt
			(first opt))))
(defun subgoal-will-modify (sub sub2)
	(subgoal-add-tagged-option sub :will-modify sub2))
(defun subgoal-will-modify-p (sub)
	(subgoal-has-tagged-option-p sub :will-modify))

(defun lookup-definition-types (pred)
   (alexandria:when-let ((def (lookup-definition pred)))
      (definition-types def)))

(defun lookup-definition (pred &optional (defs *definitions*))
   (find-if #L(string-equal pred (definition-name !1)) defs))

(defun lookup-subgoal-definition (subgoal &optional (defs *definitions*)) 
   (lookup-definition (subgoal-name subgoal) defs))

(defun lookup-def-id (def-name)
   (do-definitions (:id id :name name)
      (if (string-equal name def-name) (return-from lookup-def-id id))))

(defun lookup-extern (name)
   (find-if #L(string-equal name (extern-name !1)) *externs*))

(defun make-const-definition (name expr) `(:const ,name ,expr))
(defun const-definition-p (const) (tagged-p const :const))
(defun const-definition-name (const) (second const))
(defun const-definition-expr (const) (third const))

(defun has-constraints-p (subgoals) (some #'constraint-p subgoals))
(defun has-assignments-p (subgoals) (some #'assignment-p subgoals))
   
(defun op-to-string (op)
   (case op
		(:or "||")
      (:and "&&")
      (:plus "+")
      (:minus "-")
      (:mul "*")
      (:div "/")
      (:mod "%")
      (:equal "=")
      (:not-equal "!=")
      (:lesser "<")
      (:lesser-equal "<=")
      (:greater ">")
      (:greater-equal ">=")))
      
(defmacro eq-or (sym &rest symbols)
   `(or ,@(mapcar #'(lambda (s) `(eq ,sym ,s)) symbols)))
   
(defun eq-arith-p (sym) (eq-or sym :plus :minus :mul :div :mod))
(defun eq-num-cmp-p (sym) (eq-or sym :lesser :lesser-equal :greater :greater-equal))
(defun eq-cmp-p (sym) (eq-or sym :equal :not-equal :lesser :lesser-equal :greater :greater-equal :or :and))

;; imports
(defun make-import (imp as file) `(:import ,imp ,as ,file))
(defun import-p (x) (tagged-p x :import))
(defun import-imp (x) (second x))
(defun import-as (x) (third x))
(defun import-from (x) (fourth x))
