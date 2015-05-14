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

(define-makes :plus :minus :mul :mod :div
      :lesser :lesser-equal :greater :greater-equal
      :equal :assign :not-equal :or)

(defmacro define-is-p (&rest symbs)
   `(on-top-level
      ,@(mapcar #'(lambda (sy)
            `(defun ,(intern (concatenate 'string (symbol-name sy) "-P")) (val)
                  (tagged-p val ,sy)))
         symbs)))
         
(define-is-p :bool :int :float :var :plus :minus :mul :div :mod
            :equal :not-equal
            :lesser :lesser-equal :greater :greater-equal
            :convert-float :world :colocated
            :constraint :extern :aggregate
            :true :false :not :head
            :tail :cons :call :callf :test-nil :addr
            :nil :host-id :or)
      
(defun op-p (val)
   (any (plus-p minus-p mul-p div-p mod-p not-equal-p equal-p lesser-p lesser-equal-p greater-p greater-equal-p or-p) val))

(defun make-call (name args) `(:call ,name ,args))
(defun call-name (call) (second call))
(defun call-args (call) (third call))

(defun make-callf (name args) `(:callf ,name ,args))
(defun callf-name (call) (second call))
(defun callf-args (call) (third call))

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

(defun make-head (c) `(:head ,c))
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

(defun make-not (expr) `(:not ,expr))
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
(defun first-host-node (list)
   "Returns the home body of a body list.
   Note that other things other than subgoals may be present"
   (do-subgoals list (:args args)
      (return-from first-host-node (first args))))
      
(defun clause-head-host-node (clause)
   "Returns the host node of a clause.
   Looks first on the head and then on the body."
   (let ((head-node (first-host-node (clause-head clause))))
      (if head-node
         head-node
         (first-host-node (clause-body clause)))))

(defun clause-body-host-node (clause)
   (first-host-node (clause-body clause)))
   
(defun clause-host-node (clause)
   "Returns the host node of a clause.
   Looks first on the body and then on the head."
   (let ((host (clause-body-host-node clause)))
      (if host
         host
         (clause-head-host-node clause))))

(defun make-colocated (h1 h2)
   (list :colocated h1 h2))
(defun colocated-first (c) (second c))
(defun colocated-second (c) (third c))

(defun set-colocated-first (c new)
   (setf (second c) new))
(defsetf colocated-first set-colocated-first)

(defun set-colocated-second (c new)
   (setf (third c) new))
(defsetf colocated-second set-colocated-second)

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

(defun definition-set-local-agg (def)
   (definition-add-option def :local-agg))
(defun definition-has-local-agg-p (def)
   (definition-has-option-p def :local-agg))
(defun definition-set-strata (def level)
   (definition-add-tagged-option def :strat level))
(defun definition-get-strata (def)
   (definition-get-tagged-option def :strat))

(defun is-init-p (def)
   (definition-has-option-p def :init-tuple))
(defun is-route-p (def)
   (definition-has-option-p def :route))
(defun is-linear-p (def)
   (definition-has-option-p def :linear))
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

(defun make-aggregate (agg typ mod) `(:aggregate ,agg ,typ ,mod))
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

(defun make-extern (name ret-type types &optional id) `(:extern ,name ,ret-type ,types ,id))
(defun extern-name (ext) (second ext))
(defun extern-ret-type (ext) (third ext))
(defun extern-types (ext) (fourth ext))
(defun extern-id (ext) (fifth ext))

(defun make-constraint (expr &optional (priority 0)) (list :constraint expr priority))
(defun constraint-expr (ls) (second ls))
(defun constraint-priority (ls) (third ls))

(defun set-constraint-expr (constraint new-expr)
   (setf (second constraint) new-expr))
(defsetf constraint-expr set-constraint-expr)

(defun const-p (s)
   (or (int-p s) (float-p s)
		(string-constant-p s)
		(addr-p s)
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

(defun make-host-id () '(:host-id :type-addr))

(defun make-convert-float (expr) `(:convert-float ,expr))
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

;;;; AGGREGATES

(defun make-agg-spec (op var) `(:agg-spec ,op ,var))
(defun agg-spec-p (x) (tagged-p x :agg-spec))
(defun agg-spec-op (x) (second x))
(defun agg-spec-var (x) (third x))

(defun make-agg-construct (spec vlist body &optional head)
   `(:agg-construct ,spec ,vlist ,body ,head))
(defun agg-construct-p (x) (tagged-p x :agg-construct))
(defun agg-construct-specs (a) (second a))
(defun agg-construct-vlist (a) (third a))
(defun agg-construct-body (a) (fourth a))
(defun agg-construct-head (a) (fifth a))

(defun set-agg-construct-body (c body)
	(setf (fourth c) body))
(defsetf agg-construct-body set-agg-construct-body)
(defun set-agg-construct-vlist (c new)
	(setf (fifth c) new))
(defsetf agg-construct-vlist set-agg-construct-vlist)

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
   (if (equal name "colocated")
       (cond
          ((= (length args) 2) (make-constraint (make-colocated (first args) (second args))))
          (t (error 'expr-invalid-error "Colocated expression must have two arguments")))
       (list :subgoal name args options)))

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
	(subgoal-add-option sub `(:route ,route)))
	
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
(defun subgoal-mark-as-blocked (subgoal)
	(subgoal-add-option subgoal :blocked))
(defun subgoal-is-blocked-p (subgoal)
	(subgoal-has-option-p subgoal :blocked))
(defun subgoal-get-remote-dest (subgoal)
   (first (subgoal-get-tagged-option subgoal :route)))
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
		(every #L(and (const-p !1) (not (get-constant-p !1))) args)))
(defun subgoal-add-delay (subgoal delay)
	(assert (and (numberp delay) (> delay 0)))
	(subgoal-add-tagged-option subgoal :delay delay))
(defun subgoal-has-delay-p (subgoal)
	(subgoal-get-tagged-option subgoal :delay))
(defun subgoal-delay-value (subgoal)
	(first (subgoal-get-tagged-option subgoal :delay)))

(defun lookup-definition-types (pred)
   (when-let ((def (lookup-definition pred)))
      (definition-types def)))

(defun lookup-definition (pred &optional (defs *definitions*))
   (find-if #L(string-equal pred (definition-name !1)) defs))

(defun lookup-def-id (def-name)
   (do-definitions (:id id :name name)
      (if (string-equal name def-name) (return-from lookup-def-id id))))

(defun lookup-extern (name)
   (find-if #L(string-equal name (extern-name !1)) *externs*))

(defun lookup-const (name)
	(find-if #L(string-equal name (constant-name !1)) *consts*))

(defun has-constraints-p (subgoals) (some #'constraint-p subgoals))
(defun has-assignments-p (subgoals) (some #'assignment-p subgoals))
   
(defun op-to-string (op)
   (case op
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
(defun eq-cmp-p (sym) (eq-or sym :equal :not-equal :lesser :lesser-equal :greater :greater-equal :or))

;; imports
(defun make-import (imp as file) `(:import ,imp ,as ,file))
(defun import-p (x) (tagged-p x :import))
(defun import-imp (x) (second x))
(defun import-as (x) (third x))
(defun import-from (x) (fourth x))