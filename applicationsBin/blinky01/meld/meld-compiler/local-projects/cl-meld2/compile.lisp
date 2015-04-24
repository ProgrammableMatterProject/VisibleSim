(in-package :cl-meld)

(define-condition compile-invalid-error (error)
   ((text :initarg :text :reader text)))

(defun has-reg-p (regs reg)
	(find-if #'(lambda (x) (reg-eq-p x reg)) regs))
	
(defun extend-regs (regs reg)
	(if (has-reg-p regs reg)
		regs
		(cons reg regs)))
		
(defun find-unused-reg (regs)
	(loop for i upto (1- *num-regs*)
		do (if (not (has-reg-p regs (make-reg i)))
				(return-from find-unused-reg (make-reg i)))))
		
(defun alloc-reg (regs)
	(assert (< (length regs) *num-regs*))
	(let ((new-reg (find-unused-reg regs)))
		(values new-reg (extend-regs regs new-reg))))

(defparameter *compiling-axioms* nil)
(defparameter *compilation-clause* nil)
(defparameter *compiling-rule* nil)
(defparameter *starting-subgoal* nil)
(defparameter *vars-places* nil)
(defparameter *used-regs* nil)

(defmacro with-empty-compile-context (&body body)
	"Initiates compile context."
   `(let ((*vars-places* (make-hash-table))
          (*used-regs* nil))
      ,@body))

(defmacro with-compile-context (&body body)
	`(let ((*vars-places* (copy-hash-table *vars-places*))
			 (*used-regs* (copy-list *used-regs*)))
		,@body))

(defun alloc-new-reg () (alloc-reg *used-regs*))
(defmacro with-reg ((reg) &body body)
   `(multiple-value-bind (,reg *used-regs*) (alloc-new-reg)
      ,@body))
(defmacro with-old-reg ((reg) &body body)
	"Adds a new register to the context so it is not allocated inside body."
	`(cond
		((reg-p ,reg)
			(let ((*used-regs* (extend-regs *used-regs* ,reg)))
				,@body))
		(t
			,@body)))
			
(defun lookup-used-var (var-name)
   (multiple-value-bind (data found) (gethash var-name *vars-places*)
      (when found data)))
(defun add-used-var (var-name data) (setf (gethash var-name *vars-places*) data))
(defun remove-used-var (var-name) (remhash var-name *vars-places*))
(defun all-used-var-names () (alexandria:hash-table-keys *vars-places*))

(defun hash-table-to-stack (hash)
	(let ((new-hash (make-hash-table)))
		(loop for key in (all-used-var-names)
			do (let ((now (gethash key hash)))
					(setf (gethash key new-hash)
								(if (reg-p now)
									(let ((stack-off (reg-num now)))
										(make-vm-stack stack-off))
									now))))
		new-hash))

(defmacro with-stack-compile-context (&body body)
`(let ((*vars-places* (hash-table-to-stack *vars-places*)))
	,@body))

(defun valid-constraint-p (all-vars) #L(subsetp (all-variable-names !1) all-vars))
(defun get-compile-constraints-and-assignments (body)
   (let* ((assignments (select-valid-assignments body nil (all-used-var-names)))
          (vars-ass (mapcar #L(var-name (assignment-var !1)) assignments))
          (all-vars (append vars-ass (all-used-var-names)))
          (constraints (filter (valid-constraint-p all-vars) (get-constraints body)))
          (remain (remove-unneeded-assignments (append assignments constraints))))
      (split-mult-return #'constraint-p remain)))

(defun make-low-constraint (typ v1 v2) `(,typ ,v1 ,v2))
(defun low-constraint-type (lc) (first lc))
(defun low-constraint-v1 (lc) (second lc))
(defun low-constraint-v2 (lc) (third lc))

(defmacro return-expr (place &optional code) `(values ,place ,code (if (reg-p ,place) (extend-regs *used-regs* ,place) *used-regs*)))

(defmacro with-compiled-expr ((place code &key (force-dest nil)) expr &body body)
	`(multiple-value-bind (,place ,code *used-regs*) (compile-expr ,expr ,force-dest)
		(cond
			((and ,force-dest (not (equalp ,place ,force-dest)))
				(setf ,code (append ,code (list (make-move ,place ,force-dest))))
				(setf ,place ,force-dest)
				,@body)
			(t
				,@body))))

(defmacro with-compilation ((place code) expr &body body)
	`(multiple-value-bind (,place ,code *used-regs*) (compile-expr ,expr)
		,@body))
		
(defmacro with-compilation-on-reg ((place code) expr &body body)
	"Ensures that place is a register."
	(alexandria:with-gensyms (new-reg)
		`(with-compilation (,place ,code) ,expr
			(if (not (reg-p ,place))
				(with-reg (,new-reg)
					(setf ,code (append ,code (list (make-move ,place ,new-reg))))
					(setf ,place ,new-reg)
					,@body)
				(progn ,@body)))))
		
(defmacro with-compilation-on-rf ((place code) expr &body body)
	"Ensures that place is either a field or register."
	(alexandria:with-gensyms (new-reg)
		`(with-compilation (,place ,code) ,expr
			(if (not (or (reg-p ,place) (reg-dot-p ,place)))
				(with-reg (,new-reg)
					(setf ,code (append ,code (list (make-move ,place ,new-reg))))
					(setf ,place ,new-reg)
					,@body)
				(progn ,@body)))))

(defmacro with-dest-or-new-reg ((dest) &body body)
   `(if (null ,dest)
      (with-reg (,dest) ,@body)
      (progn ,@body)))

(defmacro compile-expr-to (expr place)
	(alexandria:with-gensyms (new-place code)
      `(multiple-value-bind (,new-place ,code *used-regs*) (compile-expr ,expr ,place)
		   (if (not (equal ,new-place ,place))
            (append ,code (list (make-move ,new-place ,place (expr-type ,expr))))
            ,code))))

(defun decide-external-function (name new-reg regs)
	"Decides if external function is pre-defined or not."
	(if (lookup-standard-external-function name)
		(make-vm-call name new-reg regs)
		(make-vm-calle name new-reg regs)))
		
(defun get-external-function-ret-type (name)
	(let ((fun (lookup-standard-external-function name)))
		(when fun
			(extern-ret-type fun))))

(defun compile-call-args (args)
	(if (null args)
		(values nil nil)
		(with-compilation-on-reg (arg-place arg-code) (first args)
			(multiple-value-bind (regs codes) (compile-call-args (rest args))
				(values (cons arg-place regs) `(,@arg-code ,@codes))))))
				
(defun compile-call (name args dest)
	(multiple-value-bind (regs codes) (compile-call-args args)
		(cond
			((null dest)
		 		(with-reg (new-dest)
					(return-expr new-dest `(,@codes ,(decide-external-function name new-dest regs)))))
			((and dest (reg-p dest))
				(return-expr dest `(,@codes ,(decide-external-function name dest regs))))
			(t
				(with-reg (new-dest)
					(return-expr dest `(,@codes ,(decide-external-function name new-dest regs) ,(make-move new-dest dest (get-external-function-ret-type name)))))))))
			
(defun compile-callf-args (args args-code n)
	(if (null args)
		args-code
		(with-compiled-expr (arg-place arg-code :force-dest (make-reg n)) (first args)
			(compile-callf-args (rest args) `(,@args-code ,@arg-code) (1+ n)))))

(defun compile-expr (expr &optional dest)
	;(warn "compile-expr ~a ~a" expr dest)
   (cond
		((bool-p expr)
		 (return-expr (make-vm-bool (bool-val expr))))
      ((int-p expr) ;; Int expression in the previous phases maybe coerced into a float
         (let ((typ (expr-type expr)))
            (return-expr (funcall (if (type-int-p typ) #'make-vm-int #'make-vm-float)
                              (int-val expr)))))
      ((float-p expr) (return-expr (make-vm-float (float-val expr))))
		((string-constant-p expr) (return-expr (make-vm-string-constant (string-constant-val expr))))
      ((addr-p expr) (return-expr (make-vm-addr (addr-num expr))))
      ((host-id-p expr) (return-expr (make-vm-host-id)))
      ((argument-p expr)
			(return-expr (make-vm-argument (argument-id expr))))
		((get-constant-p expr)
			(return-expr (make-vm-constant (get-constant-name expr))))
		((var-p expr)
			(let ((look (lookup-used-var (var-name expr))))
				(assert look)
				(return-expr look)))
      ((call-p expr)
			(let ((name (call-name expr))
					(args (call-args expr)))
				(cond
					((string-equal name "cpu-id")
						(with-dest-or-new-reg (dest)
							(with-compiled-expr (arg-place arg-code :force-dest dest) (first args)
								(return-expr dest `(,@arg-code ,(make-vm-cpu-id arg-place dest))))))
					((string-equal name "nodepriority")
						(with-dest-or-new-reg (dest)
							(with-compiled-expr (arg-place arg-code :force-dest dest) (first args)
								(return-expr dest `(,@arg-code ,(make-vm-node-priority arg-place dest))))))
					(t 
         			(compile-call name args dest)))))
		((struct-val-p expr)
			(with-dest-or-new-reg (dest)
				(let ((look (lookup-used-var (var-name (struct-val-var expr)))))
					(assert look)
					(return-expr dest `(,(make-vm-struct-val (struct-val-idx expr) look dest (expr-type expr)))))))
		((struct-p expr)
			(with-dest-or-new-reg (dest)
				(let ((ls (struct-list expr))
						instrs)
					(return-expr dest
						`(,(make-vm-push-n (length ls))
							,@(loop for x in ls
										for i from 0
								append (compile-expr-to x (make-vm-stack i)))
							,(make-vm-make-struct (expr-type expr) dest))))))
		((callf-p expr)
			(with-dest-or-new-reg (dest)
				(with-stack-compile-context
					(return-expr dest `(,(make-vm-push) ;; for pcounter
											,(make-vm-push) ;; for return value
											,(make-vm-push-registers)
											,@(compile-callf-args (callf-args expr) (list) 0)
											,(make-move (make-vm-pcounter) (make-vm-stack (1+ *num-regs*)))
											,(make-vm-callf (callf-name expr))
											,(make-vm-pop-registers)
											,(make-move (make-vm-stack 0) dest)
											,(make-vm-pop)
											,(make-vm-pop))))))
      ((convert-float-p expr)
			(let (p c)
				(with-compilation-on-reg (place code) (convert-float-expr expr)
					(setf p place
							c code))
				(with-dest-or-new-reg (dest)
					(cond
						((reg-p dest)
							(return-expr dest `(,@c ,(make-vm-convert-float p dest))))
						(t
							(with-reg (new-reg)
								(return-expr dest `(,@c ,(make-vm-convert-float p new-reg) ,(make-move new-reg dest)))))))))
      ((let-p expr)
         (with-dest-or-new-reg (dest)
            (with-compiled-expr (place-expr code-expr) (let-expr expr)
               (add-used-var (var-name (let-var expr)) place-expr)
               (with-compiled-expr (place-body code-body :force-dest dest) (let-body expr)
                  (remove-used-var (var-name (let-var expr)))
                  (return-expr place-body `(,@code-expr ,@code-body))))))
      ((if-p expr)
			(with-compiled-expr (place-cmp code-cmp) (if-cmp expr)
            (with-dest-or-new-reg (dest)
               (let ((code1 (compile-expr-to (if-e1 expr) dest))
                     (code2 (compile-expr-to (if-e2 expr) dest)))
                  (return-expr dest `(,@code-cmp ,(make-vm-if-else place-cmp code1 code2)))))))
      ((tail-p expr)
			(let (p c)
				(with-compilation-on-rf (place code) (head-list expr)
					(setf p place
							c code))
				(with-dest-or-new-reg (dest)
					(return-expr dest `(,@c ,(make-vm-tail p dest (expr-type expr)))))))
      ((head-p expr)
			(let (p c)
				(with-compilation-on-rf (place code) (head-list expr)
					(setf p place
							c code))
				(with-dest-or-new-reg (dest)
					(return-expr dest `(,@c ,(make-vm-head p dest (expr-type (vm-head-cons expr))))))))
      ((cons-p expr)
			(let (phead chead ptail ctail)
				(with-compilation-on-rf (place-head code-head) (cons-head expr)
					(with-compilation-on-rf (place-tail code-tail) (cons-tail expr)
						(setf phead place-head
								chead code-head
								ptail place-tail
								ctail code-tail)))
				(with-dest-or-new-reg (dest)
					(return-expr dest `(,@chead ,@ctail ,(make-vm-cons phead ptail dest (expr-type expr)))))))
      ((not-p expr)
			(let (p c)
				(with-compilation-on-reg (place-expr code-expr) (not-expr expr)
					(setf p place-expr
							c code-expr))
				(with-dest-or-new-reg (dest)
					(return-expr dest `(,@c ,(make-vm-not p dest))))))
      ((test-nil-p expr)
			(let (p c)
				(with-compilation-on-reg (place-expr code-expr) (test-nil-expr expr)
					(setf p place-expr
							c code-expr))
				(with-dest-or-new-reg (dest)
					(return-expr dest `(,@c ,(make-vm-test-nil p dest))))))
      ((nil-p expr) (return-expr (make-vm-nil)))
      ((world-p expr) (return-expr (make-vm-world)))
      ((op-p expr)
			(let (p1 c1 p2 c2)
				(with-compilation-on-reg (place1 code1) (op-op1 expr)
					(with-compilation-on-reg (place2 code2) (op-op2 expr)
						(setf p1 place1
								c1 code1
								p2 place2
								c2 code2)))
				(with-dest-or-new-reg (dest)
					(compile-op-expr expr p1 p2 c1 c2 dest))))
      (t (error 'compile-invalid-error :text (tostring "Unknown expression to compile: ~a" expr)))))

(defun compile-op-expr (expr new-place1 new-place2 new-code1 new-code2 dest)
	(cond
		((and dest (or (reg-dot-p dest) (vm-stack-p dest)))
			(with-reg (new-dest)
				(return-expr dest `(,@(generate-op-instr expr new-dest new-place1 new-place2 new-code1 new-code2) ,(make-move new-dest dest)))))
		((and dest (reg-p dest))
			(return-expr dest (generate-op-instr expr dest new-place1 new-place2 new-code1 new-code2)))
		((null dest)
			(with-reg (new-dest)
				(return-expr new-dest (generate-op-instr expr new-dest new-place1 new-place2 new-code1 new-code2))))
		(t (error 'compile-invalid-error :text (tostring "can't compile operation to ~a" dest)))))
      
(defun generate-op-instr (expr dest place1 place2 code1 code2)
   (let* ((base-op (op-op expr))
          (op1 (op-op1 expr)) (op2 (op-op2 expr))
          (ret-type (expr-type expr)) (operand-type (expr-type op1)))
      (cond
         ((and (equal-p expr) (or (nil-p op1) (nil-p op2)))
            (let ((place (if (nil-p op1) place2 place1))
                  (code (if (nil-p op1) code2 code1)))
               `(,@code ,(make-vm-test-nil place dest))))
         (t
				(let ((vm-op (set-type-to-op operand-type ret-type base-op)))
					(assert (not (null vm-op)))
					`(,@code1 ,@code2 ,(make-vm-op dest place1 vm-op place2)))))))

(defun get-remote-dest (subgoal)
   (lookup-used-var (var-name (subgoal-get-remote-dest subgoal))))
   
(defun get-remote-reg-and-code (subgoal default)
   (if (subgoal-is-remote-p subgoal)
      (let ((var (get-remote-dest subgoal)))
         (if (reg-p var)
            var
            (with-reg (new-reg)
               (values new-reg `(,(make-move var new-reg)))))) 
         default))

(defun find-matchable-constraint-for-var (body var reg level &optional i)
	(cond
		((int-p var) (values (make-vm-int (int-val var)) nil))
		((float-p var) (values (make-vm-float (float-val var)) nil))
		((var-p var)
			(let ((already-defined (lookup-used-var (var-name var))))
			 (multiple-value-bind (cs other-var) (when (zerop level) (find-first-assignment-constraint-to-var body var))
		   	(cond
		      	(already-defined
						(cond
							((zerop level)
							 (values already-defined nil))
							((not (reg-eq-p (reg-dot-reg already-defined) reg))
								(values already-defined nil))))
					((and (not already-defined) (zerop level) cs (lookup-used-var (var-name other-var)))
						(assert other-var)
						(assert i)
						(setf already-defined (lookup-used-var (var-name other-var)))
						(assert (reg-dot-p already-defined))
						(add-used-var (var-name var) (make-reg-dot reg i))
						(values already-defined (list cs)))
					(t
						(let ((literal-constr (find-if #L(and (op-p (constraint-expr !1)) (literal-p (op-op2 (constraint-expr !1))))
															(find-assignment-constraints body var)))
								(non-nil-constr (find-if #'(lambda (cs)
																		(let ((note (not-expr (constraint-expr cs))))
																			(when (test-nil-p note)
																				(var-eq-p var (test-nil-expr note)))))
																	(find-not-constraints body)))
								(nil-constr (find-if #'(lambda (cs)
																	(let ((v (test-nil-expr (constraint-expr cs))))
																		(var-eq-p var v)))
																(find-test-nil-constraints body))))
							(cond
								(literal-constr
									(values (op-op2 (constraint-expr literal-constr)) (list literal-constr)))
								(non-nil-constr
									(multiple-value-bind (ls new-constraints) (get-possible-list-constraint body var reg level)
										(if ls
											(values ls (cons non-nil-constr new-constraints))
											(values (make-vm-non-nil) (list non-nil-constr)))))
								(nil-constr
									(values (make-vm-nil) (list nil-constr))))))))))))
							
(defun get-possible-list-constraint (body arg reg level)
	"Looks into body if the list variable arg has constraints in relation to its structure."
	(let ((head-ass (find-if #'(lambda (ass) (let ((e (assignment-expr ass)))
																(when (head-p e)
																	(var-eq-p (head-list e) arg))))
										(get-assignments body)))
			(head-eqs (find-constraints body #L(and (equal-p !1) (head-p (op-op2 !1)) (var-eq-p (head-list (op-op2 !1)) arg))))
			(tail-ass (find-if #'(lambda (ass) (let ((e (assignment-expr ass)))
																(when (tail-p e)
																	(var-eq-p (tail-list e) arg))))
									(get-assignments body)))
			(tail-eq-nil (find-constraints body #L(and (equal-p !1) (nil-p (op-op1 !1)) (tail-p (op-op2 !1)) (var-eq-p (tail-list (op-op2 !1)) arg))))
			head-constraints head-value tail-constraints tail-value)
		(cond
			(head-ass
				(let ((head-var (assignment-var head-ass)))
					(multiple-value-bind (val head-consts) (find-matchable-constraint-for-var body head-var reg (1+ level))
						(setf head-constraints head-consts
								head-value val))))
			(head-eqs
				(let ((head-expr (op-op1 (constraint-expr (first head-eqs)))))
					(multiple-value-bind (val head-consts) (find-matchable-constraint-for-var body head-expr reg (1+ level))
						(when val
							(setf head-constraints head-eqs
									head-value val))))))
		(cond
			(tail-ass
				(let ((tail-var (assignment-var tail-ass)))
					(multiple-value-bind (val tail-consts) (find-matchable-constraint-for-var body tail-var reg (1+ level))
				 		(setf tail-constraints tail-consts
								tail-value val))))
			(tail-eq-nil
				(setf tail-constraints tail-eq-nil
						tail-value (make-vm-nil))))
		(when (or head-value tail-value)
			(unless tail-value
				(setf tail-value (make-vm-any)))
			(unless head-value
				(setf head-value (make-vm-any)))
			(values (make-vm-list head-value tail-value) (append head-constraints tail-constraints)))))
			
(defun add-subgoal (subgoal reg body &optional (in-c reg))
	"Adds subgoal to the compilation context and returns several low constraints plus the updated body of the rule."
   (with-subgoal subgoal (:args args)
        (let ((low-constraints (loop for arg in args
								              for i upto (length args)
								              append (multiple-value-bind (val constrs-to-remove) (find-matchable-constraint-for-var body arg reg 0 i)
																(when (or (not val) (not (reg-dot-p val)))
																	(add-used-var (var-name arg) (make-reg-dot reg i)))
																(when val
																	(delete-all body constrs-to-remove)
																	(list (make-low-constraint (expr-type arg) (make-reg-dot in-c i) val)))))))
				(values low-constraints body))))

(defun compile-remain-delete-args (n ls)
   (if (null ls)
      nil
      (with-compiled-expr (place instrs) (first ls)
         (multiple-value-bind (code places) (compile-remain-delete-args (1+ n) (rest ls))
             (values `(,@instrs ,@code)
                     `(,(cons n place) ,@places))))))

(defun compile-delete (delete-option subgoal)
   (let* ((args (delete-option-args delete-option))
          (mapped (mapcar #L(nth (1- !1) (subgoal-args subgoal)) args))
          (iter (first mapped))
          (remain (rest mapped))
          (minus-1 (make-minus iter '- (make-forced-int 2))))
      (with-compiled-expr (place instrs) minus-1
         (with-reg (reg)
            (let ((greater-instr `(,(make-move (make-vm-int 0) reg) (make-vm-op reg place :int-greater-equal reg))))
               (multiple-value-bind (remain-instrs places) (compile-remain-delete-args 2 remain)
                  ;(format t "remain-instrs ~a places ~a~%" remain-instrs places)
                  (let* ((delete-code (make-vm-delete (subgoal-name subgoal) `(,(cons 1 place) ,@places)))
                         (if-instr (make-vm-if reg `(,delete-code))))
                     `(,@instrs ,@remain-instrs ,@greater-instr ,if-instr))))))))
            
(defun compile-inner-delete (clause)
   (when (clause-has-delete-p clause)
      (let ((all (clause-get-all-deletes clause)))
         (loop for delete-option in all
               append (compile-delete delete-option
                        (find-if (subgoal-by-name (delete-option-name delete-option)) (get-subgoals (clause-body clause))))))))
            
(defun compile-head-move (arg i tuple-reg)
   (let ((reg-dot (make-reg-dot tuple-reg i)))
      (compile-expr-to arg reg-dot)))

(defun general-make-send (name tuple-reg send-to appears-body-p)
	(let ((def (lookup-definition name)))
		(cond
			((and (not (is-reused-p def)) (is-linear-p def) (reg-eq-p tuple-reg send-to) (not appears-body-p) (not (is-action-p def)))
			 	(make-vm-add-linear tuple-reg))
			((and (or (and (is-reused-p def) (is-linear-p def)) (not (is-linear-p def))) (reg-eq-p tuple-reg send-to) (not (is-action-p def)))
				(make-vm-add-persistent tuple-reg))
			((and (is-action-p def) (reg-eq-p tuple-reg send-to))
				(make-vm-run-action tuple-reg))
			((and (is-linear-p def) (reg-eq-p tuple-reg send-to) appears-body-p)
				(make-vm-enqueue-linear tuple-reg))
			(t
				(assert (not (reg-eq-p tuple-reg send-to)))
				(make-send tuple-reg send-to)))))
				
(defun subgoal-appears-in-any-body-p (clause name)
	(do-comprehensions (clause-head clause) (:left body)
		(when (subgoal-appears-code-p body name)
			(return-from subgoal-appears-in-any-body-p t)))
	(do-agg-constructs (clause-head clause) (:body body)
		(when (subgoal-appears-code-p body name)
			(return-from subgoal-appears-in-any-body-p t)))
	(clause-body-matches-subgoal-p clause name))
	
(defun subgoal-is-set-priority-p (name) (string-equal name "set-priority"))
(defun subgoal-is-add-priority-p (name) (string-equal name "add-priority"))
(defun subgoal-is-stop-program-p (name) (string-equal name "stop-program"))

(defun expression-is-the-same-p (arg1 arg2) (equal arg1 arg2))
	
(defun do-compile-reused-subgoal (sub name args sub-regs)
	(let* ((reused-sub (subgoal-get-reused sub))
			 (reg (cdr (assoc reused-sub sub-regs))))
		`(,@(loop for new-arg in args
					 for old-arg in (subgoal-args reused-sub)
					 for i upto (length args)
					 append (unless (expression-is-the-same-p old-arg new-arg)
									(compile-head-move new-arg i reg)))
			,(make-vm-update reg))))
			
(defun do-compile-normal-subgoal (sub name args)
	(with-reg (tuple-reg)
      `(,(make-vm-alloc name tuple-reg)
         ,@(loop for arg in args
               for i upto (length args)
               append (compile-head-move arg i tuple-reg))
         ,@(multiple-value-bind (send-to extra-code) (get-remote-reg-and-code sub tuple-reg)
            `(,@extra-code ,(if (subgoal-has-delay-p sub)
											(make-vm-send-delay tuple-reg send-to (subgoal-delay-value sub))
											(general-make-send name tuple-reg send-to (subgoal-appears-in-any-body-p *compilation-clause* name))))))))
											
(defun do-compile-set-priority (sub args)
	(assert (= (length args) 1))
	(with-compilation-on-reg (priority priority-instrs) (first args)
		(with-old-reg (priority)
			(multiple-value-bind (send-to extra-code) (get-remote-reg-and-code sub nil)
				(if (null send-to)
					`(,@priority-instrs ,(make-vm-set-priority-here priority))
					`(,@priority-instrs ,@extra-code ,(make-vm-set-priority priority send-to)))))))
				
(defun do-compile-add-priority (sub args)
	(assert (= (length args) 1))
	(with-compilation-on-reg (priority priority-instrs) (first args)
		(with-old-reg (priority)
			(multiple-value-bind (send-to extra-code) (get-remote-reg-and-code sub nil)
				(if (null send-to)
					`(,@priority-instrs ,(make-vm-add-priority-here priority))
					`(,@priority-instrs ,@extra-code ,(make-vm-add-priority priority send-to)))))))

(defun do-compile-head-subgoals (head sub-regs)
   (do-subgoals head (:name name :args args :operation append :subgoal sub)
		(cond
			((subgoal-will-reuse-other-p sub)
				(do-compile-reused-subgoal sub name args sub-regs))
			((subgoal-is-set-priority-p name)
				(do-compile-set-priority sub args))
			((subgoal-is-add-priority-p name)
				(do-compile-add-priority sub args))
			((subgoal-is-stop-program-p name)
				`(,(make-vm-stop-program)))
			(t
      		(do-compile-normal-subgoal sub name args)))))

(defconstant +plus-infinity+ 2147483647)

(defun agg-construct-start (op acc)
	(case op
		(:collect
			`(,(make-move (make-vm-nil) acc)))
		((:count :sum)
			`(,(make-move (make-vm-float 0.0) acc)))
		(:min
			`(,(make-move (make-vm-int +plus-infinity+) acc)))
		(otherwise (error 'compile-invalid-error :text (tostring "agg-construct-start: op ~a not recognized" op)))))

(defun agg-construct-end (op acc)
	(case op
		(:collect nil)
		(:count nil)
		(:sum nil)
		(:min nil)
		(otherwise (error 'compile-invalid-error :text (tostring "agg-construct-end: op ~a not recognized" op)))))
		
(defun agg-construct-step (op acc var)
	(case op
		(:collect
			(let ((dest (lookup-used-var (var-name var))))
				`(,(make-vm-cons dest acc acc (expr-type var)))))
		(:sum
			(let ((src (lookup-used-var (var-name var))))
				(assert (reg-dot-p src))
				(with-reg (new)
					`(,(make-move src new) ,(make-vm-op acc acc :float-plus new)))))
		(:count
			(with-reg (new)
				`(,(make-move (make-vm-int 1) new) ,(make-vm-op acc acc :int-plus new))))
		(:min
			(let ((src (lookup-used-var (var-name var))))
				(assert (reg-dot-p src))
				(with-reg (new)
					(with-reg (tmp)
						`(,(make-move src tmp) ,(make-vm-op new tmp :int-lesser acc) ,(make-vm-if new `(,(make-move tmp acc))))))))
		(otherwise (error 'compile-invalid-error
								:text (tostring "agg-construct-step: op ~a not recognized" op)))))
	
(defun compile-agg-construct (c)
	(with-agg-construct c (:specs specs)
		(compile-agg-construct-specs c specs nil nil)))

(defun compile-agg-construct-specs (c specs end vars-regs)
	(cond
		((null specs)
			(let ((inner-code (compile-iterate (agg-construct-body c) (agg-construct-body c) nil nil
									:head-compiler #'(lambda (h d sr s)
																(declare (ignore h d sr s))
																(loop for var-reg in vars-regs
																	append (agg-construct-step (third var-reg) (second var-reg) (first var-reg)))))))
				(dolist (var-reg vars-regs)
					(add-used-var (var-name (first var-reg)) (second var-reg)))
				(let ((head-code (do-compile-head-code (agg-construct-head c) nil nil nil)))
					(dolist (var-reg vars-regs)
						(remove-used-var (var-name (first var-reg))))
					`(,(make-vm-reset-linear (append inner-code (append end (append head-code `(,(make-vm-reset-linear-end))))))))))
		(t
			(let ((first-spec (first specs))
					 (rest-specs (rest specs)))
				(with-reg (acc)
					(with-agg-spec first-spec (:var var :op op)
						(let ((spec-end (agg-construct-end op acc)))
							(let ((inner-code (compile-agg-construct-specs c rest-specs
										(append end spec-end)
										(cons (list var acc op) vars-regs))))
								`(,@(agg-construct-start op acc) ,@inner-code)))))))))

(defun do-compile-head-comprehensions (head def subgoal)
   (let* ((*starting-subgoal* nil)
			 (code (do-comprehensions head (:left left :right right :operation append)
					  (with-compile-context
							(let ((body-comp (compile-iterate left left right nil)))
								(if (get-subgoals left)
									(list (make-vm-reset-linear `(,@body-comp ,(make-vm-reset-linear-end))))
									body-comp))))))
		code))

(defun do-compile-head-aggs (head def subgoal)
	(let ((*starting-subgoal* nil)
			(code-agg (do-agg-constructs head (:agg-construct c :operation append)
					(with-compile-context (compile-agg-construct c)))))
			code-agg))
		
(defun do-compile-one-exists (vars sub-regs exists-body)
	(cond
		((null vars) (do-compile-head-subgoals exists-body sub-regs))
		(t
			(let ((var (first vars))
					(other-vars (rest vars)))
				(with-reg (reg-var)
					(add-used-var (var-name var) reg-var)
						`(,(make-vm-new-node reg-var) ,@(do-compile-one-exists other-vars sub-regs exists-body)))))))
		
(defun do-compile-head-exists (head sub-regs def subgoal)
	(let ((code (do-exists head (:var-list vars :body body :operation append)
						(with-compile-context
							(do-compile-one-exists vars sub-regs body)))))
		code))
		
(defun do-compile-head-conditionals (head sub-regs def subgoal)
	(do-conditionals head (:cmp cmp :term1 term1 :term2 term2 :operation append)
		(let ((head1 (do-compile-head-code term1 sub-regs def subgoal))
				(head2 (do-compile-head-code term2 sub-regs def subgoal)))
			(with-compiled-expr (cmp-place cmp-code) cmp
				`(,@cmp-code ,(make-vm-if-else cmp-place head1 head2))))))

(defun do-compile-head-code (head sub-regs def subgoal)
	"Head is the head expression. Subgoal is the starting subgoal and def its definition."
	(let ((subgoals-code (do-compile-head-subgoals head sub-regs))
			(conditional-code (do-compile-head-conditionals head sub-regs def subgoal))
         (comprehensions-code (do-compile-head-comprehensions head def subgoal))
			(agg-code (do-compile-head-aggs head def subgoal))
			(exists-code (do-compile-head-exists head sub-regs def subgoal)))
		`(,@subgoals-code ,@comprehensions-code ,@agg-code ,@exists-code ,@conditional-code)))
		
(defun subgoal-to-be-deleted-p (subgoal def)
   (and (is-linear-p def)
		  (not (subgoal-is-reused-p subgoal))))
		
(defun subgoal-to-change-p (sub head)
	(when (subgoal-will-modify-p sub)
		(do-subgoals head (:subgoal head-sub)
			(let ((reused (subgoal-get-reused head-sub)))
				(when (eq reused sub)
					(return-from subgoal-to-change-p t))))
		nil))
        
(defun compile-linear-deletes-and-returns (subgoal def sub-regs inside head)
	(let* ((delete-regs (mapcar #'cdr (remove-if-not #L(let ((def (lookup-definition (subgoal-name !1))))
																						(and (subgoal-to-be-deleted-p !1 def)
																							  (not (subgoal-to-change-p !1 head))))
																sub-regs :key #'car)))
		    (deletes (mapcar #'make-vm-remove delete-regs)))
		(cond
			(*compiling-axioms* deletes)
			((and subgoal def) deletes)
			((and *compiling-rule* (clause-is-persistent-p *compilation-clause*)) deletes)
			((and (null def) (null subgoal)) `(,@deletes ,@(unless (null sub-regs) (list (make-return-derived)))))
			((and (not *compilation-clause*) (and subgoal) (subgoal-to-be-deleted-p subgoal def))
				`(,@deletes ,(make-return-linear)))
			(t	`(,@deletes ,@(if inside `(,(make-return-derived)) nil))))))

; head-compiler is isually do-compile-head-code
(defun do-compile-head (head sub-regs inside head-compiler)
   (let* ((def (if (subgoal-p *starting-subgoal*) (lookup-definition (subgoal-name *starting-subgoal*)) nil))
          (head-code (funcall head-compiler head sub-regs def *starting-subgoal*))
          (linear-code (compile-linear-deletes-and-returns *starting-subgoal* def sub-regs inside head))
          (delete-code (compile-inner-delete *compilation-clause*)))
      `(,@head-code ,@delete-code ,@linear-code)))
      
(defun compile-assignments-and-head (assignments head-fun)
   (if (null assignments)
       (funcall head-fun)
       (let ((ass (find-if (valid-assignment-p (all-used-var-names)) assignments)))
         (with-reg (new-reg)
				(with-compiled-expr (place instrs :force-dest new-reg) (assignment-expr ass)
            	(add-used-var (var-name (assignment-var ass)) place)
            	(let ((other-code (compile-assignments-and-head (remove-tree ass assignments) head-fun)))
               	`(,@instrs ,@other-code)))))))

(defun remove-defined-assignments (assignments) (mapcar #L(remove-used-var (var-name (assignment-var !1))) assignments))

(defun compile-head (body head sub-regs inside head-compiler)
	(cond
		((and (not (null head)) (clause-head-is-recursive-p head))
			(let* ((assigns (get-assignments body))
					 (code (compile-assignments-and-head assigns #L(loop for clause in head
																						append (with-compile-context (compile-iterate (clause-body clause) (clause-body clause)
																													(clause-head clause) sub-regs :inside inside))))))
				(remove-defined-assignments assigns)
				code))
		(t
   		(let* ((assigns (get-assignments body))
          		 (head-code (compile-assignments-and-head assigns #L(do-compile-head head sub-regs inside head-compiler))))
         	(remove-defined-assignments assigns)
         	(if *starting-subgoal*
            	`(,(make-vm-rule-done) ,@head-code)
            	head-code)))))

(defun select-next-subgoal-for-compilation (body)
	"Selects next subgoal for compilation. We give preference to subgoals with modifiers (random/min/etc)."
	(let ((no-args (find-if #'(lambda (sub) (and (subgoal-p sub) (null (subgoal-args sub)))) body)))
		(if no-args
			no-args
			(let ((with-mod (find-if #'(lambda (sub) (and (subgoal-p sub) (or (subgoal-has-random-p sub) (subgoal-has-min-p sub)))) body)))
				(if with-mod
					with-mod
					(find-if #'subgoal-p body))))))

(defun constraints-in-the-same-subgoal-p (reg)
	#'(lambda (c)
		(let ((v1 (low-constraint-v1 c))
				(v2 (low-constraint-v2 c)))
			(when (and (reg-dot-p v1)
							(reg-dot-p v2))
				(reg-eq-p reg (reg-dot-reg v2))))))
				
(defun transform-reg-matches (reg)
	#'(lambda (c)
		(make-low-constraint (low-constraint-type c) (make-reg-dot reg (reg-dot-field (low-constraint-v1 c))) (low-constraint-v2 c))))
		
(defun subgoal-must-be-ordered-p (sub)
	(or (subgoal-has-min-p sub)
		(subgoal-has-random-p sub)))
		
(defun create-iterate-instruction (sub def match-constraints tuple-reg iterate-code)
	(with-subgoal sub (:name name)
		(cond
			((and (not (is-linear-p def)) (not (subgoal-must-be-ordered-p sub)))
				(make-persistent-iterate name tuple-reg match-constraints iterate-code))
			((and (not (is-linear-p def)) (subgoal-must-be-ordered-p sub))
				(make-order-persistent-iterate name tuple-reg match-constraints iterate-code sub))
			((and (is-linear-p def) (subgoal-to-be-deleted-p sub def) (not (subgoal-must-be-ordered-p sub)))
				(make-linear-iterate name tuple-reg match-constraints iterate-code))
			((and (is-linear-p def) (not (subgoal-to-be-deleted-p sub def)) (not (subgoal-must-be-ordered-p sub)))
				(make-rlinear-iterate name tuple-reg match-constraints iterate-code))
			((and (is-linear-p def) (subgoal-to-be-deleted-p sub def) (subgoal-must-be-ordered-p sub))
				(make-order-linear-iterate name tuple-reg match-constraints iterate-code sub))
			((and (is-linear-p def) (not (subgoal-to-be-deleted-p sub def)) (subgoal-must-be-ordered-p sub))
				(make-order-rlinear-iterate name tuple-reg match-constraints iterate-code sub))
			(t (assert nil)))))

(defun compile-iterate (body orig-body head sub-regs &key (inside nil) (head-compiler #'do-compile-head-code))
   (multiple-value-bind (constraints assignments) (get-compile-constraints-and-assignments body)
		(let* ((next-sub (select-next-subgoal-for-compilation body))
             (body1 (remove-unneeded-assignments (remove-tree-first next-sub (remove-all body constraints)) head)))
         (compile-constraints-and-assignments constraints assignments
					(if (not next-sub)
						(compile-head body1 head sub-regs inside head-compiler)
               	(let ((next-sub-name (subgoal-name next-sub)))
                  	(with-reg (reg)
								(multiple-value-bind (low-constraints body2) (add-subgoal next-sub reg body1 :match)
									; body2 may have a reduced number of constraints
									(let* ((match-constraints (mapcar #'rest (remove-if (constraints-in-the-same-subgoal-p reg) low-constraints)))
											;; these constraints related arguments inside the matching subgoal
										 	(inner-constraints (mapcar (transform-reg-matches reg) (filter (constraints-in-the-same-subgoal-p reg) low-constraints)))
	                            	(def (lookup-definition next-sub-name))
	                            	(iterate-code (compile-low-constraints inner-constraints
																		(compile-iterate body2 orig-body head
																						(acons next-sub reg sub-regs)
																						:inside t :head-compiler head-compiler))))
	                       	`(,(create-iterate-instruction next-sub def match-constraints reg iterate-code)))))))))))
     
(defun compile-constraint (inner-code constraint)
   (let ((c-expr (constraint-expr constraint)))
      (with-compiled-expr (reg expr-code) c-expr
         `(,@expr-code ,(make-vm-if reg inner-code)))))
               
(defun select-best-constraint (constraints all-vars)
   (let ((all (filter (valid-constraint-p all-vars) constraints)))
      (if (null all)
         nil
         (first (sort all #'> :key #'constraint-priority)))))
   
(defun do-compile-constraints-and-assignments (constraints assignments inner-code)
	(if (null constraints)
		inner-code
      (let* ((all-vars (all-used-var-names))
            (new-constraint (select-best-constraint constraints all-vars)))
         (if (null new-constraint)
            (let ((ass (find-if (valid-assignment-p all-vars) assignments)))
               (with-compiled-expr (place instrs) (assignment-expr ass)
               	(add-used-var (var-name (assignment-var ass)) place)
               	(let ((other-code (do-compile-constraints-and-assignments
                                    	constraints (remove-tree ass assignments) inner-code)))
                  	`(,@instrs ,@other-code))))
            (let ((inner-code (do-compile-constraints-and-assignments
                                       (remove-tree new-constraint constraints) assignments inner-code)))
               (compile-constraint inner-code new-constraint))))))

(defun compile-constraints-and-assignments (constraints assignments inner-code)
   (always-ret (do-compile-constraints-and-assignments constraints assignments inner-code)
      (remove-defined-assignments assignments)))

(defun compile-low-constraints (constraints inner-code)
	"Compiles the low constraints when starting with some initial subgoal."
   (with-reg (reg)
      (reduce #'(lambda (c old)
						(let ((vm-op (set-type-to-op (low-constraint-type c) :type-bool :equal)))
							(assert (not (null vm-op)))
							(let ((v1 (low-constraint-v1 c))
									(v2 (low-constraint-v2 c)))
								(cond
									((and (reg-p v1) (reg-p v2))
                  				(list (make-vm-op reg v1
													vm-op
                                    	v2)
                           			(make-vm-if reg old)))
									((and (reg-p v1))
										(with-reg (r2)
											(list (make-move v2 r2) (make-vm-op reg v1 vm-op r2) (make-vm-if reg old))))
									((and (reg-p v2))
										(with-reg (r1)
											(list (make-move v1 r1) (make-vm-op reg r1 vm-op v2) (make-vm-if reg old))))
									(t
										(with-reg (r1)
											(with-reg (r2)
												(list (make-move v1 r1) (make-move v2 r2) (make-vm-op reg r1 vm-op r2) (make-vm-if reg old)))))))))
               constraints :initial-value inner-code :from-end t)))

(defun get-first-min-subgoal (body)
	(do-subgoals body (:subgoal sub)
		(when (subgoal-has-min-p sub)
			(return-from get-first-min-subgoal sub))))

(defun compile-initial-subgoal (body orig-body head subgoal)
	(let ((*starting-subgoal* subgoal)
			(body1 (remove-tree subgoal body)))
      (if (null (subgoal-args subgoal))
         (compile-iterate body1 orig-body head nil)
         (with-reg (sub-reg)
				(assert (= (reg-num sub-reg) 0))
				(multiple-value-bind (low-constraints body2) (add-subgoal subgoal sub-reg body1)
	            (let ((inner-code (compile-iterate body2 orig-body head nil)))
	               `(,@(compile-low-constraints low-constraints inner-code))))))))

(defun get-my-subgoals (body name)
   (filter #'(lambda (sub)
						(string-equal (subgoal-name sub) name))
			(get-subgoals body)))

(defun compile-with-starting-subgoal (body head &optional subgoal)
   (with-empty-compile-context
      (multiple-value-bind (first-constraints first-assignments) (get-compile-constraints-and-assignments body)
         (let* ((remaining (remove-unneeded-assignments (remove-all body first-constraints) head)))
				(compile-constraints-and-assignments first-constraints first-assignments (compile-initial-subgoal remaining body head subgoal))))))
				
(defun compile-subgoal-clause (name clause)
   (with-clause clause (:body body :head head)
		(let ((*compilation-clause* clause))
      	(loop-list (subgoal (get-my-subgoals body name) :operation append)
         	(compile-with-starting-subgoal body head subgoal)))))

(defun compile-normal-process (name clauses)
   (unless clauses (return-from compile-normal-process nil))
   (do-clauses clauses (:clause clause :operation append)
		(let ((clause-code (compile-subgoal-clause name clause)))
			(assert (not (null (clause-get-id clause))))
			`(,(make-vm-rule (clause-get-id clause)) ,@clause-code))))
      
(defun compile-const-axioms ()
	"Take all constant axioms in the program and map them to an hash table (per node).
	Then, create a SELECT NODE instruction and add NEW-AXIOM with each set of node axioms."
	(let ((hash (make-hash-table)))
		(do-const-axioms (:subgoal sub)
			(with-subgoal sub (:args args)
				(let* ((fst (first args))
					    (node (addr-num fst)))
					(setf (subgoal-args sub) (rest args)) ; remove home argument
					(push sub (gethash node hash)))))
		(let ((vm (make-vm-select-node)))
			(loop for key being the hash-keys of hash
					using (hash-value value)
					do (let ((ax (make-vm-new-axioms value)))
							(vm-select-node-push vm key (list ax))))
			(list vm))))
	
(defun compile-init-process ()
	(let ((const-axiom-code (compile-const-axioms))
			(*compiling-axioms* t))
		;; handle other axioms (non-constant)
   	(append const-axiom-code
			(do-axioms (:body body :head head :clause clause :operation :append)
      		(compile-with-starting-subgoal body head)))))

(defun compile-processes ()
	(do-definitions (:definition def :name name :operation collect)
		(if (is-init-p def)
         (make-process name `(,(make-return-linear)))
         (make-process name `(,@(compile-normal-process name (filter #'clause-is-persistent-p (find-clauses-with-subgoal-in-body name)))
                                 ,(make-return))))))

(defun compile-consts ()
	(do-constant-list *consts* (:name name :expr expr :operation append)
		(with-compiled-expr (place code) expr
			`(,@code ,(make-move place (make-vm-constant name) (expr-type expr))))))
			
(defun compile-function-arguments (body args n)
	(if (null args)
		(multiple-value-bind (dest body) (compile-expr body (make-vm-stack 32))
			`(,@body ,(make-move (make-vm-stack (1+ *num-regs*)) (make-vm-pcounter))))
		(progn
			(with-reg (r)
				(add-used-var (var-name (first args)) r)
				(compile-function-arguments body (rest args) (1+ n))))))

(defun compile-functions ()
	(do-functions *functions* (:name name :args args :ret-type ret-type :body body :operation collect)
		(with-empty-compile-context
			(compile-function-arguments body args 0))))

(defun number-clauses ()
	(do-rules (:clause clause :id id)
		(clause-add-id clause (1+ id))))

(defun rule-subgoal-ids (clause)
	(with-clause clause (:body body :head head)
		(let ((ids nil))
			(do-subgoals body (:name name)
				(push-dunion (lookup-def-id name) ids))
			(when (clause-head-is-recursive-p head)
				(do-clauses head (:body sub-body)
					(do-subgoals sub-body (:name name)
						(push-dunion (lookup-def-id name) ids))))
			ids)))

(defun compile-ast-rules ()
	(let ((init-rule (make-rule-code (with-empty-compile-context
										(with-reg (reg)
											`(,(make-vm-rule 0)
											  	,(make-linear-iterate "_init"
													reg
													nil 
													`(,(make-vm-rule-done)
														,(make-vm-remove reg)
														,@(compile-init-process)
														,(make-move (make-vm-ptr 0) (make-reg 0))
														,(make-return-derived)))
												,(make-return)))) (list (lookup-def-id "_init")) nil))
			(other-rules (do-rules (:clause clause :id id :operation collect)
								(with-clause clause (:body body :head head)
									(if (clause-is-persistent-p clause)
										(make-rule-code `(,(make-return)) (rule-subgoal-ids clause) t)
										(let ((*compilation-clause* clause)
												(*compiling-rule* t))
											(make-rule-code (with-empty-compile-context
														`(,(make-vm-rule (1+ id)) ,@(compile-iterate body body head nil) ,(make-return)))
													(rule-subgoal-ids clause)
													(clause-is-persistent-p clause))))))))
		`(,init-rule ,@other-rules)))
		
(defun identical-clause-p (other-clause-subgoals subgoals)
	(unless (= (length subgoals) (length other-clause-subgoals))
		(return-from identical-clause-p nil))
	(loop for subgoal in subgoals
			do (with-subgoal subgoal (:name name)
					(let ((found (subgoal-appears-code-p other-clause-subgoals name)))
						(if found
							(setf other-clause-subgoals (remove found other-clause-subgoals))
							(return-from identical-clause-p nil)))))
	(null other-clause-subgoals))
	
(defun find-identical-assignment (ass clause)
	(find-if #L(equal (assignment-expr ass) (assignment-expr !1)) (get-assignments (clause-body clause))))
	
(defun find-common-assignments (original-clause others)
	(let ((found-some t)
			(identical nil))
		(loop while found-some
				do (progn
						(setf found-some nil)
						(loop named inner for ass in (get-assignments (clause-body original-clause))
								do (let ((founds (mapcar #L(find-identical-assignment ass !1) others)))
										(when (every #L(not (null !1)) founds)
											(setf found-some t)
											(let* ((old-var (assignment-var ass))
													 (new-assignment-var (generate-random-var (expr-type old-var))))
												(replace-variable original-clause old-var new-assignment-var)
												(setf (clause-body original-clause) (delete ass (clause-body original-clause)))
												(loop for other-clause in others
														for other-ass in founds
														do (setf (clause-body other-clause) (delete other-ass (clause-body other-clause)))
														do (replace-variable other-clause (assignment-var other-ass) new-assignment-var))
												(push ass identical)
												(return-from inner nil)))))))
		identical))
		
(defun find-identical-constraint (c clause)
	(find-if #L(equal c !1) (get-constraints (clause-body clause))))

(defun find-common-constraints (original-clause others)
	(loop for c in (get-constraints (clause-body original-clause))
			append (let ((founds (mapcar #L(find-identical-constraint c !1) others)))
						(when (every #L(not (null !1)) founds)
							(setf (clause-body original-clause) (delete c (clause-body original-clause)))
							(loop for other-clause in others
									for other-c in founds
									do (setf (clause-body other-clause) (delete other-c (clause-body other-clause))))
							(list c)))))

(defun merge-clause (original-clause others)
	(let* ((body-original (clause-body original-clause))
			 (body-others (mapcar #'clause-body others))
			 (subgoals-original (get-subgoals body-original))
			 (subgoals-others (mapcar #'get-subgoals body-others))
			 common-subgoals)
		(assert (every #L(= (length !1) (length subgoals-original)) subgoals-others))
		;; replace all variables in 'others'
		(cond
			((= (length subgoals-original) 1)
				(let ((subgoal (first subgoals-original)))
					 	(with-subgoal subgoal (:name name)
								(push subgoal common-subgoals) 
								(let ((founds (mapcar #L(subgoal-appears-code-p !1 name) subgoals-others))
										(new-vars (mapcar #L(generate-random-var (var-type !1)) (subgoal-args subgoal))))
									(assert (every #L(not (null !1)) founds))
									(loop for var in new-vars
											for old in (subgoal-args subgoal)
											do (replace-variable original-clause old var))
									(loop for other-subgoal in founds
											for other-clause in others
											do (loop for original-arg in (subgoal-args subgoal)
														for other-arg in (subgoal-args other-subgoal)
														do (progn
																(assert (and (var-p original-arg) (var-p other-arg)))
																(unless (var-eq-p original-arg other-arg)
															;; Change other-arg with original-arg.
															(replace-variable other-clause other-arg original-arg)
															))))
									;; remove already processed subgoals
									(setf subgoals-others (mapcar #L(remove !1 !2) founds subgoals-others))
									(setf (clause-body original-clause) (remove subgoal (clause-body original-clause) :test #'equal))
									(loop for other-subgoal in founds
											for other-clause in others
											do (setf (clause-body other-clause) (remove other-subgoal (clause-body other-clause) :test #'equal))))))
				(let* ((common-ass (find-common-assignments original-clause others))
					 	 (common-cons (find-common-constraints original-clause others))
					 	 (rest-original (clause-body original-clause))
					 	 (original-head (clause-head original-clause)))
					(setf (clause-body original-clause) `(,@common-subgoals ,@common-ass ,@common-cons))
					(setf (clause-head original-clause) `(,(make-clause rest-original original-head) ,@others))))
			(t
				(setf (clause-body original-clause) nil)
				(setf (clause-head original-clause) `(,(make-clause body-original (clause-head original-clause)) ,@others))
				))))
				
(defun merge-clauses ()
	"Find subsequent identical clauses, where the body facts are shared."
	(let ((all-clauses *clauses*)
			(processed-clauses nil))
		(loop while all-clauses
				do (let ((first-clause (first all-clauses))
							(other-clauses (rest all-clauses)))
						(cond
							((rule-is-persistent-p first-clause)
								(push-end first-clause processed-clauses)
								(setf all-clauses (rest all-clauses)))
							(t
								(with-clause first-clause (:body body)
									(let ((subgoals-clause (get-subgoals body)))
										(let ((identical-clauses (filter-first #L(identical-clause-p (get-subgoals (clause-body !1)) subgoals-clause) other-clauses)))
											(cond
												((null identical-clauses)
													(push-end first-clause processed-clauses)
													(setf all-clauses (rest all-clauses)))
												(t
													(merge-clause first-clause identical-clauses)
													(push-end first-clause processed-clauses)
													(setf all-clauses (drop-first-n all-clauses (1+ (length identical-clauses)))))))))))))
		(setf *clauses* processed-clauses)))

(defun find-reusable-fact-in-head (head sub)
	(cond
		((null head) nil)
		((clause-head-is-recursive-p head)
			(let ((ret nil))
				(do-clauses head (:clause c :head subhead)
					(when (find-reusable-fact-in-head subhead sub)
						(setf ret t)))
			ret))
		(t
			(do-subgoals head (:name name :subgoal sub2)
				(when (and (string-equal name (subgoal-name sub))
								(not (subgoal-is-remote-p sub2)))
					(unless (subgoal-will-reuse-other-p sub2)
						(subgoal-will-modify sub sub2)
						(subgoal-will-reuse-other sub2 sub)
						(return-from find-reusable-fact-in-head t))))
			nil)))
			
(defun find-reusable-facts-body (body head)
	(do-subgoals body (:subgoal sub :name name)
		(let ((def (lookup-definition name)))
			(when (is-linear-p def)
				(unless (subgoal-is-reused-p sub)
					(find-reusable-fact-in-head head sub))))))
	
(defun find-reusable-facts ()
	(do-rules (:clause clause :body body :head head)
		(unless (clause-is-persistent-p clause)
			(find-reusable-facts-body body head)
			(when (clause-head-is-recursive-p head)
				(do-clauses head (:body sub-body :head sub-head)
					(find-reusable-facts-body sub-body sub-head))))))

(defun compile-ast ()
	(merge-clauses)
	(find-persistent-rules)
	(number-clauses)
	(find-reusable-facts)
	(let ((procs (compile-processes))
			(consts (compile-consts))
			(functions (compile-functions)))
		(make-instance 'code :processes procs :consts `(,@consts (:return-derived)) :functions functions)))
