(in-package :cl-meld)

(define-condition compile-invalid-error (error)
   ((text :initarg :text :reader text)))

(defun has-reg-p (regs reg)
	(find-if #'(lambda (x) (reg-eq-p x reg)) regs))
	
(defun extend-regs (regs reg)
	(if (has-reg-p regs reg)
		regs
		(cons reg regs)))

(defun free-reg (regs reg)
   (remove reg regs :count 1 :test #'reg-eq-p))
		
(defun find-unused-reg (regs)
	(loop for i upto (1- *num-regs*)
		do (if (not (has-reg-p regs (make-reg i)))
				(return-from find-unused-reg (make-reg i)))))
(defun find-unused-n-regs (regs n)
   (let ((c 0))
      (loop for i upto (1- *num-regs*)
            while (< c n)
            append (unless (has-reg-p regs (make-reg i))
                  (incf c)
                  `(,(make-reg i))))))
		
(defun alloc-reg (regs)
	(assert (< (length regs) *num-regs*))
	(let ((new-reg (find-unused-reg regs)))
		(values new-reg (extend-regs regs new-reg))))

(defun alloc-n-reg (regs n)
   (assert (< (+ n (length regs)) *num-regs*))
   (let ((new-regs (find-unused-n-regs regs n)))
      (values new-regs (append regs new-regs))))

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
(defmacro with-n-regs (n (regs) &body body)
   `(multiple-value-bind (,regs *used-regs*) (alloc-n-reg *used-regs* ,n)
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
   "Return constraints and assignments that can be used from the body."
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

(defmacro with-compiled-expr ((place code &key (force-dest nil) (top-level nil)) expr &body body)
	`(multiple-value-bind (,place ,code *used-regs*) (compile-expr ,expr :dest ,force-dest :top-level ,top-level)
		(cond
			((and ,force-dest (not (equalp ,place ,force-dest)))
				(setf ,code (append ,code (list (make-move ,place ,force-dest))))
				(setf ,place ,force-dest)
				,@body)
			(t
				,@body))))

(defmacro with-compilation ((place code &key (top-level nil)) expr &body body)
	`(multiple-value-bind (,place ,code *used-regs*) (compile-expr ,expr :top-level ,top-level)
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
		
(defmacro with-compilation-on-rf ((place code &key top-level) expr &body body)
	"Ensures that place is either a field or register."
	(alexandria:with-gensyms (new-reg)
		`(with-compilation (,place ,code :top-level ,top-level) ,expr
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

(defmacro compile-expr-to (expr place &key (top-level nil) (update-p nil))
	(alexandria:with-gensyms (new-place code)
      `(multiple-value-bind (,new-place ,code *used-regs*) (compile-expr ,expr :dest ,place :top-level ,top-level)
		   (if (not (equal ,new-place ,place))
            (append ,code (list (make-move ,new-place ,place (expr-type ,expr))))
            ,code))))

(defmacro compile-expr-to-reg (expr &key try-place (top-level nil))
   (alexandria:with-gensyms (new-place new-code)
      `(cond
        ((or (null ,try-place) (not (reg-p ,try-place)))
            (with-reg (,new-place)
               (values ,new-place (compile-expr-to ,expr ,new-place :top-level ,top-level))))
        (t
          (values ,try-place (compile-expr-to ,expr ,try-place :top-level ,top-level))))))

(defun decide-external-function (name new-reg regs gc call)
	"Decides if external function is pre-defined or not."
	(if (lookup-standard-external-function name)
		(make-vm-call name new-reg regs (make-vm-bool gc) (expr-type call))
		(make-vm-calle name new-reg regs (make-vm-bool gc) (expr-type call))))
		
(defun get-external-function-ret-type (name)
	(let ((fun (lookup-standard-external-function name)))
		(when fun
			(extern-ret-type fun))))

(defun find-polymorphic-type (extern concrete-types)
 (let ((template-types `(,(extern-ret-type extern) ,@(extern-types extern))))
  (when (and (extern-poly-p extern) (has-all-type-p template-types))
   (loop for template in template-types
         for concrete in concrete-types
         when (has-all-type-p template)
         do (progn
              (let ((typ (find-all-type template concrete)))
               (when typ
                (return-from find-polymorphic-type typ))))))))

(defun compile-call-args (args call)
   (cond
    ((null args)
     (let ((poly-typ (find-polymorphic-type (lookup-external-definition (call-name call))
                                            `(,(expr-type call) ,@(loop for arg in (call-args call) collect (expr-type arg))))))
      (cond
       (poly-typ 
        (with-reg (type-reg)
            (values `(,type-reg) `(,(make-move (make-vm-type poly-typ) type-reg)))))
       (t (values nil nil)))))
    (t
		(with-compilation-on-reg (arg-place arg-code) (first args)
			(multiple-value-bind (regs codes) (compile-call-args (rest args) call)
				(values (cons arg-place regs) `(,@arg-code ,@codes)))))))
				
(defun compile-call (name args dest call gc)
	(multiple-value-bind (regs codes) (compile-call-args args call)
		(cond
			((null dest)
		 		(with-reg (new-dest)
					(return-expr new-dest `(,@codes ,(decide-external-function name new-dest regs gc call)))))
			((and dest (reg-p dest))
				(return-expr dest `(,@codes ,(decide-external-function name dest regs gc call))))
			(t
				(with-reg (new-dest)
					(return-expr dest `(,@codes ,(decide-external-function name new-dest regs gc call)
                                   ,(make-move new-dest dest (expr-type call)))))))))
			
(defun compile-callf-args (args args-code n)
	(if (null args)
		args-code
		(with-compiled-expr (arg-place arg-code :force-dest (make-reg n)) (first args)
			(compile-callf-args (rest args) `(,@args-code ,@arg-code) (1+ n)))))

(defun compile-expr (expr &key dest top-level)
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
      ((or (host-p expr) (host-id-p expr)) (return-expr (make-vm-host-id)))
      ((argument-p expr)
			(return-expr (make-vm-argument (argument-id expr))))
		((get-constant-p expr)
			(return-expr (make-vm-constant (get-constant-name expr))))
		((var-p expr)
			(let ((look (lookup-used-var (var-name expr))))
            (unless look
             (error 'compile-invalid-error :text (tostring "Cannot find variable ~a" (var-name expr))))
				(return-expr look)))
      ((call-p expr)
			(let ((name (call-name expr))
					(args (call-args expr)))
				(cond
               ((string-equal name "fabs")
                (with-dest-or-new-reg (dest)
                 (multiple-value-bind (arg-place arg-code) (compile-expr-to-reg (first args) :try-place dest :top-level top-level)
                  (cond
                     ((reg-p dest)
                      (return-expr dest `(,@arg-code ,(make-vm-fabs arg-place dest))))
                     (t
                      (return-expr dest `(,@arg-code ,(make-vm-fabs arg-place arg-place) ,(make-move arg-place dest))))))))
					((string-equal name "cpu-id")
						(with-dest-or-new-reg (dest)
							(with-compiled-expr (arg-place arg-code :force-dest dest) (first args)
								(return-expr dest `(,@arg-code ,(make-vm-cpu-id arg-place dest))))))
               ((string-equal name "is-moving")
                  (with-dest-or-new-reg (dest)
                     (with-compiled-expr (arg-place arg-code :force-dest dest) (first args)
                        (return-expr dest `(,@arg-code ,(make-vm-is-moving arg-place dest))))))
               ((string-equal name "is-static")
                  (with-dest-or-new-reg (dest)
                     (with-compiled-expr (arg-place arg-code :force-dest dest) (first args)
                        (return-expr dest `(,@arg-code ,(make-vm-is-static arg-place dest))))))
					((string-equal name "priority")
						(with-dest-or-new-reg (dest)
							(with-compiled-expr (arg-place arg-code :force-dest dest) (first args)
								(return-expr dest `(,@arg-code ,(make-vm-node-priority arg-place dest))))))
               ((string-equal name "cpu-static")
                  (with-dest-or-new-reg (dest)
                     (with-compiled-expr (arg-place arg-code :force-dest dest) (first args)
                        (return-expr dest `(,@arg-code ,(make-vm-cpu-static arg-place dest))))))
               ((string-equal name "facts-proved")
                  (with-dest-or-new-reg (dest)
                     (with-compiled-expr (arg-place arg-code :force-dest dest) (first args)
                        (return-expr dest `(,@arg-code ,(make-vm-facts-proved arg-place dest))))))
               ((string-equal name "facts-consumed")
                  (with-dest-or-new-reg (dest)
                     (with-compiled-expr (arg-place arg-code :force-dest dest) (first args)
                        (return-expr dest `(,@arg-code ,(make-vm-facts-consumed arg-place dest))))))
					(t 
         			(compile-call name args dest expr (not top-level))))))
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
         (with-dest-or-new-reg (dest)
            (let ((code1 (compile-expr-to (if-e1 expr) dest))
                  (code2 (compile-expr-to (if-e2 expr) dest)))
               (multiple-value-bind (place-cmp code-cmp) (compile-expr-to-reg (if-cmp expr) :try-place dest :top-level top-level)
                  (return-expr dest `(,@code-cmp ,(make-vm-if-else place-cmp code1 code2 (make-vm-if-spec expr dest))))))))
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
			(let (ptail ctail phead chead d)
				(with-compilation-on-rf (place-tail code-tail :top-level top-level) (cons-tail expr)
						(setf ptail place-tail
								ctail code-tail)
                  (with-compilation-on-rf (place-head code-head :top-level top-level) (cons-head expr)
                     (setf phead place-head
                           chead code-head)))
            (with-dest-or-new-reg (dest)
               (return-expr dest `(,@ctail ,@chead ,(make-vm-cons phead ptail dest (expr-type expr)
                                                     (make-vm-bool (not top-level))))))))
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
      ((cpus-p expr) (return-expr (make-vm-cpus)))
      ((and-p expr)
      	(with-dest-or-new-reg (dest)
            (let ((code1 (compile-expr-to (op-op1 expr) dest))
                  (code2 (compile-expr-to (op-op2 expr) dest)))
               (return-expr dest `(,@code1 ,(make-vm-if dest code2))))))
      ((or-p expr)
      	(with-dest-or-new-reg (dest)
          (let ((code1 (compile-expr-to (op-op1 expr) dest))
                (code2 (compile-expr-to (op-op2 expr) dest)))
               (let ((new-reg (alloc-new-reg)))
                  (return-expr dest
                     `(,@code1 ,(make-vm-not dest new-reg)
                        ,(make-vm-if new-reg code2)))))))
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
				(return-expr dest `(,@(generate-op-instr expr new-dest new-place1 new-place2 new-code1 new-code2)
                                ,(make-move new-dest dest)))))
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
					(assert (not (null vm-op)) (expr) "Cannot find operator for expression ~a" expr)
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
                        (find-if (subgoal-by-name (delete-option-name delete-option))
                            (get-subgoals (clause-body clause))))))))
            
(defun compile-head-move (arg i tuple-reg &key (update-p nil))
   (let ((reg-dot (make-reg-dot tuple-reg i :update-p update-p)))
      (compile-expr-to arg reg-dot :top-level t)))

(defun general-make-send (sub name tuple-reg send-to appears-body-p)
	(let ((def (lookup-definition name)))
		(cond
         ((and (is-linear-p def) (subgoal-is-thread-p sub))
            (make-vm-enqueue-linear tuple-reg))
         ((and (not (is-linear-p def)) (subgoal-is-thread-p sub))
            (make-vm-add-thread-persistent tuple-reg))
         ((and (subgoal-is-thread-p sub))
            (assert nil (sub) "Cannot send subgoal ~a to thread." sub))
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
	
(defun expression-is-the-same-p (arg1 arg2) (equal arg1 arg2))
	
(defun do-compile-reused-subgoal (sub name args sub-regs)
	(let* ((reused-sub (subgoal-get-reused sub))
			 (reg (cdr (assoc reused-sub sub-regs))))
      (assert reg)
		`(,@(loop for new-arg in args
					 for old-arg in (subgoal-args reused-sub)
					 for i upto (length args)
					 append (unless (expression-is-the-same-p old-arg new-arg)
									(compile-head-move new-arg i reg :update-p t))))))
			
(defun do-compile-normal-subgoal (sub name args)
	(with-reg (tuple-reg)
      `(,(make-vm-alloc name tuple-reg)
         ,@(loop for arg in args
               for i upto (length args)
               append (compile-head-move arg i tuple-reg))
         ,@(multiple-value-bind (send-to extra-code) (get-remote-reg-and-code sub tuple-reg)
            `(,@extra-code ,(if (subgoal-has-delay-p sub)
											(make-vm-send-delay tuple-reg send-to (subgoal-delay-value sub))
											(general-make-send sub name tuple-reg send-to
                                     (subgoal-appears-in-any-body-p *compilation-clause* name))))))))
											
(defun subgoal-is-set-priority-p (name) (string-equal name "set-priority"))
(defun subgoal-is-add-priority-p (name) (string-equal name "add-priority"))
(defun subgoal-is-schedule-next-p (name) (string-equal name "schedule-next"))
(defun subgoal-is-stop-program-p (name) (string-equal name "stop-program"))
(defun subgoal-is-set-default-priority-p (name) (string-equal name "set-default-priority"))
(defun subgoal-is-set-static-p (name) (string-equal name "set-static"))
(defun subgoal-is-set-moving-p (name) (string-equal name "set-moving"))
(defun subgoal-is-set-affinity-p (name) (string-equal name "set-affinity"))
(defun subgoal-is-set-cpu-p (name) (string-equal name "set-cpu"))
(defun subgoal-is-remove-priority-p (name) (string-equal name "remove-priority"))

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

(defun do-compile-remove-priority (sub args)
	(assert (= (length args) 0))
	(multiple-value-bind (send-to extra-code) (get-remote-reg-and-code sub nil)
		(if (null send-to)
			`(,(make-vm-remove-priority-here))
			`(,@extra-code ,(make-vm-remove-priority send-to)))))

(defun do-compile-schedule-next (sub args)
   (assert (= (length args) 0))
   (multiple-value-bind (send-to extra-code) (get-remote-reg-and-code sub nil)
      (if (null send-to)
         (with-reg (dest)
            `(,(make-move (make-vm-host-id) dest) ,(make-vm-schedule-next dest)))
         `(,@extra-code ,(make-vm-schedule-next send-to)))))

(defun do-compile-set-default-priority (sub args)
   (assert (= (length args) 1))
   (with-compilation-on-reg (priority priority-instrs) (first args)
      (with-old-reg (priority)
         (multiple-value-bind (send-to extra-code) (get-remote-reg-and-code sub nil)
            (if (null send-to)
               `(,@priority-instrs ,(make-vm-set-default-priority-here priority))
               `(,@priority-instrs ,@extra-code ,(make-vm-set-default-priority priority send-to)))))))

(defun do-compile-set-static (sub args)
   (assert (= (length args) 0))
   (multiple-value-bind (send-to extra-code) (get-remote-reg-and-code sub nil)
      (if (null send-to)
          `(,(make-vm-set-static-here))
          `(,@extra-code ,(make-vm-set-static send-to)))))

(defun do-compile-set-moving (sub args)
   (assert (= (length args) 0))
   (multiple-value-bind (send-to extra-code) (get-remote-reg-and-code sub nil)
      (if (null send-to)
         `(,(make-vm-set-moving-here))
         `(,@extra-code ,(make-vm-set-moving send-to)))))

(defun do-compile-set-affinity (sub args)
   (assert (= (length args) 1))
   (with-compilation-on-reg (target target-instrs) (first args)
      (with-old-reg (target)
         (multiple-value-bind (send-to extra-code) (get-remote-reg-and-code sub nil)
            (if (null send-to)
               `(,@target-instrs ,(make-vm-set-affinity-here target))
               `(,@target-instrs ,@extra-code ,(make-vm-set-affinity target send-to)))))))

(defun do-compile-remote-update-subgoal (def sub name args)
   (let* ((target-def (definition-get-update-definition def))
          (count (definition-get-update-count def))
          (regs-needed (- (length (subgoal-args sub)) count)))
       (with-n-regs (length (subgoal-args sub)) (regs)
         (let ((arg-code (loop for arg in (subgoal-args sub)
                               for reg in regs
                               append (compile-expr-to arg reg :top-level t))))
         (multiple-value-bind (send-to extra-code) (get-remote-reg-and-code sub nil)
             (assert send-to)
           `(,@arg-code ,@extra-code ,(make-vm-remote-update send-to def target-def regs count)))))))

(defun do-compile-head-subgoal (sub sub-regs)
   (let ((def (lookup-subgoal-definition sub)))
      (with-subgoal sub (:name name :args args)
         (cond
            ((definition-is-update-p def)
             (do-compile-remote-update-subgoal def sub name args))
            ((subgoal-will-reuse-other-p sub)
               (do-compile-reused-subgoal sub name args sub-regs))
            ((subgoal-is-set-priority-p name)
               (do-compile-set-priority sub args))
            ((subgoal-is-add-priority-p name)
               (do-compile-add-priority sub args))
            ((subgoal-is-schedule-next-p name)
               (do-compile-schedule-next sub args))
            ((subgoal-is-set-default-priority-p name)
               (do-compile-set-default-priority sub args))
            ((subgoal-is-set-static-p name)
               (do-compile-set-static sub args))
            ((subgoal-is-set-moving-p name)
               (do-compile-set-moving sub args))
            ((subgoal-is-set-affinity-p name)
               (do-compile-set-affinity sub args))
            ((subgoal-is-stop-program-p name)
               `(,(make-vm-stop-program)))
            ((subgoal-is-remove-priority-p name)
               (do-compile-remove-priority sub args))
            (t
               (do-compile-normal-subgoal sub name args))))))

(defun do-compile-head-subgoals-rec (subgoals sub-regs)
   (cond
    ((null subgoals) nil)
    (t
     (let ((f (first subgoals))
           (r (rest subgoals)))
      (cond
			((subgoal-will-reuse-other-p f)
            (let ((other (subgoal-get-reused f))
                  (saved-vars nil))
               (with-subgoal other (:args args-other)
                  (with-subgoal f (:args args-this)
                   (let ((rest-args-this args-this))
                     (dolist2 (argo args-other) (argt args-this)
                        (setf rest-args-this (cdr rest-args-this))
                        (when (and (not (equal argo argt))
                                    (or (expr-uses-var-p r argo)
                                        (expr-uses-var-p rest-args-this argo)))
                           (assert (var-p argo))
                           (assert (not (reference-type-p (expr-type argo))))
                           (push argo saved-vars))))))
               (with-n-regs (length saved-vars) (regs)
                  (let ((backup-code (loop for var in saved-vars 
                                           for r in regs
                                           append (let ((place (lookup-used-var (var-name var))))
                                             (add-used-var (var-name var) r)
                                             (assert place)
                                             `(,(make-move place r))))))
                  `(,@backup-code ,@(do-compile-head-subgoal f sub-regs)
                    ,@(do-compile-head-subgoals-rec r sub-regs))))))
         (t
            (append (do-compile-head-subgoal f sub-regs)
                     (do-compile-head-subgoals-rec r sub-regs))))))))

(defun do-compile-head-subgoals (subgoals sub-regs)
   (multiple-value-bind (reuse non-reuse) (split-mult-return #'subgoal-will-reuse-other-p subgoals)
      (let ((sorted-subgoals (append non-reuse reuse)))
         (do-compile-head-subgoals-rec sorted-subgoals sub-regs))))

(defconstant +plus-infinity+ 2147483647)

(defun agg-construct-start (op acc spec)
	(case op
      (:custom
         (with-agg-spec spec (:args args)
            (let ((start (second args)))
             (compile-expr-to start acc))))
		(:collect
			`(,(make-move (make-vm-nil) acc)))
		((:count :sum)
       (with-agg-spec spec (:var var)
         (let ((typ (var-type var)))
            (cond
             ((type-int-p typ) `(,(make-move (make-vm-int 0) acc)))
             ((type-float-p typ) `(,(make-move (make-vm-float 0.0) acc)))
             (t (assert nil))))))
		(:min
         (with-agg-spec spec (:var var)
            (let ((typ (var-type var)))
             (cond
              ((type-int-p typ) `(,(make-move (make-vm-int +plus-infinity+) acc)))
              ((type-float-p typ) (assert nil))
              (t (assert nil))))))
		(otherwise (error 'compile-invalid-error :text (tostring "agg-construct-start: op ~a not recognized" op)))))

(defun agg-construct-end (op acc)
	(case op
		(:collect nil)
		(:count nil)
		(:sum nil)
		(:min nil)
      (:custom nil)
		(otherwise (error 'compile-invalid-error :text (tostring "agg-construct-end: op ~a not recognized" op)))))
		
(defun agg-construct-post (op acc var)
	(case op
		(:collect nil)
		(:count nil)
		(:sum nil)
		(:min nil)
      (:custom nil)
		(otherwise (error 'compile-invalid-error :text (tostring "agg-construct-post op ~a not recognized" op)))))

(defun agg-construct-step (op acc var gc spec)
	(case op
      (:custom
         (with-agg-spec spec (:args args)
            (let* ((fun (first args))
                   (extern (lookup-external-definition fun))
                   (dest (lookup-used-var (var-name var)))
                   (poly-typ (find-polymorphic-type extern (list (expr-type var) (expr-type var) (expr-type var)))))
             (cond
              (poly-typ
                  (with-reg (arg-place)
                   (let ((arg-code `(,(make-move (make-vm-type poly-typ) arg-place))))
                      (if (reg-p dest)
                        `(,@arg-code ,(make-vm-call fun acc (list acc dest arg-place) (make-vm-bool gc) (expr-type var)))
                        (with-reg (reg)
                            `(,@arg-code ,(make-move dest reg) ,(make-vm-call fun acc (list acc reg arg-place) (make-vm-bool gc) (expr-type var))))))))
              (t
                   (if (reg-p dest)
                     `(,(make-vm-call fun acc (list acc dest) (make-vm-bool gc) (expr-type var)))
                     (with-reg (reg)
                         `(,(make-move dest reg) ,(make-vm-call fun acc (list acc reg) (make-vm-bool gc) (expr-type var))))))))))
		(:collect
			(let ((dest (lookup-used-var (var-name var))))
				`(,(make-vm-cons dest acc acc (expr-type var) (make-vm-bool gc)))))
		(:sum
       (with-agg-spec spec (:var var)
			(let ((src (lookup-used-var (var-name var)))
               (typ (var-type var)))
            (cond
             ((reg-p src)
               `(,(make-vm-op acc acc (if (type-int-p typ) :int-plus :float-plus) src)))
             (t
               (with-reg (new)
                  `(,(make-move src new) ,(make-vm-op acc acc (if (type-int-p typ) :int-plus :float-plus) new))))))))
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
	
(defun compile-agg-construct (c sub-regs)
	(with-agg-construct c (:specs specs)
		(compile-agg-construct-specs c specs nil nil nil nil sub-regs)))

(defun compile-agg-construct-specs (c specs end post vars-regs gcs sub-regs)
	(cond
		((null specs)
			(let ((inner-code (compile-iterate (agg-construct-body c) (agg-construct-body c) nil nil
									:head-compiler #'(lambda (h d sr s)
																(declare (ignore h d sr s))
                                                (let ((step-code (loop for var-reg in vars-regs
                                                                        for spec in (agg-construct-specs c)
                                                                        for gc in gcs
                                                                        append (agg-construct-step (third var-reg) (second var-reg) (first var-reg) gc spec))))
                                                 `(,@step-code ,@(do-compile-head-code (agg-construct-head0 c) nil nil nil)))))))
				(dolist (var-reg vars-regs)
					(add-used-var (var-name (first var-reg)) (second var-reg)))
				(let ((head-code (do-compile-head-code (agg-construct-head c) sub-regs nil nil)))
					(dolist (var-reg vars-regs)
						(remove-used-var (var-name (first var-reg))))
					`(,(make-vm-reset-linear `(,@inner-code ,@end ,(make-vm-reset-linear-end))) ,@head-code ,@post))))
		(t
			(let ((first-spec (first specs))
					 (rest-specs (rest specs)))
				(with-reg (acc)
					(with-agg-spec first-spec (:var var :op op)
						(let ((spec-end (agg-construct-end op acc))
                        (spec-post (agg-construct-post op acc var)))
							(let ((inner-code (compile-agg-construct-specs c rest-specs
										(append end spec-end)
                              spec-post
										(cons (list var acc op) vars-regs)
                              (cons (not (variable-used-in-head-p var (agg-construct-head c))) gcs)
                              sub-regs)))
								`(,@(agg-construct-start op acc first-spec) ,@inner-code)))))))))

(defun do-compile-head-comprehensions (head def subgoal)
   (let* ((*starting-subgoal* nil)
			 (code (do-comprehensions head (:left left :right right :operation append)
					  (with-compile-context
							(let ((body-comp (compile-iterate left left right nil)))
								(if (get-subgoals left)
									(list (make-vm-reset-linear `(,@body-comp ,(make-vm-reset-linear-end))))
									body-comp))))))
		code))

(defun do-compile-head-aggs (head sub-regs def subgoal)
	(let ((*starting-subgoal* nil)
			(code-agg (do-agg-constructs head (:agg-construct c :operation append)
					(with-compile-context (compile-agg-construct c sub-regs)))))
			code-agg))
		
(defun do-compile-one-exists (vars sub-regs exists-body def subgoal)
	(cond
		((null vars) (do-compile-head-code exists-body sub-regs def subgoal))
		(t
			(let ((var (first vars))
					(other-vars (rest vars)))
				(with-reg (reg-var)
					(add-used-var (var-name var) reg-var)
						`(,(make-vm-new-node reg-var) ,@(do-compile-one-exists other-vars sub-regs exists-body def subgoal)))))))
		
(defun do-compile-head-exists (head sub-regs def subgoal)
	(let ((code (do-exists head (:var-list vars :body body :operation append)
						(with-compile-context
							(do-compile-one-exists vars sub-regs body def subgoal)))))
		code))
		
(defun do-compile-head-conditionals (head sub-regs def subgoal)
	(do-conditionals head (:cmp cmp :term1 term1 :term2 term2 :operation append)
		(let ((head1 (do-compile-head-code term1 sub-regs def subgoal))
				(head2 (do-compile-head-code term2 sub-regs def subgoal)))
			(with-compiled-expr (cmp-place cmp-code) cmp
				`(,@cmp-code ,(make-vm-if-else cmp-place head1 head2))))))

(defun do-compile-head-code (head sub-regs def subgoal)
	"Head is the head expression. Subgoal is the starting subgoal and def its definition."
   (let ((ass (get-assignments head)))
      (compile-assignments-and-head ass head
         #'(lambda ()
            `(,@(do-compile-head-conditionals head sub-regs def subgoal)
            ,@(do-compile-head-comprehensions head def subgoal)
            ,@(do-compile-head-aggs head sub-regs def subgoal)
            ,@(do-compile-head-exists head sub-regs def subgoal)
            ,@(do-compile-head-subgoals (get-subgoals head) sub-regs))))))
		
(defun subgoal-to-be-deleted-p (subgoal def)
   (and (is-linear-p def)
		  (not (subgoal-is-reused-p subgoal))))
		
(defun subgoal-to-change-p (sub head)
	(when (subgoal-will-modify-p sub)
		(do-subgoals head (:subgoal head-sub)
			(let ((reused (subgoal-get-reused head-sub)))
				(when (eq reused sub)
					(return-from subgoal-to-change-p t))))
      (do-exists head (:body body)
         (when (subgoal-to-change-p sub body)
            (return-from subgoal-to-change-p t)))
      (do-agg-constructs head (:head agg-head)
         (when (subgoal-to-change-p sub agg-head)
          (return-from subgoal-to-change-p t)))
		nil))
        
(defun compile-linear-deletes-and-returns (subgoal def sub-regs inside head)
	(let* ((delete-regs (mapcar #'cdr (remove-if-not #L(let ((def (lookup-definition (subgoal-name !1))))
																						(and (subgoal-to-be-deleted-p !1 def)
																							  (not (subgoal-to-change-p !1 head))))
																sub-regs :key #'car)))
          (update-regs (mapcar #'cdr (remove-if-not #L(let ((def (lookup-definition (subgoal-name !1))))
                                                            (subgoal-to-change-p !1 head))
                           sub-regs :key #'car)))
		    (deletes (mapcar #'make-vm-remove delete-regs))
          (updates (mapcar #'make-vm-update update-regs))
          (reg-instrs `(,@deletes ,@updates)))
		(cond
			(*compiling-axioms* reg-instrs)
			((and subgoal def) reg-instrs)
			((and *compiling-rule* (clause-is-persistent-p *compilation-clause*)) reg-instrs)
			((and (null def) (null subgoal)) `(,@reg-instrs ,@(unless (null sub-regs) (list (make-return-derived)))))
			((and (not *compilation-clause*) (and subgoal) (subgoal-to-be-deleted-p subgoal def))
				`(,@reg-instrs ,(make-return-linear)))
			(t	`(,@reg-instrs ,@(if inside `(,(make-return-derived)) nil))))))

(defun do-compile-head (head sub-regs inside head-compiler)
   ;; head-compiler is isually do-compile-head-code
   (let* ((def (if (subgoal-p *starting-subgoal*) (lookup-definition (subgoal-name *starting-subgoal*)) nil))
          (head-code (funcall head-compiler head sub-regs def *starting-subgoal*))
          (linear-code (compile-linear-deletes-and-returns *starting-subgoal* def sub-regs inside head))
          (delete-code (compile-inner-delete *compilation-clause*)))
      `(,@head-code ,@delete-code ,@linear-code)))

(defun variable-used-in-subgoal-argument-p (arg var)
   "Check if variable is going to be used directly for creating the subgoal argument.
   This is for GC purposes for runtime objects (list, array, structs).
   If the variable is used immediatelly in the subgoal or inside a cons, then we do not need
   to garbage collect it."
   (cond
    ((var-p arg) (var-eq-p arg var))
    ((cons-p arg)
     (or (variable-used-in-subgoal-argument-p (cons-head arg) var)
         (variable-used-in-subgoal-argument-p (cons-tail arg) var)))
    ((struct-p arg)
     (dolist (a (struct-list arg))
       (when (variable-used-in-subgoal-argument-p a var)
        (return-from variable-used-in-subgoal-argument-p t)))
     nil)))

(defun variable-used-in-head-p (var head)
   "For all subgoals in the head, check if the variable is used directly to create a subgoal argument.
   This is for GC purposes for runtime objects (list, array, structs)."
   (do-subgoals head (:args args)
      (dolist (arg args)
         (when (variable-used-in-subgoal-argument-p arg var)
            (return-from variable-used-in-head-p t))))
   (do-exists head (:var-list vars :body body)
      (dolist (v vars)
         (when (var-eq-p v var)
            (return-from variable-used-in-head-p nil)))
      (when (variable-used-in-head-p var body)
       (return-from variable-used-in-head-p t)))
   (do-comprehensions head (:right right :variables vars)
      (dolist (v vars)
         (when (var-eq-p v var)
            (return-from variable-used-in-head-p nil)))
      (when (variable-used-in-head-p var right)
       (return-from variable-used-in-head-p t)))
	(do-conditionals head (:term1 term1 :term2 term2)
      (when (and (variable-used-in-head-p var term1)
                (variable-used-in-head-p var term2))
       (return-from variable-used-in-head-p t)))
   nil)
      
(defun compile-assignments-and-head (assignments head head-fun)
   (if (null assignments)
       (funcall head-fun)
       (let ((ass (find-if (valid-assignment-p (all-used-var-names)) assignments)))
         (with-reg (new-reg)
            (with-assignment ass (:var var :expr expr)
               (with-compiled-expr (place instrs
                                    :force-dest new-reg
                                    :top-level (variable-used-in-head-p var head)) expr
            	(add-used-var (var-name var) place)
            	(let ((other-code (compile-assignments-and-head (remove-tree ass assignments) head head-fun)))
               	`(,@instrs ,@other-code))))))))

(defun remove-defined-assignments (assignments)
   "Removes all defined variables from the context."
   (mapcar #L(remove-used-var (var-name (assignment-var !1))) assignments))

(defun compile-head (body head sub-regs inside head-compiler)
    (let ((assigns (get-assignments body)))
      (cond
         ((and (not (null head)) (clause-head-is-recursive-p head))
            (let ((code (compile-assignments-and-head assigns head #L(loop for clause in head
                                                                     append (with-compile-context (compile-iterate (clause-body clause) (clause-body clause)
                                                                                          (clause-head clause) sub-regs :inside inside))))))
               (remove-defined-assignments assigns)
               code))
         (t
            (let ((head-code (compile-assignments-and-head assigns head #L(do-compile-head head sub-regs inside head-compiler))))
               (remove-defined-assignments assigns)
               (if *starting-subgoal*
                  `(,(make-vm-rule-done) ,@head-code)
            	head-code))))))

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
         ((and (subgoal-is-thread-p sub) (is-linear-p def))
          (make-thread-linear-iterate name tuple-reg match-constraints iterate-code))
         ((and (subgoal-is-thread-p sub) (not (is-linear-p def)))
          (make-thread-persistent-iterate name tuple-reg match-constraints iterate-code))
         ((and (subgoal-is-thread-p sub))
          (assert nil (sub) "Subgoal ~a cannot be iterated over." sub))
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

(defun compile-constraint (constraint rest-compiler)
   "Compiles constraint and continues the rule compilation with rest-compiler."
   (let ((c-expr (constraint-expr constraint))
         r instrs)
      (with-compiled-expr (reg expr-code) c-expr
         (setf r reg
               instrs expr-code))
      `(,@instrs ,(make-vm-if r (funcall rest-compiler)))))
               
(defun do-compile-constraints-and-assignments (constraints assignments body head rest-compiler tmp-assignments)
   (cond
    ((null constraints)
     (assert (not assignments))
      (remove-defined-assignments (mapcar #'car tmp-assignments))
      (let ((unneeded-regs (mapcar #'cdr tmp-assignments)))
         (loop for reg in unneeded-regs
               do (setf *used-regs* (free-reg *used-regs* reg))))
     (funcall rest-compiler))
    (t
      (let* ((all-vars (all-used-var-names))
            (new-constraint (select-best-constraint constraints all-vars)))
         (if (null new-constraint)
            (let* ((ass (find-if (valid-assignment-p all-vars) assignments)))
               (with-assignment ass (:var var :expr expr)
                  (with-compiled-expr (place instrs) expr
                   (let* ((ass-used-after-p (expr-uses-var-p (append body head) var))
                          (new-tmp-ass (if ass-used-after-p
                                           tmp-assignments
                                           (cons (cons ass place) tmp-assignments))))
                        (add-used-var (var-name var) place)
                        `(,@instrs ,@(do-compile-constraints-and-assignments
                              constraints (remove-tree ass assignments) body head rest-compiler new-tmp-ass))))))
            (compile-constraint new-constraint
               #'(lambda ()
               (do-compile-constraints-and-assignments (remove-tree new-constraint constraints)
                        assignments body head rest-compiler tmp-assignments))))))))

(defun compile-constraints-and-assignments (constraints assignments body head rest-compiler)
   (do-compile-constraints-and-assignments constraints assignments body head rest-compiler nil))

(defun compile-iterate (body orig-body head sub-regs &key (inside nil) (head-compiler #'do-compile-head-code))
   (multiple-value-bind (constraints assignments) (get-compile-constraints-and-assignments body)
		(let ((body0 (remove-all body (append constraints assignments))))
         (compile-constraints-and-assignments constraints assignments body0 head
            #'(lambda ()
               (let* ((next-sub (select-next-subgoal-for-compilation body0))
                      (body1 (remove-tree-first next-sub body0)))
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
                              `(,(create-iterate-instruction next-sub def match-constraints reg iterate-code)))))))))))))
     
(defun select-best-constraint (constraints all-vars)
   (let ((all (filter (valid-constraint-p all-vars) constraints)))
      (if (null all)
         nil
         (first (sort all #'> :key #'constraint-priority)))))
   
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
         (let* ((remaining (remove-all body (append first-constraints first-assignments))))
				(compile-constraints-and-assignments first-constraints first-assignments remaining head
               #'(lambda () (compile-initial-subgoal remaining body head subgoal)))))))
				
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

(defun compile-node-axioms (axioms)
   (let* ((regular nil)
          (spec (loop for axiom in axioms
                 append (with-subgoal axiom (:name name :args args)
                   (cond
                    ((subgoal-is-set-priority-p name)
                     (with-compilation-on-reg (priority priority-instrs) (first args)
                      `(,@priority-instrs ,(make-vm-set-priority-here priority))))
                    ((subgoal-is-add-priority-p name)
                     (with-compilation-on-reg (priority priority-instrs) (first args)
                      `(,@priority-instrs ,(make-vm-add-priority-here priority))))
                    ((subgoal-is-remove-priority-p name)
                    	`(,(make-vm-remove-priority-here)))
                    ((subgoal-is-stop-program-p name)
                     `(,(make-vm-stop-program)))
                    ((subgoal-is-set-default-priority-p name)
                     (with-compilation-on-reg (priority priority-instrs) (first args)
                      `(,@priority-instrs ,(make-vm-set-default-priority-here priority))))
                    ((subgoal-is-set-static-p name)
                     `(,(make-vm-set-static-here)))
                    ((subgoal-is-set-moving-p name)
                     `(,(make-vm-set-moving-here)))
                    ((subgoal-is-set-affinity-p name)
                     (with-compilation-on-reg (target target-instrs) (first args)
                      `(,@target-instrs ,(make-vm-set-affinity-here target))))
                    ((subgoal-is-set-cpu-p name)
                     (with-compilation-on-reg (cpu cpu-instrs) (first args)
                      `(,@cpu-instrs ,(make-vm-set-cpu-here cpu))))
                    ((subgoal-is-schedule-next-p name)
                     (error 'compile-invalid-error :text (tostring "Cannot compile the axiom ~a." axiom)))
                    (t
                        (cond
                         ((subgoal-is-const-p axiom)
                           (push axiom regular)
                           nil)
                         (t
                           (do-compile-head-subgoal axiom nil)))))))))
      (if regular
         `(,@spec ,(make-vm-new-axioms regular))
         spec)))
      
(defun compile-const-axioms ()
	"Take all constant axioms in the program and map them to an hash table (per node).
	Then, create a SELECT NODE instruction and add NEW-AXIOM with each set of node axioms."
	(let ((hash (make-hash-table)))
		(do-node-const-axioms (:subgoal sub)
			(with-subgoal sub (:args args)
				(let* ((fst (first args))
					    (node (addr-num fst)))
					(setf (subgoal-args sub) (rest args)) ; remove home argument
					(push sub (gethash node hash)))))
		(let ((vm (make-vm-select-node)))
			(loop for key being the hash-keys of hash
					using (hash-value value)
               do (vm-select-node-push vm key (compile-node-axioms value)))
			(list vm))))
	
(defun compile-init-process ()
	(let ((const-axiom-code (compile-const-axioms))
			(*compiling-axioms* t))
   	(append const-axiom-code
			(do-node-var-axioms (:body body :head head :clause clause :operation :append)
      		(compile-with-starting-subgoal body head)))))

(defun compile-init-thread-process ()
   (do-thread-var-axioms (:body body :head head :clause clause :operation :append)
      (compile-with-starting-subgoal body head)))

(defun compile-processes ()
	(do-definitions (:definition def :name name :operation append)
		(if (is-init-p def)
         nil
         (let ((clauses (filter #'clause-is-persistent-p (find-clauses-with-subgoal-in-body name))))
            (if clauses
             (list (make-process name `(,@(compile-normal-process name clauses) ,(make-return))))
             nil)))))

(defun compile-consts ()
	(do-constant-list *consts* (:name name :expr expr :operation append)
		(with-compiled-expr (place code) expr
			`(,@code ,(make-move place (make-vm-constant name) (expr-type expr))))))
			
(defun compile-function-arguments (body args n)
	(if (null args)
		(multiple-value-bind (dest body) (compile-expr body :dest (make-vm-stack 32))
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
   (let (init-rules)
      (when (ast-has-thread-facts-p *ast*)
         (push (make-rule-code (with-empty-compile-context
                                  (with-reg (reg)
                                    `(,(make-vm-rule 1)
                                       ,(make-thread-linear-iterate "_init_thread"
                                          reg
                                          nil
                                          `(,(make-vm-rule-done)
                                             ,(make-vm-remove reg)
                                             ,@(compile-init-thread-process)
                                             ,(make-return-derived)))
                                       ,(make-return)))) (list (lookup-def-id "_init_thread")) nil "_init_thread -o thread-axioms.")
               init-rules))
      (push (make-rule-code (with-empty-compile-context
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
												,(make-return)))) (list (lookup-def-id "_init")) nil "_init -o node-axioms.")
               init-rules)
      `(,@init-rules
		  ,@(do-rules (:clause clause :id id :operation append :body body :head head)
                       (unless (clause-is-persistent-p clause)
                           (let ((*compilation-clause* clause)
                                 (*compiling-rule* t))
                              (list (make-rule-code (with-empty-compile-context
                                       `(,(make-vm-rule (+ id (length init-rules)))
                                          ,@(compile-iterate body body head nil) ,(make-return)))
                                       (rule-subgoal-ids clause)
                                       clause))))))))
		
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
															(replace-variable other-clause other-arg original-arg)))))
									;; remove already processed subgoals
									(setf subgoals-others (mapcar #L(remove !1 !2) founds subgoals-others))
									(setf (clause-body original-clause) (remove subgoal (clause-body original-clause) :test #'equal))
									(loop for other-subgoal in founds
											for other-clause in others
											do (setf (clause-body other-clause)
                                             (remove other-subgoal (clause-body other-clause) :test #'equal))))))
				(let* ((common-ass (find-common-assignments original-clause others))
					 	 (common-cons (find-common-constraints original-clause others))
					 	 (rest-original (clause-body original-clause))
					 	 (original-head (clause-head original-clause)))
					(setf (clause-body original-clause) `(,@common-subgoals ,@common-ass ,@common-cons))
					(setf (clause-head original-clause) `(,(make-clause rest-original original-head) ,@others))))
			(t
				(setf (clause-body original-clause) nil)
				(setf (clause-head original-clause) `(,(make-clause body-original (clause-head original-clause)) ,@others))))))
				
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
										(let ((identical-clauses (filter-first #L(identical-clause-p
                                                                        (get-subgoals (clause-body !1)) subgoals-clause) other-clauses)))
											(cond
												((null identical-clauses)
													(push-end first-clause processed-clauses)
													(setf all-clauses (rest all-clauses)))
												(t
													(merge-clause first-clause identical-clauses)
													(push-end first-clause processed-clauses)
													(setf all-clauses (drop-first-n all-clauses
                                                          (1+ (length identical-clauses)))))))))))))
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
                        (not (subgoal-has-delay-p sub2))
								(not (subgoal-is-remote-p sub2)))
					(unless (subgoal-will-reuse-other-p sub2)
						(subgoal-will-modify sub sub2)
						(subgoal-will-reuse-other sub2 sub)
						(return-from find-reusable-fact-in-head t))))
         (do-agg-constructs head (:head agg-head)
            (when (find-reusable-fact-in-head agg-head sub)
               (return-from find-reusable-fact-in-head t)))
			nil)))
			
(defun find-reusable-facts-body (body head)
	(do-subgoals body (:subgoal sub :name name)
		(let ((def (lookup-definition name)))
			(when (and (is-linear-p def)
                    (not (definition-is-thread-p def)))
				(unless (subgoal-is-reused-p sub)
					(find-reusable-fact-in-head head sub))))))
	
(defun find-reusable-facts ()
	(do-rules (:clause clause :body body :head head)
		(unless (clause-is-persistent-p clause)
			(find-reusable-facts-body body head)
         ;; If there are recursive head clauses, look for reusable facts there.
			(when (clause-head-is-recursive-p head)
				(do-clauses head (:body sub-body :head sub-head)
					(find-reusable-facts-body sub-body sub-head))))))

(defun find-same-subgoal (ls sub constraints)
	(when (subgoal-is-remote-p sub)
		(return-from find-same-subgoal nil))
	(with-subgoal sub (:name name :args args)
		(do-subgoals ls (:name other :args args-other :subgoal sub-other)
			(when (and (string-equal name other)
							(not (subgoal-is-remote-p sub-other)))
				(when (test-same-arguments-p args args-other constraints)
					(return-from find-same-subgoal sub-other)))))
	nil)

(defun find-persistent-facts-comprehension (comp)
   "Find reusable subgoals de-derived in the comprehension head."
   (with-comprehension comp (:left body :right head)
      (let ((tmp-head (get-subgoals head))
            (constraints (get-constraints body))
            mark-subgoals to-remove)
         (do-subgoals body (:name name :subgoal sub)
          (let ((def (lookup-definition name)))
           (when (is-linear-p def)
               (cond
                ((subgoal-is-reused-p sub))
                (t
                 (let ((found (find-same-subgoal tmp-head sub constraints)))
                  (when found
                    (push sub mark-subgoals)
                    (push found to-remove)
                    (setf tmp-head (remove found tmp-head)))))))))
         (dolist (sub to-remove)
            (setf (comprehension-right comp) (delete sub (comprehension-right comp))))
         (dolist (sub mark-subgoals)
          (subgoal-set-reused sub)))))

(defun find-persistent-facts-agg-construct (agg)
   "Find reusable subgoals re-derived in the aggregate head0."
   (with-agg-construct agg (:body body :head0 head0)
      (let ((tmp-head (get-subgoals head0))
            (constraints (get-constraints body))
            mark-subgoals to-remove)
       (do-subgoals body (:name name :subgoal sub)
        (let ((def (lookup-definition name)))
         (when (is-linear-p def)
          (cond
           ((subgoal-is-reused-p sub))
           (t
            (let ((found(find-same-subgoal tmp-head sub constraints)))
             (when found
              (push sub mark-subgoals)
              (push found to-remove)
              (setf tmp-head (remove found tmp-head)))))))))
       (dolist (sub to-remove)
        (setf (agg-construct-head0 agg) (delete sub (agg-construct-head0 agg))))
       (dolist (sub mark-subgoals)
        (subgoal-set-reused sub)))))
	
(defun find-persistent-rule (clause)
	"Returns T if we just use persistent facts in the rule and if any linear
	facts are used, then are re-derived in the head of the rule."
	(with-clause clause (:body body :head head)
		(let ((tmp-head (get-subgoals head))
				(constraints (get-constraints body))
				(mark-subgoals nil)
				(to-remove nil)
				(linear-fail nil))
         (do-comprehensions head (:comprehension c)
            (find-persistent-facts-comprehension c))
         (do-agg-constructs head (:agg-construct agg)
            (find-persistent-facts-agg-construct agg))
			(do-subgoals body (:name name :subgoal sub)
				(let ((def (lookup-definition name)))
					(when (is-linear-p def)
						(cond
							((subgoal-is-reused-p sub) )
							(t
								(let ((found (find-same-subgoal tmp-head sub constraints)))
									(cond
										(found
											(push sub mark-subgoals)
											(push found to-remove)
											(setf tmp-head (remove found tmp-head)))
										(t (setf linear-fail t)))))))))
			(dolist (sub to-remove)
				(setf (clause-head clause) (delete sub (clause-head clause))))
			(dolist (sub mark-subgoals)
				(subgoal-set-reused sub))
			(unless linear-fail
				(do-subgoals body (:name name :subgoal sub)
					(when (subgoal-is-reused-p sub)
						(let ((def (lookup-definition name)))
							(unless (is-reused-p def)
								(definition-set-reused def)))))))))

(defun rule-is-persistent-p (clause)
	"Returns T if we just use persistent facts in the rule and if any linear
	facts are used, then are re-derived in the head of the rule."
	(with-clause clause (:body body :head head)
		(when (or (get-comprehensions head)
					 (get-agg-constructs head))
			(return-from rule-is-persistent-p nil))
		(let ((tmp-head (get-subgoals head)))
			(do-subgoals body (:name name :subgoal sub)
				(let ((def (lookup-definition name)))
					(when (and (is-linear-p def)
									(not (subgoal-is-reused-p sub)))
						(return-from rule-is-persistent-p nil)))))
		t))
		
(defun find-persistent-rules ()
	(do-rules (:clause clause :head head)
		(unless (clause-head-is-recursive-p head)
			(find-persistent-rule clause)))
	(do-rules (:clause clause :head head)
		(unless (clause-head-is-recursive-p head)
			(when (rule-is-persistent-p clause)
				(clause-set-persistent clause)))))

(defun subgoal-used-in-clauses-before-p (name first-clause)
   (do-rules (:clause clause :body body)
      (when (eq clause first-clause)
         (return-from subgoal-used-in-clauses-before-p nil))
      (do-subgoals body (:name sub-name)
         (when (string-equal sub-name name)
            (return-from subgoal-used-in-clauses-before-p t))))
   nil)

(defun check-remote-update-subgoal (name def)
  (let (first-clause)
    (do-rules (:clause clause :body body)
      (when (subgoal-appears-code-p body name)
         (when first-clause
            (return-from check-remote-update-subgoal nil))
         (setf first-clause clause)))
    ;; check if clause matches the requirements
    (unless first-clause
     (return-from check-remote-update-subgoal nil))
    (with-clause first-clause (:body body :head head)
       (when (and (= (length head) 1)
                  (= (length (get-subgoals body)) 2)
                  (= (length (get-subgoals head)) 1)
                  (every #L(not (subgoal-is-remote-p !1)) head))
         (let ((other-sub-body (find-if #L(not (string-equal (subgoal-name !1) name)) (get-subgoals body)))
               (edit-subgoal (find-if #L(string-equal (subgoal-name !1) name) (get-subgoals body)))
               (other-sub-head (first head)))
             (unless (and (string-equal (subgoal-name other-sub-body) (subgoal-name other-sub-head))
                          (= (length (subgoal-args edit-subgoal)) (length (subgoal-args other-sub-body))))
               (return-from check-remote-update-subgoal nil))
             (when (subgoal-used-in-clauses-before-p (subgoal-name other-sub-body) first-clause)
               (return-from check-remote-update-subgoal nil))
             (when (get-assignments body)
               (return-from check-remote-update-subgoal nil))
             (let ((constrs (get-constraints body))
                   (count 0)
                   found-diff
                   (valid t))
                (loop for arg-edit in (subgoal-args edit-subgoal)
                      for arg-main in (subgoal-args other-sub-body)
                      for arg-result in (subgoal-args other-sub-head)
                      do (cond
                           ((and (equal arg-edit arg-main) (equal arg-edit arg-result))
                              (when found-diff (setf valid nil))
                              (incf count))
                           (t
                            (multiple-value-bind (constr var) (find-first-assignment-constraint-to-var body arg-edit)
                             (cond
                              ((and constr (var-eq-p var arg-main) (or (var-eq-p var arg-result) (var-eq-p arg-edit arg-result)))
                               (setf constrs (remove constr constrs :test #'equal))
                               (when found-diff (setf valid nil))
                               (incf count))
                              ((var-eq-p arg-edit arg-result)
                               (setf found-diff t))
                              (t (setf valid nil)))))))
                (when (and valid (null constrs))
                   (definition-set-update def (lookup-subgoal-definition other-sub-head) count))))))))

(defun find-remote-updates ()
   (do-definitions (:name name :types types :definition def)
    (when (and (> (length types) 0) (is-linear-p def))
      (check-remote-update-subgoal name def))))

(defun compile-ast ()
	(merge-clauses)
	(find-persistent-rules)
	(number-clauses)
	(find-reusable-facts)
   (find-remote-updates)
	(let ((procs (compile-processes))
			(consts (compile-consts))
			(functions (compile-functions)))
		(make-instance 'code :processes procs :consts `(,@consts (:return-derived))
                           :functions functions)))

