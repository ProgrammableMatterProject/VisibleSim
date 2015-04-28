(in-package :cl-meld)

(define-condition compile-invalid-error (error)
   ((text :initarg :text :reader text)))

(defun alloc-reg (regs data)
   (let ((reg (make-reg (length regs))))
      (values reg (cons data regs))))

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

(defun alloc-new-reg (data) (alloc-reg *used-regs* data))
(defmacro with-reg ((reg &optional data) &body body)
   `(multiple-value-bind (,reg *used-regs*) (alloc-new-reg ,data)
      ,@body))
      
(defun lookup-used-var (var-name)
   (multiple-value-bind (data found) (gethash var-name *vars-places*)
      (when found data)))
(defun add-used-var (var-name data) (setf (gethash var-name *vars-places*) data))
(defun remove-used-var (var-name) (remhash var-name *vars-places*))
(defun all-used-var-names () (hash-table-keys *vars-places*))

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

(defmacro return-expr (place &optional code) `(values ,place ,code *used-regs*))

(defmacro with-compiled-expr ((place code &key (force-dest nil)) expr &body body)
   `(multiple-value-bind (,place ,code *used-regs*) (compile-expr ,expr ,force-dest)
		,@body))
   
(defmacro with-dest-or-new-reg ((dest) &body body)
   `(if (null ,dest)
      (with-reg (,dest) ,@body)
      (progn ,@body)))

(defun build-special-move (from to)
   (cond
      ((vm-nil-p from) (make-move-nil to))
      (t (make-move from to))))

(defmacro compile-expr-to (expr place)
   (with-gensyms (new-place code)
      `(multiple-value-bind (,new-place ,code *used-regs*) (compile-expr ,expr ,place)
         (if (not (equal ,new-place ,place))
            (append ,code (list (build-special-move ,new-place ,place)))
            ,code))))

(defun decide-external-function (name new-reg regs)
	"Decides if external function is pre-defined or not."
	(if (lookup-standard-external-function name)
		(make-vm-call name new-reg regs)
		(make-vm-calle name new-reg regs)))

(defun compile-call (name args regs code)
   (if (null args)
      (with-reg (new-reg)
         (let ((new-code `(,@code ,(decide-external-function name new-reg regs))))
            (return-expr new-reg new-code)))
      (with-compiled-expr (arg-place arg-code) (first args)
         (multiple-value-bind (place code *used-regs*)
            (compile-call name (rest args) `(,@regs ,arg-place) `(,@code ,@arg-code))
            (return-expr place code)))))

(defun compile-callf-args (args args-code n)
	(if (null args)
		args-code
		(with-compiled-expr (arg-place arg-code :force-dest (make-reg n)) (first args)
			(if (not (equal arg-place (make-reg n)))
				(setf arg-code `(,@arg-code ,(make-move arg-place (make-reg n)))))
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
         (compile-call (call-name expr) (call-args expr) nil nil))
		((struct-val-p expr)
			(with-dest-or-new-reg (dest)
				(let ((look (lookup-used-var (var-name (struct-val-var expr)))))
					(assert look)
					(return-expr dest `(,(make-vm-struct-val (struct-val-idx expr) look dest))))))
		((struct-p expr)
			(with-dest-or-new-reg (dest)
				(let ((ls (struct-list expr))
						instrs)
					(return-expr dest
						`(,@(loop for x in ls
								append `(,(make-vm-push) ,@(compile-expr-to x (make-vm-stack 0))))
							,(make-vm-make-struct (expr-type expr) dest))))))
		((callf-p expr)
			(with-dest-or-new-reg (dest)
				(return-expr dest `(,(make-vm-push) ;; for pcounter
											,(make-vm-push) ;; for return value
											,(make-vm-push-registers)
											,@(compile-callf-args (callf-args expr) (list) 0)
											,(make-move (make-vm-pcounter) (make-vm-stack (1+ *num-regs*)))
											,(make-vm-callf (callf-name expr))
											,(make-vm-pop-registers)
											,(make-move (make-vm-stack 0) dest)
											,(make-vm-pop)
											,(make-vm-pop)))))
      ((convert-float-p expr)
         (with-compiled-expr (place code) (convert-float-expr expr)
            (with-dest-or-new-reg (dest)
               (return-expr dest `(,@code ,(make-vm-convert-float place dest))))))
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
                  (return-expr dest `(,@code-cmp ,(make-vm-if place-cmp code1)
                        ,(make-vm-not place-cmp place-cmp) ,(make-vm-if place-cmp code2)))))))
      ((tail-p expr) (with-compiled-expr (place code) (tail-list expr)
                        (with-dest-or-new-reg (dest)
                           (return-expr dest `(,@code ,(make-vm-tail place dest (expr-type expr)))))))
      ((head-p expr) (with-compiled-expr (place code) (head-list expr)
                        (with-dest-or-new-reg (dest)
                           (let ((typ (expr-type (vm-head-cons expr))))
                              (return-expr dest `(,@code ,(make-vm-head place dest typ)))))))
      ((cons-p expr) (with-compiled-expr (place-head code-head) (cons-head expr)
                        (with-compiled-expr (place-tail code-tail) (cons-tail expr)
                           (with-dest-or-new-reg (dest)
                              (let ((cons (make-vm-cons place-head place-tail dest (expr-type expr))))
                                 (return-expr dest `(,@code-head ,@code-tail ,cons)))))))
      ((not-p expr) (with-compiled-expr (place-expr code-expr) (not-expr expr)
                        (with-dest-or-new-reg (dest)
                           (return-expr dest `(,@code-expr ,(make-vm-not place-expr dest))))))
      ((test-nil-p expr) (with-compiled-expr (place-expr code-expr) (test-nil-expr expr)
                           (with-dest-or-new-reg (dest)
                              (return-expr dest `(,@code-expr ,(make-vm-test-nil place-expr dest))))))
      ((nil-p expr) (return-expr (make-vm-nil)))
      ((world-p expr) (return-expr (make-vm-world)))
      ((colocated-p expr)
         (with-compiled-expr (first-place first-code) (colocated-first expr)
            (with-compiled-expr (second-place second-code) (colocated-second expr)
               (with-dest-or-new-reg (dest)
                  (return-expr dest `(,@first-code ,@second-code ,(make-vm-colocated first-place second-place dest)))))))
      ((op-p expr)
         (with-compiled-expr (place1 code1) (op-op1 expr)
            (with-compiled-expr (place2 code2) (op-op2 expr)
               (with-dest-or-new-reg (dest)
                  (return-expr dest (generate-op-instr expr dest place1 place2 code1 code2))))))
      (t (error 'compile-invalid-error :text (tostring "Unknown expression to compile: ~a" expr)))))
      
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
   (lookup-used-var (subgoal-get-remote-dest subgoal)))
   
(defun get-remote-reg-and-code (subgoal default)
   (if (subgoal-is-remote-p subgoal)
      (let ((var (get-remote-dest subgoal)))
         (if (reg-p var)
            var
            (with-reg (new-reg)
               (values new-reg `(,(make-move var new-reg)))))) 
         default))

(defun add-subgoal (subgoal reg &optional (in-c reg))
   (with-subgoal subgoal (:args args)
      (filter #L(not (null !1))
         (loop for arg in args
               for i upto (length args)
               collect (let ((already-defined (lookup-used-var (var-name arg))))
                        (cond
                           (already-defined (make-low-constraint (expr-type arg) (make-reg-dot in-c i) already-defined))
                           (t (add-used-var (var-name arg) (make-reg-dot reg i)) nil)))))))

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
            (let ((greater-instr (make-vm-op reg place :int-greater-equal (make-vm-int 0))))
               (multiple-value-bind (remain-instrs places) (compile-remain-delete-args 2 remain)
                  ;(format t "remain-instrs ~a places ~a~%" remain-instrs places)
                  (let* ((delete-code (make-vm-delete (subgoal-name subgoal) `(,(cons 1 place) ,@places)))
                         (if-instr (make-vm-if reg `(,delete-code))))
                     `(,@instrs ,@remain-instrs ,greater-instr ,if-instr))))))))
            
(defun compile-inner-delete (clause)
   (when (clause-has-delete-p clause)
      (let ((all (clause-get-all-deletes clause)))
         (loop for delete-option in all
               append (compile-delete delete-option
                        (find-if (subgoal-by-name (delete-option-name delete-option)) (get-subgoals (clause-body clause))))))))
            
(defun compile-head-move (arg i tuple-reg)
   (let ((reg-dot (make-reg-dot tuple-reg i)))
      (compile-expr-to arg reg-dot)))

(defun do-compile-head-subgoals (head clause)
   (do-subgoals head (:name name :args args :operation append :subgoal sub)
      (with-reg (tuple-reg)
         (let ((res `(,(make-vm-alloc name tuple-reg)
            ,@(loop for arg in args
                  for i upto (length args)
                  append (compile-head-move arg i tuple-reg))
            ,@(multiple-value-bind (send-to extra-code) (get-remote-reg-and-code sub tuple-reg)
               `(,@extra-code ,(if (subgoal-has-delay-p sub)
												(make-vm-send-delay tuple-reg send-to (subgoal-delay-value sub))
												(make-send tuple-reg send-to)))))))
            res))))

(defconstant +plus-infinity+ 2147483647)

(defun agg-construct-start (op acc)
	(case op
		(:collect
			`(,(make-move-nil acc)))
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
			`(,(make-vm-op acc acc :float-plus src))))
		(:count
			`(,(make-vm-op acc acc :int-plus (make-vm-int 1))))
		(:min
			(let ((src (lookup-used-var (var-name var))))
				(with-reg (new)
					`(,(make-vm-op new src :int-lesser acc) ,(make-vm-if new `(,(make-move src acc))))))) 
		(otherwise (error 'compile-invalid-error
								:text (tostring "agg-construct-step: op ~a not recognized" op)))))
	
(defun compile-agg-construct (c)
	(with-agg-construct c (:specs specs)
		(compile-agg-construct-specs c specs nil nil)))

(defun compile-agg-construct-specs (c specs end vars-regs)
	(cond
		((null specs)
			(let ((inner-code (compile-iterate (agg-construct-body c) (agg-construct-body c) nil nil nil nil
									:head-compiler #'(lambda (h c d s)
																(declare (ignore h c d s))
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

(defun do-compile-head-comprehensions (head clause def subgoal)
   (let* ((code (do-comprehensions head (:left left :right right :operation collect)
                  (with-compile-context (make-vm-reset-linear `(,@(compile-iterate left left right nil nil nil) ,(make-vm-reset-linear-end)))))))
		code))

(defun do-compile-head-aggs (head clause def subgoal)
	(let ((code-agg (do-agg-constructs head (:agg-construct c :operation append)
				(with-compile-context (compile-agg-construct c)))))
		code-agg))
		
(defun do-compile-one-exists (vars exists-body clause)
	(cond
		((null vars) (do-compile-head-subgoals exists-body clause))
		(t
			(let ((var (first vars))
					(other-vars (rest vars)))
				(with-reg (reg-var)
					(add-used-var (var-name var) reg-var)
						`(,(make-vm-new-node reg-var) ,@(do-compile-one-exists other-vars exists-body clause)))))))
		
(defun do-compile-head-exists (head clause def subgoal)
	(let ((code (do-exists head (:var-list vars :body body :operation append)
						(with-compile-context
							(do-compile-one-exists vars body clause)))))
		code))

(defun do-compile-head-code (head clause def subgoal)
   (let ((subgoals-code (do-compile-head-subgoals head clause))
         (comprehensions-code (do-compile-head-comprehensions head clause def subgoal))
			(agg-code (do-compile-head-aggs head clause def subgoal))
			(exists-code (do-compile-head-exists head clause def subgoal)))
		`(,@subgoals-code ,@comprehensions-code ,@agg-code ,@exists-code)))
		
(defun subgoal-to-be-deleted-p (subgoal def)
   (and (is-linear-p def) (not (subgoal-has-option-p subgoal :reuse))))
        
(defun compile-linear-deletes-and-returns (subgoal def delete-regs inside)
	(let ((deletes (mapcar #'make-vm-remove delete-regs)))
      (if (subgoal-to-be-deleted-p subgoal def)
         `(,@deletes ,(make-return-linear))
         `(,@deletes ,@(if inside `(,(make-return-derived)) nil)))))

; head-compiler is isually do-compile-head-code
(defun do-compile-head (subgoal head clause delete-regs inside head-compiler)
   (let* ((def (if (subgoal-p subgoal) (lookup-definition (subgoal-name subgoal)) nil))
          (head-code (funcall head-compiler head clause def subgoal))
          (linear-code (compile-linear-deletes-and-returns subgoal def delete-regs inside))
          (delete-code (compile-inner-delete clause)))
      `(,@head-code ,@delete-code ,@linear-code)))
      
(defun compile-assignments-and-head (assignments head-fun)
   (if (null assignments)
       (funcall head-fun)
       (let ((ass (find-if (valid-assignment-p (all-used-var-names)) assignments)))
         (with-compiled-expr (place instrs) (assignment-expr ass)
            (add-used-var (var-name (assignment-var ass)) place)
            (let ((other-code (compile-assignments-and-head (remove-tree ass assignments) head-fun)))
               `(,@instrs ,@other-code))))))

(defun remove-defined-assignments (assignments) (mapcar #L(remove-used-var (var-name (assignment-var !1))) assignments))

(defun compile-head (body head clause subgoal delete-regs inside head-compiler)
   (let* ((assigns (filter #'assignment-p body))
          (head-code (compile-assignments-and-head assigns #L(do-compile-head subgoal head clause delete-regs inside head-compiler))))
         (remove-defined-assignments assigns)
         (if subgoal
            `(,(make-vm-rule-done) ,@head-code)
            head-code)))

(defun select-next-subgoal-for-compilation (body)
	"Selects next subgoal for compilation. We give preference to subgoals with modifiers (random/min/etc)."
	(let ((no-args (find-if #'(lambda (sub) (and (subgoal-p sub) (null (subgoal-args sub)))) body)))
		(if no-args
			no-args
			(let ((not-blocked (find-if #'(lambda (sub) (and (subgoal-p sub) (not (subgoal-is-blocked-p sub)))) body)))
				(if not-blocked
					not-blocked
					(let ((with-mod (find-if #'(lambda (sub) (and (subgoal-p sub) (or (subgoal-has-random-p sub) (subgoal-has-min-p sub)))) body)))
						(if with-mod
							with-mod
							(find-if #'subgoal-p body))))))))
                     
(defun compile-iterate (body orig-body head clause subgoal delete-regs &key (inside nil) (head-compiler #'do-compile-head-code))
   (multiple-value-bind (constraints assignments) (get-compile-constraints-and-assignments body)
      (let* ((next-sub (select-next-subgoal-for-compilation body))
             (rem-body (remove-unneeded-assignments (remove-tree-first next-sub (remove-all body constraints)) head)))
         (compile-constraints-and-assignments constraints assignments
            (if (not next-sub)
					(compile-head rem-body head clause subgoal delete-regs inside head-compiler)
               (let ((next-sub-name (subgoal-name next-sub)))
                  (with-reg (reg next-sub)
                     (let* ((match-constraints (mapcar #'rest (add-subgoal next-sub reg :match)))
                            (def (lookup-definition next-sub-name))
                            (new-delete-regs (if (subgoal-to-be-deleted-p next-sub def)
                                                (cons reg delete-regs) delete-regs))
                            (iterate-code (compile-iterate rem-body orig-body head clause subgoal new-delete-regs
																					:inside t :head-compiler head-compiler))
                            (other-code `(,(make-move :tuple reg) ,@iterate-code)))
                        `(,(make-iterate next-sub-name
											match-constraints other-code
											:random-p (subgoal-has-random-p next-sub)
											:min-p (subgoal-has-min-p next-sub)
											:min-arg (subgoal-get-min-variable-position next-sub)
											:to-delete-p (subgoal-to-be-deleted-p next-sub def)))))))))))
      
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
   (with-reg (reg)
      (reduce #'(lambda (c old)
						(let ((vm-op (set-type-to-op (low-constraint-type c) :type-bool :equal)))
							(assert (not (null vm-op)))
                  	(list (make-vm-op reg (low-constraint-v1 c)
												vm-op
                                    (low-constraint-v2 c))
                           (make-vm-if reg old))))
               constraints :initial-value inner-code :from-end t)))

(defun compile-initial-subgoal-aux (body orig-body head clause subgoal)
	(let ((without-subgoal (remove-tree subgoal body)))
      (if (null (subgoal-args subgoal))
         (compile-iterate without-subgoal orig-body head clause subgoal nil)
         (with-reg (sub-reg subgoal)
            (let ((start-code (make-move :tuple sub-reg))
                  (low-constraints (add-subgoal subgoal sub-reg))
                  (inner-code (compile-iterate without-subgoal orig-body head clause subgoal nil)))
               `(,start-code ,@(compile-low-constraints low-constraints inner-code)))))))

(defun get-first-min-subgoal (body)
	(do-subgoals body (:subgoal sub)
		(when (subgoal-has-min-p sub)
			(return-from get-first-min-subgoal sub))))

(defun compile-initial-subgoal (body orig-body head clause subgoal)
	(compile-initial-subgoal-aux body orig-body head clause subgoal))

(defun get-my-subgoals (body name)
   (filter #'(lambda (sub)
						(string-equal (subgoal-name sub) name))
			(get-subgoals body)))

(defun compile-with-starting-subgoal (body head clause &optional subgoal)
   (with-empty-compile-context
      (multiple-value-bind (first-constraints first-assignments) (get-compile-constraints-and-assignments body)
         (let* ((remaining (remove-unneeded-assignments (remove-all body first-constraints) head))
                (inner-code (compile-initial-subgoal remaining body head clause subgoal)))
				(compile-constraints-and-assignments first-constraints first-assignments inner-code)))))

(defun compile-subgoal-clause (name clause)
   (with-clause clause (:body body :head head)
      (loop-list (subgoal (get-my-subgoals body name) :operation append)
         (compile-with-starting-subgoal body head clause subgoal))))

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
	(let ((const-axiom-code (compile-const-axioms)))
		;; handle other axioms (non-constant)
   	(append const-axiom-code
			(do-axioms (:body body :head head :clause clause :operation :append)
      		(compile-with-starting-subgoal body head clause)))))

(defun compile-processes ()
	(do-definitions (:definition def :name name :operation collect)
      (if (is-init-p def)
         (make-process name `(,(make-return-linear)))
         (make-process name `(,@(compile-normal-process name (filter #'clause-is-persistent-p (find-clauses-with-subgoal-in-body name)))
                                 ,(make-return))))))

(defun compile-consts ()
	(do-constant-list *consts* (:name name :expr expr :operation append)
		(with-compiled-expr (place code) expr
			`(,@code ,(make-move place (make-vm-constant name))))))
			
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
	(with-clause clause (:body body)
		(let ((ids nil))
			(do-subgoals body (:name name)
				(let ((id (lookup-def-id name)))
					(push-dunion id ids)))
			ids)))

(defun compile-ast-rules ()
	(let ((init-rule (make-rule-code (with-empty-compile-context
										(with-reg (reg)
											`(,(make-vm-rule 0)
											  	,(make-iterate "_init"
													nil 
													`(,@(compile-init-process)
														,(make-vm-rule-done)
														,(make-move :tuple reg)
														,(make-vm-remove reg)
														,(make-move (make-vm-ptr 0) (make-reg 0))
														,(make-return-derived))
												:to-delete-p t)
												,(make-return)))) (list (lookup-def-id "_init")) nil))
			(other-rules (do-rules (:clause clause :id id :operation collect)
								(with-clause clause (:body body :head head)
		 							(make-rule-code (with-empty-compile-context
												`(,(make-vm-rule (1+ id)) ,@(compile-iterate body body head clause t nil) ,(make-return)))
											(rule-subgoal-ids clause)
											(clause-is-persistent-p clause))))))
		`(,init-rule ,@other-rules)))
											

(defun compile-ast ()
	(number-clauses)
	(let ((procs (compile-processes))
			(consts (compile-consts))
			(functions (compile-functions)))
		(make-instance 'code :processes procs :consts `(,@consts (:return-derived)) :functions functions)))
