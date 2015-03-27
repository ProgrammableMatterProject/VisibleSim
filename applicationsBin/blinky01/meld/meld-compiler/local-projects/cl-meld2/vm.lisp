(in-package :cl-meld)

(defparameter *num-regs* 32)

(defmacro do-type-conversion (op base-type)
   `(case ,op
      ,@(mapcar #L`(,(format-keyword "~a" !1) ,(format-keyword "~a-~a" base-type !1))
                  '(equal not-equal lesser lesser-equal greater greater-equal or plus mul div mod minus))))

(defun set-type-to-op (typ-args typ-ret op)
   (declare (ignore typ-ret))
	(assert (not (null typ-args)))
	(assert (not (null op)))
   (case typ-args
		(:type-bool (case op
							(:or :bool-or)))
      (:type-addr (case op
                     (:equal :addr-equal)
							(:not-equal :addr-not-equal)
							(:greater :addr-greater)))
      (:type-int (do-type-conversion op int))
      (:type-float (do-type-conversion op float))))
      
(defun op-eq-p (a b) (eq a b))

(defun make-process (name instrs) (list :process name instrs))
(defun process-name (proc) (second proc))
(defun process-instrs (proc) (third proc))
(defun set-process-instrs (proc new-instrs)
   (setf (third proc) new-instrs))
(defsetf process-instrs set-process-instrs)

(defun specialize-move (from to typ)
	(let ((ref-type-p (reference-type-p typ)))
		(cond
			((vm-argument-p from)
				(cond
					((reg-p to)
						`(:move-argument-to-reg ,from ,to))))
			((vm-pcounter-p from)
				(cond
					((vm-stack-p to)
						`(:move-pcounter-to-stack ,from ,to))))
			((vm-stack-p from)
				(cond
					((vm-pcounter-p to)
						`(:move-stack-to-pcounter ,from ,to))
					((reg-dot-p to)
						`(:move-stack-to-field ,from ,to))
					((reg-p to)
						`(:move-stack-to-reg ,from ,to))))
			((vm-world-p from)
				(cond
					((reg-p to)
						`(:move-world-to-reg ,from ,to))
					((vm-constant-p to)
						`(:move-int-to-constant ,(make-vm-int (number-of-nodes *nodes*)) ,to))
					((reg-dot-p to)
						`(:move-world-to-field ,from ,to))))
			((vm-float-p from)
				(cond
					((vm-stack-p to) `(:move-float-to-stack ,from ,to))
					((reg-dot-p to) `(:move-float-to-field ,from ,to))
					((reg-p to) `(:move-float-to-reg ,from ,to))))
			((vm-addr-p from)
				(cond
					((reg-dot-p to) `(:move-addr-to-field ,from ,to))
					((reg-p to) `(:move-addr-to-reg ,from ,to))))
			((vm-constant-p from)
				(cond
					((reg-p to)
						`(:move-constant-to-reg ,from ,to))
					((reg-dot-p to)
						(if ref-type-p
							`(:move-constant-to-field-ref ,from ,to)
							`(:move-constant-to-field ,from ,to)))))
			((vm-host-id-p from)
				(cond
					((reg-dot-p to) `(:move-host-id-to-field ,from ,to))
					((reg-p to) `(:move-host-id-to-reg ,from ,to))))
			((vm-nil-p from)
				(cond
					((reg-p to) `(:move-nil-to-reg ,from ,to))
					((reg-dot-p to) `(:move-nil-to-field ,from ,to))))
			((vm-ptr-p from)
				(cond
					((reg-p to) `(:move-ptr-to-reg ,from ,to))))
			((int-p from)
				(cond
					((reg-p to) `(:move-int-to-reg ,from ,to))
					((reg-dot-p to) `(:move-int-to-field ,from ,to))
					((vm-stack-p to) `(:move-int-to-stack ,from ,to))))
			((reg-p from)
				(cond
					((reg-p to)
						`(:move-reg-to-reg ,from ,to))
					((reg-dot-p to)
						(if ref-type-p
							`(:move-reg-to-field-ref ,from ,to)
							`(:move-reg-to-field ,from ,to)))
					((vm-constant-p to)
						`(:move-reg-to-constant ,from ,to))
					((vm-stack-p to)
						`(:move-reg-to-stack ,from ,to))))
			((reg-dot-p from)
				(cond
					((reg-dot-p to)
						(if ref-type-p
							`(:move-field-to-field-ref ,from ,to)
							`(:move-field-to-field ,from ,to)))
					((reg-p to) `(:move-field-to-reg ,from ,to)))))))

(defun make-move (from to &optional typ)
	(assert (not (null from)))
	(assert (not (null to)))
	"We specialize the move instruction."
	(let ((ret (specialize-move from to typ)))
		(cond
			(ret ret)
			(t (warn "could not specialize move ~a ~a" from to)
				(assert ret)
				`(:move ,from ,to)))))
				
(defun move-to (mv) (third mv))
(defun move-from (mv) (second mv))

(defun make-return () '(:return))
(defun make-return-linear () '(:return-linear))
(defun make-return-select () '(:return-select))
(defun make-return-derived () '(:return-derived))

(defun make-vm-push () `(:push))
(defun make-vm-pop () `(:pop))

(defun make-vm-push-n (n) `(:push-n ,n))
(defun vm-push-n (p) (second p))

(defun instr-is-return-p (instr)
   (case (instr-type instr)
      ((:return :return-linear :return-select :return-derived) t)
      (otherwise nil)))

(defun instr-type (instr) (first instr))

(defun make-reg (n) `(:reg ,n))
(defun reg-p (r) (tagged-p r :reg))
(defun reg-num (r) (second r))
(defun reg-eq-p (a b) (and (reg-p a) (reg-p b) (= (reg-num a) (reg-num b))))

(defun make-vm-stack (off) `(:stack ,off))
(defun vm-stack-p (x) (tagged-p x :stack))
(defun vm-stack-offset (x) (second x))

(defun make-vm-pcounter () `(:pcounter))
(defun vm-pcounter-p (x) (tagged-p x :pcounter))

(defun make-vm-ptr (v) `(:ptr ,v))
(defun vm-ptr-val (x) (second x))
(defun vm-ptr-p (x) (tagged-p x :ptr))

(defun make-reg-dot (reg field) `(:reg-dot ,reg ,field))
(defun reg-dot-reg (reg-dot) (second reg-dot))
(defun reg-dot-field (reg-dot) (third reg-dot))
(defun reg-dot-p (reg-dot) (tagged-p reg-dot :reg-dot))

(defun make-vm-nil () '(:nil))
(defun vm-nil-p (n) (tagged-p n :nil))

(defun make-vm-non-nil () '(:non-nil))
(defun vm-non-nil-p (n) (tagged-p n :non-nil))

(defun make-vm-list (head tail) `(:list ,head ,tail))
(defun vm-list-head (x) (second x))
(defun vm-list-tail (x) (third x))
(defun vm-list-p (x) (tagged-p x :list))

(defun make-vm-any () '(:any))
(defun vm-any-p (n) (tagged-p n :any))

(defun make-vm-world () :world)
(defun vm-world-p (w) (eq w :world))

(defun make-vm-not (place dest)
	(assert (and (reg-p place) (reg-p dest)))
	`(:not ,place ,dest))
(defun vm-not-place (n) (second n))
(defun vm-not-dest (n) (third n))

(defun make-vm-test-nil (place dest) (assert (and (reg-p place) (reg-p dest))) `(:test-nil ,place ,dest))
(defun vm-test-nil-place (tn) (second tn))
(defun vm-test-nil-dest (tn) (third tn))

(defun make-vm-cons (head tail dest typ)
	(assert (or (reg-p head) (reg-dot-p head)))
	(assert (or (reg-p tail) (reg-dot-p tail)))
	(assert (or (reg-p dest) (reg-dot-p dest)))
	(cond
		((and (reg-p head) (reg-p tail) (reg-p dest))
			`(:cons-rrr ,head ,tail ,dest ,typ))
		((and (reg-p head) (reg-dot-p tail) (reg-dot-p dest))
			`(:cons-rff ,head ,tail ,dest ,typ))
		((and (reg-dot-p head) (reg-p tail) (reg-dot-p dest))
			`(:cons-frf ,head ,tail ,dest ,typ))
		((and (reg-dot-p head) (reg-dot-p tail) (reg-p dest))
			`(:cons-ffr ,head ,tail ,dest ,typ))
		((and (reg-p head) (reg-p tail) (reg-dot-p dest))
			`(:cons-rrf ,head ,tail ,dest ,typ))
		((and (reg-p head) (reg-dot-p tail) (reg-p dest))
			`(:cons-rfr ,head ,tail ,dest ,typ))
		((and (reg-dot-p head) (reg-p tail) (reg-p dest))
			`(:cons-frr ,head ,tail ,dest ,typ))
		((and (reg-dot-p head) (reg-dot-p tail) (reg-dot-p dest))
			`(:cons-fff ,head ,tail ,dest ,typ))
		(t (assert nil))))

(defun vm-cons-head (c) (second c))
(defun vm-cons-tail (c) (third c))
(defun vm-cons-dest (c) (fourth c))
(defun vm-cons-type (c) (fifth c))
(defun vm-cons-p (c) (tagged-p c :cons))

(defun make-vm-head (con dest typ)
	(assert (type-list-p typ))
	(let* ((subtype (type-list-element typ))
			 (ref-type-p (reference-type-p typ)))
		(cond
			((and (reg-p con) (reg-p dest))
		 	 `(:head-rr ,con ,dest))
			((and (reg-dot-p con) (reg-p dest))
		 	 `(:head-fr ,con ,dest))
			((and (reg-dot-p con) (reg-dot-p dest))
				(if ref-type-p
					`(:head-ffr ,con ,dest)
		 	 		`(:head-ff ,con ,dest)))
			((and (reg-p dest) (reg-dot-p dest))
				(if ref-type-p
					`(:head-rfr ,con ,dest)
		 	 		`(:head-rf ,con ,dest)))
			(t (assert nil)))))

(defun vm-head-cons (h) (second h))
(defun vm-head-dest (h) (third h))
(defun vm-head-type (h) (fourth h))
(defun vm-head-p (h) (tagged-p h :head))

(defun make-vm-tail (con dest typ)
	(assert (type-list-p typ))
	(cond
		((and (reg-p con) (reg-p dest))
	 	 `(:tail-rr ,con ,dest))
		((and (reg-dot-p con) (reg-p dest))
	 	 `(:tail-fr ,con ,dest))
		((and (reg-dot-p con) (reg-dot-p dest))
	 	 `(:tail-ff ,con ,dest))
		((and (reg-p dest) (reg-dot-p dest))
	 	 `(:tail-rf ,con ,dest))
		(t (assert nil))))

(defun vm-tail-cons (tail) (second tail))
(defun vm-tail-dest (tail) (third tail))
(defun vm-tail-type (tail) (fourth tail))
(defun vm-tail-p (tail) (tagged-p tail :tail))

(defun make-vm-struct-val (idx from to val-type)
	(assert (or (reg-p from) (reg-dot-p from)))
	(assert (or (reg-p to) (reg-dot-p to)))
	(let ((ref-type-p (reference-type-p val-type)))
		(cond
			((reg-p from)
				(cond
					((reg-p to) `(:struct-valrr ,idx ,from ,to))
					((reg-dot-p to)
						(if ref-type-p
							`(:struct-valrf-ref ,idx ,from ,to)
							`(:struct-valrf ,idx ,from ,to)))))
			((reg-dot-p from)
				(cond
					((reg-p to) `(:struct-valfr ,idx ,from ,to))
					((reg-dot-p to)
						(if ref-type-p
							`(:struct-valff-ref ,idx ,from ,to)
							`(:struct-valff ,idx ,from ,to))))))))
(defun vm-struct-val-idx (x) (second x))
(defun vm-struct-val-from (x) (third x))
(defun vm-struct-val-to (x) (fourth x))

(defun make-vm-make-struct (typ to)
	(assert (or (reg-p to) (reg-dot-p to)))
	(cond
		((reg-p to) `(:structr ,typ ,to))
		((reg-dot-p to) `(:structf ,typ ,to))))
(defun vm-make-struct-to (x) (third x))
(defun vm-make-struct-type (x) (second x))

(defun make-vm-if (r instrs) (list :if r instrs))
(defun vm-if-reg (i) (second i))
(defun vm-if-instrs (i) (third i))
(defun vm-if-p (i) (tagged-p i :if))

(defun make-vm-if-else (r instrs1 instrs2) (list :if-else r instrs1 instrs2))
(defun vm-if-else-reg (i) (second i))
(defun vm-if-else-instrs1 (i) (third i))
(defun vm-if-else-instrs2 (i) (fourth i))
(defun vm-if-else-p (i) (tagged-p i :if-else))

(defun specialize-op (dst v1 op v2)
	(case op
		(:addr-not-equal `(:addr-not-equal ,dst ,v1 ,v2 ,dst))
		(:addr-equal `(:addr-equal ,dst ,v1 ,v2 ,dst))
		(:int-minus `(:int-minus ,dst ,v1 ,v2 ,dst))
		(:int-equal `(:int-equal ,dst ,v1 ,v2 ,dst))
		(:int-plus `(:int-plus ,dst ,v1 ,v2 ,dst))
		(:int-lesser `(:int-lesser ,dst ,v1 ,v2 ,dst))
		(:int-greater-equal `(:int-greater-equal ,dst ,v1 ,v2 ,dst))
		(:int-not-equal `(:int-not-equal ,dst ,v1 ,v2 ,dst))
		(:int-lesser-equal `(:int-lesser-equal ,dst ,v1 ,v2 ,dst))
		(:int-greater `(:int-greater ,dst ,v1 ,v2 ,dst))
		(:int-mul `(:int-mul ,dst ,v1 ,v2 ,dst))
		(:int-div `(:int-div ,dst ,v1 ,v2 ,dst))
		(:int-mod `(:int-mod ,dst ,v1 ,v2 ,dst))
		(:float-plus `(:float-plus ,dst ,v1 ,v2 ,dst))
		(:float-minus `(:float-minus ,dst ,v1 ,v2 ,dst))
		(:float-mul `(:float-mul ,dst ,v1 ,v2 ,dst))
		(:float-div `(:float-div ,dst ,v1 ,v2 ,dst))
		(:float-equal `(:float-equal ,dst ,v1 ,v2 ,dst))
		(:float-not-equal `(:float-not-equal ,dst ,v1 ,v2 ,dst))
		(:float-lesser `(:float-lesser ,dst ,v1 ,v2 ,dst))
		(:float-lesser-equal `(:float-lesser-equal ,dst ,v1 ,v2 ,dst))
		(:float-greater `(:float-greater ,dst ,v1 ,v2 ,dst))
		(:float-greater-equal `(:float-greater-equal ,dst ,v1 ,v2 ,dst))
		(:bool-equal `(:bool-equal ,dst ,v1 ,v2 ,dst))
		(:bool-not-equal `(:bool-not-equal ,dst ,v1 ,v2 ,dst))
		(:bool-or `(:bool-or ,dst ,v1 ,v2 ,dst))))

(defun make-vm-op (dst v1 op v2)
	(assert (and (reg-p dst) (reg-p v1) (reg-p v2)))
	(let ((sp (specialize-op dst v1 op v2)))
		(cond
			(sp sp)
			(t
				(warn "NOT SPECIALIZING op ~a" op) 
				`(:op ,dst ,v1 ,v2 ,op)))))
(defun vm-op-dest (st) (second st))
(defun vm-op-v1 (st) (third st))
(defun vm-op-v2 (st) (fourth st))
(defun vm-op-op (st) (fifth st))
(defun vm-op-p (st) (tagged-p st :op))

(defun iterate-name (i) (second i))
(defun iterate-reg (i) (third i))
(defun iterate-matches (i) (fourth i))
(defun iterate-instrs (i) (fifth i))

(defun set-iterate-instrs (i instrs)
	(setf (fifth i) instrs))
(defsetf iterate-instrs set-iterate-instrs) 
(defun match-left (m) (first m))
(defun match-right (m) (second m))

(defun make-persistent-iterate (name reg matches instrs) `(:persistent-iterate ,name ,reg ,matches ,instrs))
(defun make-order-persistent-iterate (name reg matches instrs sub) `(:order-persistent-iterate ,name ,reg ,matches ,instrs ,sub))
(defun make-order-linear-iterate (name reg matches instrs sub) `(:order-linear-iterate ,name ,reg ,matches ,instrs ,sub))
(defun make-order-rlinear-iterate (name reg matches instrs sub) `(:order-rlinear-iterate ,name ,reg ,matches ,instrs ,sub))

(defun make-linear-iterate (name reg matches instrs) `(:linear-iterate ,name ,reg ,matches ,instrs))
(defun make-rlinear-iterate (name reg matches instrs) `(:rlinear-iterate ,name ,reg ,matches ,instrs))

(defun order-iterate-subgoal (x) (sixth x))

(defun make-vm-update (reg) `(:update ,reg))
(defun vm-update-reg (x) (second x))

(defun make-vm-alloc (tuple reg) `(:alloc ,tuple ,reg))
(defun vm-alloc-tuple (alloc) (second alloc))
(defun vm-alloc-reg (alloc) (third alloc))

(defun make-vm-bool (v) `(:bool ,v))
(defun vm-bool-val (v) (second v))
(defun vm-bool-p (v) (tagged-p v :bool))

(defun make-vm-int (int) `(:int ,int))
(defun vm-int-p (int) (tagged-p int :int))
(defun vm-int-val (int) (second int))

(defun make-vm-float (flt) `(:float ,flt))
(defun vm-float-p (flt) (tagged-p flt :float))
(defun vm-float-val (flt) (second flt))

(defun make-vm-string-constant (v) `(:string ,v))
(defun vm-string-constant-p (x) (tagged-p x :string))
(defun vm-string-constant-val (x) (second x))

(defun make-vm-argument (id) `(:argument ,id))
(defun vm-argument-p (a) (tagged-p a :argument))
(defun vm-argument-id (a) (second a))

(defun make-vm-convert-float (place dest)
	(assert (reg-p place))
	(assert (reg-p dest))
	`(:convert-float ,place ,dest))
(defun vm-convert-float-p (flt) (tagged-p flt :convert-float))
(defun vm-convert-float-place (flt) (second flt))
(defun vm-convert-float-dest (flt) (third flt))

(defun make-vm-host-id () :host-id)
(defun vm-host-id-p (h) (eq h :host-id))

(defun make-vm-addr (num) `(:addr ,num))
(defun vm-addr-num (addr) (second addr))
(defun vm-addr-p (addr) (tagged-p addr :addr))

(defun make-send (from to) `(:send ,from ,to))
(defun make-send-self (reg) (make-send reg reg))
(defun send-from (send) (second send))
(defun send-to (send) (third send))

(defun make-vm-add-linear (reg) `(:add-linear ,reg))
(defun vm-add-linear-reg (x) (second x))

(defun make-vm-add-persistent (reg) `(:add-persistent ,reg))
(defun vm-add-persistent-reg (x) (second x))

(defun make-vm-run-action (reg) `(:run-action ,reg))
(defun vm-run-action-reg (x) (second x))

(defun make-vm-enqueue-linear (reg) `(:enqueue-linear ,reg))
(defun vm-enqueue-linear-reg (x) (second x))

(defun make-vm-send-delay (from to delay) `(:send-delay ,from ,to ,delay))
(defun vm-send-delay-from (send) (second send))
(defun vm-send-delay-to (send) (third send))
(defun vm-send-delay-time (send) (fourth send))

(defun make-vm-callf (name) `(:callf ,name))
(defun vm-callf-name (call) (second call))

(defun make-vm-call (name dest args)
	(assert (and (every #'reg-p args) (reg-p dest)))
	(let ((size (length args)))
		(cond
			((= size 0) `(:call0 ,name ,dest ,args))
			((= size 1) `(:call1 ,name ,dest ,args))
			((= size 2) `(:call2 ,name ,dest ,args))
			((= size 3) `(:call3 ,name ,dest ,args))
			(t
				(warn "maybe we should optimize this ~a-args call" size)
				`(:call ,name ,dest ,args)))))
(defun vm-call-name (call) (second call))
(defun vm-call-dest (call) (third call))
(defun vm-call-args (call) (fourth call))

(defun make-vm-calle (name dest args) `(:calle ,name ,dest ,args))
(defun vm-calle-name (call) (vm-call-name call))
(defun vm-calle-dest (call) (vm-call-dest call))
(defun vm-calle-args (call) (vm-call-args call))

(defun make-vm-push-registers () `(:push-registers))
(defun make-vm-pop-registers () `(:pop-registers))
(defun make-vm-funcall (name) `(:callf ,name))
(defun vm-funcall-name (call) (second call))

(defun make-vm-select-node () (list :select-node (make-hash-table)))
(defmacro vm-select-node-iterate (vsn (n instrs &optional (operation 'do)) &body body)
	(alexandria:with-gensyms (instrs1)
		`(loop for ,n being the hash-keys of (second ,vsn)
				using (hash-value ,instrs1)
				,operation (let ((,instrs (append ,instrs1 (list (make-return-select)))))
									,@body))))
(defun vm-select-node-push (vsn n instrs)
	"For select-node instruction 'vsn' add node n with instructions 'instrs'."
	(setf (gethash n (second vsn)) instrs))
(defun vm-select-node-empty-p (vsn)
	(= (hash-table-count (second vsn)) 0))
(defun set-vm-select-hash (vsn hash)
	(setf (second vsn) hash))
(defun merge-vm-select-node (a b)
	(iterate-hash ((second b) n instrs)
		(multiple-value-bind (old found-p) (gethash n (second a))
			(setf (gethash n (second a)) (append old instrs))))
	a)
(defun vm-select-node-p (x) (tagged-p x :select-node))

(defun make-vm-new-axioms (subgoals) `(:new-axioms ,subgoals))
(defun vm-new-axioms-subgoals (na) (second na))
   
(defun make-vm-colocated (h1 h2 dest) (list :colocated h1 h2 dest))
(defun vm-colocated-first (c) (second c))
(defun vm-colocated-second (c) (third c))
(defun vm-colocated-dest (c) (fourth c))
(defun vm-colocated-p (c) (tagged-p c :colocated))

(defun make-vm-remove (reg) (list :remove reg))
(defun vm-remove-reg (rm) (second rm))
(defun vm-remove-p (rm) (tagged-p rm :remove))

(defun make-vm-delete (pred filter) `(:delete ,pred ,filter))
(defun vm-delete-name (d) (second d))
(defun vm-delete-filter (d) (third d))
(defun vm-delete-p (d) (tagged-p d :delete))

(defun make-vm-reset-linear (instrs) `(:reset-linear ,instrs))
(defun vm-reset-linear-instrs (reset) (second reset))
(defun make-vm-reset-linear-end () '(:end-linear))

(defun make-vm-constant (name) `(:constant ,name))
(defun vm-constant-p (c) (tagged-p c :constant))
(defun vm-constant-name (c) (second c))

(defun match-p (m) (eq m :match))

(defun make-vm-rule (id) `(:rule ,id))
(defun vm-rule-id (rule) (second rule))
(defun vm-rule-p (rule) (tagged-p rule :rule))

(defun make-vm-rule-done () `(:rule-done))

(defun make-vm-new-node (reg) `(:new-node ,reg))
(defun vm-new-node-p (nn) (tagged-p nn :new-node))
(defun vm-new-node-reg (nn) (second nn))

(defun make-vm-set-priority-here (prio) `(:set-priority-here ,prio))
(defun make-vm-set-priority (prio node) `(:set-priority ,prio ,node))
(defun vm-set-priority-priority (x) (second x))
(defun vm-set-priority-node (x) (third x))

(defun make-vm-add-priority-here (prio) `(:add-priority-here ,prio))
(defun make-vm-add-priority (prio node) `(:add-priority ,prio ,node))
(defun vm-add-priority-priority (x) (second x))
(defun vm-add-priority-node (x) (third x))

(defun make-vm-stop-program () `(:stop-program))

(defun make-vm-cpu-id (node dest) `(:cpu-id ,node ,dest))
(defun vm-cpu-id-node (x) (second x))
(defun vm-cpu-id-dest (x) (third x))

(defun make-vm-node-priority (node dest) `(:node-priority ,node ,dest))
(defun vm-node-priority-node (x) (second x))
(defun vm-node-priority-dest (x) (third x))

(defun print-place (place)
   (cond
      ((vm-int-p place) (tostring "~a" (vm-int-val place)))
      ((vm-float-p place) (tostring "~a" (vm-float-val place)))
      ((vm-host-id-p place) "host-id")
      ((vm-addr-p place) (tostring "@~a" (vm-addr-num place)))
      ((vm-nil-p place) "nil")
      ((vm-world-p place) "@world")
      ((reg-p place) (tostring "reg ~a" (reg-num place)))
      ((reg-dot-p place)
         (tostring "~a.~a"
            (if (match-p (reg-dot-reg place))
               "(match)"
               (reg-num (reg-dot-reg place)))
            (reg-dot-field place)))))

(defmacro generate-print-op (basic-typs basic-ops &body body)
   `(on-top-level
      (defun print-op (op)
         (case op
            ,@(loop for typ in basic-typs
                  appending (mapcar #L`(,(format-keyword "~a-~a" typ !1)
                                          ,(substitute #\Space #\- (tostring "~A ~A" typ !1))) basic-ops))
            (otherwise ,@body)))))
            
(generate-print-op (addr int float) (equal not-equal lesser lesser-equal greater greater-equal plus minus mul div mod))

(defun print-instr-ls (instrs)
   (reduce #L(if !1 (concatenate 'string !1 (list #\Newline) (print-instr !2)) (print-instr !2))
                  instrs :initial-value nil))
                  
(defun print-match (m) (tostring "  ~a=~a~%" (print-place (first m)) (print-place (second m))))
(defun print-matches (matches)
   (if matches
      (reduce #L(concatenate 'string !1 (print-match !2)) matches :initial-value nil)
      ""))

(defun print-call-args (ls)
   (reduce #L(if (null !1) (print-place !2) (concatenate 'string !1 ", " (print-place !2))) ls :initial-value nil))

(defun print-select-node (instr)
   (let ((ls (vm-select-node-iterate instr (node instrs collect)
                  (tostring "~a:~%~a" node (print-instr-ls instrs)))))
      (merge-strings ls #\Newline))) 

(defun print-instr (instr)
   (case (instr-type instr)
      (:return "RETURN")
      (:test-nil (tostring "TEST-NIL ~a TO ~a" (print-place (vm-test-nil-place instr)) (print-place (vm-test-nil-dest instr))))
      (:cons (tostring "CONS (~a::~a) TO ~a" (print-place (vm-cons-head instr))
               (print-place (vm-cons-tail instr)) (print-place (vm-cons-dest instr))))
      (:head (tostring "HEAD ~a TO ~a" (print-place (vm-head-cons instr)) (print-place (vm-head-dest instr))))
      (:tail (tostring "TAIL ~a TO ~a" (print-place (vm-tail-cons instr)) (print-place (vm-tail-dest instr))))
      (:call (tostring "CALL ~a TO ~a = (~a)" (vm-call-name instr) (reg-num (vm-call-dest instr))
                  (print-call-args (vm-call-args instr))))
      (:send (tostring "SEND ~a TO ~a" (print-place (send-from instr))
                  (print-place (send-to instr))))
      (:alloc (tostring "ALLOC ~a TO ~a" (vm-alloc-tuple instr) (print-place (vm-alloc-reg instr))))
      (:iterate (tostring "ITERATE OVER ~a MATCHING~%~a~a~%NEXT" (iterate-name instr)
                  (print-matches (iterate-matches instr)) (print-instr-ls (iterate-instrs instr)))) 
      (:op (tostring "OP ~a ~a ~a TO ~a" (print-place (vm-op-v1 instr)) (print-op (vm-op-op instr))
                                             (print-place (vm-op-v2 instr)) (print-place (vm-op-dest instr))))
      (:not (tostring "NOT ~a TO ~a" (print-place (vm-not-place instr)) (print-place (vm-not-dest instr))))
      (:if (tostring "IF (~a) THEN~%~a~%ENDIF" (print-place (vm-if-reg instr)) (print-instr-ls (vm-if-instrs instr))))
      (:move (tostring "MOVE ~a TO ~a" (print-place (move-from instr)) (print-place (move-to instr))))
      (:convert-float (tostring "FLOAT ~a TO ~a" (print-place (vm-convert-float-place instr)) (print-place (vm-convert-float-dest instr))))
      (:select-node (tostring "START SELECT BY NODE~%~a~%-~%END SELECT BY NODE" (print-select-node instr)))
      (:return-select (tostring "RETURN SELECT"))
      (:colocated (tostring "COLOCATED (~a, ~a) TO ~a" (print-place (vm-colocated-first instr))
                                 (print-place (vm-colocated-second instr))
                                 (print-place (vm-colocated-dest instr))))
      (:delete (tostring "DELETE ~a FROM ~a" (vm-delete-name instr) (print-place (vm-delete-filter instr))))
      (t (error 'compile-invalid-error :text (tostring "Unknown instruction to print: ~a" instr)))))

(defun print-vm-list (out instrs)
   (dolist (instr instrs)
      (format out "~a~%" (print-instr instr))))

(defun process-print (proc &optional (str t))
   (with-process proc (:name name :instrs instrs)
      (format str "PROCESS ~a:~%" name)
      (print-vm-list str instrs)
      (format str "~%")))
      
(defun print-vm ()
   (with-output-to-string (str)
      (do-processes (:process proc)
         (process-print proc str))))

(defclass code ()
   ((processes
      :initarg :processes
      :initform (error "missing processes.")
      :accessor processes)
	 (functions
		:initarg :functions
		:initform (error "missing functions.")
		:accessor functions)
    (consts
		:initarg :consts
		:initform (error "missing const code.")
		:accessor consts)))
         
(defun vm-find (name-find)
   (do-processes (:name name :process proc)
      (when (equal name-find name)
         (return-from vm-find proc))))

(defclass rule-code ()
	((rule-code
		:initarg :rule-code
		:initform (error "missing code.")
		:accessor rule-code)
	 (subgoal-ids
		:initarg :subgoal-ids
		:initform (error "missing subgoal ids.")
		:accessor subgoal-ids)
	 (persistent-p
		:initarg :persistent-p
		:initform (error "missing persistent-p.")
		:accessor persistent-p)))
		
(defun make-rule-code (code subgoals is-persistent)
	(make-instance 'rule-code :rule-code code :subgoal-ids subgoals :persistent-p is-persistent))
