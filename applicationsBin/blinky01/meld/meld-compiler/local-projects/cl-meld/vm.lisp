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

(defun make-move (from to)
	(assert (not (null from)))
	(assert (not (null to)))
	`(:move ,from ,to))
(defun move-to (mv) (third mv))
(defun move-from (mv) (second mv))

(defun make-move-nil (to) `(:move-nil ,to))
(defun move-nil-to (mv) (second mv))
(defun move-nil-p (mv) (tagged-p mv :move-nil))

(defun make-return () '(:return))
(defun make-return-linear () '(:return-linear))
(defun make-return-select () '(:return-select))
(defun make-return-derived () '(:return-derived))

(defun make-vm-push () `(:push))
(defun make-vm-pop () `(:pop))

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

(defun make-vm-pcounter () `(:pc-counter))
(defun vm-pcounter-p (x) (tagged-p x :pc-counter))

(defun make-vm-ptr (v) `(:ptr ,v))
(defun vm-ptr-val (x) (second x))
(defun vm-ptr-p (x) (tagged-p x :ptr))

(defun make-reg-dot (reg field) `(:reg-dot ,reg ,field))
(defun reg-dot-reg (reg-dot) (second reg-dot))
(defun reg-dot-field (reg-dot) (third reg-dot))
(defun reg-dot-p (reg-dot) (tagged-p reg-dot :reg-dot))

(defun make-vm-nil () '(:nil))
(defun vm-nil-p (n) (tagged-p n :nil))

(defun make-vm-world () :world)
(defun vm-world-p (w) (eq w :world))

(defun make-vm-not (place dest) `(:not ,place ,dest))
(defun vm-not-place (n) (second n))
(defun vm-not-dest (n) (third n))

(defun make-vm-test-nil (place dest) `(:test-nil ,place ,dest))
(defun vm-test-nil-place (tn) (second tn))
(defun vm-test-nil-dest (tn) (third tn))

(defun make-vm-cons (head tail dest typ) `(:cons ,head ,tail ,dest ,typ))
(defun vm-cons-head (c) (second c))
(defun vm-cons-tail (c) (third c))
(defun vm-cons-dest (c) (fourth c))
(defun vm-cons-type (c) (fifth c))
(defun vm-cons-p (c) (tagged-p c :cons))

(defun make-vm-head (con dest typ) `(:head ,con ,dest ,typ))
(defun vm-head-cons (h) (second h))
(defun vm-head-dest (h) (third h))
(defun vm-head-type (h) (fourth h))
(defun vm-head-p (h) (tagged-p h :head))

(defun make-vm-tail (con dest typ) `(:tail ,con ,dest ,typ))
(defun vm-tail-cons (tail) (second tail))
(defun vm-tail-dest (tail) (third tail))
(defun vm-tail-type (tail) (fourth tail))
(defun vm-tail-p (tail) (tagged-p tail :tail))

(defun make-vm-struct-val (idx from to) `(:struct-val ,idx ,from ,to))
(defun vm-struct-val-idx (x) (second x))
(defun vm-struct-val-from (x) (third x))
(defun vm-struct-val-to (x) (fourth x))

(defun make-vm-make-struct (typ to) `(:struct ,typ ,to))
(defun vm-make-struct-to (x) (third x))
(defun vm-make-struct-type (x) (second x))

(defun make-vm-if (r instrs) (list :if r instrs))
(defun vm-if-reg (i) (second i))
(defun vm-if-instrs (i) (third i))
(defun vm-if-p (i) (tagged-p i :if))

(defun make-vm-op (dst v1 op v2) (list :op dst :to v1 op v2))
(defun vm-op-dest (st) (second st))
(defun vm-op-v1 (st) (fourth st))
(defun vm-op-op (st) (fifth st))
(defun vm-op-v2 (st) (sixth st))
(defun vm-op-p (st) (tagged-p st :op))

(defun make-iterate (name matches instrs &key (random-p nil) (to-delete-p nil) (min-p nil) (min-arg nil))
	`(:iterate ,name ,matches ,instrs ,random-p ,to-delete-p ,min-p ,min-arg))
(defun iterate-name (i) (second i))
(defun iterate-matches (i) (third i))
(defun iterate-instrs (i) (fourth i))
(defun iterate-random-p (i) (fifth i))
(defun iterate-to-delete-p (i) (sixth i))

(defun set-iterate-instrs (i instrs)
	(setf (fourth i) instrs))
(defsetf iterate-instrs set-iterate-instrs) 

(defun iterate-min-p (i) (seventh i))
(defun iterate-min-arg (i) (nth 7 i))
(defun match-left (m) (first m))
(defun match-right (m) (second m))

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

(defun make-vm-convert-float (place dest) `(:convert-float ,place ,dest))
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

(defun make-vm-send-delay (from to delay) `(:send-delay ,from ,to ,delay))
(defun vm-send-delay-from (send) (second send))
(defun vm-send-delay-to (send) (third send))
(defun vm-send-delay-time (send) (fourth send))

(defun make-vm-callf (name) `(:callf ,name))
(defun vm-callf-name (call) (second call))

(defun make-vm-call (name dest args) `(:call ,name ,dest ,args))
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

(defun make-vm-select-node () (list :select-node))
(defmacro vm-select-node-iterate (vsn (n instrs &optional (operation 'do)) &body body)
   `(loop for (,n ,instrs) in (cdr ,vsn)
          ,operation ,@body))
(defun vm-select-node-push (vsn n instrs)
	"For select-node instruction 'vsn' add node n with instructions 'instrs'."
   (setf (cdr vsn) (cons `(,n (,@instrs ,(make-return-select))) (cdr vsn))))
(defun vm-select-node-empty-p (vsn)
	(null (cdr vsn)))

(defun make-vm-new-axioms (subgoals) `(:new-axioms ,subgoals))
(defun vm-new-axioms-subgoals (na) (second na))
   
(defun make-vm-colocated (h1 h2 dest) (list :colocated h1 h2 dest))
(defun vm-colocated-first (c) (second c))
(defun vm-colocated-second (c) (third c))
(defun vm-colocated-dest (c) (fourth c))
(defun vm-colocated-p (c) (tagged-p c :colocated))

(defun make-vm-remove (reg) (list :remove reg))
(defun vm-remove-reg (rm) (second rm))

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

(defun tuple-p (tp) (eq tp :tuple))
(defun match-p (m) (eq m :match))

(defun make-vm-rule (id) `(:rule ,id))
(defun vm-rule-id (rule) (second rule))
(defun vm-rule-p (rule) (tagged-p rule :rule))

(defun make-vm-rule-done () `(:rule-done))

(defun make-vm-new-node (reg) `(:new-node ,reg))
(defun vm-new-node-p (nn) (tagged-p nn :new-node))
(defun vm-new-node-reg (nn) (second nn))

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
            (reg-dot-field place)))
      ((tuple-p place) "tuple")))

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
      (:move-nil (tostring "MOVE-NIL ~a" (print-place (move-nil-to instr))))
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