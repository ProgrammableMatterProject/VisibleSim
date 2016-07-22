
(in-package :cl-meld)

(define-condition output-invalid-error (error)
   ((text :initarg :text :reader text)))

(defparameter *value-mask* #b00111111)
(defparameter *reg-mask* #b00011111)
(defparameter *op-mask* #b00011111)
(defparameter *tuple-id-mask* #b01111111)
(defparameter *extern-id-mask* #b01111111)

(defparameter *output-string-constants* nil)
(defun push-string-constant (str)
	"Adds string constant to database if not found and returns the string integer code."
	(let ((pos (position str *output-string-constants* :test #'string-equal)))
		(if pos
			pos
			(progn
				(push-end str *output-string-constants*)
				(1- (length *output-string-constants*))))))
				
(defmacro with-memory-stream (s &body body)
   `(let ((,s (make-in-memory-output-stream)))
      ,@body
      s))

(defparameter *node-values-positions* nil)
(defun add-node-value (vec)
	(let ((pos (length vec)))
		(push pos *node-values-positions*)))

(defmacro add-byte (b vec) `(vector-push-extend ,b ,vec))
(defun add-bytes (vec ls)
	(dolist (b ls) (add-byte b vec)))
   
(defun output-int (int)
   (loop for i upto 3
      collect (ldb (byte 8 (* i 8)) int)))
(defun output-int64 (int)
	(loop for i upto 7
		collect (ldb (byte 8 (* i 8)) int)))
(defun output-float32 (flt) (output-int (encode-float32 (coerce flt 'double-float))))
(defun output-float64 (flt) (output-int64 (encode-float64 (coerce flt 'double-float))))
(defun output-float (flt) (output-float64 flt))
(defun output-string (str)
	(map 'list #'char-code str))
(defun output-addr (addr)
	(output-int64 (vm-addr-num addr)))
	
(defconstant +any-value-flag+ #b001111)
(defconstant +int-value-flag+ #b000001)
	
(defun output-list (ls vec)
	(let ((head (vm-list-head ls))
			 (tail (vm-list-tail ls)))
		(output-value head vec)
		(output-value tail vec)))
		
(defun variable-value-p (val)
	(cond
		((reg-p val) t)
		((reg-dot-p val) t)
		((vm-pcounter-p val) t)
		((vm-stack-p val) t)
		((vm-list-p val)
			(or (variable-value-p (vm-list-head val))
				(variable-value-p (vm-list-tail val))))
		(t nil)))
		
(defun output-value-data (val vec)
	(cond
		((vm-any-p val))
		((vm-bool-p val) (if (vm-bool-val val)
									(add-byte #b1 vec)
									(add-byte #b0 vec)))
		((vm-int-p val) (output-list-bytes vec (output-int (vm-int-val val))))
		((vm-float-p val) (output-list-bytes vec (output-float (vm-float-val val))))
		((vm-list-p val) (output-list val vec))
		((vm-string-constant-p val)
			(let* ((str (vm-string-constant-val val))
					 (code (push-string-constant str)))
				(output-list-bytes vec (output-int code))))
		((vm-addr-p val)
			(add-node-value vec) 
			(output-list-bytes vec (output-addr val)))
		((vm-ptr-p val) (output-list-bytes vec (output-int64 (vm-ptr-val val))))
		((vm-host-id-p val))
		((vm-nil-p val))
		((vm-non-nil-p val))
		((vm-world-p val) (output-value-data (make-vm-int (number-of-nodes *nodes*)) vec))
		((vm-pcounter-p val))
		((vm-stack-p val) (add-byte (vm-stack-offset val) vec))
		((reg-p val))
		((reg-dot-p val)
			(add-byte (reg-dot-field val) vec)
			(add-byte (reg-num (reg-dot-reg val)) vec))
		((vm-argument-p val)
			(add-byte (vm-argument-id val) vec))
		((vm-constant-p val)
			(output-list-bytes vec (output-int (lookup-const-id (vm-constant-name val)))))
		(t (error 'output-invalid-error :text (tostring "invalid expression value: ~a" val)))))
			
(defun output-value (val vec)
   (let ((flag (cond
						((vm-any-p val) +any-value-flag+)
						((vm-bool-p val) #b001100)
				      ((vm-int-p val) +int-value-flag+)
				      ((vm-float-p val) #b000000)
						((vm-list-p val) #b001110)
						((vm-string-constant-p val) #b000110)
				      ((vm-addr-p val) #b000101)
				      ((vm-ptr-p val) #b001011)
						((vm-host-id-p val) #b000011)
				      ((vm-nil-p val) #b000100)
						;; special value to handle matches with non-nil lists
						((vm-non-nil-p val) #b001101)
				      ((vm-world-p val) +int-value-flag+)
				      ((vm-pcounter-p val) #b001010)
						((vm-stack-p val) #b001001)
				      ((reg-p val) (logior #b100000 (logand #b011111 (reg-num val))))
				      ((reg-dot-p val) #b000010)
						((vm-argument-p val) #b00000111)
						((vm-constant-p val) #b00001000)
				      (t (error 'output-invalid-error :text (tostring "Invalid expression value: ~a" val))))))
		(add-byte flag vec)
		(output-value-data val vec)))

(defun output-values (vec vals)
	(loop for val in vals
			do
				(if (reg-p val)
					(add-byte (reg-to-byte val) vec)
					(output-value-data val vec))))
					
(defun output-instr-and-values-extra (vec instr extra-bytes vals)
	(add-byte instr vec)
	(output-list-bytes vec extra-bytes)
	(output-values vec vals))
	
(defun output-instr-and-values (vec instr &rest vals)
	(output-instr-and-values-extra vec instr nil vals))
	
(defun output-instr-type-and-values (vec instr type &rest vals)
	(output-instr-and-values-extra vec instr (list (lookup-type-id type)) vals))
	
(defun output-instr-index-and-values (vec instr idx &rest vals)
	(output-instr-and-values-extra vec instr (list idx) vals))
	
(defun output-call (vec call instr &optional extra-bytes)
	(let ((extern-id (lookup-external-function-id (vm-call-name call)))
         (typ (vm-call-type call))
         (args (vm-call-args call)))
      (add-byte instr vec)
      (add-byte (logand *extern-id-mask* extern-id) vec)
      (add-byte (logand *reg-mask* (reg-to-byte (vm-call-dest call))) vec)
      (add-byte (lookup-type-id typ) vec)
      (output-value-data (vm-call-gc call) vec)
      (output-list-bytes vec extra-bytes)
      (output-values vec args)))

(defun output-calle (vec call instr &optional extra-bytes)
	(let ((extern-id (lookup-custom-external-function-id (vm-calle-name call)))
             (args (vm-call-args call)))
         (add-byte instr vec)
         (add-byte (logand *extern-id-mask* extern-id) vec)
         (add-byte (logand *reg-mask* (reg-to-byte (vm-call-dest call))) vec)
         (output-value-data (vm-call-gc call) vec)
			(output-list-bytes vec extra-bytes)
         (output-values vec args)))
      
(defun reg-to-byte (reg) (reg-num reg))
      
(defun lookup-extern-id (ast extern)
   (do-externs ast (:id id :name name)
      (if (string-equal name extern) (return-from lookup-extern-id id))))

(defun lookup-const-id (const)
	(do-constant-list *consts* (:name name :id id)
		(if (string-equal name const) (return-from lookup-const-id id))))
		
(defun lookup-function-id (name)
	(do-functions *functions* (:name fun-name :id id)
		(if (string-equal name fun-name)
			(return-from lookup-function-id id))))

(defun output-match (match vec)
   (let* ((reg-dot (match-left match))
          (field (reg-dot-field reg-dot)))
      (add-byte field vec)
		(output-value (match-right match) vec)))

(defun output-list-bytes (vec ls)
	(dolist (b ls)
      (add-byte b vec)))

(defun output-matches (matches vec)
	(let ((len (length matches)))
		(add-byte len vec)
		(loop for match in matches
			do (output-match match vec))))
			
(defconstant +code-offset-size+ 4)
(defun write-offset (vec off &optional (pos 0))
   (let ((ls (output-int off)))
      (loop for i from 0 to 3
            for part in ls
            do (setf (aref vec (+ pos i)) part))))
            
(defmacro jumps-here (vec)
   `(progn
      (add-byte #b0 ,vec)
      (add-byte #b0 ,vec)
      (add-byte #b0 ,vec)
      (add-byte #b0 ,vec)))
      
(defmacro save-pos ((pos vec) &body body)
   `(let ((,pos (length ,vec)))
      ,@body))
      
(defmacro write-jump (vec jump-many &body body)
   (alexandria:with-gensyms (pos)
      `(save-pos (,pos ,vec)
          ,@body
         (write-offset ,vec (- (length ,vec) ,pos) (+ ,pos ,jump-many)))))
(defmacro backwards-write-jump (vec jump-many &body body)
	(alexandria:with-gensyms (pos)
		`(save-pos (,pos ,vec)
			,@body
			(write-offset ,vec
				(- (length ,vec) ,pos) (- ,pos (+ ,jump-many +code-offset-size+))))))
				
(defun output-axiom-argument (arg vec subgoal intercept)
   (funcall intercept arg vec)
	(cond
		((addr-p arg)
			(output-list-bytes vec (output-addr arg)))
		((int-p arg)
			(if (type-float-p (expr-type arg))
				(output-list-bytes vec (output-float (int-val arg)))
				(output-list-bytes vec (output-int (int-val arg)))))
		((float-p arg) (output-list-bytes vec (output-float (float-val arg))))
		((string-constant-p arg) (output-list-bytes vec (output-int (push-string-constant (string-constant-val arg)))))
		((nil-p arg) (add-byte #b0 vec))
		((cons-p arg)
			(add-byte #b1 vec)
			(output-axiom-argument (cons-head arg) vec subgoal intercept)
			(output-axiom-argument (cons-tail arg) vec subgoal intercept))
      ((struct-p arg)
         (loop for x in (struct-list arg)
               do (output-axiom-argument x vec subgoal intercept)))
		(t (error 'output-invalid-error :text (tostring "don't know how to output this subgoal: ~a" subgoal)))))

(defun constant-matches-p (iter-matches)
	(loop for match in iter-matches
			do (let ((val (match-right match)))
					(when (variable-value-p val)
						(return-from constant-matches-p nil))))
	t)

(defun iterate-options-byte-list (iter)
	(let ((sub (order-iterate-subgoal iter)))
		(cond
			((subgoal-has-random-p sub)
				(list #b00000001 #b0))
			((subgoal-has-min-p sub)
				(list #b00000100 (subgoal-get-min-variable-position sub)))
			(t (assert nil)))))

(defun output-iterate (vec instr-code instr optional-bytes)
	(write-jump vec 16 ;; outside jump
		(write-jump vec 12 ;; inner jump
      	(add-byte instr-code vec)
			(loop for i from 1 upto 8
				do (add-byte #b0 vec))
      	(add-byte (lookup-def-id (iterate-name instr)) vec)
			(add-byte (logand *reg-mask* (reg-to-byte (iterate-reg instr))) vec)
			(add-byte (if (constant-matches-p (iterate-matches instr)) #b1 #b0) vec)
			(jumps-here vec)
			(jumps-here vec)
			(add-bytes vec optional-bytes)
      	(output-matches (iterate-matches instr) vec))
      (output-instrs (iterate-instrs instr) vec)
      (add-byte #b00000001 vec))) ; next

(defun output-axioms (vec axioms intercept)
 (do-subgoals axioms (:name name :args args :subgoal axiom)
  (add-byte (logand *tuple-id-mask* (lookup-def-id name)) vec)
  (dolist (arg args)
   (output-axiom-argument arg vec axiom intercept))))

(defun output-instr (instr vec)
   (case (instr-type instr)
      (:return (add-byte #x0 vec))
      (:next (add-byte #x1 vec))
      (:return-linear (add-byte #b11010000 vec))
      (:return-derived (add-byte #b11110000 vec))
		(:new-axioms
			(write-jump vec 1
				(add-byte #b00010100 vec)
				(jumps-here vec)
            (output-axioms vec (vm-new-axioms-subgoals instr) #'(lambda (arg vec)
                                                                  (when (addr-p arg)
                                                                   (add-node-value vec))))))
		(:send-delay
			(add-byte #b00010101 vec)
			(add-byte (logand *reg-mask* (reg-to-byte (vm-send-delay-from instr))) vec)
         (add-byte (logand *reg-mask* (reg-to-byte (vm-send-delay-to instr))) vec)
			(output-list-bytes vec (output-int (vm-send-delay-time instr))))
      (:reset-linear
         (write-jump vec 1
            (add-byte #b00001110 vec)
            (jumps-here vec)
            (output-instrs (vm-reset-linear-instrs instr) vec)))
		(:end-linear
			(add-byte #b00001111 vec))
		(:push
			(add-byte #b00010110 vec))
		(:pop
			(add-byte #b00010111 vec))
		(:push-registers
			(add-byte #b00011000 vec))
		(:pop-registers
			(add-byte #b00011001 vec))
      (:remove
            (let ((reg (vm-remove-reg instr)))
               (add-byte #b10000000 vec)
               (add-byte (logand *reg-mask* (reg-to-byte reg)) vec)))
      (:alloc (let ((tuple-id (lookup-def-id (vm-alloc-tuple instr)))
                    (reg (reg-to-byte (vm-alloc-reg instr))))
                  (add-byte #b01000000 vec)
                  (add-byte (logand *tuple-id-mask* tuple-id) vec)
                  (add-byte (logand *reg-mask* reg) vec)))
      (:send (add-byte #b00001000 vec)
             (add-byte (logand *reg-mask* (reg-to-byte (send-from instr))) vec)
             (add-byte (logand *reg-mask* (reg-to-byte (send-to instr))) vec))
		(:callf (add-byte #b00011010 vec)
				  (add-byte (lookup-function-id (vm-callf-name instr)) vec))
      (:call
			(output-call vec instr #b00100000 (list (length (vm-call-args instr)))))
		(:calle
			(output-calle vec instr #b00011011 (list (length (vm-calle-args instr)))))
      (:if (assert (reg-p (vm-if-reg instr)))
      		(let ((reg-b (reg-to-byte (vm-if-reg instr))))
             (write-jump vec 2
               (add-byte #b01100000 vec)
               (add-byte (logand *reg-mask* reg-b) vec)
               (jumps-here vec)
               (output-instrs (vm-if-instrs instr) vec))))
		(:if-else (let ((reg-b (reg-to-byte (vm-if-else-reg instr))))
						(write-jump vec (+ 2 +code-offset-size+)
							(write-jump vec 2
								(add-byte #b10000001 vec)
								(add-byte (logand *reg-mask* reg-b) vec)
								(jumps-here vec) ;; jump to else code
								(jumps-here vec) ;; jump outside this instruction
								(output-instrs (vm-if-else-instrs1 instr) vec)
								(add-byte #b10000010 vec) ;; jump instruction
								(jumps-here vec))
							(backwards-write-jump vec 0
								(output-instrs (vm-if-else-instrs2 instr) vec)))))
		(:move-stack-to-field
			(output-instr-and-values vec #b10000011 (move-from instr) (move-to instr)))
		(:persistent-iterate (output-iterate vec #b00000010 instr nil))
		(:order-persistent-iterate (output-iterate vec #b00000100 instr (iterate-options-byte-list instr)))
		(:linear-iterate (output-iterate vec #b00000101 instr nil))
		(:rlinear-iterate (output-iterate vec #b00000110 instr nil))
		(:order-linear-iterate (output-iterate vec #b00001100 instr (iterate-options-byte-list instr)))
		(:order-rlinear-iterate (output-iterate vec #b00010010 instr (iterate-options-byte-list instr)))
		(:move-int-to-field
			(output-instr-and-values vec #b00011110 (move-from instr) (move-to instr)))
		(:move-int-to-reg
			(output-instr-and-values vec #b00011111 (move-from instr) (move-to instr)))
		(:move-field-to-field
			(output-instr-and-values vec #b00100001 (move-from instr) (move-to instr)))
		(:move-field-to-reg
			(output-instr-and-values vec #b00100010 (move-from instr) (move-to instr)))
		(:move-ptr-to-reg
			(output-instr-and-values vec #b00100011 (move-from instr) (move-to instr)))
      (:move-nil-to-field
			(output-instr-and-values vec #b01110000 (move-to instr)))
		(:move-nil-to-reg
			(output-instr-and-values vec #b00100100 (move-to instr)))
		(:move-field-to-field-ref
			(output-instr-and-values vec #b00100101 (move-from instr) (move-to instr)))
		(:move-reg-to-field
			(output-instr-and-values vec #b00100110 (move-from instr) (move-to instr)))
		(:move-reg-to-field-ref
			(output-instr-and-values vec #b00100111 (move-from instr) (move-to instr)))
		(:move-host-id-to-field
			(output-instr-and-values vec #b00101000 (move-from instr) (move-to instr)))
		(:move-reg-to-constant
			(output-instr-and-values vec #b00101001 (move-from instr) (move-to instr)))
		(:move-constant-to-field
			(output-instr-and-values vec #b00101010 (move-from instr) (move-to instr)))
		(:move-constant-to-field-ref
			(output-instr-and-values vec #b00101011 (move-from instr) (move-to instr)))
		(:move-addr-to-field
			(output-instr-and-values vec #b00101100 (move-from instr) (move-to instr)))
		(:move-float-to-field
			(output-instr-and-values vec #b00101101 (move-from instr) (move-to instr)))
		(:move-float-to-reg
			(output-instr-and-values vec #b00101110 (move-from instr) (move-to instr)))
		(:move-int-to-constant
			(output-instr-and-values vec #b00101111 (move-from instr) (move-to instr)))
		(:move-world-to-field
			(output-instr-and-values vec #b00110001 (move-to instr)))
		(:move-stack-to-pcounter
			(output-instr-and-values vec #b00110010 (move-from instr)))
    	(:move-pcounter-to-stack
			(output-instr-and-values vec #b00110011 (move-to instr)))
		(:move-stack-to-reg
			(output-instr-and-values vec #b00110100 (move-from instr) (move-to instr)))
		(:move-reg-to-stack
			(output-instr-and-values vec #b00110101 (move-from instr) (move-to instr)))
		(:move-addr-to-reg
			(output-instr-and-values vec #b00110110 (move-from instr) (move-to instr)))
		(:move-host-id-to-reg
			(output-instr-and-values vec #b00110111 (move-to instr)))
		(:move-argument-to-reg
			(output-instr-and-values vec #b01111100 (move-from instr) (move-to instr)))
		(:addr-not-equal
			(output-instr-and-values vec #b00111000 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:addr-equal
			(output-instr-and-values vec #b00111001 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-minus
			(output-instr-and-values vec #b00111010 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-equal
			(output-instr-and-values vec #b00111011 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-not-equal
			(output-instr-and-values vec #b00111100 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-plus
			(output-instr-and-values vec #b00111101 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-lesser
			(output-instr-and-values vec #b00111110 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-greater-equal
			(output-instr-and-values vec #b00111111 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:bool-or
			(output-instr-and-values vec #b01000001 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-lesser-equal
			(output-instr-and-values vec #b01000010 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-greater
			(output-instr-and-values vec #b01000011 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-mul
			(output-instr-and-values vec #b01000100 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-div
			(output-instr-and-values vec #b01000101 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:int-mod
			(output-instr-and-values vec #b01111101 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-plus
			(output-instr-and-values vec #b01000110 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-minus
			(output-instr-and-values vec #b01000111 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-mul
			(output-instr-and-values vec #b01001000 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-div
			(output-instr-and-values vec #b01001001 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-equal
			(output-instr-and-values vec #b01001010 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-not-equal
			(output-instr-and-values vec #b01001011 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-lesser
			(output-instr-and-values vec #b01001100 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-lesser-equal
			(output-instr-and-values vec #b01001101 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-greater
			(output-instr-and-values vec #b01001110 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:float-greater-equal
			(output-instr-and-values vec #b01001111 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:move-reg-to-reg
			(output-instr-and-values vec #b01010000 (move-from instr) (move-to instr)))
  		(:test-nil
			(output-instr-and-values vec #b00000011 (vm-test-nil-place instr) (vm-test-nil-dest instr)))
      (:not
			(output-instr-and-values vec #b00000111 (vm-not-place instr) (vm-not-dest instr)))
		(:bool-equal
			(output-instr-and-values vec #b01010001 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:bool-not-equal
			(output-instr-and-values vec #b01010010 (vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr)))
		(:head-rr
			(output-instr-and-values vec #b01010011 (vm-head-cons instr) (vm-head-dest instr)))
		(:head-fr
			(output-instr-and-values vec #b01010100 (vm-head-cons instr) (vm-head-dest instr)))
		(:head-ff
			(output-instr-and-values vec #b01010101 (vm-head-cons instr) (vm-head-dest instr)))
		(:head-rf
			(output-instr-and-values vec #b01010110 (vm-head-cons instr) (vm-head-dest instr)))
		(:head-ffr
			(output-instr-and-values vec #b01010111 (vm-head-cons instr) (vm-head-dest instr)))
		(:head-rfr
			(output-instr-and-values vec #b01011000 (vm-head-cons instr) (vm-head-dest instr)))
		(:tail-rr
			(output-instr-and-values vec #b01011001 (vm-tail-cons instr) (vm-tail-dest instr)))
		(:tail-fr
			(output-instr-and-values vec #b01011010 (vm-tail-cons instr) (vm-tail-dest instr)))
		(:tail-ff
			(output-instr-and-values vec #b01011011 (vm-tail-cons instr) (vm-tail-dest instr)))
		(:tail-rf
			(output-instr-and-values vec #b01011100 (vm-tail-cons instr) (vm-tail-dest instr)))
		(:move-world-to-reg
			(output-instr-and-values vec #b01011101 (move-to instr)))
		(:move-constant-to-reg
			(output-instr-and-values vec #b01011110 (move-from instr) (move-to instr)))
		(:cons-rrr
			(output-instr-type-and-values vec #b01011111 (vm-cons-type instr) (vm-cons-head instr)
             (vm-cons-tail instr) (vm-cons-dest instr) (vm-cons-gc instr)))
		(:cons-rff
			(output-instr-and-values vec #b01100001 (vm-cons-head instr) (vm-cons-tail instr)
            (vm-cons-dest instr)))
		(:cons-frf
			(output-instr-and-values vec #b01100010 (vm-cons-head instr) (vm-cons-tail instr)
            (vm-cons-dest instr)))
		(:cons-ffr
			(output-instr-and-values vec #b01100011 (vm-cons-head instr) (vm-cons-tail instr)
          (vm-cons-dest instr) (vm-cons-gc instr)))
		(:cons-rrf
			(output-instr-and-values vec #b01100100 (vm-cons-head instr) (vm-cons-tail instr) (vm-cons-dest instr)))
		(:cons-rfr
			(output-instr-and-values vec #b01100101 (vm-cons-head instr) (vm-cons-tail instr)
          (vm-cons-dest instr) (vm-cons-gc instr)))
		(:cons-frr
			(output-instr-type-and-values vec #b01100110 (vm-cons-type instr) (vm-cons-head instr)
          (vm-cons-tail instr) (vm-cons-dest instr) (vm-cons-gc instr)))
		(:cons-fff
			(output-instr-and-values vec #b01100111 (vm-cons-head instr) (vm-cons-tail instr) (vm-cons-dest instr)))
		(:call0
			(output-call vec instr #b01101000))
		(:call1
			(output-call vec instr #b01101001))
		(:call2
			(output-call vec instr #b01101010))
		(:call3
			(output-call vec instr #b01101011))
		(:move-int-to-stack
			(output-instr-and-values vec #b01101100 (move-from instr) (move-to instr)))
		(:push-n
			(add-byte #b01101101 vec)
			(add-byte (vm-push-n instr) vec))
		(:structr
			(output-instr-type-and-values vec #b00011101 (vm-make-struct-type instr) (vm-make-struct-to instr)))
		(:structf
			(output-instr-and-values vec #b01101110 (vm-make-struct-to instr)))
		(:struct-valrr
			(output-instr-index-and-values vec #b01101111 (vm-struct-val-idx instr) (vm-struct-val-from instr) (vm-struct-val-to instr)))
		(:struct-valfr
			(output-instr-index-and-values vec #b01110001 (vm-struct-val-idx instr) (vm-struct-val-from instr) (vm-struct-val-to instr)))
		(:struct-valrf
			(output-instr-index-and-values vec #b01110010 (vm-struct-val-idx instr) (vm-struct-val-from instr) (vm-struct-val-to instr)))
		(:struct-valrf-ref
			(output-instr-index-and-values vec #b01110011 (vm-struct-val-idx instr) (vm-struct-val-from instr) (vm-struct-val-to instr)))
		(:struct-valff
			(output-instr-index-and-values vec #b01110100 (vm-struct-val-idx instr) (vm-struct-val-from instr) (vm-struct-val-to instr)))
		(:struct-valff-ref
			(output-instr-index-and-values vec #b01110101 (vm-struct-val-idx instr) (vm-struct-val-from instr) (vm-struct-val-to instr)))
		(:move-float-to-stack
			(output-instr-and-values vec #b01110110 (move-from instr) (move-to instr)))
		(:add-linear
			(output-instr-and-values vec #b01110111 (vm-add-linear-reg instr)))
		(:add-persistent
			(output-instr-and-values vec #b01111000 (vm-add-persistent-reg instr)))
		(:run-action
			(output-instr-and-values vec #b01111001 (vm-run-action-reg instr)))
		(:enqueue-linear
			(output-instr-and-values vec #b01111010 (vm-enqueue-linear-reg instr)))
		(:update
			(output-instr-and-values vec #b01111011 (vm-update-reg instr)))
      (:convert-float
			(output-instr-and-values vec #b00001001 (vm-convert-float-place instr) (vm-convert-float-dest instr)))
		(:set-priority
			(output-instr-and-values vec #b00011100 (vm-set-priority-priority instr) (vm-set-priority-node instr)))
		(:set-priority-here
			(output-instr-and-values vec #b00110000 (vm-set-priority-priority instr)))
		(:add-priority
			(output-instr-and-values vec #b10100000 (vm-add-priority-priority instr) (vm-add-priority-node instr)))
		(:add-priority-here
			(output-instr-and-values vec #b10100001 (vm-add-priority-priority instr)))
      (:set-default-priority-here
         (output-instr-and-values vec #b10100011 (vm-set-default-priority-priority instr)))
      (:set-default-priority
         (output-instr-and-values vec #b10100100 (vm-set-default-priority-priority instr) (vm-set-default-priority-node instr)))
      (:set-static-here
         (output-instr-and-values vec #b10100101))
      (:set-static
         (output-instr-and-values vec #b10100110 (vm-set-static-node instr)))
      (:set-moving-here
         (output-instr-and-values vec #b10100111))
      (:set-moving
         (output-instr-and-values vec #b10101000 (vm-set-moving-node instr)))
      (:set-affinity-here
         (output-instr-and-values vec #b10101001 (vm-set-affinity-target instr)))
      (:set-affinity
         (output-instr-and-values vec #b10101010 (vm-set-affinity-target instr) (vm-set-affinity-node instr)))
		(:cpu-static
			(output-instr-and-values vec #b10101011 (vm-cpu-static-node instr) (vm-cpu-static-dest instr)))
      (:move-cpus-to-reg 
         (output-instr-and-values vec #b10101100 (move-to instr)))
      (:set-cpu-here
         (output-instr-and-values vec #b10101101 (vm-set-cpu-cpu instr)))
      (:is-static
         (output-instr-and-values vec #b10101110 (vm-is-static-node instr) (vm-is-static-dest instr)))
      (:is-moving
         (output-instr-and-values vec #b10101111 (vm-is-moving-node instr) (vm-is-moving-dest instr)))
      (:facts-proved
         (output-instr-and-values vec #b10110000 (vm-facts-proved-node instr) (vm-facts-proved-dest instr)))
      (:remove-priority
         (output-instr-and-values vec #b10110001 (vm-remove-priority-node instr)))
      (:remove-priority-here
         (output-instr-and-values vec #b10110010))
      (:facts-consumed
         (output-instr-and-values vec #b10110011 (vm-facts-consumed-node instr) (vm-facts-consumed-dest instr)))
		(:thread-linear-iterate (output-iterate vec #b10110100 instr nil))
		(:add-thread-persistent
			(output-instr-and-values vec #b10110101 (vm-add-persistent-reg instr)))
      (:schedule-next
         (output-instr-and-values vec #b10110110 (vm-schedule-next-node instr)))
		(:thread-persistent-iterate (output-iterate vec #b10110111 instr nil))
      (:fabs
         (output-instr-and-values vec #b10111000 (vm-fabs-float instr) (vm-fabs-dest instr)))
      (:move-type-to-reg
         (output-instr-and-values vec #b10111001 (make-vm-int (lookup-type-id (vm-type-get (move-from instr)))) (move-to instr)))
      (:remote-update
         (output-instr-and-values vec #b10000100)
         (add-byte (logand *reg-mask* (reg-to-byte (vm-remote-update-dest instr))) vec)
         (add-byte (lookup-def-id (definition-name (vm-remote-update-edit-definition instr))) vec)
         (add-byte (lookup-def-id (definition-name (vm-remote-update-target-definition instr))) vec)
         (add-byte (vm-remote-update-count instr) vec)
         (add-byte (length (vm-remote-update-regs instr)) vec)
         (loop for reg in (vm-remote-update-regs instr)
               do (add-byte (logand *reg-mask* (reg-to-byte reg)) vec)))
      (:stop-program
			(output-instr-and-values vec #b10100010))
	   (:cpu-id
			(output-instr-and-values vec #b01111110 (vm-cpu-id-node instr) (vm-cpu-id-dest instr)))
	   (:node-priority
			(output-instr-and-values vec #b01111111 (vm-node-priority-node instr) (vm-node-priority-dest instr)))
     (:select-node
							(when (vm-select-node-empty-p instr)
								(return-from output-instr nil))
							(let* ((total-nodes (number-of-nodes *nodes*))
                           (size-header (* 2 +code-offset-size+))
                           (hash (make-hash-table))
                           (end-hash (make-hash-table)))
                        (add-byte #b00001010 vec)
                        (save-pos (start vec)
                           (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec) ; size of complete select instruction
                           (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec) ; table size
                           (write-offset vec total-nodes (+ start +code-offset-size+))
                           (loop for i from 0 to (1- total-nodes)
                                 do (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec))
                           (save-pos (begin-instrs vec)
                              (vm-select-node-iterate instr (n instrs)
                                 (save-pos (cur vec)
                                    (setf (gethash n hash) (- cur begin-instrs))
                                    (output-instrs instrs vec)
                                    (save-pos (end vec)
                                       (setf (gethash n end-hash) (- end +code-offset-size+))))))
                           (save-pos (end-select vec)
                              (loop for i from 0 to (1- total-nodes)
                                    for pos = (* i +code-offset-size+)
                                    do (alexandria:when-let ((offset (gethash i hash)))
                                          ;; offset is always one more, since when 0 it means there is
                                          ;; no code for the corresponding node
                                          (write-offset vec (1+ offset) (+ start size-header pos)))
                                    do (alexandria:when-let ((write-end (gethash i end-hash)))
                                          (write-offset vec (- end-select (1- write-end)) write-end)))
                              (write-offset vec (- end-select (1- start)) start)))))
      (:return-select (add-byte #b00001011 vec) (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec))
      (:rule
         (add-byte #b00010000 vec)
			(output-list-bytes vec (output-int (vm-rule-id instr))))
      (:rule-done
         (add-byte #b00010001 vec))
		(:new-node
			(add-byte #b00010011 vec)
			(add-byte (logand *reg-mask* (reg-num (vm-new-node-reg instr))) vec))
      (otherwise (error 'output-invalid-error :text (tostring "Unknown instruction to output ~a" instr)))))
                
(defun output-instrs (ls vec)
   (dolist (instr ls)
      (output-instr instr vec)))

(defmacro make-byte-code (name vec &body body)
	`(progn
		(let ((*node-values-positions* nil))
			,@body
			(list :byte-code ,name ,vec *node-values-positions*))))
			
(defun byte-code-name (b) (second b))
(defun byte-code-vec (b) (third b))
(defun byte-code-nodes (b) (fourth b))
(defun byte-code-size (b) (length (byte-code-vec b)))
(defun byte-code-find (name ls)
   (dolist (bc ls)
      (when (string-equal name (byte-code-name bc))
         (return-from byte-code-find bc))))

(defun write-byte-code (stream bc)
	(write-vec stream (byte-code-vec bc))
	(write-int-stream stream (length (byte-code-nodes bc)))
	(dolist (node (byte-code-nodes bc))
		(write-int-stream stream node)))

(defun output-processes ()
   (do-processes (:name name :instrs instrs :operation collect)
      (let ((vec (create-bin-array)))
			(make-byte-code name vec
         	(output-instrs instrs vec)))))

(defun type-to-bytes (typ)
	(cond
		((symbolp typ)
			(case typ
      		(:type-int '(#b0000))
      		(:type-float '(#b0001))
      		(:type-addr '(#b0010))
            ; #b0011 type-list
            ; #b0100 type-struct
				(:type-bool '(#b0101))
            (:type-thread '(#b0110))
            ; #b0111 type-array
				(:type-string '(#b1001))
				(otherwise (error 'output-invalid-error :text (tostring "invalid arg type: ~a" typ)))))
      ((type-node-p typ) '(#b0010))
		((type-list-p typ)
			(let* ((sub (type-list-element typ))
                (id (lookup-type-id sub)))
				`(,#b0011 ,id)))
      ((type-array-p typ)
         (let* ((sub (type-array-element typ))
                (id (lookup-type-id sub)))
          `(,#b0111 ,id)))
		((type-struct-p typ)
			(let ((ls (type-struct-list typ)))
				(let ((x `(,#b0100 ,(length ls) ,@(loop for ty in ls collect (lookup-type-id ty)))))
					x)))
		(t (error 'output-invalid-error :text (tostring "invalid arg type: ~a" typ)))))

(defun output-arg-type (typ vec)
	(add-byte (lookup-type-id typ) vec))
		
(defun write-arg-type (stream typ)
	(write-hexa stream (lookup-type-id typ)))
		
(defun output-aggregate-type (agg typ)
   (case agg
      (:first #b0001)
      (:min (case typ
               (:type-int #b0011)
               (:type-float #b0110)))
      (:max (case typ
               (:type-int #b0010)
               (:type-float #b0101)))
      (:sum (cond
					((type-int-p typ) #b0100)
					((type-float-p typ) #b0111)
					((type-list-p typ)
						(let ((sub (type-list-element typ)))
							(cond
								((type-float-p sub) #b1011))))))))

(defun output-aggregate (types)
   (let ((agg (find-if #'aggregate-p types)))
      (if agg
         (let ((pos (position-if #'aggregate-p types))
               (agg (aggregate-agg agg))
               (typ (aggregate-type agg)))
            (logior (logand #b11110000 (ash (output-aggregate-type agg typ) 4))
                    (logand #b00001111 pos)))
         #b00000000)))

(defun output-properties (def)
   (letret (prop #b00000000)
      (when (definition-aggregate def)
         (setf prop (logior prop #b00000001)))
      (when (is-reverse-route-p def)
         (setf prop (logior prop #b00000100)))
      (when (is-route-p def)
         (setf prop (logior prop #b00000010)))
      (when (is-linear-p def)
         (setf prop (logior prop #b00001000)))
      (when (is-action-p def)
         (setf prop (logior prop #b00010000)))
		(when (is-reused-p def)
			(setf prop (logior prop #b00100000)))
		(when (definition-is-cyclical-p def)
			(setf prop (logior prop #b01000000)))
      (when (definition-is-thread-p def)
         (setf prop (logior prop #b10000000)))
      prop))

(defparameter *max-tuple-name* 32)
(defparameter *max-agg-info* 32)

(defmacro refill-up-to ((vec max) &body body)
   (alexandria:with-gensyms (pos new-pos)
      `(save-pos (,pos ,vec)
         ,@body
         (save-pos (,new-pos ,vec)
            (loop for i from (1+ (- ,new-pos ,pos)) to ,max
                  do (add-byte #b0 ,vec))))))

(defun output-tuple-name (name vec)
   (refill-up-to (vec *max-tuple-name*)
      (loop for x being the elements of name
         do (add-byte (char-code x) vec))))
         
(defun output-tuple-type-args (types vec)
	(dolist (typ (definition-arg-types types))
		(output-arg-type typ vec)))
         
(defun output-stratification-level (def)
   (let ((level (definition-get-tagged-option def :strat)))
      (assert (not (null level)))
      (coerce level '(unsigned-byte 8))))
      
(defun get-aggregate-remote (def)
   "Gets remote aggregate info from a definition (only applicable to aggregates)."
   (alexandria:when-let ((agg (definition-aggregate def)))
      (let ((mod (aggregate-mod agg)))
         (cond
            ((and *use-stratification* (aggregate-mod-is-input-p mod))
               (values (generate-inverse-name (aggregate-mod-io-name mod))
                       (aggregate-mod-includes-home-p mod)))
            ((and *use-stratification* (aggregate-mod-is-output-p mod))
               (values (aggregate-mod-io-name mod)
                       (aggregate-mod-includes-home-p mod)))))))
      
(defconstant +agg-local-aggregate-byte+         #b00000001)
(defconstant +agg-remote-aggregate-byte+        #b00000010)
(defconstant +agg-remote-aggregate-home-byte+   #b00000100)
(defconstant +agg-immediate-aggregate-byte+     #b00001000)
(defconstant +agg-unsafe-byte+                  #b00000000)

(defun output-aggregate-info (def vec)
   (refill-up-to (vec *max-agg-info*)
      (let ((agg (definition-aggregate def)))
               (when agg
                  (cond
                     ((definition-has-local-agg-p def)
                        (add-byte +agg-local-aggregate-byte+ vec)
                        (let ((level (definition-get-strata def)))
                           (assert level)
                           ;(format t "local agg ~a ~a~%" (definition-name def) (1 level))
                           (add-byte (1+ level) vec)))
                     (t
                        (let ((aggmod (aggregate-mod agg)))
                           (cond
                              ((or (aggregate-mod-is-input-p aggmod)
                                    (aggregate-mod-is-output-p aggmod))
                                 (multiple-value-bind (remote-pred use-home-p) (get-aggregate-remote def)
                                    (when remote-pred
                                       (add-byte (if use-home-p +agg-remote-aggregate-home-byte+ +agg-remote-aggregate-byte+) vec)
                                       (add-byte (lookup-def-id remote-pred) vec))))
                              ((aggregate-mod-is-immediate-p aggmod)
                                 (printdbg "THIS PROGRAM HAS IMMEDIATE AGGREGATES!")
                                 (add-byte +agg-immediate-aggregate-byte+ vec))
                              (t (printdbg "THIS PROGRAM HAS UNSAFE AGGREGATES!")
                                 (add-byte +agg-unsafe-byte+ vec))))))))))

(defun output-descriptors ()
   (do-definitions (:definition def :name name :types types :operation collect)
      (letret (vec (create-bin-array))
         (add-byte (output-properties def) vec) ; property byte
         (add-byte (output-aggregate types) vec) ; aggregate byte
         (add-byte (output-stratification-level def) vec)
         (let ((index (find-index-name name)))
          (cond
           (index
            (add-byte (1- (index-field index)) vec))
           (t
            (add-byte 0 vec))))
         (add-byte (length types) vec) ; number of args
         (output-tuple-type-args types vec) ; argument type information
         (output-tuple-name name vec) ;; predicate name
         (output-aggregate-info def vec) ;; aggregate info
      )))

(defun output-consts ()
	(let ((vec (create-bin-array)))
		(make-byte-code "consts" vec
			(output-instrs *consts-code* vec))))

(defun output-functions ()
	(printdbg "Processing functions")
	(loop for code in *function-code*
		collect (let ((vec (create-bin-array)))
						(make-byte-code "function" vec
							(output-instrs code vec)))))
   
(defparameter *total-written* 0)

(defun write-hexa (stream int) (incf *total-written*) (write-byte int stream))
(declaim (inline write-hexa))

(defun write-vec (stream vec)
   (write-sequence vec stream)
   (incf *total-written* (length vec)))
      
(defun write-short-stream (stream int)
   (let ((ls (output-int int)))
      (write-hexa stream (first ls))
      (write-hexa stream (second ls))))

(defun write-list-stream (stream bytes)
	(dolist (b bytes)
		(write-hexa stream b)))

(defun write-int-stream (stream int)
	(write-list-stream stream (output-int int)))

(defun write-int64-stream (stream int)
	(write-list-stream stream (output-int64 int)))

(defun write-string-stream (stream str)
   (loop for x being the elements of str
      do (write-hexa stream (char-code x))))

(defun write-float-stream (stream flt)
	(write-list-stream stream (output-float flt)))

(defun write-nodes (stream nodes)
   ;(when (zerop (number-of-nodes nodes))
   ;   (format t "WARNING: there are no nodes defined in this program~%"))
   (write-int-stream stream (number-of-nodes nodes))
   (iterate-nodes (fake-id real-id nodes)
      (write-int64-stream stream fake-id)
      (write-int64-stream stream real-id)))
		
(defun priority-order-bit (order)
	(case order
		(:asc #b00000001)
		(:desc #b00000000)))
		
;;
;; the following 3 functions output the main byte-code options
;;

(defun output-initial-priority (stream)
	"Writes priority information."
	(write-hexa stream 2)
	(write-hexa stream 1) ; float
	(let* ((static (get-priority-static))
          (byt (priority-order-bit (get-priority-order))))
		(when static
			(setf byt (logior byt #b00000010)))
		(write-hexa stream byt))
	(write-float-stream stream (get-initial-priority)))
		
(defun output-data-file-info (stream)
	(write-hexa stream 3))
			
(defun write-rules (stream)
   (write-int-stream stream (length *code-rules*))
   (loop for code-rule in *code-rules*
         do (let ((str (rule-string code-rule)))
               (write-int-stream stream (length str))
               (write-string-stream stream str))))

(defun output-all-rules (&optional (is-data-p nil))
	(printdbg "Processing rules...")
	(loop for code-rule in *code-rules*
			for count = 0 then (1+ count)
			collect
   			(let ((vec (create-bin-array))
			 			(code (rule-code code-rule)))
					(make-byte-code "rule" vec
						(if (and (= count 0) is-data-p)
								;; for data files just print the select by node instruction
								(let* ((iterate (second (rule-code code-rule)))
								  		 (select (find-if #'(lambda (instr) (eq (first instr) :select-node)) (iterate-instrs iterate))))
									(output-instrs `(,(make-vm-rule 0) ,select ,(make-return-derived)) vec))
								(output-instrs code vec))))))
				
(defparameter +meld-magic+ '(#x6d #x65 #x6c #x64 #x20 #x66 #x69 #x6c))

(defun do-output-header (stream)
	(printdbg "Processing header...")
	(dolist (magic +meld-magic+)
		(write-hexa stream magic))
	(write-int-stream stream *major-version*)
	(write-int-stream stream *minor-version*)
	(write-hexa stream (length *definitions*))
   (write-nodes stream *nodes*)
	(write-hexa stream (length *program-types*))
   (printdbg "Writing program types...")
	(loop for typ in *program-types*
			do (let ((bytes (type-to-bytes typ)))
					(write-list-stream stream bytes)))
	(write-int-stream stream (length *imported-predicates*))
	(do-imports *imported-predicates* (:imp imp :as as :from file)
		(write-int-stream stream (length imp))
		(write-vec stream (output-string imp))
		(write-int-stream stream (length as))
		(write-vec stream (output-string as))
		(write-int-stream stream (length file))
		(write-vec stream (output-string file)))
	(write-int-stream stream (length *exported-predicates*))
	(dolist (name *exported-predicates*)
		(write-int-stream stream (length name))
		(write-vec stream (output-string name))))

(defun do-output-descriptors (stream descriptors processes)
	(printdbg "Processing predicates...")
	(loop for vec-desc in descriptors
         for def in *definitions*
         do (let ((code (byte-code-find (definition-name def) processes)))
               (if code
                  (write-int-stream stream (byte-code-size code)) ; write code size first
                  (write-int-stream stream 0)))
         do (write-vec stream vec-desc)))

(defun do-output-string-constants (stream)
	(write-int-stream stream (length *output-string-constants*))
	(loop for str in *output-string-constants*
			for i from 0
			do (write-int-stream stream (length str))
			do (write-vec stream (output-string str))))
			
(defun do-output-consts (stream consts)
	(printdbg "Processing constants...")
	(write-int-stream stream (length *consts*))
	(do-constant-list *consts* (:type typ)
		(write-arg-type stream typ))
	(write-int-stream stream (byte-code-size consts))
	(write-byte-code stream consts))
	
(defun do-output-rules (stream rules)
	(printdbg "Processing rules...")
	(write-int-stream stream (length *code-rules*))
   (assert (= (length rules) (length *code-rules*)))
	(dolist2 (bc rules) (code-rule *code-rules*)
	 (let* ((ids (subgoal-ids code-rule)))
      (assert (> (byte-code-size bc) 0))
		(write-int-stream stream (byte-code-size bc))
		(write-byte-code stream bc)
		(write-int-stream stream (length ids))
		(dolist (id ids)
			(write-hexa stream id)))))
		
(defun do-output-functions (stream functions)
	(printdbg "Processing functions...")
	(write-int-stream stream (length functions))
	(dolist (fun functions)
		(write-int-stream stream (byte-code-size fun))
		(write-byte-code stream fun)))
		
(defconstant +max-extern-function-name+ 256)
(defconstant +max-extern-file-name+ 1024)
		
(defun do-output-externs (stream)
	(write-int-stream stream (length *externs*))
	(do-externs *externs* (:name name :extern ex :types types :ret-type ret)
		(write-int-stream stream (extern-id ex))
		(let ((len (length name)))
			(write-string-stream stream name)
			(loop for i from len upto (1- +max-extern-function-name+)
				do (write-hexa stream 0)))
		(loop for i from 1 upto +max-extern-file-name+
			do (write-hexa stream 0))
		(write-int64-stream stream 0)
		(write-int-stream stream (length types))
		(write-arg-type stream ret)
		(dolist (typ types)
			(write-arg-type stream typ))))
			
(defun do-output-data (stream)
	(do-output-header stream)
	(when (> (args-needed *ast*) 0)
		(error 'output-invalid-error :text (tostring "Data files cannot have arguments")))
	(when (> (length *consts*) 0)
		(error 'output-invalid-error :text (tostring "Cannot have constants in data files")))
	(when (> (length *functions*) 0)
		(error 'output-invalid-error :text (tostring "Cannot have functions in data files")))
	(write-hexa stream 0) ;; 0 args needed
	(write-rules stream)
	(let* ((*output-string-constants* nil)
			 (descriptors (output-descriptors))
			 (rules (output-all-rules t))
			 (functions (output-functions))
			 (consts (output-consts)))
		(do-output-string-constants stream)
		(do-output-consts stream consts)
		(do-output-functions stream functions)
		; write external definitions
		(do-output-externs stream)
		(do-output-descriptors stream descriptors nil)
		(output-data-file-info stream)
		;; write init-code
		(when (> (length rules) 1)
			(warn "Too many rules in data file"))
		(do-output-rules stream rules)))
	
(defun do-output-code (stream)
   (do-output-header stream)
	(write-hexa stream (args-needed *ast*))
   (write-rules stream)
   (let* ((*output-string-constants* nil)
			 (processes (output-processes))
          (descriptors (output-descriptors))
			 (rules (output-all-rules))
			 (consts (output-consts))
			 (functions (output-functions)))
		;; output strings
		(do-output-string-constants stream)
		; output constant code
		(do-output-consts stream consts)
		; write functions
		(do-output-functions stream functions)
		; write external definitions
		(do-output-externs stream)
		; output predicate descriptions
      (do-output-descriptors stream descriptors processes)
		; output global priority predicate, if any
		(output-initial-priority stream)
      (dolist (bc processes)
			(write-byte-code stream bc))
		(do-output-rules stream rules)))
   
(defmacro with-output-file ((stream file) &body body)
   `(with-open-file (,stream ,file
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      ,@body))

(defun output-code (file &key (write-ast nil) (write-code nil))
   (let ((*total-written* 0)
         (byte-file (concatenate 'string file ".m")))
      (with-open-file (stream byte-file
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create
                        :element-type '(unsigned-byte 8))
         (do-output-code stream))
      (when write-ast
         (with-output-file (stream (concatenate 'string file ".m.ast"))
            (format stream "~a~%" *ast*)))
      (when write-code
         (with-output-file (stream (concatenate 'string file ".m.code"))
            (format stream "~a" (print-vm))))))

(defmacro with-binary-file ((stream file) &body body)
   `(let ((*total-written* 0))
      (with-open-file (,stream ,file :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :element-type '(unsigned-byte 8))
         ,@body)))

(defun output-data-file (file)
	(let ((byte-file (concatenate 'string file ".md")))
      (with-binary-file (stream byte-file)
			(do-output-data stream))))
