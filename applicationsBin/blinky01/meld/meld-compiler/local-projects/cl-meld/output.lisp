
(in-package :cl-meld)

(define-condition output-invalid-error (error)
   ((text :initarg :text :reader text)))

(defparameter *output-string-constants* nil)
(defun push-string-constant (str)
	"Adds string constant to database if not found and returns the string integer code."
	(let ((pos (position str *output-string-constants* :test #'string-equal)))
		(if pos
			pos
			(progn
				(push-end str *output-string-constants*)
				(1- (length *output-string-constants*))))))
				
(defparameter *program-types* nil)

(defmacro with-memory-stream (s &body body)
   `(let ((,s (make-in-memory-output-stream)))
      ,@body
      s))
   
(defun output-int (int)
   (loop for i upto 3
      collect (ldb (byte 8 (* i 8)) int)))
(defun output-int64 (int)
	(loop for i upto 7
		collect (ldb (byte 8 (* i 8)) int)))
(defun output-float32 (flt) (output-int (encode-float32 (coerce flt 'float))))
(defun output-float64 (flt) (output-int64 (encode-float64 (coerce flt 'float))))
(defun output-float (flt) (output-float64 flt))
(defun output-string (str)
	(map 'list #'char-code str))

(defun output-value (val)
   (cond
		((vm-bool-p val) (list #b001100 (list (if (vm-bool-val val) #b1 #b0))))
      ((vm-int-p val) (list #b000001 (output-int (vm-int-val val))))
      ((vm-float-p val) (list #b000000 (output-float (vm-float-val val))))
		((vm-string-constant-p val)
			(let* ((str (vm-string-constant-val val))
					 (code (push-string-constant str)))
				(list #b000110 (output-int code))))
      ((vm-addr-p val) (list  #b000101 (output-int (vm-addr-num val))))
      ((vm-ptr-p val) (list #b001011 (output-int64 (vm-ptr-val val))))
		((vm-host-id-p val) (list #b000011))
      ((vm-nil-p val) (list #b000100))
      ((vm-world-p val) (output-value (make-vm-int (number-of-nodes *nodes*))))
      ((tuple-p val) (list #b011111))
		((vm-pcounter-p val) (list #b001010))
		((vm-stack-p val) (list #b001001 (list (vm-stack-offset val))))
      ((reg-p val) (list (logior #b100000 (logand #b011111 (reg-num val)))))
      ((reg-dot-p val) (list #b000010 (list (reg-dot-field val) (reg-num (reg-dot-reg val)))))
		((vm-argument-p val) (list #b00000111 (list (vm-argument-id val))))
		((vm-constant-p val) (list #b00001000 (output-int (lookup-const-id (vm-constant-name val)))))
      (t (error 'output-invalid-error :text (tostring "Invalid expression value: ~a" val)))))

(defmacro add-byte (b vec) `(vector-push-extend ,b ,vec))
(defun add-bytes (vec ls)
	(dolist (b ls) (add-byte b vec)))

(defmacro do-vm-values (vec vals &rest instrs)
   (labels ((map-value (i) (case i (1 'first-value) (2 'second-value) (3 'third-value)))
            (map-value-bytes (i) (case i (1 'first-value-bytes) (2 'second-value-bytes) (3 'third-value-bytes))))
      (let* ((i 0)
             (vals-code (mapcar #'(lambda (val) `(output-value ,val)) vals))
             (instrs-code `(progn ,@(mapcar #'(lambda (instr) `(add-byte ,instr ,vec)) instrs)
                              ,@(loop for i from 1 upto (length vals)
                                    collect `(dolist (bt ,(map-value-bytes i))
                                                (add-byte bt ,vec))))))
         (reduce #'(lambda (all val)
                     (incf i)
                     `(let* ((value ,val)
                           (,(map-value i) (first value))
                           (,(map-value-bytes i) (second value)))
                              ,all))
                  vals-code :initial-value instrs-code :from-end nil))))

(defun get-op-byte (op)
   (case op
      (:float-not-equal #b00000)
      (:int-not-equal #b00001)
      (:float-equal #b00010)
      (:int-equal #b00011)
      (:float-lesser #b00100)
      (:int-lesser #b00101)
      (:float-lesser-equal #b00110)
      (:int-lesser-equal #b00111)
      (:float-greater #b01000)
      (:int-greater #b01001)
      (:float-greater-equal #b01010)
      (:int-greater-equal #b01011)
      (:float-mod #b01100)
      (:int-mod #b01101)
      (:float-plus #b01110)
      (:int-plus #b01111)
      (:float-minus #b10000)
      (:int-minus #b10001)
      (:float-mul #b10010)
      (:int-mul #b10011)
      (:float-div #b10100)
      (:int-div #b10101)
      (:addr-not-equal #b10110)
		(:addr-equal #b10111)
		(:addr-greater #b11000)
		(:bool-or #b11001)
      (otherwise (error 'output-invalid-error :text (tostring "Unknown operation to convert ~a" op)))))
      
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

(defun output-match (match vec fs)
   (let* ((val (output-value (match-right match)))
          (val-byte (first val))
          (val-bytes (second val))
          (reg-dot (match-left match))
          (field (reg-dot-field reg-dot)))
      (add-byte field vec)
      (add-byte (logior fs val-byte) vec)
      (dolist (by val-bytes)
         (add-byte by vec))))
(defun output-list-bytes (vec ls)
	(dolist (b ls)
      (add-byte b vec)))

(defun output-matches (matches vec)
   (cond
      ((null matches)
         (add-byte #b00000000 vec)
         (add-byte #b11000000 vec))
      ((one-elem-p matches) (output-match (first matches) vec #b01000000))
      (t (output-match (first matches) vec #b00000000)
         (output-matches (rest matches) vec))))

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
   (with-gensyms (pos)
      `(save-pos (,pos ,vec)
          ,@body
         (write-offset ,vec (- (length ,vec) ,pos) (+ ,pos ,jump-many)))))
         
(defun output-axiom-argument (arg vec subgoal)
	(cond
		((addr-p arg) (output-list-bytes vec (output-int (addr-num arg))))
		((int-p arg)
			(if (type-float-p (expr-type arg))
				(output-list-bytes vec (output-float (int-val arg)))
				(output-list-bytes vec (output-int (int-val arg)))))
		((float-p arg) (output-list-bytes vec (output-float (float-val arg))))
		((string-constant-p arg) (output-list-bytes vec (output-int (push-string-constant (string-constant-val arg)))))
		((nil-p arg) (add-byte #b0 vec))
		((cons-p arg)
			(add-byte #b1 vec)
			(output-axiom-argument (cons-head arg) vec subgoal)
			(output-axiom-argument (cons-tail arg) vec subgoal))
		(t (error 'output-invalid-error :text (tostring "don't know how to output this subgoal: ~a" subgoal)))))

(defparameter *value-mask* #b00111111)
(defparameter *reg-mask* #b00011111)
(defparameter *op-mask* #b00011111)
(defparameter *tuple-id-mask* #b01111111)
(defparameter *extern-id-mask* #b01111111)

(defun iterate-options-bytes (iter)
	(let ((opt #b00000000)
			(snd #b00000000))
		(when (iterate-random-p iter)
			(setf opt (logior opt #b00000001)))
		(when (iterate-to-delete-p iter)
			(setf opt (logior opt #b00000010)))
		(when (iterate-min-p iter)
			(setf snd (iterate-min-arg iter))
			(setf opt (logior opt #b00000100)))
		(values opt snd)))

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
				(let ((axioms (vm-new-axioms-subgoals instr)))
					(do-subgoals axioms (:name name :args args :subgoal axiom)
						(add-byte (logand *tuple-id-mask* (lookup-def-id name)) vec)
						(dolist (arg args)
							(output-axiom-argument arg vec axiom))))))
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
      (:op (let ((op (get-op-byte (vm-op-op instr))))
               (do-vm-values vec ((vm-op-v1 instr) (vm-op-v2 instr) (vm-op-dest instr))
                           #b11000000
                           (logand *value-mask* first-value)
                           (logand *value-mask* second-value)
                           (logand *value-mask* third-value)
                           (logand *op-mask* op))))
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
      (:call (let ((extern-id (lookup-external-function-id (vm-call-name instr)))
                   (args (vm-call-args instr)))
               (add-byte #b00100000 vec)
               (add-byte (logand *extern-id-mask* extern-id) vec)
               (add-byte (length args) vec)
               (add-byte (logand *reg-mask* (reg-to-byte (vm-call-dest instr))) vec)
               (dolist (arg args)
                  (let ((res (output-value arg)))
                     (add-byte (first res) vec)
                     (add-bytes vec (second res))))))
		(:calle (let ((extern-id (lookup-custom-external-function-id (vm-calle-name instr)))
						 (args (vm-calle-args instr)))
						(add-byte #b00011011 vec)
						(add-byte (logand *extern-id-mask* extern-id) vec)
						(add-byte (length args) vec)
						(add-byte (logand *reg-mask* (reg-to-byte (vm-calle-dest instr))) vec)
						(dolist (arg args)
							(let ((res (output-value arg)))
								(add-byte (first res) vec)
								(add-bytes vec (second res))))))
      (:if (let ((reg-b (reg-to-byte (vm-if-reg instr))))
             (write-jump vec 2
               (add-byte #b01100000 vec)
               (add-byte (logand *reg-mask* reg-b) vec)
               (jumps-here vec)
               (output-instrs (vm-if-instrs instr) vec))))
      (:iterate (write-jump vec 4
                  (add-byte #b10100000 vec)
                  (add-byte (lookup-def-id (iterate-name instr)) vec)
						(multiple-value-bind (b1 b2) (iterate-options-bytes instr)
							(add-byte b1 vec)
							(add-byte b2 vec))
                  (jumps-here vec)
                  (output-matches (iterate-matches instr) vec)
                  (output-instrs (iterate-instrs instr) vec)
                  (add-byte #b00000001 vec)))
		(:struct-val
			(do-vm-values vec ((vm-struct-val-from instr) (vm-struct-val-to instr))
				#b00011100
				(vm-struct-val-idx instr)
				(logand *value-mask* first-value)
				(logand *value-mask* second-value)))
		(:struct
			(do-vm-values vec ((vm-make-struct-to instr))
				#b00011101
				(lookup-type-id (vm-make-struct-type instr))
				(logand *value-mask* first-value)))
      (:move
				(do-vm-values vec ((move-from instr) (move-to instr))
                #b00110000
                (logand *value-mask* first-value)
                (logand *value-mask* second-value)))
      (:move-nil (do-vm-values vec ((move-nil-to instr))
                  #b01110000
                  (logand *value-mask* first-value)))
      (:test-nil (do-vm-values vec ((vm-test-nil-place instr) (vm-test-nil-dest instr))
                     #b00000011
                     (logand *value-mask* first-value)
                     (logand *value-mask* second-value)))
      (:not (do-vm-values vec ((vm-not-place instr) (vm-not-dest instr))
               #b00000111
               (logand *value-mask* first-value)
               (logand *value-mask* second-value)))
      (:cons (do-vm-values vec ((vm-cons-head instr) (vm-cons-tail instr) (vm-cons-dest instr))
                  #b00000100
						(lookup-type-id (vm-cons-type instr))
                  (logand *value-mask* first-value)
                  (logand *value-mask* second-value)
                  (logand *value-mask* third-value)))
      (:head (do-vm-values vec ((vm-head-cons instr) (vm-head-dest instr))
                  #b00000101
						(lookup-type-id (vm-head-type instr))
                  (logand *value-mask* first-value)
                  (logand *value-mask* second-value)))
      (:tail (do-vm-values vec ((vm-tail-cons instr) (vm-tail-dest instr))
                  #b000000110
						(lookup-type-id (vm-tail-type instr))
                  (logand *value-mask* first-value)
                  (logand *value-mask* second-value)))
      (:convert-float (do-vm-values vec ((vm-convert-float-place instr) (vm-convert-float-dest instr))
                        #b00001001
                        (logand *value-mask* first-value)
                        (logand *value-mask* second-value)))
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
                                    do (when-let ((offset (gethash i hash)))
                                          ;; offset is always one more, since when 0 it means there is
                                          ;; no code for the corresponding node
                                          (write-offset vec (1+ offset) (+ start size-header pos)))
                                    do (when-let ((write-end (gethash i end-hash)))
                                          (write-offset vec (- end-select (1- write-end)) write-end)))
                              (write-offset vec (- end-select (1- start)) start)))))
      (:return-select (add-byte #b00001011 vec) (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec) (add-byte 0 vec))
      (:colocated
         (do-vm-values vec ((vm-colocated-first instr) (vm-colocated-second instr))
            #b00001100
            (logand *value-mask* first-value)
            (logand *value-mask* second-value)
            (logand *reg-mask* (reg-to-byte (vm-colocated-dest instr)))))
      (:rule
         (add-byte #b00010000 vec)
			(output-list-bytes vec (output-int (vm-rule-id instr))))
      (:rule-done
         (add-byte #b00010001 vec))
		(:new-node
			(add-byte #b00010011 vec)
			(add-byte (logand *reg-mask* (reg-num (vm-new-node-reg instr))) vec))
      (:delete (let* ((filters (vm-delete-filter instr)))
                  (add-byte #b00001101 vec)
                  (add-byte (logand *tuple-id-mask* (lookup-def-id (vm-delete-name instr))) vec)
                  (add-byte (length filters) vec)
                  (dolist (filter filters)
                     (let* ((ind (car filter))
                            (val (cdr filter))
                            (res (output-value val)))
                        (add-byte (1- ind) vec)
                        (add-byte (first res) vec)
                        (add-bytes vec (second res))))))
      (otherwise (error 'output-invalid-error :text (tostring "Unknown instruction to output ~a" instr)))))
                
(defun output-instrs (ls vec)
   (dolist (instr ls)
      (output-instr instr vec)))
                             
(defun output-processes ()
   (do-processes (:name name :instrs instrs :operation collect)
      (letret (vec (create-bin-array))
         (output-instrs instrs vec))))

(defun lookup-type-id (typ)
	(let ((ret (position typ *program-types* :test #'equal)))
		(assert (integerp ret))
		ret))

(defun type-to-bytes (typ)
	(cond
		((symbolp typ)
			(case typ
      		(:type-int '(#b0000))
      		(:type-float '(#b0001))
      		(:type-addr '(#b0010))
				(:type-bool '(#b0101))
				(:type-string '(#b1001))
				(otherwise (error 'output-invalid-error :text (tostring "invalid arg type: ~a" typ)))))
		((type-list-p typ)
			(let* ((sub (type-list-element typ))
					 (bytes (type-to-bytes sub)))
				`(,#b0011 ,@bytes)))
		((type-struct-p typ)
			(let ((ls (type-struct-list typ)))
				(let ((x `(,#b0100 ,(length ls) ,@(loop for ty in ls append (type-to-bytes ty)))))
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
      prop))

(defparameter *max-tuple-name* 32)
(defparameter *max-agg-info* 32)

(defmacro refill-up-to ((vec max) &body body)
   (with-gensyms (pos new-pos)
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
   (when-let ((agg (definition-aggregate def)))
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
         (add-byte (length types) vec) ; number of args
         (output-tuple-type-args types vec) ; argument type information
         (output-tuple-name name vec) ;; predicate name
         (output-aggregate-info def vec) ;; aggregate info
      )))

(defun output-consts ()
	(letret (vec (create-bin-array))
		(output-instrs *consts-code* vec)))

(defun output-functions ()
	(printdbg "Processing functions")
	(loop for code in *function-code*
		collect (letret (vec (create-bin-array))
						(output-instrs code vec))))
   
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
   (when (zerop (number-of-nodes nodes))
      (format t "WARNING: there are no nodes defined in this program~%"))
   (write-int-stream stream (number-of-nodes nodes))
   (iterate-nodes (fake-id real-id nodes)
      (write-int-stream stream fake-id)
      (write-int-stream stream real-id)))
		
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
	(let* ((order (find-if #'priority-order-p *priorities*))
			 (static (find-if #'priority-static-p *priorities*))
			 (byt (priority-order-bit (if order (priority-order order) :desc))))
		(when static
			(setf byt (logior byt #b00000010)))
		(write-hexa stream byt))
	(let ((prio (get-initial-priority)))
		(write-float-stream stream (if prio prio 0.0))))
		
(defun output-data-file-info (stream)
	(write-hexa stream 3))

(defun get-initial-priority ()
	(let ((found (find-if #'initial-priority-p *priorities*)))
		(when found
			(initial-priority-value found))))
			
(defun write-rules (stream)
   (write-int-stream stream (1+ (length *clauses*)))
	(let ((init-rule-str "init -o axioms"))
		(write-int-stream stream (length init-rule-str))
		(write-string-stream stream init-rule-str))
   (do-rules (:clause clause)
      (let ((str (clause-to-string clause)))
         (write-int-stream stream (length str))
         (write-string-stream stream str))))

(defun output-all-rules (&optional (is-data-p nil))
	(printdbg "Processing rules...")
	(loop for code-rule in *code-rules*
			for count = 0 then (1+ count)
			collect
   			(let ((vec (create-bin-array))
			 			(code (rule-code code-rule)))
					(if (and (= count 0) is-data-p)
						;; for data files just print the select by node instruction
						(let* ((iterate (second (rule-code code-rule)))
								(select (find-if #'(lambda (instr) (eq (first instr) :select-node)) (iterate-instrs iterate))))
							(output-instrs `(,(make-vm-rule 0) ,select ,(make-return-derived)) vec))
						(output-instrs code vec))
					vec)))
				
(defparameter +meld-magic+ '(#x6d #x65 #x6c #x64 #x20 #x66 #x69 #x6c))

(defun add-type-to-typelist (types new)
	(if (member new types :test #'equal)
		types
		(push new types)))
		
(defun collect-all-types ()
	(setf *program-types* nil)
	(do-definitions (:types types)
		(dolist (ty types)
			(setf *program-types* (add-type-to-typelist *program-types* (arg-type ty)))))
	(do-constant-list *consts* (:type typ)
		(setf *program-types* (add-type-to-typelist *program-types* typ)))
	(do-externs *externs* (:types types :ret-type ret)
		(setf *program-types* (add-type-to-typelist *program-types* ret))
		(dolist (typ types)
			(setf *program-types* (add-type-to-typelist *program-types* typ))))
	(do-functions *functions* (:ret-type typ :args args)
		(setf *program-types* (add-type-to-typelist *program-types* typ))
		(dolist (arg args)
			(setf *program-types* (add-type-to-typelist *program-types* (var-type arg))))))

(defun do-output-header (stream)
	(printdbg "Processing header...")
	(dolist (magic +meld-magic+)
		(write-hexa stream magic))
	(write-int-stream stream *major-version*)
	(write-int-stream stream *minor-version*)
	(write-hexa stream (length *definitions*))
   (write-nodes stream *nodes*)
	(collect-all-types)
	(write-hexa stream (length *program-types*))
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
         for vec-proc in processes
         do (write-int-stream stream (length vec-proc)) ; write code size first
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
	(write-int-stream stream (length consts))
	(write-vec stream consts))
	
(defun do-output-rules (stream rules)
	(printdbg "Processing rules...")
	(write-int-stream stream (length *code-rules*))
	(dolist2 (vec rules) (code-rule *code-rules*)
	 (let* ((ids (subgoal-ids code-rule))
			 (pers-p (persistent-p code-rule)))
		(write-int-stream stream (length vec))
		
		(write-vec stream vec)
		
		(if pers-p
			(write-hexa stream 1)
			(write-hexa stream 0))
		
		(write-int-stream stream (length ids))
		(dolist (id ids)
			(write-hexa stream id))
		)))
		
(defun do-output-functions (stream functions)
	(printdbg "Processing functions...")
	(write-int-stream stream (length functions))
	(dolist (fun functions)
		(write-int-stream stream (length fun))
		(write-vec stream fun)))
		
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
			 (processes (loop for desc in descriptors
								collect (list)))
			 (rules (output-all-rules t))
			 (functions (output-functions))
			 (consts (output-consts)))
		(do-output-string-constants stream)
		(do-output-consts stream consts)
		(do-output-functions stream functions)
		; write external definitions
		(do-output-externs stream)
		(do-output-descriptors stream descriptors processes)
		(output-data-file-info stream)
		;; write init-code
		(when (> (length rules) 1)
			(warn "Too many rules in data file"))
		(do-output-rules stream rules)
	))
	
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
      (dolist (vec processes) (write-vec stream vec))
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

(defun output-data-file (file)
	(let ((*total-written* 0)
			(byte-file (concatenate 'string file ".md")))
		(with-open-file (stream byte-file
								:direction :output
								:if-exists :supersede
								:if-does-not-exist :create
								:element-type '(unsigned-byte 8))
			(do-output-data stream))))
