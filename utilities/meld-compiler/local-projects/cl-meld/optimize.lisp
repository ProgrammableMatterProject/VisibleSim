
(in-package :cl-meld)

(defun matches-mv-host-to-reg-p (instr)
	(eq (first instr) :move-host-id-to-reg))
         
(defun matches-op-addr-equal-p (instr)
   (and (eq (first instr) :addr-equal)
        (reg-p (vm-op-dest instr))
        (reg-eq-p (vm-op-dest instr) (make-reg 2))
        (reg-p (vm-op-v1 instr))
        (reg-p (vm-op-v2 instr))
		  (reg-eq-p (vm-op-v1 instr) (make-reg 0))
		  (reg-eq-p (vm-op-v2 instr) (make-reg 1))))
		
(defun matches-mv-addr-reg-p (instr)
	(and (eq (first instr) :move-addr-to-reg)
		  (reg-eq-p (move-to instr) (make-reg 1))))
        
(defun matches-op-if-1-p (instr)
   (and (vm-if-p instr)
        (reg-eq-p (vm-if-reg instr) (make-reg 2))))
        
(defun add-instrs-to-node (hash node instrs)
   (multiple-value-bind (other-instrs found-p) (gethash node hash)
      (declare (ignore found-p))
      (setf (gethash node hash) (append other-instrs instrs))))

(defun get-target-node (op-instr) (addr-num (move-from op-instr)))

(defun select-node-init (start-instrs)
   (declare (optimize (speed 3) (safety 0)))
   (let ((hash (make-hash-table))
         (new-start nil)
         (ptr nil)
         (current start-instrs))
      (loop while (not (null current))
            do (cond
                  ((at-least-n-p current 4)
                     (cond
                        ((and (matches-mv-host-to-reg-p (nth 0 current))
										(matches-mv-addr-reg-p (nth 1 current))
										(matches-op-addr-equal-p (nth 2 current))
										(matches-op-if-1-p (nth 3 current)))
                           (add-instrs-to-node hash (get-target-node (nth 1 current)) (vm-if-instrs (nth 3 current)))
                           (setf current (drop-first-n current 4)))
                        (new-start
                           (setf (cdr ptr) current
                                 ptr current
                                 current (cdr current)
                                 (cdr ptr) nil))
                        (t
                           (setf new-start current
                                 ptr current
                                 current (cdr current)
                                 (cdr ptr) nil))))
                  (t
                     (cond
                        (new-start
                           (setf (cdr ptr) current
                                 current nil))
                        (t
                           (setf new-start current
                                 current nil))))))
      (values hash new-start)))
            
(defun make-vm-select-with-rules (hash)
   (letret (instr (make-vm-select-node))
		(set-vm-select-hash instr hash)))
		
(defun merge-select-instrs (new old)
	(if (null old)
		new
		(merge-vm-select-node new old)))
      
(defun optimize-init ()
   (let ((def (find-init-predicate *definitions*)))
      (assert (not (null def)))
      (with-definition def (:name init-name)
         (let ((init-proc (vm-find init-name)))
          (when init-proc
            (with-process init-proc (:instrs instrs :process proc)
            (multiple-value-bind (hash to-keep) (select-node-init instrs)
					(if (= (hash-table-count hash) 0)
						(setf (process-instrs proc) to-keep)
               	(let ((new-instr (make-vm-select-with-rules hash)))
                  	(setf (process-instrs proc) (cons new-instr to-keep))))))))))
	; do the same for rule code
	(let* ((init (first *code-rules*))
			 (iterate (second (rule-code init)))
			 (init-code (iterate-instrs iterate)))
		(assert (not (null init-code)))
		(multiple-value-bind (hash to-keep1) (select-node-init init-code)
			(if (= (hash-table-count hash) 0)
				(setf (iterate-instrs iterate) to-keep1)
				(let* ((new-instr (make-vm-select-with-rules hash))
					    (old-instr (find-if #'vm-select-node-p to-keep1))
						 (to-keep (remove old-instr to-keep1)))
					(setf new-instr (merge-select-instrs new-instr old-instr))
					(setf (iterate-instrs iterate) `(,(make-vm-remove (make-reg 0)) ,new-instr ,@(remove-if #'(lambda (x) (and (vm-remove-p x) (= 0 (reg-num (vm-remove-reg x))))) to-keep))))))))
                  
(defmacro iterate-code ((&key instrs proc) &body body)
   (alexandria:with-gensyms (name)
      `(do-definitions (:name ,name)
         (with-process (vm-find ,name) (:instrs ,instrs :process ,proc)
            ,@body))))
       
(defun optimize-return-instr-list (instrs)
  (loop for instr-list on instrs
     do (let ((instr (first instr-list)))
         (case (instr-type instr)
            (:iterate
               (optimize-return-instr-list (iterate-instrs instr)))
            (:if
               (optimize-return-instr-list (vm-if-instrs instr)))
            (:reset-linear
               (optimize-return-instr-list (vm-reset-linear-instrs instr)))
            (otherwise
               (when (and (instr-is-return-p instr)
                           (instr-is-return-p (second instr-list)))
                  ;; remove second return
                  (setf (rest instr-list) (rest (rest instr-list)))))))))
               
(defun optimize-returns ()
   (iterate-code (:instrs instrs :proc proc)
      (declare (ignore proc))
      (optimize-return-instr-list instrs)))

(defun optimize-code ()
   (unless *use-optimizations*
      (return-from optimize-code nil))
	(when (> (hash-table-count *nodes*) 0)
   	(optimize-init))
   (optimize-returns))
   
