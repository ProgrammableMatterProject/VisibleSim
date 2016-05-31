
(in-package :cl-meld)
         
(defun matches-op-host-p (instr)
   (and (vm-op-p instr)
        (reg-p (vm-op-dest instr))
        (reg-eq-p (vm-op-dest instr) (make-reg 0))
        (vm-host-id-p (vm-op-v1 instr))
        (vm-addr-p (vm-op-v2 instr))
        (op-eq-p (vm-op-op instr) :addr-equal)))
        
(defun matches-op-if-0-p (instr)
   (and (vm-if-p instr)
        (reg-eq-p (vm-if-reg instr) (make-reg 0))))
        
(defun add-instrs-to-node (hash node instrs)
   (multiple-value-bind (other-instrs found-p) (gethash node hash)
      (declare (ignore found-p))
      (setf (gethash node hash) (append other-instrs instrs))))

(defun get-target-node (op-instr) (addr-num (vm-op-v2 op-instr)))

(defun select-node-init (start-instrs)
   (declare (optimize (speed 3) (safety 0)))
   (let ((hash (make-hash-table))
         (new-start nil)
         (ptr nil)
         (current start-instrs))
      (loop while (not (null current))
            do (cond
                  ((at-least-n-p current 2)
                     (cond
                        ((and (matches-op-host-p (nth 0 current))
                              (matches-op-if-0-p (nth 1 current)))
                           (add-instrs-to-node hash (get-target-node (nth 0 current)) (vm-if-instrs (nth 1 current)))
                           (setf current (drop-first-n current 2)))
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
      (iterate-hash (hash node instrs)
         (vm-select-node-push instr node instrs))))
      
(defun optimize-init ()
   (let ((def (find-init-predicate *definitions*)))
      (assert (not (null def)))
      (with-definition def (:name init-name)
         (with-process (vm-find init-name) (:instrs instrs :process proc)
            (multiple-value-bind (hash to-keep) (select-node-init instrs)
					(if (= (hash-table-count hash) 0)
						(setf (process-instrs proc) to-keep)
               	(let ((new-instr (make-vm-select-with-rules hash)))
                  	(setf (process-instrs proc) (cons new-instr to-keep))))))))
	; do the same for rule code
	(let* ((init (first *code-rules*))
			 (iterate (second (rule-code init)))
			 (init-code (iterate-instrs iterate)))
		(assert (not (null init-code)))
		(multiple-value-bind (hash to-keep) (select-node-init init-code)
			(if (= (hash-table-count hash) 0)
				(setf (iterate-instrs iterate) to-keep)
				(let ((new-instr (make-vm-select-with-rules hash)))
					(setf (iterate-instrs iterate) (cons new-instr to-keep)))))))
                  
(defmacro iterate-code ((&key instrs proc) &body body)
   (with-gensyms (name)
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

(defun is-move-followed-by-iter (ls)
	(let (found-move register)
		(loop for instr in ls
				do (case (instr-type instr)
						(:move
							(cond
								(found-move
									(return-from is-move-followed-by-iter nil))
								((and (tuple-p (move-from instr))
										(reg-p (move-to instr)))
									(setf found-move t)
									(when (null register)
										(setf register (move-to instr)))
									(unless (reg-eq-p register (move-to instr))
										(return-from is-move-followed-by-iter nil)))
								(t
									(return-from is-move-followed-by-iter nil))))
						(:iterate
							(unless found-move
								(return-from is-move-followed-by-iter nil))
							(setf found-move nil))
						(:return )))
		register))
	
(defun remove-moves-except-first (instrs reg)
	(loop for instr-list on instrs
			do (let ((instr2 (second instr-list)))
					(case (instr-type instr2)
						(:move
							(when (and (reg-eq-p reg (move-to instr2))
											(tuple-p (move-from instr2)))
								(setf (rest instr-list) (rest (rest instr-list)))))
						(otherwise)))))
			
(defun optimize-multiple-move-0s-list (ls)
	(awhen (is-move-followed-by-iter ls)
		(remove-moves-except-first ls it)))

(defun optimize-multiple-move-0s ()
	(iterate-code (:instrs instrs :proc proc)
		(declare (ignore proc))
		(optimize-multiple-move-0s-list instrs)))
	
(defun optimize-code ()
   (unless *use-optimizations*
      (return-from optimize-code nil))
	(when (> (hash-table-count *nodes*) 0)
   	(optimize-init))
   (optimize-returns)
	; this is buggy when we have subgoals with simple conditions
	; (optimize-multiple-move-0s)
	)
   
