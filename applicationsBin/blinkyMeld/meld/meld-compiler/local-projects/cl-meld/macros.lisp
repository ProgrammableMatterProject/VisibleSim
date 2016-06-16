(in-package :cl-meld)

(defun build-bind (var body)
   (if var `((,var ,body))))

;; Several macro utilities

(defmacro mac (expr)
 `(pprint (macroexpand-1 ',expr)))
 
(defmacro tostring (&rest args)
   (alexandria:with-gensyms (stream)
      `(with-output-to-string (,stream)
         (format ,stream ,@args))))
	  
(defmacro format-keyword (control &rest arguments)
 `(alexandria:format-symbol "KEYWORD" ,control ,@arguments))
		 
(defmacro output-symbol (control &rest arguments)
 `(intern (string-upcase (tostring ,control ,@arguments))))
      
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
	   (progn ,@body)))
         
(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
    (let ((cl1 (car clauses)))
	   (if (eq (car cl1) t)
	      `(progn ,@(cdr cl1))
	      (let ((sym (gensym)))
            `(let ((,sym ,(car cl1)))
	            (if ,sym
	               (let ((it ,sym)) ,@(cdr cl1))
	                  (acond ,@(cdr clauses)))))))))
	   
(defmacro ensure-bool (form) `(if ,form t nil))
 
(defmacro on-top-level (&rest forms)
   `(eval-when (:compile-toplevel :load-toplevel :execute)
         ,@forms))

(defmacro with-var (var &body body)
   `(let (,var)
      ,@body
      ,var))

(defmacro with-ret (var &body body)
   `(with-var ,var
      ,@body
      ,var))

(defmacro letret ((var form) &body body)
   `(let ((,var ,form))
      ,@body
      ,var))

(defmacro always-ret (form &body body)
   (alexandria:with-gensyms (ret)
      `(letret (,ret ,form)
         ,@body)))

(defmacro iff (test thing)
   (alexandria:with-gensyms (ret)
      `(let ((,ret ,thing))
         (when (,test ,ret) ,ret))))
      
(defmacro dolist2 ((el1 ls1) (el2 ls2) &body body)
   `(loop for ,el1 in ,ls1
          for ,el2 in ,ls2
          do ,@body))
          
(defmacro with-optional-counter (id &body body)
   (if (null id)
      body
      `(let ((,id 0))
         ,@body
         (incf ,id))))
      
(defmacro dolist-filter ((el list filter &optional id) &body body)
   `(let (,@(if id `((,id 0))))
      (dolist (,el ,list)
         (when (,filter ,el)
            ,@(if id `((incf ,id)))
            ,@body))))
         
(defmacro dolist-count ((el list &optional id) &body body)
   `(let (,@(if id `((,id 0))))
      (dolist (,el ,list)
         ,@(if id `((incf ,id)))
         ,@body)))
         
(defmacro loop-list ((el list &key (id nil) (operation 'do)) &body body)
   `(loop for ,el in ,list
          ,@(when id `(for ,id upto (length ,list)))
         ,operation ,@body))
         
(defmacro with-symbol ((symb expr) &body body)
   `(symbol-macrolet ((,symb ,expr))
      ,@body))
      
(defmacro with-car ((cr cons) &body body)
   `(with-symbol (,cr (car ,cons))
      ,@body))
      
(defmacro with-cdr ((cr cons) &body body)
   `(with-symbol (,cr (cdr ,cons))
      ,@body))
         
(defmacro loop-cons ((el list) &body body)
   `(loop for ,el on ,list
          do (progn ,@body)))
         
(defmacro loop-cons-car ((el-car list) &body body)
   (alexandria:with-gensyms (el)
      `(loop-cons (,el ,list)
         (with-car (,el-car ,el)
            ,@body))))
      
(defmacro equal-or (ls &body rest)
   `(or ,@(mapcar #'(lambda (el) `(equal ',el ,ls)) rest)))
   
(defmacro iterate-hash ((hash key val &key (op 'do)) &body body)
   `(loop for ,key being the hash-keys of ,hash
          using (hash-value ,val)
          ,op ,@body))
          
(defmacro in-directory (new-dir &body body)
   "Executes a piece of code inside directory 'new-dir' and goes back to the initial directory."
   (alexandria:with-gensyms (old-dir)
      `(let ((,old-dir *default-pathname-defaults*))
         (unwind-protect
            (progn
               (setf *default-pathname-defaults* ,new-dir)
               ,@body)
            (setf *default-pathname-defaults* ,old-dir)))))

(defmacro define-with (name list-keywords &key use-self-p)
   (alexandria:with-gensyms (thing)
      (let ((with-name (output-symbol "with-~a" name))
            (self (if use-self-p `((build-bind ,`,name ,`,thing)) nil))
            (build-binds (loop for kw in list-keywords
                           collect `(build-bind ,kw `(,',(output-symbol "~a-~a" name kw) ,,thing)))))                  
         `(on-top-level
            (defmacro ,with-name (,thing (&key ,@list-keywords ,@(if use-self-p `(,name) nil)) &body code-body)
               `(let (,@,@(loop for a in build-binds
                              collect a)
                     ,@,@self)
                  ,@code-body))))))
                  
(defmacro define-loop (name with-name list-name list-keywords &key filter-p)
   (let* ((key-keys (loop for kw in list-keywords
                       append
                        `(,(format-keyword "~a" kw) ,kw)))
          (base ``(,',with-name ,el (,,@key-keys)
                              ,@code-body)))
   `(on-top-level
      (defmacro ,list-name (ls (&key (id nil) (,name nil) (operation 'do)
                  ,@(mapcar #'(lambda (kw) `(,kw nil)) list-keywords)) &body code-body)
          (alexandria:with-gensyms (el)
             `(loop-list (,el ,,(if filter-p ``(filter ,',filter-p ,ls) ``,ls) :id ,id :operation ,operation)
                (let (,@(build-bind ,name el))
                  ,,base)))))))
       
(defmacro define-term-construct (name filter-p list-keywords)
   (let* ((do-list-name (output-symbol "do-~a-list" name))
          (with-name (output-symbol "with-~a" name))
          (do-filter-name (output-symbol "do-~as" name))
          (name-keyword (format-keyword "~a" name)))
   `(on-top-level
      (define-with ,name ,list-keywords)
      (define-loop ,name ,with-name ,do-list-name ,list-keywords :filter-p ,filter-p)
      (defmacro ,do-filter-name (clause (&key (id nil) (,name nil) (operation 'do)
               ,@(mapcar #'(lambda (kw) `(,kw nil)) list-keywords)) &body code-body)
         (let ((arg-list `(,',name-keyword ,,name :operation ,operation :id ,id
                           ,,@(loop for kw in list-keywords
                              append `(,(format-keyword "~a" kw) ,kw)))))
            `(cond
               ((clause-p ,clause)
                  (,',do-list-name (clause-body ,clause) ,arg-list ,@code-body)
                  (,',do-list-name (clause-head ,clause) ,arg-list ,@code-body))
               (t (,',do-list-name ,clause ,arg-list ,@code-body))))))))

;; Meld related code

(define-with definition (name types options num-args) :use-self-p t)
(define-loop definition with-definition do-definitions-list (name types options))
      
(defmacro do-definitions ((&key definition name types options id (operation 'do)) &body body)
   `(do-definitions-list *definitions* (:name ,name :types ,types :options ,options :definition ,definition
                                       :id ,id :operation ,operation)
      ,@body))
            
(defmacro par-collect-definitions ((&key definition name types options) &body body)
   (alexandria:with-gensyms (el)
      `(par-mapcar #'(lambda (,el)
                        (with-definition ,el (:name ,name :types ,types :options ,options :definition ,definition)
                           ,@body))
                  *definitions*)))
                   
(defmacro do-node-definitions ((&key definition name types options id (operation 'do))
                                &body body)
   `(do-definitions-list *node-definitions* (:name ,name :types ,types :options ,options :definition ,definition
                                       :operation ,operation :id ,id)
      ,@body))
         
(define-with extern (name ret-type types))
(define-loop extern with-extern do-externs (name ret-type types))

(define-with clause (head body options) :use-self-p t)
(define-loop clause with-clause do-clauses (head body options))

(define-with function (name args ret-type body))
(define-loop function with-function do-functions (name args ret-type body))

(define-with import (imp as from))
(define-loop import with-import do-imports (imp as from))
     
(defmacro do-rules ((&key head body clause options id (operation 'do)) &body rest)
   `(do-clauses *clauses* (:head ,head :body ,body :clause ,clause
                           :options ,options :id ,id :operation ,operation)
      ,@rest))
      
(defmacro do-all-rules ((&key head body clause options) &body rest)
   `(progn
      (do-rules (:head ,head :body ,body :clause ,clause :options ,options)
         ,@rest)))
         
(defmacro do-node-var-axioms ((&key head body clause options id (operation 'do)) &body rest)
   `(do-clauses *node-var-axioms* (:head ,head :body ,body :clause ,clause
                           :options ,options :id ,id :operation ,operation)
      ,@rest))

(defmacro do-thread-var-axioms ((&key head body clause options id (operation 'do)) &body rest)
   `(do-clauses *thread-var-axioms* (:head ,head :body ,body :clause ,clause
                           :options ,options :id ,id :operation ,operation)
      ,@rest))

(defmacro do-all-var-axioms ((&key head body clause options) &body rest)
   `(progn
      (do-node-var-axioms (:head ,head :body ,body :clause ,clause :options ,options)
         ,@rest)
      (do-thread-var-axioms (:head ,head :body ,body :clause ,clause :options ,options)
         ,@rest)))

(defmacro do-node-const-axioms ((&key subgoal) &body rest)
	(alexandria:with-gensyms (head)
		`(do-clauses *node-const-axioms* (:head ,head)
			(do-subgoals ,head (:subgoal ,subgoal)
				,@rest))))

(defmacro do-thread-const-axioms ((&key subgoal) &body rest)
	(alexandria:with-gensyms (head)
		`(do-clauses *thread-const-axioms* (:head ,head)
			(do-subgoals ,head (:subgoal ,subgoal)
				,@rest))))

(defmacro do-all-const-axioms ((&key subgoal) &body rest)
   `(progn
      (do-node-const-axioms (:subgoal ,subgoal)
         ,@rest)
      (do-thread-const-axioms (:subgoal ,subgoal)
         ,@rest)))
      
(define-term-construct subgoal #'subgoal-p (name args options))
(define-term-construct comprehension #'comprehension-p (left right variables))
(define-term-construct exist #'exist-p (var-list body))
(define-term-construct constraint #'constraint-p (expr))
(define-term-construct assignment #'assignment-p (var expr))
(define-term-construct agg-construct #'agg-construct-p (specs vlist body head head0 spec-vars))
(define-term-construct agg-spec #'agg-spec-p (op var args))
(define-term-construct constant #'constant-p (name expr type))
(define-term-construct conditional #'conditional-p (cmp term1 term2))
(define-term-construct index #'index-p (name field))

(define-with process (name instrs) :use-self-p t)
(define-with get-constant (name))

(defmacro do-processes ((&key (process nil) (name nil) (instrs nil) (operation 'do)) &body body)
   (alexandria:with-gensyms (el)
      `(loop-list (,el *processes* :operation ,operation)
         (with-process ,el (:name ,name :instrs ,instrs :process ,process)
            ,@body))))
