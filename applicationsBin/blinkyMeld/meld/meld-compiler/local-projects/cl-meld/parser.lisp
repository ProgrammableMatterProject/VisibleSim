
(in-package :cl-meld)

(define-string-lexer meld-lexer1
   ("[-]?[0-9]+(\\.[0-9]+|[0-9]+)?" (return (values :number $@)))
	("\"[^\"]*\""							(return (values :string $@)))
	("\\,"                           (return (values :comma $@)))
	("\\["                           (return (values :lsparen $@)))
	("\\]"                           (return (values :rsparen $@)))
	("\\{"                           (return (values :lcparen $@)))
	("\\}"                           (return (values :rcparen $@)))
   ("\\~"                           (return (values :tilde $@)))
   ("\\."                           (return (values :dot $@)))
	("@initial"								(return (values :initial-priority $@)))
	("@static"								(return (values :static-priority $@)))
	("@cluster"								(return (values :cluster-priority $@)))
	("@random"								(return (values :random-priority $@)))
   ("@no_initial_priorities"        (return (values :no-initial-priorities $@)))
	("@asc"									(return (values :asc $@)))
	("@desc"									(return (values :desc $@)))
	("@type"									(return (values :priority-type $@)))
	("@order"								(return (values :priority-order $@)))
	("@\\+[0-9]+s"							(return (values :delay-seconds $@)))
	("@\\+[0-9]+ms"						(return (values :delay-ms $@)))
   ("@host"                         (return (values :host $@)))
	("@world"                        (return (values :world $@)))
   ("@cpus"                         (return (values :cpus $@)))
	("@arg[0-9]"							(return (values :arg $@)))
	("@"                             (return (values :local $@)))
	("-o"                            (return (values :lolli $@)))
	("\\!"                           (return (values :bang $@)))
	("\\?"									(return (values :question-mark $@)))
	("\\$"                           (return (values :dollar $@)))
	("=>"                            (return (values :to $@)))
	("\\:\\("								(return (values :lpaco $@)))
	(":-"                            (return (values :arrow  $@)))
	("\\:"                           (return (values :colon $@)))
	("\\("			                  (return (values :lparen $@)))
	("\\)"			                  (return (values :rparen $@)))
	("\\|\\|"								(return (values :or $@)))
	("\\|"                           (return (values :bar $@)))
   ("\\&\\&"                        (return (values :and $@)))
	("\\/\\*.*\\*\\/"                (return (values :comment)))
   ("\\+\\+"                          (return (values :append $@)))
	("\\+"                           (return (values :plus $@)))
	("\\-"                    		   (return (values :minus $@)))
	("\\*"                           (return (values :mul $@)))
	("\\%"                           (return (values :mod $@)))
	("\\/"                           (return (values :div $@)))
	("\\<="                          (return (values :lesser-equal $@)))
	("\\<>"                          (return (values :not-equal $@)))
	("\\<"                           (return (values :lesser $@)))
	("\\>="                          (return (values :greater-equal $@)))
	("\\>"                           (return (values :greater $@)))
	("\\="                           (return (values :equal $@)))
	("\_"				            		(return (values :variable $@)))
 	("[a-z]([a-z]|[A-Z]|[0-9]|\\-|\_|\\?|\\-)*"		   (return (values :const $@)))
	("'\\w+"		                     (return (values :const $@)))
	("\\#.+"                         (return (values :file $@)))
	("[A-Z]([a-z]|[0-9]|[A-Z]|\_)*"	(return (values :variable $@))))
	
(defparameter *line-number* 0)
(defparameter *parsed-header* nil)

(defparameter *parser-typedef-types* nil)
(defun add-typedef (name typ)
   (setf (gethash name *parser-typedef-types*) typ))

(define-condition parse-failure-error (error)
   ((text :initarg :text :reader text) (line :initarg :line :reader line)))

(defmacro lex-const (const &rest pairs)
	`(cond
		,@(loop for (str key) in pairs
				collect `((string-equal ,const ,str) (values ,key ,const *line-number*)))
		(t (values :const ,const))))
				
(defun meld-lexer (str)
	(let ((lexer (meld-lexer1 str)))
		(incf *line-number*)
		(lambda ()
			(multiple-value-bind (typ rest) (funcall lexer)
				(if (eq typ :const)
              (multiple-value-bind (ty found-p) (gethash rest *parser-typedef-types*)
                  (cond
                   (found-p (values :type-name rest))
                   ((has-node-type-p rest) (values :node-type rest))
                   (t
                    (lex-const rest ("type" :type)
                     ("exists" :exists)
                     ("extern" :extern)
                     ("const" :const-decl)
                     ("bool" :type-bool)
                     ("int" :type-int)
                     ("float" :type-float)
                     ("node" :type-addr)
                     ("thread" :type-thread)
                     ("string" :type-string)
                     ("list" :type-list)
                     ("array" :type-array)
                     ("include" :include)
                     ("random" :random)
                     ("route" :route)
                     ("action" :action)
                     ("linear" :linear)
                     ("export" :export)
                     ("import" :import)
                     ("custom" :custom)
                     ("from" :from)
                     ("as" :as)
                     ("let" :let)
                     ("ins" :ins)
                     ("in" :in)
                     ("end" :end)
                     ("if" :if)
                     ("then" :then)
                     ("fun" :fun)
                     ("else" :else)
                     ("otherwise" :otherwise)
                     ("min" :min)
                     ("priority" (if *parsed-header* :const :prio))
                     ("index" (if *parsed-header* :const :index))
                     ("true" :true)
                     ("false" :false)
                     ("nil" :nil)))))
					(values typ rest *line-number*))))))
					
(defun make-var-parser (var)
   (if (equal var "_")
	   (generate-random-var)
		(make-var var)))
			
(defun parse-base-number (str)
	(if (find #\. str)
		(read-from-string str)
		(parse-integer str)))

(defun parse-number (str)
   (if (find #\. str)
      (make-float (read-from-string str))
      (make-int (parse-integer str))))

(defun parse-delay-seconds (str)
	(let* ((remain1 (subseq str 2))
			 (remain (subseq remain1 0 (1- (length remain1)))))
		(* 1000 (parse-integer remain))))
(defun parse-delay-ms (str)
	(let* ((remain1 (subseq str 2))
			 (remain (subseq remain1 0 (- (length remain1) 2))))
		(parse-integer remain)))

(defun parse-call (name args)
 (cond
  ((has-function-call-p name)
   (make-callf name args))
  (t (make-call name args))))

(defvar *parsed-consts* nil)
(defun lookup-const-def (name)
	(let ((x (find-if #L(equal (const-definition-name !1) name) *parsed-consts*)))
		(assert x)
		(const-definition-expr x)))
(defun has-const-def-p (name)
	(find-if #L(string-equal (const-definition-name !1) name) *parsed-consts*))
   
(defmacro return-const (const)
   `#'(lambda (&rest x) (declare (ignore x)) ,const))
   
(defvar *found-nodes* nil)

(defun add-found-node (i)
   (multiple-value-bind (yes found-p) (gethash i *found-nodes*)
      (declare (ignore yes))
      (unless found-p
         (setf (gethash i *found-nodes*) t))))
         
(defun defined-nodes-list ()
   (loop for k being the hash-keys in *found-nodes* collect k))

(defvar *included-asts* nil)
(defun add-included-ast (ast) (push ast *included-asts*))

(defvar *defined-functions* nil)
(defun add-defined-function (fun-name) (push fun-name *defined-functions*))

(defun has-function-call-p (name)
	(dolist (fun-name *defined-functions*)
      (when (string-equal fun-name name)
         (return-from has-function-call-p t)))
   nil)

(defparameter *seen-subgoals* nil)
(defun add-seen-subgoal (name)
   (unless (member name *seen-subgoals* :test #'string-equal)
    (push name *seen-subgoals*)))

(defvar *needed-externs* nil)
(defun add-needed-extern (extern-name) (push extern-name *needed-externs*))

(defvar *max-arg-needed* 0)

(defvar *parser-exported-predicates* nil)
(defun add-exported-predicate (name) (push name *parser-exported-predicates*))

(defvar *parser-imported-predicates* nil)
(defun add-imported-predicate (imp) (push imp *parser-imported-predicates*))

(defvar *parser-node-types* nil)
(defun add-node-type (name)
   (push name *parser-node-types*))
(defun has-node-type-p (name)
   (member name *parser-node-types* :test #'string-equal))
   
(defun generate-part-expression (final-type body fun-args args)
   (let ((this-fun-arg (first fun-args))
         (this-arg (first args))
         (rest-fun-args (rest fun-args))
         (rest-args (rest args)))
      (cond
         ((var-p this-arg)
            (let ((new-body (map-one-variable-to-another body this-fun-arg this-arg)))
               (setf (var-type this-arg) (var-type this-fun-arg))
               (cond
                  ((one-elem-p fun-args) new-body)
                  (t (generate-part-expression final-type
                           new-body
                           rest-fun-args
                           rest-args)))))
         (t
            (let* ((new-var-name (generate-random-var-name))
                   (new-var (make-var new-var-name (var-type this-fun-arg)))
                   (new-body (map-one-variable-to-another body this-fun-arg new-var)))
               (cond
                  ((one-elem-p fun-args)
                     (make-let new-var
                        this-arg
                        new-body 
                        final-type))
                  (t
                     (make-let new-var
                        this-arg
                        (generate-part-expression final-type new-body rest-fun-args rest-args)
                        final-type))))))))

(defun generate-expression-by-function-call (fun args)
   (with-function fun (:args fun-args :ret-type ret-type :body body :name name)
      (unless (= (length args) (length fun-args))
         (error (make-condition 'parse-failure-error :text (tostring "function call to ~a has invalid number of arguments" name) :line *line-number*)))
      (generate-part-expression ret-type body fun-args args)
      ))

(defun parser-make-definition (name types &optional options)
	(make-definition name types options))

(defun parse-agg-construct (str)
   (cond
      ((string-equal str "count") :count)
      ((string-equal str "sum") :sum)
		((string-equal str "collect") :collect)
		((string-equal str "min") :min)))
      
(defun parse-agg-decl (str)
   (cond
      ((string-equal str "sum") :sum)
      ((string-equal str "min") :min)
      ((string-equal str "max") :max)
      ((string-equal str "first") :first)
      (t
         (error (make-condition 'parse-failure-error :text (tostring "aggregate declaration not recognized ~a" str) :line *line-number*)))))

(define-parser meld-parser
   (:muffle-conflicts t)
 	(:start-symbol program)
 	
 	(:precedence ((:left :mul :div :mod :in :ins) (:left :plus :minus :append) (:right :and) (:right :or) (:left :otherwise)))
 	
	(:terminals (:const :type :true :false :variable :number :string :lparen :rparen
								:bar :arrow :dot :comma :type-bool :type-int :type-addr :type-thread
								:type-float :type-string :plus :minus :mul :mod :div
								:lesser :lesser-equal :greater :greater-equal :equal
								:extern :const-decl :arg :tilde :append
								:lsparen :rsparen :nil :type-list :local
                        :type-array
								:route :include :file :world :cpus :action
								:linear :dollar :lcparen :rcparen :lolli
								:bang :to :let :in :ins :fun :end :colon
								:not-equal :if :then :else :otherwise :prio :random
								:min :asc :desc :or :and :export :custom :import :as :from
								:exists :initial-priority :priority-type :priority-order
								:delay-seconds :delay-ms :question-mark
								:static-priority :cluster-priority
								:random-priority :lpaco :host :index :type-name
                        :no-initial-priorities :node-type))

	(program
	  (includes definitions directives externs consts
      funs before-statement statements #L(make-ast  !2 ; definitions
                           !4 ; externs
                           (remove-if #'is-axiom-p !8) ; clauses
                           (filter #'is-axiom-p !8) ; axioms
                           !6 ; functions
                           (defined-nodes-list) ; nodes
                           !3 ; directives
                           !5 ; consts
                           *parser-exported-predicates*
                           *parser-imported-predicates*
                           *max-arg-needed*))) ;; args-needed

   (before-statement
      (#'(lambda () (setf *parsed-header* t))))

	(includes
	   ()
	   (include includes #'(lambda (a b) (declare (ignore a b)))))

	(include
	   (:include :file #'(lambda (i f)
									(declare (ignore i))
									(let* ((filename (subseq f 1))
										    (inner-ast (parse-meld-file-rec filename)))
										(add-included-ast inner-ast)))))

	(definitions
	 ()
	 (definition definitions #'(lambda (d ls) (if (null d) ls (cons d ls)))))

   (definition
      (:type-addr const :dot #'(lambda (n node-name d)
                              (declare (ignore n d))
                              (when (has-node-type-p node-name)
                                 (error (make-condition 'parse-failure-error :text (tostring "repeated node type ~a" node-name) :line *line-number*)))
                              (add-node-type node-name)
                              nil))
      (:type atype const :dot #'(lambda (ty orig-type name d)
                                    (declare (ignore ty d))
                                    (multiple-value-bind (ty found-p) (gethash name *parser-typedef-types*)
                                       (declare (ignore ty))
                                       (when found-p
                                        (error (make-condition 'parse-failure-error :text (tostring "repeated type ~a" name) :line *line-number*))))
                                    (add-typedef name orig-type)
                                    nil))
                                                            
		(:import predicate-options const type-args-part :as const :from const :dot #'(lambda (i opts name args as imported-name fr file d)
																											(declare (ignore i as d fr))
																											(add-imported-predicate (make-import name imported-name file))
																											(parser-make-definition imported-name args opts)))
		(:export const :dot #'(lambda (e name d) (declare (ignore e d)) (add-exported-predicate name) nil))
      (:type predicate-options const type-args-part :dot #'(lambda (ty opts name args d)
																					(declare (ignore ty d))
																					(parser-make-definition name args opts))))

	(directives
		()
      (index directives #'cons)
		(priority directives #'cons))

   (index
      (:index :const :div :number :dot #'(lambda (i name s field d)
                                          (declare (ignore i s d))
                                          (make-index name (parse-integer field)))))
	
	(priority
		(:prio :static-priority :dot #'(lambda (p s d) (declare (ignore p s d)) (make-priority-static)))
		(:prio :cluster-priority :static-priority :dot
				#'(lambda (p i s d) (declare (ignore p i s d))
					(make-priority-cluster :in-file)))
      (:prio :no-initial-priorities :dot #'(lambda (p n d) (declare (ignore p n d))
                                       (make-priority-no-initial)))
		(:prio :cluster-priority :random-priority :dot
				#'(lambda (p i s d) (declare (ignore p i s d))
					(make-priority-cluster :random)))
		(:prio :initial-priority :number :dot #'(lambda (p i n dot) (declare (ignore p i dot)) (make-initial-priority (parse-base-number n))))
		(:prio :priority-order asc-desc :dot #'(lambda (p o ad dot) (declare (ignore p o dot)) (make-priority-order ad)))
		(:prio const :lesser const :dot #'(lambda (p name1 l name2 d) (declare (ignore p l d)) (make-descending-priority name1 name2)))
		(:prio const :greater const :dot #'(lambda (p name1 g name2 d) (declare (ignore p g d)) (make-ascending-priority name1 name2))))
		
	(asc-desc
		(:asc (return-const :asc))
		(:desc (return-const :desc)))
		
   (consts
      ()
      (const-definition consts #'cons))

	(const-definition
	   (:const-decl const-name :equal expr :dot #'(lambda (a name e expr dot)
   	                                             (declare (ignore a e dot))
   	                                             (push (make-const-definition name expr) *parsed-consts*)
																	(make-constant name expr))))

	(const-name
		const)
		
   (funs
      ()
      (fun funs #'cons))
      
   (fun
      (fun-name :lparen fun-args :rparen :colon atype :equal expr :dot
            #'(lambda (name l args r c ret-type eq body d)
               (declare (ignore l r c eq d))
                  (make-function name args ret-type body))))

	(fun-name
		(:fun const #'(lambda (f name) (declare (ignore f)) (add-defined-function name) name)))
                  
   (fun-args
      (fun-arg #'list)
      (fun-arg :comma fun-args #'cons))
      
   (fun-arg
      (atype variable #'(lambda (typ var) (make-var (var-name var) typ))))
         
   (externs
      ()
      (extern-definition externs #'cons))

   (extern-definition
      (:extern atype const :lparen type-args :rparen :dot #'(lambda (e ret-type name l args r d)
   	                                                         (declare (ignore e l r d))
																					(add-needed-extern name)
   	                                                         (make-extern name ret-type args (1- (length *needed-externs*))))))
	(predicate-options
		()
		(predicate-option predicate-options #'cons))
		
	(predicate-option
	   (:route (return-const :route))
	   (:action (return-const :action))
	   (:linear (return-const :linear)))
	   
	(type-args-part
	   (:lparen type-args :rparen #'(lambda (l typs r) (declare (ignore l r)) typs)))

	(type-args
	 (type-decl #'list)
	 (type-decl :comma type-args #'(lambda (ty comma ls) (declare (ignore comma)) (cons ty ls))))

   (type-decl
    (atype #'identity)
    (atype variable #'(lambda (typ name) (declare (ignore name)) typ))
    (aggregate-decl atype #'make-aggregate))
    
   (aggregate-decl
		(:min (return-const :min))
      (const #L(parse-agg-decl !1)))

	(atype
	 base-type
    (:node-type #'(lambda (name)
                    (unless (has-node-type-p name)
                        (error (make-condition 'parse-failure-error :text (tostring "invalid node type ~a" name) :line *line-number*)))
                    (make-type-node name)))
    (:type-name #'(lambda (name)
               (multiple-value-bind (typ found-p) (gethash name *parser-typedef-types*)
                (unless found-p
                 (error (make-condition 'parse-failure-error :text (tostring "invalid type name ~a" name) :line *line-number*)))
                typ)))
	 (:lpaco type-list :rparen #'(lambda (l tl r)
												(declare (ignore l r))
												(make-struct-type tl)))
	 (:type-list atype #'(lambda (l ty)
	                           (declare (ignore l))
										(make-list-type ty)))
    (:type-array atype #'(lambda (a ty)
                               (declare (ignore a))
                               (make-array-type ty))))
										
	(type-list
		(atype #'list)
		(atype :comma type-list #'(lambda (ty comma ls) (declare (ignore comma)) (cons ty ls))))

	(base-type
	 (:type-bool (return-const :type-bool))
 	 (:type-int (return-const :type-int))
 	 (:type-float (return-const :type-float))
    (:type-thread (return-const :type-thread))
 	 (:type-addr (return-const :type-addr))
	 (:type-string (return-const :type-string)))
	   
	(statements
	 ()
	 (statement statements #'cons))

	(statement
		(:lsparen :colon subgoal-mod variable :bar terms :rsparen :lolli head :dot
				#'(lambda (l colon mod v b body r lolli head d)
						(declare (ignore l colon b r lolli d))
						(let ((clause (make-clause body head)))
							(case mod
								(:random (clause-add-random clause v))
								(:min (clause-add-min clause v)))
							clause)))
	   (body :lolli head :dot #'(lambda (body l head d) (declare (ignore l d)) (make-clause body head)))
	   (body :lolli :dot #'(lambda (body l d) (declare (ignore l d)) (make-clause body nil)))
	   (:arrow terms :dot #'(lambda (x body y) (declare (ignore x y)) (make-clause body nil)))
	   (head :dot #'(lambda (head d) (declare (ignore d)) (make-clause nil head)))
		(head :arrow terms :dot #'(lambda (conc y perm w) (declare (ignore y w)) (make-clause perm conc))))
		
	(subgoal-mod
		(:random (return-const :random))
		(:min (return-const :min)))
	
	(head
		(:number #'(lambda (str)
						(let ((num (parse-integer str)))
							(if (= num 1)
								nil
								(error (make-condition 'parse-failure-error :text (tostring "invalid head number ~a" str) :line *line-number*))))))
		(terms #'identity))
		
	(body
		terms)
		
   (terms
      (term #'list)
		(term :comma terms #'(lambda (el x ls) (declare (ignore x)) (cons el ls))))
   
	(subhead-term
		(exists #'identity)
		(subgoal #'identity))
		
	(head-term
		(conditional #'identity)
		(subhead-term #'identity)
		(comprehension #'identity)
		(aggregate-construct #'identity))
		
   (term
		(head-term #'identity)
      (constraint #'identity))

	(exists
		(:exists variable-list :dot :lparen terms :rparen #'(lambda (e var-list d l terms r)
													(declare (ignore e d l r))
                                       (setf *has-exists-p* t)
													(make-exist var-list terms))))
	(subgoal
	   (inner-subgoal  #'identity)
	   (:dollar inner-subgoal #'(lambda (d sub)
	                                 (declare (ignore d))
	                                 (subgoal-set-reused sub)
	                                 sub))
	 	(:bang inner-subgoal  #'(lambda (o sub)
	 	                                 (declare (ignore o))
	 	                                 (subgoal-add-option sub :persistent)
	 	                                 sub))
		(:question-mark inner-subgoal #'(lambda (o sub)
													(declare (ignore o))
													(subgoal-add-option sub :linear)
													sub)))
	 	                                 
	 (inner-subgoal
	    (const :lparen args :rparen #'(lambda (name x args y)
	                                       (declare (ignore x y))
                                          (add-seen-subgoal name)
	                                       (make-subgoal name args)))
		 (const :lparen args :rparen tuple-delay #'(lambda (name x args y delay)
																(declare (ignore x y))
                                                (add-seen-subgoal name)
																(let ((sub (make-subgoal name args)))
																	(subgoal-add-delay sub delay)
																	sub))))
	
	(tuple-delay
		(:delay-seconds #L(parse-delay-seconds !1))
		(:delay-ms #L(parse-delay-ms !1)))
		
	(conditional
		(:lparen :if cmp :then head :otherwise head :end :rparen #'(lambda (l i cmp then terms-true otherwise terms-false end r)
																(declare (ignore l i then otherwise end r))
																(make-conditional cmp terms-true terms-false))))

	(comprehension
	    (:lcparen variable-list :bar terms :bar comprehension-terms :rcparen #'(lambda (l vl b1 left b2 right r) (declare (ignore l b1 b2 r))
                                              (make-comprehension left right vl))))
   
	(comprehension-terms
		()
		(terms #'identity))

	(variable-list
		(:dot #'(lambda (x) (declare (ignore x)) (list)))
		(variable #'list)
	   (variable :comma variable-list #'(lambda (v c l) (declare (ignore c)) (cons v l))))
	
	(aggregate-construct
		(:lsparen multiple-aggregate-spec :bar terms :bar terms :rsparen
			#'(lambda (l specs b1 body b2 head r)
					(declare (ignore l b1 b2 r))
					(make-agg-construct specs nil body head)))
		(:lsparen multiple-aggregate-spec :bar variable-list :bar terms :bar terms :rsparen
			#'(lambda (l specs b1 vlist b2 body b3 head r)
				(declare (ignore l r b1 b2 b3 r))
				(make-agg-construct specs vlist body head)))
      (:lsparen multiple-aggregate-spec :bar variable-list :bar terms :bar terms :bar terms :rsparen
         #'(lambda (l specs b1 vlist b2 body b3 head0 b4 head r)
            (declare (ignore l r b1 b2 b3 b4 r))
            (make-agg-construct specs vlist body head head0)))
      (:lsparen multiple-aggregate-spec :bar terms :bar terms :bar terms :rsparen
         #'(lambda (l specs b1 body b2 head0 b3 head r)
            (declare (ignore l r b1 b2 b3 r))
            (make-agg-construct specs nil body head head0)))
	   (:lsparen multiple-aggregate-spec :bar variable-list :bar terms :rsparen
	         #'(lambda (l specs b1 vlist b2 terms r) (declare (ignore l r b1 b2))
	               (make-agg-construct specs vlist terms))))
		
	(multiple-aggregate-spec
		(aggregate-spec #L(list !1))
		(aggregate-spec :comma multiple-aggregate-spec #L(cons !1 !3)))
		
	(aggregate-spec
      (:custom const expr :to variable #L(make-agg-spec :custom !5 (list !2 !3)))
		(aggregate-mod :to variable #L(make-agg-spec (parse-agg-construct !1) !3)))
	
	(aggregate-mod
		(:min (return-const "min"))
		(const #'identity))
	                          
   (constraint
      (cmp #'(lambda (c) (make-constraint c))))

	(args
	 	(expr #'list)
		(expr :comma args #'(lambda (x y z) (declare (ignore y)) (cons x z))))

	(expr
	   variable
		arg
		(:true (return-const (make-bool t)))
		(:false (return-const (make-bool nil)))
		(:string #'(lambda (x) (make-string-constant (subseq x 1 (1- (length x)))))) ;; need to trim the first and final ""
	   (const :lparen args :rparen #'(lambda (name l args r) (declare (ignore l r)) (parse-call name args)))
	   (const #L(if (has-const-def-p !1)
						(make-get-constant !1)
						(error (make-condition 'parse-failure-error :text (tostring "constant not recognized: \"~a\"" !1) :line *line-number*))))
	   (:local :number #L(let ((val (parse-integer !2))) (add-found-node val) (make-addr val)))
		number
	   (:lparen expr :rparen #'(lambda (l expr r) (declare (ignore l r)) expr))
		(:lpaco expr-list :rparen #'(lambda (l ls r) (declare (ignore l r)) (make-struct ls)))
	   (:type-float :lparen expr :rparen #'(lambda (f l expr r) (declare (ignore f l r)) (make-convert-float expr)))
	   (:world (return-const (make-world)))
      (:cpus (return-const (make-cpus)))
      (:host (return-const (make-host)))
      (expr :append expr #L(make-call "lappend" (list !1 !3)))
	   (expr :minus expr #'make-minus)
	   (expr :mul expr #'make-mul)
	   (expr :mod expr #'make-mod)
	   (expr :div expr #'make-div)
	   (expr :plus expr #'make-plus)
	   (:if cmp :then expr :else expr :end #'(lambda (if cmp then e1 else e2 end)
	                                                (declare (ignore if then else end))
	                                                (make-if cmp e1 e2)))
	   (:let variable :equal expr :in expr :end #'(lambda (l var eq expr i body e) (declare (ignore l eq i e)) (make-let var expr body)))
	   (list-expr #'identity))

	(list-expr
	   (:lsparen sub-list :rsparen #'(lambda (a b c) (declare (ignore a c)) b))
	   (:lsparen :rsparen #'(lambda (a b) (declare (ignore a b)) (make-nil)))
	   (:nil #'(lambda (a) (declare (ignore a)) (make-nil))))
	   
	(arg
		(:arg (lambda (x)
			(let ((num-arg (parse-integer (subseq x 4 5))))
				(setf *max-arg-needed* (max num-arg *max-arg-needed*))
				(make-argument num-arg)))))

	(sub-list
	   (expr #'(lambda (expr) (make-cons expr (make-nil))))
	   (expr :comma sub-list #'(lambda (expr x sub) (declare (ignore x)) (make-cons expr sub)))
	   (expr :bar expr #'(lambda (a b c) (declare (ignore b)) (make-cons a c))))

	(expr-list
		(expr #'list)
		(expr :comma expr-list #'(lambda (x c xs) (declare (ignore c)) (cons x xs))))
		
   (cmp
      (expr :in expr #L(make-call "lexists" (list !3 !1)))
      (expr :ins expr #L(make-call "lexistss" (list !3 !1)))
	   (const :lparen args :rparen #'(lambda (name l args r) (declare (ignore l r)) (parse-call name args)))
      (:tilde cmp #L(make-not !2))
		(cmp :or cmp #'make-or)
      (cmp :and cmp #'make-and)
		(:lparen cmp :rparen #'(lambda (l cmp r) (declare (ignore l r)) cmp))
      (expr :equal expr #'make-equal)
      (expr :not-equal expr #'make-not-equal)
      (expr :lesser expr #'make-lesser)
      (expr :lesser-equal expr #'make-lesser-equal)
      (expr :greater expr #'make-greater)
      (expr :greater-equal expr #'make-greater-equal))

	(number
		(:number #L(parse-number !1)))
      
	(variable
	 (:variable (lambda (x) (make-var-parser x))))

	(const
	 (:const #'identity)))
	 
(defmacro with-parse-context (&body body)
   `(let ((*parsed-consts* nil)
          (*seen-subgoals* nil)
          (*parser-typedef-types* (make-hash-table :test #'equal))
          (*parser-node-types* nil)
			 (*needed-externs* nil))
      ,@body))
      
(defmacro with-inner-parse-context (&body body)
   `(let ((*found-nodes* (make-hash-table))
			 (*line-number* 0)
          (*parsed-header* nil)
			 (*parser-imported-predicates* nil)
			 (*parser-exported-predicates* nil))
      ,@body))

(defun strip-comments-from-line (line)
	(ppcre:regex-replace "//.+" line ""))
      
(defun read-source-line (stream)
   (multiple-value-bind (line missing-newline-p) (read-line stream nil nil)
		(declare (ignore missing-newline-p))
      (strip-comments-from-line line)))

(defun simple-stream-lexer (read-source-line string-lexer &key (stream *standard-input*))
  (let (eof line-lexer (update t))
    (labels ((update-line-lexer ()
               (let ((line (funcall read-source-line stream)))
                  (if (null line)
                     (setf eof t))
                  (setf line-lexer (funcall string-lexer line))))
            (get-next-token ()
               (multiple-value-bind (token value)
                     (funcall line-lexer)
                  (if token
                     (values token value)
                     (if eof
                        nil
                        (progn
                           (update-line-lexer)
                           (get-next-token)))))))
      (lambda ()
         (when eof
            (error 'end-of-file :stream stream))
         (when update
            (update-line-lexer)
            (setf update nil))
         (get-next-token)))))
         
(defun parse-file-as-stream (file)
   "Takes a stream-based lexer for the file and parses the stream."
   (unless (file-exists-p file)
      (error 'file-not-found-error :text file))
   (with-open-file (input-stream file
                        :direction :input
                        :if-does-not-exist :error)
      (let* ((lexer (simple-stream-lexer #'read-source-line
                                  #'meld-lexer
                                  :stream input-stream)))
      (let ((new-dir (if (eq #\/ (char file 0)) (directory-namestring (pathname file))
                          (concatenate 'string (directory-namestring *default-pathname-defaults*)
                               (directory-namestring (pathname file))))))
         (in-directory (pathname new-dir)
            (handler-case (parse-with-lexer lexer meld-parser)
               (yacc-parse-error (c) (error (make-condition 'parse-failure-error
                  :text (tostring "in file ~a: unexpected terminal ~S (value ~S)"
                                       file
                                       (yacc-parse-error-terminal c)
                                       (yacc-parse-error-value c))
               :line *line-number*)))))))))
   
(defun parse-string (str)
   "Parses a string of Meld code."
   (let* ((lexer (meld-lexer str)))
      (parse-with-lexer lexer meld-parser)))
      
(defun parse-file (file)
   "Parses a file of Meld code."
   (unless (file-exists-p file)
      (error 'file-not-found-error :text file))
   (parse-string (read-file file)))
             
(define-condition file-not-found-error (error)
   ((text :initarg :text :reader text)))

(defun parse-meld-file-rec (file)
   "Parses a Meld file, including included files."
   (let* ((*included-asts* nil)
          (*defined-functions* nil)
			 (*max-arg-needed* 0)
          (ast (with-inner-parse-context
                  (parse-file-as-stream file))))
       (reduce #'merge-asts *included-asts*
                  :initial-value ast)))

(defun parse-meld-file (file)
   (setf *has-exists-p* nil)
   (with-parse-context
      (let ((ast (parse-meld-file-rec file)))
         (ast-prepare ast *seen-subgoals*)
         ast)))
