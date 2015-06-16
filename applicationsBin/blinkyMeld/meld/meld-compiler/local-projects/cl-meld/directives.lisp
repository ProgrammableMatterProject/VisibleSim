(in-package :cl-meld)

(defun make-index (name field) `(:index ,name ,field))
(defun index-name (x) (second x))
(defun index-field (x) (third x))
(defun index-p (x) (tagged-p x :index))
(defun find-index-name (name) (find-if #L(and (index-p !1) (string-equal name (index-name !1))) *directives*))

(defun make-descending-priority (a b) `(:prio ,a ,b))
(defun make-ascending-priority (a b) `(:prio ,b ,a))

(defun priority-p (x) (tagged-p x :prio))
(defun priority-left (x) (second x))
(defun priority-right (x) (third x))

(defun make-initial-priority (num) `(:initial-prio ,num))
(defun initial-priority-value (p) (second p))
(defun initial-priority-p (p) (tagged-p p :initial-prio))

(defun make-priority-no-initial () `(:no-initial-priorities))
(defun priority-no-initial-p (p) (tagged-p p :no-initial-priorities))

(defun make-priority-order (asc-desc) `(:priority-order ,asc-desc))
(defun priority-order (x) (second x))
(defun priority-order-p (x) (tagged-p x :priority-order))

(defun make-priority-static () `(:priority-static))
(defun priority-static-p (x) (tagged-p x :priority-static))

(defun make-priority-cluster (typ) `(:priority-cluster ,typ))
(defun priority-cluster-type (x) (second x))
(defun priority-cluster-p (x) (tagged-p x :priority-cluster))

(defun all-start-nodes (priorities)
	(remove-duplicates (mapcar #'priority-left priorities) :test #'string-equal))
(defun all-end-nodes (priorities)
	(remove-duplicates (mapcar #'priority-right priorities) :test #'string-equal))

(defun select-start-nodes (priorities)
	(set-difference (all-start-nodes priorities) (all-end-nodes priorities) :test #'string-equal))
			
(defun remove-all-start-nodes (priorities start-nodes)
	(split-mult-return #'(lambda (prio)
						(let* ((what (priority-left prio))
								 (x (find what start-nodes :test #'string-equal)))
							(if x t nil)))
			priorities))

(defun get-priority-static () (find-if #'priority-static-p *directives*))

(defun get-priority-order ()
	"Returns priority ordering for the program."
	(let ((order (find-if #'priority-order-p *directives*)))
		(if order
			(priority-order order)
			:desc)))

(defun get-default-priority ()
	"Returns default priority for nodes of the program."
	(case (get-priority-order)
		(:asc most-positive-double-float)
		(:desc most-negative-double-float)))

(defun get-initial-priority ()
	"Returns initial priority for nodes of the program."
	(let ((found (find-if #'initial-priority-p *directives*)))
		(if found
			(initial-priority-value found)
         (let ((no-initial (find-if #'priority-no-initial-p *directives*)))
          (if no-initial
            (get-default-priority)
            (case (get-priority-order)
             (:asc most-negative-double-float)
             (:desc most-positive-double-float)))))))

(defun assign-priorities (base priorities)
	"Using the start priority 'base', it assigns increasing priorities taking into
account the dependencies between predicates."
	(let ((start-nodes (select-start-nodes priorities)))
		(cond
			((null start-nodes) nil)
			(t
				(multiple-value-bind (removed remaining-nodes)
						(remove-all-start-nodes priorities start-nodes)
					(let* ((end-nodes (all-end-nodes removed))
							 (disappearing-nodes (set-difference end-nodes (all-start-nodes remaining-nodes) :test #'string-equal))
						 	 (result (mapcar #L(cons !1 base) start-nodes)))
						(append result
							(append (mapcar #L(cons !1 (1+ base)) disappearing-nodes)
								(if (null remaining-nodes) nil (assign-priorities (1+ base) remaining-nodes))))))))))
								
(defun find-priorities ()
	"Takes the *ast* code and finds new priorities from the comprehensions and aggregates."
   (return-from find-priorities nil)
	(do-rules (:head head :body body)
		(do-comprehensions head (:left left)
			(do-subgoals left (:name name1)
				(do-subgoals body (:name name2)
					(push-end (make-descending-priority name1 name2) *directives*))))
		(do-agg-constructs head (:body aggbody)
			(do-subgoals aggbody (:name name1)
				(do-subgoals body (:name name2)
					(push-end (make-descending-priority name1 name2) *directives*))))))
					
