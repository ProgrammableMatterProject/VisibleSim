(in-package :cl-meld)

(defun node-used-id (mapping-set id)
   (gethash id mapping-set))
   
(defun add-mapping (mapping-set node map)
   (setf (gethash node mapping-set) map))

(defun has-mapping-p (mapping-set node)
	(multiple-value-bind (x found-p) (gethash node mapping-set)
		(declare (ignore x))
		found-p))
   
(defun make-mapping-set ()
   (make-hash-table :test #'eq))
   
(defun number-of-nodes (nodes) (hash-table-count nodes))

(defmacro iterate-nodes ((fake real nodes) &body body)
   (alexandria:with-gensyms (ls)
      `(let ((,ls (loop for value being the hash-values of ,nodes
                        using (hash-key key)
                        collect (cons value key))))
         (loop for (,fake . ,real) in (sort ,ls #'< :key #'car)
               do ,@body))))

(defun flip-nodes (hash expr)
   (transform-expr #'addr-p #'(lambda (expr)
                                 (setf (addr-num expr) (node-used-id hash (addr-num expr)))
                                 (values nil :stop))
                  expr))
                  
(defun link-subgoal-p (subgoal routes)
   (with-subgoal subgoal (:name name :args args)
		(unless (some #L(equal name !1) routes)
			(return-from link-subgoal-p nil))
		(some #'addr-p (rest args))))
      
(defun get-link-info (subgoal)
   (with-subgoal subgoal (:args args)
      (list (addr-num (first args))
            (addr-num (find-if #'addr-p (rest args))))))
            
(defun add-edge-to-set (hash info)
   (let ((from (first info))
         (to (second info)))
      ;(format t "from ~a to ~a~%" from to)
      (setf (gethash from hash) (cons to (gethash from hash)))))
      
(defun get-neighbors-from-set (hash from)
   (multiple-value-bind (ls found-p) (gethash from hash)
      (when found-p
         (remhash from hash)
         ls)))
         
(defun make-edge-set ()
   (make-hash-table :test #'eq))

(defun find-edge-set (routes)
   (letret (hash (make-edge-set))
		(do-node-const-axioms (:subgoal subgoal)
			(when (link-subgoal-p subgoal routes)
				(add-edge-to-set hash (get-link-info subgoal))))))
      
(defun empty-edge-set-p (edge-set) (zerop (hash-table-count edge-set)))
(defun filter-visited-nodes (ls node-set) (filter #L(in-hash-set-p node-set !1) ls))
         
(defun get-random-node-from-node-set (node-set)
   (do-hash-set (node node-set)
      (return-from get-random-node-from-node-set node)))
      
(defun aux-graph-has-cycles-p (edge-set node-set &optional (queue (list)))
   (when (find-if #L(not (in-hash-set-p node-set !1)) queue)
      (return-from aux-graph-has-cycles-p t))
   (cond
      ((and (empty-edge-set-p edge-set)
            (null queue))
         nil)
      (t
         (unless queue
            (push (get-random-node-from-node-set node-set) queue))
         (unless queue
            (return-from aux-graph-has-cycles-p nil))
         (let* ((node (pop queue))
                (new-edges (get-neighbors-from-set edge-set node)))
            (remove-hash-set node-set node)
            (aux-graph-has-cycles-p edge-set node-set (append queue new-edges))))))

(defun graph-has-cycles-p ()
   (aux-graph-has-cycles-p (find-edge-set (get-route-names)) (create-hash-set *nodes*)))

(defun bfs-ordering (edge-set node-set &key (mapping-set (make-mapping-set)) (queue (list)) (count 0))
   (cond
      ((empty-edge-set-p edge-set)
         (do-hash-set (node node-set) ; add remaining nodes
            (add-mapping mapping-set node count)
            (incf count))
         mapping-set)
      (t
         (setf queue (filter-visited-nodes queue node-set))
         (unless queue
            (unless (zerop (hash-table-count node-set))
               (push (get-random-node-from-node-set node-set) queue)))
         (unless queue
            (return-from bfs-ordering mapping-set))
         (let* ((node (pop queue))
                (new-edges (get-neighbors-from-set edge-set node)))
            (add-mapping mapping-set node count)
            (remove-hash-set node-set node)
            (bfs-ordering edge-set node-set
                           :mapping-set mapping-set
                           :queue (append queue new-edges)
                           :count (1+ count))))))
            
(defun naive-ordering (nodes &key (start-count 0) (mapping (make-mapping-set)))
   (loop for node in nodes
         for count = start-count then (1+ count)
         do (add-mapping mapping node count))
   mapping)

(defun in-file-ordering (nodes &key (mapping (make-mapping-set)))
	(let ((max-node 0))
		(loop for node in nodes
				do (add-mapping mapping node node)
				do (when (> node max-node)
						(setf max-node node)))
		(loop for i from 0 to max-node
				do (unless (has-mapping-p mapping i)
						(add-mapping mapping i i))))
	mapping)

(defun random-ordering (nodes &key (start-count 0) (mapping (make-mapping-set)))
   (naive-ordering (shuffle-list nodes) :start-count start-count :mapping mapping))

(defun print-mapping (mapping-set)
   (iterate-nodes (fake real mapping-set)
      (format t "REAL: ~a FAKE: ~a~%" real fake)))
      
(defun is-constant-node-list-p (ls)
   (cond
      ((nil-p ls) t)
      ((cons-p ls)
         (and (addr-p (cons-head ls))
            (is-constant-node-list-p (cons-tail ls))))))

(defun get-constant-list-addrs (ls)
   (cond
      ((nil-p ls) nil)
      ((cons-p ls)
         (cons (addr-num (cons-head ls))
               (get-constant-list-addrs (cons-tail ls))))
      (t (assert nil))))
      
(defun build-initial-mapping-set (addrs total)
   (letret (mapping (make-mapping-set))
      (loop for i from 0 upto (1- total)
            for node in addrs
            do (add-mapping mapping node i))))

(defun do-topology-ordering ()
	(setf *nodes* (reverse *nodes*))
	(let* ((found (find-if #'priority-cluster-p *directives*))
		  	 (ordering-type (if found (priority-cluster-type found) *ordering-type*)))
   	(case ordering-type
	     (:naive (naive-ordering *nodes*))
	     (:random (random-ordering *nodes*))
		  (:in-file (in-file-ordering *nodes*))
	     (:breadth (let ((edge-set (find-edge-set (get-route-names)))
	                     (node-set (create-hash-set *nodes*)))
	                 (bfs-ordering edge-set node-set)))
	     (otherwise (assert nil)))))

(defun optimize-topology ()
   (let ((mapping (do-topology-ordering)))
      ;(print-mapping mapping)
      (setf *nodes* mapping)
		;(loop for key being the hash-keys of mapping
	   ;     using (hash-value value)
	   ;     do (format t "The value associated with the key ~S is ~S~%" key value))
      (do-all-var-axioms (:clause clause)
         (flip-nodes mapping clause))
		(do-all-const-axioms (:subgoal subgoal)
			(flip-nodes mapping subgoal))
		(do-constant-list *consts* (:constant c)
			(flip-nodes mapping c))
		(do-rules (:clause clause)
			(flip-nodes mapping clause))))
