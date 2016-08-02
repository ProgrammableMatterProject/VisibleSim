(in-package :cl-meld)

(defparameter *debug-msgs* t)

(defmacro printdbg (str &rest args)
   `(if *debug-msgs*
      (format t ,(concatenate 'string str "~%") ,@args)))

(defun create-bin-array (&optional (size 0)) (make-array size :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun str->sym (str) (values (intern str)))

(defun tagged-p (list tag)
   (and (listp list)
        (eq (first list) tag)))
(defun tagged-tag (list) (first list))

(defun one-elem-p (list)
   (if (listp list)
      (null (cdr list))
      t))

(defun one-elem-this (list this)
	(and (one-elem-p list)
			(equal (car list) this)))
   
(defun at-least-n-p (ls n)
   (if (zerop n)
      t
      (and (not (null ls))
           (at-least-n-p (cdr ls) (1- n)))))
           
(defun get-first-n (ls n &optional (app nil))
   (if (zerop n)
      app
      (cons (car ls)
            (get-first-n (cdr ls) (1- n) app))))
            
(defun drop-first-n (ls n)
   (if (zerop n)
      ls
      (drop-first-n (cdr ls) (1- n))))

(defun try-one (ls)
   (if (one-elem-p ls)
      (first ls)
      ls))

(defun has-elem-p (list el) (ensure-bool (member el list)))
(defun has-test-elem-p (list el test) (ensure-bool (member el list :test test)))
(defun find-anywhere (item tree)
   "Does item occur anywhere in tree?"
   (if (atom tree)
      (if (eql item tree) tree)
      (or (find-anywhere item (first tree))
          (find-anywhere item (rest tree)))))

(defun create-hash-set (ls)
   (let ((hash (make-hash-table :test #'equal)))
      (dolist (a ls)
         (setf (gethash a hash) t))
      hash))
      
(defun remove-hash-set (hash-set el) (remhash el hash-set))
(defun in-hash-set-p (hash-set el) (gethash el hash-set))

(defmacro do-hash-set ((el hash-set) &body body)
   `(loop for ,el being the hash-keys of ,hash-set
         do (progn ,@body)))
         
(defun copy-hash-table (h1 &optional (copy-fn #'identity))
  (let ((h2 (make-hash-table :test #'equal)))
    (maphash #'(lambda (key val) (setf (gethash key h2) (funcall copy-fn val)))
	     h1)
    h2))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defmacro any (predicates val)
   `(or ,@(mapcar (lambda (pred) `(,pred ,val)) predicates)))
   
(defun dunion (l1 l2) (union l1 l2 :test #'equal))

(defmacro filter (&rest args) `(remove-if-not ,@args))

(defun mapfilter (trans f l) (mapcar trans (filter f l)))

(defun filter-first (fun ls)
	"Find all the initial items x from ls where fun(x) is true."
	(cond
		((null ls) nil)
		(t
			(when (funcall fun (first ls))
				(cons (first ls) (filter-first fun (rest ls)))))))

(defun enumerate (a b)
   (if (> a b)
      nil
      (cons a (enumerate (1+ a) b))))

(defun remove-tree (tree ls) (remove tree ls :test #'equal))
(defun remove-tree-first (tree ls) (remove tree ls :test #'equal :count 1))

(defmacro delete-one (from item)
	`(setf ,from (delete ,item ,from :test #'equal)))

(defmacro delete-all (from ls)
   (alexandria:with-gensyms (el)
      `(progn
         (dolist (,el ,ls)
            (setf ,from (delete ,el ,from :test #'equal)))
         ,from)))

(defun remove-all (from ls) (reduce #L(remove-tree !2 !1) ls :initial-value from))

(defmacro push-all (ls to)
   (alexandria:with-gensyms (el)
      `(dolist (,el ,ls)
         (push ,el ,to))))
         
(defmacro push-dunion (el to) `(setf ,to (dunion (list ,el) ,to)))

(defmacro push-dunion-all (ls to) `(setf ,to (dunion ,ls ,to)))

(defmacro set-tree-difference (t1 t2) `(set-difference ,t1 ,t2 :test #'equal))
(defmacro tree-intersection (t1 t2) `(intersection ,t1 ,t2 :test #'equal))
(defmacro tree-subsetp (t1 t2) `(subsetp ,t1 ,t2 :test #'equal))

(defun flatten (ls)
	(cond
		((null ls) (list))
		((listp ls)
			(append (flatten (first ls)) (flatten (rest ls))))
		(t (list ls))))

(defun set-equal-p (s1 s2)
	(and (subsetp s1 s2)
		  (subsetp s2 s1)))

(defun intersection-all (lists)
   "Returns the intersection of all sub-lists in 'lists'."
   (reduce #'intersection (rest lists) :initial-value (first lists)))

(defun split (fn l)
	(let (y n)
		(loop for el in l
				do
					(if (funcall fn el)
						(push el y)
						(push el n)))
		(cons (reverse y) (reverse n))))

(defun split-mult-return (fn l)
   (destructuring-bind (filtered . removed) (split fn l)
      (values filtered removed)))
         
(defmacro push-end (el ls)
   `(if (null ,ls)
      (setf ,ls (list ,el))
      (nconc ,ls (list ,el))))

(defun addify (ls &optional (n 0))
   (if (null ls)
      nil
      (cons n (addify (rest ls) (+ n (first ls))))))
      
(defun merge-strings (ls sep)
   (if (null ls)
      ""
      (reduce #L(if !1 (concatenate 'string !1 (list sep) !2) !2) ls)))
      
(defun get-tagged-elem (ls key)
   (when ls
      (let ((found (find-if #L(eq (first !1) key) ls)))
         (when found
            (rest found)))))

(defun shuffle-list (ls)
   (let ((hash-tbl (make-hash-table :test #'eq)))
      (sort ls #'< :key #'(lambda (x)
                              (multiple-value-bind (val found-p) (gethash x hash-tbl)
                                 (if found-p
                                    val
                                    (setf (gethash x hash-tbl) (random 1.0))))))))

(defun read-file (file)
   "Reads the entire file and returns a string."
   (with-open-file (str file
                        :direction :input
                        :if-does-not-exist :error)
      (reduce #L(concatenate 'string !1 !2 (list #\newline))
         (loop for line = (read-line str nil nil)
                while line
                collect line) :initial-value "")))
                
(defun file-exists-p (file)
   (probe-file file))
   
(defun list-of-lists-p (ls)
   (and (listp ls)
        (every #'listp ls)))
        
(defmacro loop-pairwise ((a b) ls &body body)
   (alexandria:with-gensyms (tail)
      `(loop for (,a . ,tail) on ,ls by #'cdr
             do (unless (null ,tail)
                  (let ((,b (car ,tail)))
                     ,@body)))))

(defun all-equal-p (ls &key (test #'eq))
   (loop-pairwise (a b) ls
      (unless (funcall test a b)
         (return-from all-equal-p nil)))
   t)
   
(defun always-true (&rest rest)
   "Function that always returns true."
   (declare (ignore rest))
   t)
   
(defun ordered-p (ls &optional (fn #'<))
   (all-equal-p ls :test fn))
   
(defun mappend (fn &rest lsts)
  "maps elements in list and finally appends all resulted lists."
  (apply #'append (apply #'mapcar fn lsts)))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defun next-multiple-of-uint (x) (ceiling x 64))
