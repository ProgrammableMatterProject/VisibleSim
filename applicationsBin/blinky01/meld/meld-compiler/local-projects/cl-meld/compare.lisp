(in-package :cl-meld)

(defmacro expr-cond (&rest rest)
   `(cond
      ,@(loop for (item . code) in rest
              collect (if (eq item t)
                          `(t ,@code)
                          `((,item a) (when (,item b) ,@code))))))

(defun expr-eq-p (a b)
   "Compares expression a and expression b and returns T if they are equal, nil otherwise."
   (expr-cond
      (int-p
         (eq (int-val a)
             (int-val b)))
      (var-p (var-eq-p a b))
      (float-p (eq (float-val a) (float-val b)))
      (host-id-p t)
      (nil-p t)
      (world-p t)
      (addr-p (eq (addr-num a) (addr-num b)))
      (call-p (and (string-equal (call-name a) (call-name b))
                   (every #L(expr-eq-p !1 !2) (call-args a) (call-args b))))
      (cons-p (and (expr-eq-p (cons-head a) (cons-head b))
                   (expr-eq-p (cons-tail a) (cons-tail b))))
      (head-p (expr-eq-p (head-list a) (head-list b)))
      (tail-p (expr-eq-p (tail-list a) (tail-list b)))
      (not-p (expr-eq-p (not-expr a) (not-expr b)))
      (test-nil-p (expr-eq-p (test-nil-expr a) (test-nil-expr b)))
      (convert-float-p (expr-eq-p (convert-float-expr a) (convert-float-expr b)))
      (op-p (and (eq (op-op a) (op-op b))
                 (expr-eq-p (op-op1 a) (op-op1 b))
                 (expr-eq-p (op-op2 a) (op-op2 b))))
      (t (error 'expr-invalid-error
               :text (tostring "Invalid expression type for expr-eq-p: ~a" a)))))