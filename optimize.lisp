(set-dispatch-macro-character #\# #\f
   (lambda (stream sub-char numarg)
     (declare (ignore stream sub-char))
     (setq numarg (or numarg 3))
     (unless (<= numarg 3)
       (error "Bad value for #f: ~a" numarg))
     `(declare (optimize (speed ,numarg)
                         (safety ,(- 3 numarg))))))


(defmacro fast-progn (&rest body)
  `(locally #f ,@body))


(defmacro safe-progn (&rest body)
  `(locally #0f ,@body))

(defmacro dis (args &rest body)
  `(disassemble
     (compile nil
       (lambda ,(mapcar (lambda (a)
                          (if (consp a)
                            (cadr a)
                            a))
                        args)
         (declare
           ,@(mapcar
               #`(type ,(car a1) ,(cadr a1))
               (remove-if-not #'consp args)))
         ,@body))))

(dis ((fixnum x)) (the fixnum (+ x 1)))
(dis ((fixnum x)) #f (the fixnum (+ x 1)))


(defun xplusone (x)
  (declare (type fixnum x))
  (fast-progn
   (the fixnum (+ 1 x))))


;;define an inner function and execute it
(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
				,@body))
		   (,n ,@(mapcar #'cadr letargs))))


(nlet fact ((n n))
		(if (zerop n) 1
			(* n (fact (- n 1)))))

;;n = fact, letargs = ((n n )), body = (if ...)
;;(lables ((fact ))
(LABELS
 ((FACT (N)
   (IF (ZEROP N)
       1
       (* N (FACT (- N 1))))))
 (FACT N))

(cdr nil)

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                        (car x)
                        (rec (cdr x) acc))))))
    (rec x nil)))

(defun g!-symbol-p (s)
  (and (symbolp s)
	   (> (length (symbol-name s)) 2)
	   (string= (symbol-name s) "G!" :start1 0 :end1 2)))


(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
			   (remove-if-not #'g!-symbol-p
							  (flatten body)))))
	`(defmacro ,name ,args
	   (let ,(mapcar
			  (lambda (s)
				`(,s (gensym ,(subseq (symbol-name s) 2))))
			  syms)
		 ,@body))))

(defmacro/g! nif (expr pos zero neg)
  `(let ((g!result ,expr))
	 (cond ((plusp g!result) ,pos)
		   ((zerop g!result) ,zero)
		   (t ,neg))))

(nif 0 "pos" "zero" "neg")
