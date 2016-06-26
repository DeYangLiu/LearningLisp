
;;return multiple values in a flatten list.
;;apply #'append (list nil (list v1 v2))
(defun numbers-and-negations (input)
  (apply 
   #'append (mapcar #'(lambda (x)
			(if (numberp x) (list x (- x))))
		    input)))
(numbers-and-negations '(a 1 b)) ;=> (1 -1)

;;^ --> lambda : create new functions at run time. sec 3.6

