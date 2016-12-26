#|
debugging

;;;;debug flag = 3, normal is 1.
(declaim (optimize (debug 3)))

C-M-x ;run
SLDB show Retry, Abort, and  backtraces of errors.
add break points by (break) and fix it.
C-M-x ;recompile
choose Retry in SLDY

recompile code at runtime.
it acted as if nothing had ever gone wrong.

slime-repl:
right click --> inspect --> modify value

trace diaglog buffer:
C-c T ;open trace diaglog buffer
C-c M-t your_function_name ;trace your function
G ;in trace buffer, show most recent trace


|#

(defun binary-search (value array)
  (let ((low 0)
        (high (1- (length array))))
 
    (do () ((< high low) nil)
      (let ((middle (floor (+ low high) 2)))
 
        (cond ((> (aref array middle) value)
               (setf high (1- middle)))
 
              ((< (aref array middle) value)
               (setf low (1+ middle)))
 
              (t (return middle)))))))

(defun span (predicate list)
  (let ((tail (member-if-not predicate list)))
    (values (ldiff list tail) tail)))
 
(defun less-than (x)
  (lambda (y) (< y x)))
 
(defun insert (list elt)
  (multiple-value-bind (left right) (span (less-than elt) list)
    (append left (list elt) right)))
 
(defun insertion-sort (list)
  (reduce #'insert list :initial-value nil))

(do ((i 0 (1+ i))) ((> i 5))
  (if (= i 4) (return i)))

(block done
  (do ((i 0 (1+ i))) ((> i 5))
	(if (= i 4) (progn
					(print i)
					(return-from nil i)))))


(defun fib (n)
  (if (<= 0 n 1)
	  n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(fib 10)

(defun sum (xs &optional (acc 0))
  (if (null xs)
      acc
      (sum (cdr xs) (+ (car xs) acc))))
 
(sum '(1 2 3 4 5))
