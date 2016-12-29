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

#|
loop on:
number list vector hash package form

loop LABEL-form for var FROM-form TO-form BY-form Condition-form BODY-form
TO: to up down  below above
BODY: do collect append nconc count sum maximize minimize
BODY-form: verb form [into var]
Condition-form: conditional test-form
conditional: if when unless
LABEL-form: named symbol

Termination-form: [while unless always never thereis] test-form


loop for var IN-form list-form by-func BODY-form
IN: in across on

loop for var being the things in hash-or-package ...
loop for k being the hash-keys in h using (hash-value v) ...

loop clause-orders:
named
initially with for repeat
un-conditional-execution accumulation termination-test
finally
ref http://www.gigamonkeys.com/book/loop-for-black-belts.html
|#

(loop for i from 0 to 10 by 2 do (print i))

(defun read-word (&optional (stream *standard-input*))
  (loop
     for c = (peek-char nil stream nil nil) ;include whitespace
     while (and c (eql c (peek-char t stream nil nil))) ;skip whitespace
     collect (read-char stream) into letters
     finally (return (coerce letters 'string))))

(loop for i from 1 to 10
		if (evenp i)
		minimize i into min-even and 
		maximize i into max-even and
		sum i into even-total
		else
		minimize i into min-odd and
		maximize i into max-odd and
		when (zerop (mod i 5)) 
		sum i into fives-total
		end
		and sum i into odd-total
		do ;;empty
		finally (format t "~a" min-even))

;;alpha-char-p
(defun space-char-p (ch)
  (if (find ch '(#\Space #\Newline #\Tab #\Return #\Page #\Vt)) t nil))

(defun skip-space (stream)
  (do ((ch (peek-char nil stream nil nil) (peek-char nil stream nil nil)))
	  ((or (eql ch nil) (not (space-char-p ch))) ch)
	 (read-char stream nil nil)))

(defun read-word (stream)
  (skip-space stream)
  (do ((letters nil) (ch (peek-char nil stream nil nil) (peek-char nil stream nil nil)))
	  ((or (eql ch nil) (space-char-p ch)) (if letters (coerce letters 'string)))
	(setf letters (append letters (list (read-char stream nil nil))))))


(defun read-word (stream)
  (do ((letters nil) (ch (peek-char nil stream nil nil) (peek-char nil stream nil nil)))
	  ((or (eql ch nil) (not (eql ch (peek-char t stream nil nil)))) (if letters (coerce letters 'string)))
	(setf letters (append letters (list (read-char stream nil nil))))))

(with-input-from-string (stream "ab c d e ")
  (do ((w (read-word stream) (read-word stream))) ((not w))
	(format t "==~a==~%" w)))
