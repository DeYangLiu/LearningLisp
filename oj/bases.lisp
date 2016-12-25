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
