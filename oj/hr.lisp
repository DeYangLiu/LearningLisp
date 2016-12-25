#|
stdio:
read-line
parse-integer
read

princ

do list of variable initial-value next-value,
   finish condition and return value,
   body.

array to list:
format t "~{~a~^ ~}~%" (map 'list #'identity arr)
list to array:
(make-array len :initial-contents left)

test:
cat in.txt | sbcl --script hr.lisp


|#

;;;;(defvar n 5)
;;;;(defvar arr (make-array n :initial-contents '(2 4 6 8 3)))

(defun insert-sort (arr size)
  (do ((n 2 (1+ n))) ((> n size))
	(let ((ii -1) (e (aref arr (1- n))))
	  (do ((i (1- (1- n)) (1- i))) ((< i 0) )
		(if (> e (aref arr i))
			(progn
			  (setf ii (1+ i))
			  (return i)))
		(setf ii i)
		(setf (aref arr (1+ i)) (aref arr i)))
	  (setf (aref arr ii) e)
	  (print-array arr))))

(defun shifts-of-insert-sort (arr size)
  (let ((cnt 0))
	(do ((n 2 (1+ n))) ((> n size) cnt)
	  (let ((ii -1) (e (aref arr (1- n))))
		;;;;(format t "n ~a e ~a array ~a cnt ~a~%" n e arr cnt)
		(do ((i (1- (1- n)) (1- i))) ((< i 0) )
		  (if (>= e (aref arr i))
			  (progn
				(setf ii (1+ i))
				(return i)))
		  (incf cnt)
		  (setf ii i)
		  (setf (aref arr (1+ i)) (aref arr i)))
		(setf (aref arr ii) e)
		))))


(defun print-array (arr)
  (format t "~{~a~^ ~}~%" (map 'list #'identity arr)))


(defun partition-array (arr)
  ;;(format t "enter: ~a~%" arr)
  (let ((size (length arr)) (equal (aref arr 0)) (left '()) (right '()))
	(do ((i 0 (1+ i))) ((>= i size))
	  (if (> equal (aref arr i))
		  (push (aref arr i) left)
		  (if (< equal (aref arr i))
			  (push (aref arr i) right))))
	(setf left  (nreverse left))
	(setf right  (nreverse right))
	;;(format t "reversed: ~a == ~a ~%" left right)
	(let ((len (length left)))
	  (if (> len 1)
		  (setf left (partition-array (make-array len :initial-contents left)))))
	(let ((len (length right)))
	  (if (> len 1)
		  (setf right (partition-array (make-array len :initial-contents right)))))
	(setf equal (concatenate 'list left (list  equal) right))
	(format t "~{~a~^ ~}~%" equal)
	equal))

(defun partition-array (arr left right)
  (let ((pivot (aref arr right)) (stored left) (tmp 0))
	(do ((i left (1+ i))) ((= i right))
	  (let ((a (aref arr i)) )
		(if (< a pivot)
			(progn 
			  (setf tmp a)
			  (setf (aref arr i) (aref arr stored))
			  (setf (aref arr stored) tmp)
			  (incf stored)))))
	(setf tmp (aref arr stored))
	(setf (aref arr stored) pivot)
	(setf (aref arr right) tmp)
	stored))

(defun quicksort (arr left right)
  (let ((stored left))
	(if (> (- right left) 0)
		(progn
		  ;;(format t "beg: ~a == ~a ~a~%" arr left right)
		  (setf stored (partition-array arr left right))
		  ;;(format t "end: ~a == ~a ~a stored ~a~%" arr left right stored)
		  (print-array arr)
		  (if (> (- stored left) 1) (quicksort arr left (1- stored)))
		  (if (> (- right stored) 1) (quicksort arr (1+ stored) right))
		  )))
  arr)



(defvar n (read)
  )
(defvar arr (make-array n ;:initial-contents '(1 3 9 8 2 7 5)
						))


(do ((i 0 (1+ i)))
	((= i n))
  (setf (aref arr i) (read)))


(quicksort arr 0 (1- n))









