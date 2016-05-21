#|
destructuring-bind examples:

(destructuring-bind (&key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z)) ==> (:X 3 :Y 2 :Z 1)

(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2) 3)
  (list :x x :y1 y1 :y2 y2 :z z)) ==> (:X 1 :Y1 2 :Y2 NIL :Z 3)

(destructuring-bind (x (y1 y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z)) ==> (:X 1 :Y1 2 :Y2 20 :Z 3)

|#

(defun quicksort (list)
  (when list
    (destructuring-bind (p . xs) list
      (let ((lesser (remove-if-not (lambda (x) (< x p)) xs))
            (greater (remove-if-not (lambda (x) (>= x p)) xs)))
        (append (quicksort lesser) (list p) (quicksort greater))))))

(quicksort '(1 2 4 3))
;=> (1 2 3 4)

(defmacro @ (value bind list test)
  (let ((newlist (gensym)))
    `(let ((,newlist nil))
       (dolist (,bind ,list)
         (when ,test
           (push ,value ,newlist)))
       (nreverse ,newlist))))
;;note that lexical scope and push+nreverse.


;; Something like this in Haskell: [x*2 | x<-[1, 2, 3, 4, 5, 6, 7], x>3]
(@ (* 2 x) x (list 1 2 3 4 5 6 7) (> x 3))
;;-> (8 10 12 14)

(defgeneric lt (some other))
(defmethod lt ((some number) (other number)) (< some other))
(defmethod lt ((some string) (other string)) (string< some other))


;;Haskel
;; qsort (p:xs) = qsort [x | x<-xs, x<p] ++ [p] ++ qsort [x | x<-xs, x>=p]
(defun qsort (l)
  (when l (destructuring-bind (p . xs) l
	    (append (qsort (@ x x xs (lt x p))) (list p)
		    (qsort (@ x x xs (not (lt x p))))))))

(qsort '("list" "is" "only" "a" "test"))
;=> ("a" "is" "list" "only" "test")
(qsort '(12/11 -1.09 300))
;=> (-1.09 12/11 300)
