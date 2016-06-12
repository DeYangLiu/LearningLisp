#|
notes from ansi common lisp

essential elements of lisp programming:
lists, specialized data structures, control, functions, io, symbols, numbers

advanced topics:
macro, CLOS, optimization, matching

toplevel -- interactive front-end.
atoms + lists = all lisp expressions.
evaluation rule:
 arguments of function calling are evaluated from left to right.
 atoms -- integer and strings are evaluating to themselves.
 symbols -- are varibales names, need quoted, otherwise will treat as variables.
 lists -- if not quoted, the list is treated as code and will evaluate.
list operations:
 cons, car, cdr, listp

nil -- (null nil) == (not nil)

\(defun name list-of-parameters expressions+)

recursion -- use metaphor of process rules, other than machine.

reading lisp code by indentation not by parentheses, and simply ignore them.

vim :set sm emacs: M-x lisp-mode

local variables:
 (let ((variable expression)+) expressions+)

global variables:
 defparameter

assignment:
 setf variable_or_expr value
functional programming:
 work by returning values instead of modifying things.
 for interactive testing

iteration:
 do ((variable inital update)+) (test expr*) bodys*
 dolist (obj lst) (setf len (+ len 1))

functions as objects:
 function apply funcall
 
lambda expression: lambda list-of-parameters body-expr*
 earlier lisp tell functions from oridnary list.
 refer function literally; 
 refer an interger literally by series of digits.

manifest typing:
 values have types, not variables.
 an object has more than one type: 27: fixnum, integer, rational, real, number, atom, t
 

|#

(defun ask-number ()
  (format t "please enter a number: ")
  (let ((var (read)))
    (if (numberp var)
	var
	(ask-number))))

(defparameter *glob* 1)
(defconstant limit (+ *glob* 1)) ;;note no star here

; (let ((limit 1)) limit) ;;constant can't be used as local variable.

(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(show-squares 2 5)

;;function is special operator, like quote
(function +) ;== #'+

(typep 27 'fixnum)

#|
value is pointer.

building lists:
 list cons copy-list append 

access:
 nthcdr last

copy-tree: recur both on car and cdr

is recur correct:
 cover all cases
 work for length of list = n, also work for n+1.
how do you see which parenthesis matches which?
how do our visualize all recur invocations?
 our-copy-tree: atom, one cons, n+1 cons

|#
(setf x '(1 2 3)) ;copy pointer to x
(setf y x) ;y = x
(eql y x) ;=> T

;;apply function to car of each list.
(mapcar #'list '(1 2 3) '(a b c d)) ;=> ((1 A) (2 B) (3 C))

(defun our-copy-list (lst)
  (if (atom lst)
      lst
      (cons (car lst) (our-copy-list (cdr lst)))))
(setf x '((a ) (b c) d)
      y (our-copy-list x))

(defun our-copy-tree (tree)
  (if (atom tree)
      tree
      (cons (our-copy-tree (car tree))
	    (our-copy-tree (cdr tree)))))
(setf x '(a (b c) (d e))
      y (our-copy-tree x))


(defun our-subst (new old tree)
  (if (eql old tree)
      new
      (if (atom tree)
	  tree
	  (cons (our-subst new old (car tree))
		(our-subst new old (cdr tree))))))
(our-subst 'y 'x '(and (integerp x) (zerop (mod x 2))))

(member 'a '((a b) (c d)) :key #'car :test #'equal)
(adjoin 'b '(a b c)) ;prefix member union

;;set union, intersection, set-difference
(union '(a b c) '(c b s)) ;=> (a c b s)

;;sequence length, subseq, reverse, sort
(sort '(0 2 1 3 8) #'<)
;;some and every predicate
(some #'evenp '(1 2 3))

;;;dot pairs
(assoc '- '((+ . "add") (- . "sub" ))) ;=> (- . "sub")


;;;3 versions of pos+: elem + its position, 
(defun pos+ (lst)
  (let ((n -1))
      (mapcar #'(lambda (x) 
		  (setf n (1+ n))
		  (format t "~a + ~a = ~a ~%" x n (+ x n))
		  (+ x n)
		  )
	      lst )))
(pos+ '(1 2 3 4)) ;=> (1 3 5 7)

(defun my-pos (lst pos)
  (if (null lst)
      nil
      (cons (+ (car lst) pos)
	    (my-pos (cdr lst) (1+ pos)))))
(defun pos+ (lst)
  (my-pos lst 0))

(defun pos+ (lst)
  (let ((ans nil))
    (do ((i 0 (1+ i))  )
	((null lst) 'done)
      (format t "~a ~a ~a ~%" i (car lst) ans )
      (push (+ i (car lst)) ans)
      (setf lst (cdr lst))) (reverse ans)))

(defun occurrences (lst)
  (let ((res nil))
    (dolist (obj lst)
      (let ((a (assoc obj res)))
	(if (null a)
	    (push (cons obj 1) res)
	    (incf (cdr a))
	    )))
    (sort res #'> :key #'(lambda (x) (cdr x)))))
(occurrences '(a b a d a c d c a)) ;=> ((A . 4) (C . 2) (D . 2) (B . 1))

;;print dot notation of a list
(defun showdots (lst)
  (if (null lst)
      (format t "NIL")
      (progn
	 (format t "(~a . " (car lst))
	 (showdots (cdr lst))
	 (format t ")"))))
(showdots '(A B C)) ;=> (A . (B . (C . NIL)))

;;;;shortest path
;; represent network with ((node . neighbors))
(setf min '((a b c) (b c) (c d)))

;; direct paths from node, prefix to path.
(defun new-paths (path node net)
  (mapcar #'(lambda (n)
	      (cons n path))
	  (cdr (assoc node net))))

;; bread fist search, using queue.
(defun bfs (end queue net)
  (if (null queue)
      nil
      (let* ((path (car queue)) (node (car path)))
	(print queue)
	(if (eql node end)
	    (reverse path)
	    (bfs end
		 (append (cdr queue) (new-paths path node net))
		 net)))))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(shortest-path 'a 'd min) ;=> (A C D)

#|
queue = 
\((A)) ;initial queue has start node.
\((B A) (C A)) ;dequeue a node, all paths from a are (a b) (a c), enqueue them.
\((C A) (C B A)) ;dequeued  is (b a), node is b. all paths from b is (c b).
\((C B A) (D C A)) ;node is c, all paths from c is (c d).
\((D C A) (D C B A)) ;node is c, all paths from c is (c d).
\(A C D) ;node is d, we find the end, reverse the path.
|#

;;;;find a longest path from a network which may conatain cycles.
(defun dfs (path net)
  (let* ((node (car path)) (neib (cdr (assoc node net))) (ret (reverse path)))
    (if (null neib)
	nil ;(format t "single: ~a~%" ret)
	(dolist (x neib)
	  (if (member x path :test #'equal)
	      nil ;(format t "circle: ~a ~%" ret)
	      (let* ((new-path (cons x path)) (tmp  (dfs new-path net)))
		(if (> (length tmp) (length ret))
		    (setf ret tmp))))))
    ret))

;(dfs '(c) '((a b c) (b c) (c d a)))

(defun longest-path (net)
  (let ((tmp nil))
    (dolist (adj net)
      (let* ((a (dfs (list (car adj)) net)))
	(if (> (length a) (length tmp))
	    (setf tmp a))
	nil ;(format t "==ret ~a~%" a)
	))
    tmp))

(longest-path '((a b c) (b c) (c d a))) ;=>(A B C D)

#|
sequences: include lists and vectors, 
 sequence function: subseq, reverse, sort, every, elt

make-array: aref
vector: svref
strings: char format

lisp programs could write lisp programs:
\(defstruct A x y\)--> make-P, A-p, copy-A, A-x A-y
typep: A < structure < atom < t

make-hash-table: gethash remhash

string parsing: many builtin functions.

|#
(elt '(a b c) 1) ;=> B
(position #\a "fantasia" :start 3 :end 5) ;=> 4
(position 3 '(1 0 7 5) :test #'<) ;=> 2
(remove-duplicates "abracadabra") ;=> last ocurrs: "cdbra"
(sort "elbow" #'char<)
(char "abc" 1) ;=> #\b
(format nil "~a or ~a" "true" "false")
(concatenate 'string "true " "false")

;;;; efficiency v.s. fluid
(setf ht (make-hash-table :test #'equal))
(setf (gethash 'shape ht) 'spherical
      (gethash 'size ht) 'giant
      (gethash 'color ht) 'red)
(maphash #'(lambda (k v)
	     (format t "~A:~A~%" k v)) ht)


;;;; rotate two dimension array 90 degree clockwise
;; first row --> last col: (0,j) --> (j,m-1)
;; i'th row --> last i col: (i,j) --> (j,m-1-i)
(defun quarter-turn (a)
    (let* ((dim (array-dimensions a)) (m (first dim)) (n (second dim))
	   (b (make-array (list n m))))
      (dotimes (j n)
	(dotimes (i m)
	  (setf (aref b j (- m 1 i)) (aref a i j))))
      b))

(quarter-turn #2A ((a b) (c d))) ;=> #2A((C A) (D B))

#|
chap5: control
blocks: progn, block, tagbody (goto)
context: let, destructuring-bind
conditionals: if, when, unless; cond, case;
iteration: (variable initial update)
multiple-values: values, multiple-value-bind
abort: catch-throw-protect
|#
;;;;list precedes of x in a vector 
(defun precedes (x v)
  (print v)
  (cond
    ((null v)
     nil)
    ((and (> (length v) 1) (equal x (aref v 1)))
     (cons (aref v 0) (precedes x (subseq v 1))))
    ((> (length v) 1) (precedes x (subseq v 1)))))

(precedes #\a "abracadabra")

(defun precedes-iter (x v)
  (let ((res nil) (m (1- (length v))))
    (dotimes (i m)
      (when (equal x (aref v (1+ i)))
	(push (aref v i) res)))
    res))

(precedes-iter #\a "abracadabra")

(coerce '(#\1 #\2 #\3) 'string)
(coerce "123" 'list)

(defun intersperse (x lst)
  (cond ((null lst)
	 nil)
	((= 1 (length lst))
	 lst)
	(t 
	 (cons (car lst) (cons x (intersperse x (cdr lst)))))))

(intersperse '- '(a b c d))

(defun itersperse (x lst)
  (let ((res (list (car lst))) (m (1- (length lst))))
    (dotimes (i m)
      (setf lst (cdr lst))
      (setf res (append res (list x (car lst)))))
    res))

(defun successive (lst)
  (if (< (length lst) 2)
      t
      (if (= 1 (abs (- (first lst) (second lst))))
	  (successive (cdr lst))
	  nil)))

(successive '(1 2 1 2 1 0))

;;put return value in test.
(defun successive (lst)
  (do ((tmp t))
      ((< (length lst) 2) tmp)
    ;(format t "~a ~a~%" tmp lst)
    (setf tmp (and tmp (= 1 (abs (- (first lst) (second lst)))))
	  lst (cdr lst))))

;;use block-return
(defun successive (lst)
  (block nil 
    (mapc #'(lambda (x y)
	      (if (= 1 (abs (- x y)))
		  t
		  (return nil)))
	  lst (cdr lst))
    t))

;;;;return max and min of a vector
(defun max-min (v)
  (if (<= (length v) 1)
      (values (car v) (car v))
      (multiple-value-bind (x y) (max-min (cdr v))
	(values (max (car v) x) (min (car v) y)))))

(max-min '(1 2 3))

;;;catch-throw-protect
(setf x 1)

(defun sub ()
  ;(throw 'abort 99)
  t)

(catch 'abort
  (unwind-protect
    (format t "before sub x ~a~%" x)
    (sub)
    (setf x 2))
  (format t "normal goes here"))
