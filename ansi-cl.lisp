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

