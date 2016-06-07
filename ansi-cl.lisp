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
