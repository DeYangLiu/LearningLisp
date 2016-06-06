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
