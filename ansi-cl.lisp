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

#|
local function: labels
\(name parameter-list . body)

(do ((x a (b x)))
    (test ret)
  body)
<===>
(lables ((recur (x)
		(cond
		  (test ret)
		  (t
		   body
		   (recur (b x))))))
	(recur a))

dynamic scope: 
change temporarily some global variable.

closure:
a function that refers to a variable defined outside it.

recursion:
 list -- a list is nil or cons whose cdr is list.
 member -- something is a member of a list, if it is the car, or member of cdr; empty list has no member.

|#




(let ((*print-base* 16))
  (princ 127))

(defun combiner (x)
  (typecase x
    (number #'+)
    (list #'append)
    (t #'list)))
(defun combine (&rest args)
  (apply (combiner (car args)) args))


(setf fn (let ((i 3))
	   #'(lambda (x) (+ x i))))
(funcall fn 2)

(defun our-funcall (fn &rest args)
  (apply fn args))

(defun arg-num (&rest args)
  (length args))

(arg-num 1 2 3)

(defun bin-search (obj vec &optional (start 0)
			     (end (1- (length vec))))
  ;(format t "~a ~a~%" start end)
  (let ((range (- end start)))
    (if (zerop range)
	(if (equal obj (aref vec start))
	    start)
	(let* ((mid (+ start (round (/ range 2))))
	       (obj2 (aref vec mid)))
	  ;(format t "mid: ~a obj: ~a~%" mid obj2)
	  (if (< obj obj2)
	      (bin-search obj vec start (1- mid))
	      (if (> obj obj2)
		  (bin-search obj vec (1+ mid) end)
		  mid))))))


(bin-search 2 '#(1 2 3))
;(bin-search #\b "abc")

;;;;return current largest number
(let ((largest nil))
  (defun current-largest (x)
    (if (null largest)
	(setf largest x)
	(if (> x largest)
	    (setf largest x)))
    largest))

(current-largest 1)

#|
character stream: src/dst of characters
binary stream: :element-type 'unsigned-byte


print1 prints double-quotes around string.
princ doesn't

format nil ... <==> sprintf ...

macro characters:
'a --> (quote a)
#' --> (function ...)
#(...) --> (vector ...)
#nA(...) -->(make-array n )
#\ --> char
#S(n ...) --> (defstruct ...)

|#
(setf path (make-pathname :name "my.fasl"))
(setf st (open path :direction :output :if-exists :supersede))
(format st "hi~%")
(close st)

(setf st (open path :direction :input))
(read-line st nil 'eof)
(close st)

(defun my-cat (file)
  (with-open-file (st file :direction :input)
    (do ((line (read-line st nil 'eof)
	       (read-line st nil 'eof)))
	((eql line 'eof))
      (format t "~a~%" line))))
(my-cat path)

;;;; ~S <==> print1
(let ((*print-array* t))
  (read-from-string (format nil "~S" (vector 1 2))))

;;;;put each line of a file into a list
(defun file-list (file)
  (with-open-file (st file :direction :input)
    (let ((lst nil))
      (do ((line (read-line st nil 'eof) (read-line st nil 'eof))) ((eql line 'eof))
	(push line lst))
      (nreverse lst))))

(file-list "my.fasl")

(defun copy-file (from to)
  (with-open-file (in from :direction :input :element-type 'unsigned-byte)
    (with-open-file (out to :direction :output :element-type 'unsigned-byte)
      (do ((c (read-byte in nil -1)
	      (read-byte in nil -1)))
	  ((minusp c))
	(declare (fixnum c))
	(write-byte c out)))))


#|
the name of symbol: 
 character converted to uppercase, but anything between vertical bar pair not.

a symbol is a object with structure:
 name
 plist
 package
 value
 function

package has name-symbol tables:
defpackage/in-package
intern -- let a variable belong  a package. 
intern = find-symbol + make-symbol.


keyword package: accessible anywhere
 :x <==> keyword:x

a symbol used as a lexical variable is just placeholder,
it will be compiled to a register or memory location.

eq -- is the same object (same address)
eql -- eq and also check number type.
equal -- eql and compare lists using eql on the leaves.

|#

(eql 3 3.0) ;=> NIL
(equal '("a") '("a"))

;;symbol to name
(symbol-name 'abc) ;=> "ABC"
(symbol-name '|abc |) ;=> "abc "
(symbol-plist 'abc)


;; to symbol, create in current package if not exist.
(intern "my-symbol")

;;note: upper-cased name of symbol
(defpackage "MY-APP"
  (:use "COMMON-LISP")
  (:export "FOO" "NOISE"))
(in-package "MY-APP")

(defun noise (animal)
  (case animal
    (:dog :woof)
    (:cat :meow)
    (:pig :oink)))

(in-package "COMMON-LISP-USER")
(my-app:noise :cat)


#|
number types:
 bit/fixnum/bignum/
 ratio
 short-float/single-float/double-float/long-float
 complex

fixnum -- integer small enough to fit in one word.

|#

(defun our-truncate (n)
  (if (> n 0)
      (floor n)
      (ceiling n)))

(our-truncate -1.3)


(typep (1+ most-positive-fixnum) 'bignum)

;;how many bits are used for fixnum? 29
(length  (format nil "~b" most-positive-fixnum))

(type-of 1L0) ;=> double-float
(type-of 1S0) ;=> single-float

;;;;
(defun intersect-point (x1 y1 x2 y2 x3 y3 x4 y4))

#|
method combination: 
 Mediator, Observer.

first-class functions:
 Command, Strategy, Visitor, Template-Method

multimethods: Builder

first-class types: 
 Abstract-Factory, Proxy

macro: 
 translate "go", direction (D) --> command(move(D))

backquote read-macro, comma, comma at.
unintended variable capture, multiple evaluation.

|#
;; observer is notify after every change.
(mapc #'notify-after '(cut paste edit))
(defun notify-after (fn)
  (eval `(defmethod ,fn :after (x)
		    (mapc #'notify (observers x)))))

(defmethod convert (type == "font" target::Tex))
(convert token.type tex-target)

(defmacro nil! (x)
  (list 'setf x nil))

;; while test body
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(let ((x 0))
  (while (< x 10)
    (princ x)
    (incf x)))

(defmacro ntimes (n &body body)
  (let ((g (gensym))
	 (h (gensym)))
    `(do ((,g 0 (+ ,g 1)) (,h ,n)) ((>= ,g ,h))
      ;(format t "~% ~a ~a ~%" ,g ,h)
      ,@body)))

(let ((n 5))
  (ntimes (decf n) (princ "."))) ;=> ....


(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))

(for x 1 5 (princ x)) ;=> 12345

;;evaluate length at compile-time
(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

(avg 1 2 3)

(defun in (obj &rest items)
  (dolist (x items)
    (if (eql x obj) (return t))))

(in (car '(* 1 2)) '+ '-)

;;double backquote
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (x)
		     `(,x (gensym))) syms)
     ,@body))

(let ((x -1))
  (with-gensyms (x y z)
    (setf x 0 y 1 z 2)
    (format t "~a ~a ~a~%" x y z))
  x)

;;macro that capture variables
(defmacro my-double (a)
  `(setf ,a (* ,a 2)))
(let ((x 1))
  (my-double x) x) ;=> 2


;;define if using cond
(defmacro my-if (test then &optinal else)
  `(cond
     (,test ,then)
     (t ,else)))

(if () 1)

#| drawbacks:
 n may capture outer variable n.
 n may be not known on compile-time.
 eval in macro is evil. 
|#

(defmacro nth-expr (n &rest exprs)
  `(eval (nth (1- ,n) (quote ,exprs))))

#| compute at compile-time: 
the symbol i is gone after macroexpand-1.
|#
(defmacro nth-expr (n &rest exprs)
  (let ((gn (gensym)))
    `(let ((,gn ,n) )
       (cond ,@(let ((i 0))
		    (mapcar 
		     #'(lambda (e)
			 (incf i)
			 `((= ,i ,gn) ,e)) exprs))))))

(let ((n 2))
  (nth-expr n (/ 1 0) (+ 1 2) (/ 1 0))) ;=> 3

#| expain
&rest exp ;=> calling parameters A B C  convert to (A B C)
`f ;=> f ;the symbol f
`,f ;=> (A B C) ;the symbol refers to
`,@f ;=> undefined, sbcl isuues error.

suppose e, f dosn't begin with `',@:
`(e ,f) ;=> list of symbol e and the evaulation of the form f.

`(e ,@f) ;=> 
 list of e, and the result of "evaluate and spice the form f";
 the form f are normally evaluated.
 if f is atom, f appear in middle : `(e ,@f g) will cause an error.

`(a ,@b c) <= (cons 'a (append b (list 'c)))

conclusion:
 * lisp form is composed of atom and list.
 * _quote and evalute_ apply to the lisp expression.
 * quote is to be literal, evalute is to compute.
 * comma must be in backquote list form.
 * nested ` and , search symbol from inter to outer.
|#

(defmacro my-expand (&rest exp)
  `(e ,@(print exp)))

(macroexpand-1 '(my-expand a b c)) 

#|
 repl shows: (A B C)
 returns: (E A B C)
why?
 \(print obj) print obj and return obj itself. 
so we see repl shows, and then substitude as:
`(e ,@'(E A B C))
and then spice:
`(e E A B C)
no common stuff, this is quote:
'(e E A B C)
|#

;;recusive macro
(defmacro ntime (n &body body)
  `(if (<= ,n 0)
       nil
       (progn
	 ,@body
	 (ntime (1- ,n) ,@body))))

(ntime 5 (princ "*") (princ "&")) ;=> *&*&*&*&*&

;; n of expr
(defmacro n-of (n expr)
  (let ((gn (gensym)) (gi (gensym)) (gret (gensym)))
    `(let ((,gn ,n) (,gret nil))
       (do ((,gi 0 (incf ,gi))) ((>= ,gi ,gn))
	 (push ,expr ,gret))
       (nreverse ,gret))))

(let ((i 0) (n 4))
  (n-of n (incf i))) ;=> (1 2 3 4)

;;define-modify-macro must be: lambda (place args)
(define-modify-macro append1f (val)
  (lambda (lst val) (append lst (list val))))

(let ((lst '(a)))
  (append1f lst 'b) lst)

;;debuging macro
(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro mvbind (&rest args)
  (let ((name 'multiple-value-bind))
    `(,name ,@args)))

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     (let ((name ',long))
       `(,name ,@args))))

;;outer ` and inner ,
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(abbrev mvbind multiple-value-bind)

(defmacro abbrevs (&rest names)
  `(progn
     ,@(mapcar
	#'(lambda (pair)
	    `(abbrev ,@pair))
	(group names 2))))

(abbrevs mvbind multiple-value-bind
	 dbind destructuring-bind)

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((recur (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (recur rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc))))))
    (if source (recur source nil))))

(group '(a b c d e) 2) ;=> ((A B) (C D) (E))
