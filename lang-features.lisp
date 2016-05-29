#|在读取期定义读取宏，使得用户可以重新调整（reprogram）Lisp的语法；
在编译期运行代码，则是Lisp宏的工作基础；
在运行期编译代码，使得Lisp可以在Emacs这样的程序中，充当扩展语言（extension language）；
在运行期读取代码，使得程序之间可以用S-表达式（S-expression）通信，
compile
load


代码使用符号和常量组成的树形表示法

函数也是一种数据类型。

在Lisp语言中，所有变量实际上都是指针，所指向的值有类型之分，而变量本身没有。复制变量就相当于复制指针，而不是复制它们指向的数据。

符号实际上是一种指针，指向储存在哈希表中的字符串。

垃圾回收机制。

程序只由表达式（expression）组成。语句，块不需要。

# 累加器
(defun foo (n)
  (lambda (i) (incf n i)))

# exception
handler-case, restart-bind

# fasl (fast loading)
sbcl --noinform --eval "(compile-file \"in.lisp\")" --eval "(quit)" 

sbcl --noinform --load in.fasl --quit # --end-toplevel-options "$@"

|#

(let ((y 7))
  (defun scope-test (x)
    (list x y)))

(let ((y 5))
  (scope-test 3)) ;=> (3 7)

(defvar x 3)
(defun foo (x)  (foo2))
(defun foo2 () (print x))
(let ((x 4)) (foo x)) ;=> 4


#|
# lexical vs. dynamic scope
;;=> (3 7) is lexical scope, 4 is dynamic scope (funcall chain).
dynamic attribute is set by defvar or defparameter or (declare (special x))
dynamic scope can be for temporarily masking a global variable.


In Common Lisp, default is lexical scope.
bound variable: when a symbol is used for expressing a variable.
e.g. let, do, function parameters
closure: function definition + free variables

procedure abstraction
modularity
side effect
historical evolution

decomposition of state:
a low-level procedure may have state variables which are not of interest to
intermediate routines, but which must be controlled at a high level.
dynamic scoping allows any procedure to access/ignore states.



modularity:
a good package encapsulates a concept independent of its implementation,
independent of its context -- referecntial transparency.
dynamic scoping -- funarg problem and not refercential transparency.
lexcial scoping -- can't forward reference

ref: the art of the interpreter or the modularity complex
|#

(defun list+ (lst n)
  (mapcar #'(lambda (x) (+ x n)) lst))
(list+ '(1 2 3) 10)

(let ((cnt 0))
  (defun new-id () (incf cnt)))

#|
# static vs. dynamic typing: only values have types.
# strong vs. weak: "8" - 5, C pointer dereference,

|#

;;;; (random 100)
(defun random-generator (seed)
  (lambda () (setq seed (mod (+ (* seed 204013) 2531011) 100))))

(defvar r (random-generator 1))
(funcall r) 

(defun make-secret-keeper ()
  (let ((password nil) (secret nil))
    (lambda (operation &rest arguments)
	(ecase operation
	  (set-password
	   (let ((new-passwd (first arguments)))
	     (if password
		 "Can't-already set"
		 (setq password new-passwd))))
	  (change-password
	   (let ((old-passwd (first arguments)) (new-passwd (second arguments)))
	     (if (eq old-passwd password)
		 (setq password new-passwd)
		 "Not changed")))
	  (set-secret
	   (let ((passwd (first arguments)) (new-secret (second arguments)))
	     (if (eq passwd password)
		 (setq secret new-secret)
		 '|Wrong password|)))
	  (get-secret
	   (let ((passwd (first arguments)))
	     (if (eq passwd password)
		 secret
		 "Wrong password")))))))

(defvar secret-1 (make-secret-keeper))
(funcall secret-1 'set-password 'valentine)
(funcall secret-1 'set-secret 'valentine 'deep-dark)
(funcall secret-1 'get-secret 'valentine)
(funcall secret-1 'change-password 'valentine 'valen)
(funcall secret-1 'get-secret 'valentine)


;;;; multiple-values
(setf (values q r) (floor 11 4))

(multiple-value-bind (inc dec)
    (let ((counter 0))
      (values
       (lambda () (incf counter) (print counter))
       (lambda () (decf counter) (print counter))))
  (funcall inc)
  (funcall dec)
  )

;;;; symbol vs. value
;;; value has two more names
(setq L1 '(a b c))
(setq L2 L1)
(eq L2 L1) ;=> t
(setq L3 '(a b c))
(eq L3 L1) ;=> nil
(equal L3 L1) ;=> t
;;; a symbol can name a value for function, variable, print name,  property list...
(setq first '(the first one))
(first '(1 2 3))
(setf (get 'first 'color) 'red)
(symbol-plist 'first)


;;;; lisp does right things for numbers: rational, bignum
(/ 1 3)
(* 1000 123456789012345)

#|
 defun is to define top-level function.
 labels is to define local recursive functions.
 flet is to define local functions.

defun is a macro which do
(setf (symbol-function 'fn-name) lambda-expr)
(document ...)


|#
(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))
(flatten '(1 (2 3 () (4 5)))) ;=> (1 2 3 4 5)

;;;;intern: string -> symbol
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))
(defun symb (&rest args)
   (values (intern (apply #'mkstr args))))

(mkstr 'a "BC" #\( 0) ;=> "ABC(0"
(symb 'a "BC" #\( 0) ;=> |ABC(0|
(type-of *) ;=> SYMBOL

(symbol-name 'symb) ;=> "SYMB"
