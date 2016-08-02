;;;;aima-lisp: aima-compile
;;;; Can't declare constant variable locally special: +NO-BINDINGS+
;;sbcl relative: can't redefine constant
;;http://www.sbcl.org/manual/#Defining-Constants
;;file:logic/algorithms/unify.lisp
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

;;change defconstant to define-constant
(define-constant +no-bindings+ '((nil))
  "Indicates unification success, with no variables.")
;;file: logic/algorithms/normal.lisp
(define-constant +logical-connectives+ '(and or not => <=>))
(define-constant +logical-quantifiers+ '(forall exists))

;;tell-task.lisp : ask-each:
;;remark (declare (special +no-bindings+))


;;;;declare error
(defconstant +len+ 3)

(defun foo (x) 
  (declare (type (simple-array fixnum (+len+)) x))
  x)

;;change to:
(deftype len-3-fixnum-array () `(array fixnum (,+len+)))

(defun foo (x)
  (declare (type len-3-fixnum-array x))
  (print x))

(foo (make-array 3 :element-type 'fixnum))
;; #(0 0 0)

