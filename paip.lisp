
;;return multiple values in a flatten list.
;;apply #'append (list nil (list v1 v2))
(defun numbers-and-negations (input)
  (apply 
   #'append (mapcar #'(lambda (x)
			(if (numberp x) (list x (- x))))
		    input)))
(numbers-and-negations '(a 1 b)) ;=> (1 -1)

;;^ --> lambda : create new functions at run time. sec 3.6

;;the first matching item
(find 1 '(0 -1.0 2 1) :key #'abs :test #'equalp) ;=> 1.0

;;all matchings
;;remove-if-not
(remove 1 '(1 2 3 1) :test #'/=) ;=> (1 1)
(defun find-all (item sequence &key (test #'eql) )
  (remove item sequence :test (complement test)))

(find-all 1 '(1 2 3 1) )

(defvar *state* nil "the current state: a list of conditions.")
(defvar *ops* nil "a list of available operators.")
(defstruct op
  "an operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun GPS (*state* goals *ops*)
  "General Problem Solver: achive all goals using *ops*"
  (if (every #'achieve goals) 'solved))

(defun achieve (goal)
  "A goal is achieved if it already holds or exists applicable appropriate op."
  (or (member goal *state*)
      (some #'apply-op (find-all goal *ops* :test #'appropriate-p)))
  )

(defun appropriate-p (goal op)
  "An op is appropriate if it is in goal's add list."
  (member goal (op-add-list op)))

(defun apply-op (op)
  "Print a message and update *state* if op is applicable."
  (when (every #'achieve (op-preconds op))
    (format t "executing ~a ~%" (op-action op))
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))
    t))

(defparameter *school-ops*
  (list
   (make-op :action 'drive-son-to-school
	    :preconds '(son-at-home car-works)
	    :add-list '(son-at-school)
	    :del-list '(son-at-home))
   (make-op :action 'shop-installs-battery
	    :preconds '(car-needs-battery shop-knows-problem shop-has-money)
	    :add-list '(car-works))
   (make-op :action 'tell-shop-problem
	    :preconds '(in-communication-with-shop)
	    :add-list '(shop-knows-problem))
   (make-op :action 'telephone-shop
	    :preconds '(know-phone-number)
	    :add-list '(in-communication-with-shop))
   (make-op :action 'loop-up-number
	    :preconds '(have-phone-book)
	    :add-list '(know-phone-number))
   (make-op :action 'give-shop-money
	    :preconds '(have-money)
	    :add-list '(shop-has-money)
	    :del-list '(have-money))))

(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school)
     *school-ops*)
"=> SOLVED
executing LOOP-UP-NUMBER 
executing TELEPHONE-SHOP 
executing TELL-SHOP-PROBLEM 
executing GIVE-SHOP-MONEY 
executing SHOP-INSTALLS-BATTERY 
executing DRIVE-SON-TO-SCHOOL
"
