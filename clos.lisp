#|
common lisp object system
;;
$ sbcl --noinform --load clos.lisp  --quit
#<AARDVARK {1005F2A003}> is an animal. It has 4 legs and comes from china.
123 could be anything, for all I care.
$

;;define structrue
(defstruct point
  x
  y
  z)
;=> define 
;make-point point-p point-x point-y point-z copy-point
(defun distance-from-origin (point)
  (let* ((x (point-x point))
	 (y (point-y point))
	 (z (point-z point)))
    (sqrt (+ (* x x) (* y y) (* z z)))))

(defun reflect-in-y-axis (point)
  (setf (point-y point) (- (point-y point))))

(defvar my-point (make-point :x 3 :y 4 :z 12))

(reflect-in-y-axis my-point)

(defvar a-similar-point #s(point :x 3 :y -4 :z 12))

(equalp my-point a-similar-point)

(unintern 'point)
|#


(defclass point ()
  (x
   y
   z))
(defvar my-point (make-instance 'point))


(defun set-point-values (point x y z)
  (setf (slot-value point 'x) x)
  (setf (slot-value point 'y) y)
  (setf (slot-value point 'z) z))



(set-point-values my-point 3 4 12)
(defun distance-from-origin (point)
  (with-slots (x y z)
      point
    (sqrt (+ (* x x) (* y y) (* z z)))))
(distance-from-origin my-point)

(defclass daft-point ()
  ((x  :accessor daft-x  :initarg :x)
   (y  :accessor daft-y  :initform 3.14159)
   (z  :reader   daft-z  :allocation :class)))

(setf (slot-value (make-instance 'daft-point) 'z) 42)
(defvar my-daft-point (make-instance 'daft-point :x 19))
(daft-z my-daft-point)

(defclass animal ()
  ((legs  :reader leg-count  :initarg :legs)
   (comes-from  :reader comes-from  :initarg :comes-from)))
(defclass mammal (animal)
  ((diet  :initform 'antelopes  :initarg :diet)))
(defclass aardvark (mammal)
  ((cute-p  :accessor cute-p  :initform nil)))

(fmakunbound 'my-describe)
(defgeneric my-describe (thing)) ;;;suppress warning

(defmethod my-describe (thing)
  (format t "~s could be anything, for all I care.~%" thing))

(defmethod my-describe ((a animal))
  (format t "~s is an animal. It has ~d leg~:p and comes from ~a.~%"
	  a
	  (leg-count a)
	  (comes-from a)))

(my-describe (make-instance
	      'aardvark :diet 'green :legs 4 :comes-from "china"))
(my-describe 123)

