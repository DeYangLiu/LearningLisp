#|
http download sync v.s. async

some tests:
(setf drakma:*header-stream* *standard-output*)
(defvar x (drakma:http-request "http://lisp.org/"))

|#

(defparameter *urls*
    (list
        "http://blog.thezerobit.com/"
        "http://sbcl.org/"
	))


(ql:quickload "drakma")
(ql:quickload "chanl")

(defmacro time-it (&body body)
  (let ((start-time (gensym)))
    `(let ((,start-time (get-internal-real-time)))
       ,@body
       (format t "Runtime: ~a milliseconds.~%" (- (get-internal-real-time) ,start-time)))))


(defun do-request (url)
    (let ((start-time (get-internal-real-time)))
        (format t "Starting request: ~a~%" url)
        (drakma:http-request url)
        (let ((elapsed (- (get-internal-real-time) start-time)))
            (format t "Completed request in ~a ms: ~a~%" elapsed url))))

(time-it (dolist (url *urls*) (do-request url)))


(defun do-request-chan (url chan)
    (let ((start-time (get-internal-real-time)))
        (chanl:send chan (format nil "Starting req: ~a~%" url))
        (drakma:http-request url)
        (let ((elapsed (- (get-internal-real-time) start-time)))
           (chanl:send chan
                (format nil "Completed in ~a ms: ~a~%" elapsed url)))))

(time-it
    (let ((chan (make-instance 'chanl:channel)))
        (dolist (url *urls*)
            (chanl:pexec () (do-request-chan url chan)))
        (dotimes (x (* 2 (list-length *urls*)))
           (format t (chanl:recv chan)))))





