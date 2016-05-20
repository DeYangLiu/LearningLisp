;;;;https://github.com/markbrown778/mandelbrot
(defpackage :mandelbrot
  (:use :common-lisp :zpng)
  (:export #:write-file))

(in-package :mandelbrot)

(defconstant +max-iterations+ 255)


(defun mandelbrot-function (z c)
  (+ c (* z z)))


(defun terminate-p (niter z)
  (or (= niter +max-iterations+) 
      (> (abs z) 2.0)))


(defun mandelbrot-eval (c)
  (labels ((x-mandelbrot-eval (niter z)
             (if (terminate-p niter z)
                 niter
                 (x-mandelbrot-eval (1+ niter) (mandelbrot-function z c)))))
     (x-mandelbrot-eval 1 #C(0 0))))


(defun compute-mandelbrot-set (re-min re-max im-min im-max xnsteps ynsteps)
  (let ((im-step (/ (- im-max im-min) (1- ynsteps)))
        (re-step (/ (- re-max re-min) (1- xnsteps))))
     (loop repeat ynsteps
           for im = im-min then (+ im im-step)
           collect
       (loop repeat xnsteps
             for re = re-min then (+ re re-step)
             collect (mandelbrot-eval (complex re im))))))


(defun map-value-to-grey (v)
  (let ((ramp (/ v 50)))
    (if (> ramp 1.0) 255 (floor (* 255.0 ramp)))))


(defun write-file (fname size)
  (let* ((png   (make-instance 'png
                               :color-type :grayscale
                               :width  size
                               :height size))
         (image (data-array png))
         (brot  (compute-mandelbrot-set -2 1 -1.5 1.5 size size)))
    (loop for row in brot
          for y from (1- size) downto 0
          do (loop for val in row
                   for x from 0 to (1- size)
                   do (setf (aref image y x 0) (map-value-to-grey val))))
    (write-png png fname)))

;; End 
