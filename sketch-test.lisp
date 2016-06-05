#|
shell>
glxinfo | grep "OpenGL version" #should 3.3+
sudo apt-get install libsdl2-image-dev libsdl2-ttf-dev
repl>
(ql:quickload :sketch)
(ql:quickload :sketch-examples)
(make-instance 'sketch-examples:hello-world)

|#

;;;;origin version: hello-world.lisp
(in-package #:sketch-examples)

(defsketch hello-world
    ((title "Hello, world!")
     (unit (/ width 10))
     (height width))
  (background (gray 0.6))
  (with-pen (make-pen :fill (rgb 0.380 0.695 0.086) :stroke (rgb 1 1 0) :weight 4)
    (polygon (* 5 unit) unit unit (* 9 unit) (* 9 unit) (* 9 unit))
    (text title 20 20)))

;;;;modified
(defsketch hello-world
    ((title "Hello, world!")
     (unit (/ width 10))
     (height width))
  (background +yellow+)
  (rect 100 100 200 200)
  (bezier 0 400 100 100 300 100 400 400)
  )

