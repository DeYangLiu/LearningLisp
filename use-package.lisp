
#|
# how to install and use third party's package
## package 
is defined by language standard,
which specify how to create, modify,delete,find.

you load lisp code which in turn create a package.
import symbols. e.g. apt-get v.s. quicklisp 

## system
 is a bunch of code plus some instructions which
tell depending order. e.g. make v.s ASDF

## module
require, provide is deprecated.

load a single file: read the text, eval the expressions.
require : load many files if not loaded

C-c C-l M-x slime-load-file


##first time setting
(load "~/quicklisp/setup.lisp")
(ql:add-to-init-file) ;=> ~/.sbclrc

##quicklisp usage
(ql:quickload "drakma")
(ql:where-is-system "cl-ppcre")


##ref
https://www.quicklisp.org/beta/faq.html

# roll your own package
install packages from "practical common lisp"
http://www.gigamonkeys.com/book/

;M-x slime:
 asdf:*central-registry* ;check the path is ended with /

;;html.asd
(push "E:/PL/practicals-1.0.3/Chapter31/" asdf:*central-registry*)
;;depends on macro-utilities.asd
(push "E:/PL/practicals-1.0.3/Chapter08/" asdf:*central-registry*)

(asdf:compile-system :html)

*package* ;check where we are
(in-package :com.gigamonkeys.html) ;change to this

;;examples from the book
(html (:p "foo " (:i "bar") " baz"))
;=> <p>foo <i>bar</i> baz</p>

(html (:p :id "x" :style "foo" "Foo"))
;=> <p id='x' style='foo'>Foo</p>


|#

#|
 M-x locate-library RET slime RET 
 M-x list-packages ;search slime and install
 C-h m ;check modes
|#

#|
 C-M i ;completion
 C-@ ;selection
 C-c C-r ;evaluate the selection
|#

;;;;beautiful common lisp sniplets

(defun inc (x) 
  (+ x 1))

 (mapcar #'inc '(1 2 3 4))

(with-input-from-string (s "(1.23);(3 4)")
  (read s))

(with-output-to-string (out)
  (format out "hello ~s" (list 1 2 3)))
