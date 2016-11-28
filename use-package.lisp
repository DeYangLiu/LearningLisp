#|
# ASDF

(native-namestring "c:/a")
*load-truename*
*central-registry* ;system def files
initialize-source-registry, environment variable, central user configuration file, modular user configuration directory, central system configuration files, modular system configuration directories, implementation configuration

*default-pathname-defaults*
 UIOP:*nil-pathname*

:compile-check :around-compile

(and list (declare-list-of passenger))


|#
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

#|
package: define a namespace,
control reader how to translate textual names into symbol objects.

|#

;;;;make-symbol, make-package
(gentemp) ;=> T1
(gensym) ;=> #:G947

(setf s1 (make-symbol "my-symbol"))
(import s1) ;put a symbol into a package
(unintern s1) ;un-import
;;intern == find-symbol, make-symbol,import
(symbol-name s1) ;=> "my-symbol"

(defparameter p 'print)
(funcall p "hello")

(setf (symbol-function 'p) #'print)
(p "hello")


(symbol-package 'car) ;=> home package of car
(package-use-list :cl-user) ;=> check what are used

(make-package :cc :use '(:cl :cl-user))
(in-package :cc)
(setf s0 (make-symbol "my-symbol"))
(defun foo () "in cc's foo")
(defun fun () "in cc's fun")
(export '(s0 foo fun))

(in-package :cl-user)
(shadow 'foo)
(defun foo () "in cl-user's foo")
(foo)
(cc:foo)

(use-package :cc)
(foo)
(fun)

(unuse-package :cc)

;eval-when to avoid can't repeatedly make-package
(defpackage :com.ludi.foolib
  (:use :cl :cl-user))

(in-package com.ludi.foolib) ;need to copy this to repl
(defun foo () "foo in foolib")
(cl-user::foo)
(foo)

(package-used-by-list :CL-USER)

(ql:quickload "quickproject")
;;;;make-project create *.asd and add path to asdf:*central-registry*
(quickproject:make-project "~/my-app/" :depends-on '(vecto))
;;;;so we can
(ql:quickload "my-app")

;;;; reload
(load "~/quicklisp/setup.lisp")
(pushnew (truename "~/my-app/")  ql:*local-project-directories*)
(ql:quickload "my-app")
;;(ql:register-local-projects)





