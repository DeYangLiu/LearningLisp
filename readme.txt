Introduction Use of Emacs and SBCL
====

# install newest sbcl & emacs: 
http://jaist.dl.sourceforge.net/project/sbcl/sbcl/1.3.5/sbcl-1.3.5-x86-windows-binary.msi 
http://mirrors.ustc.edu.cn/gnu/emacs/windows/emacs-24.5-bin-i686-mingw32.zip
the following are tested both on windows xp and ubuntu14.04

# notions
"cmd>" means run in cmd.exe.
";;;;" ";;;" ";;" ";" are comments in lisp.
M-x means hold on Alt and press x 
C-x 0 means hold on Ctrl and press x, release, press 0
C-g interrupt input sequence.

# install package manager
current art is using quicklisp which obsoletes asdf-install.
cmd>
set HOME=d:\ludi ;;important!!!
curl -k -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
sbcl>
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(ql:quickload "quicklisp-slime-helper")

emacs>
;;when need to re-run quicklisp: 
;;(load "~/quicklisp/setup.lisp")
;add the following to $HOME/.emacs.d/init.el
;in linux, i actually has done this:
; ln -s LearningLisp/elisp/init.el $HOME/.emacs.d/init.el
; ln -s ~/LearningLisp/elisp $HOME/.emacs.d/elisp

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

emacs>
M-x slime
F11 M-x toggle-frame-fullscreen

# shortcuts

window:
C-x 3 vertical split
C-x o move to next window
C-x 0 kill current window
C-x 1 kill all windows except current window


buffer:
C-x C-f find and load file
C-x C-w save as
C-x C-s save
C-mouse 
C-x LEFT/RIGHT ARROW switch buffer
C-x C-q toggle read-only/read-write
C-x k kill current buffer

frame:
C-x 5 2 make new frame
C-x 5 o move to next frame

Edit:
C-a C-k kill whole line
C-x u undo last operaption
C-x z repeat last command
Esc n op repead <op> n times
M-x replace-string RET string RET newstring RET
M-^ join line
M-d delete word after cursor
M-backspace delete word before cursor

Move:
C-a 移到行首(ahead)，C-e行尾, C-f (forward a char), 
C-n (next line), C-p (previous line)
C-v page down M-v page up
Esc 9 M-< (move to 90%) M-> (to end)
ESC a/e goto begin/end of the sentence.
ESC </> goto begin/end of the file.
C-x C-@ goto previous position.
C-M-u/d/n backward-up-list/forward-to-indentation/forward-list
M-x backward-sexp C-x z z z z ;;move backward

Search:
M-x o ;vimgrep
C-s C-w C-w ... C-s ;search words under cursor
C-s M-c ;toggle case-fold-search
C-s M-p/n ;search history


Highlight:
M-s h r ;highlight regexp
M-s h u ;unhighlight regexp

Setting:
C-h v A;check variables A's value
M-x set-variable next-screen-context-lines
M-: (getenv "HOME")
M-: (expand-file-name "~")
M-x linum-mode ; toggles the line number mode on and off
M-x (require 'vlf-setup) ; M-x vlf  to open very large file
M-x eval-last-sexp
M-x load-file ~/.emacs.d/init.el

C-Settings: CC-mode
c-default-style
indent-tabs-mode
tab-width 4 c-basic-offset 4

C-edit:
C-M-a/e ;jump to begining of the function
M-a/e ;jump to previous statement
C-c C-u ;jump to #if
C-M-q  ;format the block
C-c C-q ;format the function


Text encoding:
see/change C-x RET f ;i prefer gbk-unix or utf-8-unix
by character C-u C-x =


M-x slime 打开repl buffer
* 当焦点在repl buffer时:
TAB 命令补全
C-Enter 自动补充上括号并求值
C-c M-o 清空显示

* 当焦点在text buffer时:
C-c C-k 编译当前文件并加载
C-c C-l 选择一个文件加载
C-c C-c 编译当前光标处的toplevel form。
C-M-q 自动缩进
C-c C-] 补全右括号
C-M-a C-M-e M-x slime-end-of-defun

C-c C-c 是体现了lisp的interactive
C-x C-e 对光标处求值，不用print
C-M-x  对当前的toplevel form求值
C-c C-r 对选中的区域求值

C-c < list callers
C-c > list callees

C-c C-t 打开或关闭当前光标处的trace.
M-x slime-untrace-all
C-M-i 补全提示，再tab键补全。


* 当焦点在repl buffer时:
TAB 命令补全
C-Enter 自动补充上括号并求值
C-c M-o 清空显示


# msys>
start emacs from mingw32/msys
check HOME PS1

M-x shell:
add this line to /etc/profile:
 export ESHELL="E:/MinGW/msys/1.0/bin/bash.exe"

then you can enjoy the glorry of msys.


# resources
http://norvig.com/
http://www.gigamonkeys.com/book/
Paul Graham's On Lisp and ANSI Common Lisp
Christian Queinnec's Lisp in Small Pieces
Abelson and Sussman Structure and Interpretation of Computer Programs
Daniel Friedman The Seasoned Schemer. 



# reference
https://gist.github.com/jteneycke/7947353
http://smacs.github.io/elisp/
