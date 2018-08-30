;;;; .emacs
(add-to-list 'load-path "~/.emacs.d/elisp/")
(package-initialize) ;auto set load-path of installed pkg and load them
(setq package-enable-at-startup nil)


(setq inhibit-startup-message t)
(setq gnus-inhibit-startup-message t)
(setq frame-title-format "%b")
(tool-bar-mode -1)
(setq default-major-mode 'text-mode)
(setq column-number-mode t)
(setq next-screen-context-lines 4) ;for page down.
(setq kill-ring-max 200) ;for history record
(setq message-log-max t)
(setq visible-bell 1)
(setq split-width-threshold 9999) ;force horizontal positioning of pop-up windows  

;(setq make-backup-files nil); no backup!
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs_saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(setq scroll-margin 3
      scroll-conservatively 10000)

(cua-mode t)
    (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
    (transient-mark-mode 1)               ;; No region when it is not highlighted
    (setq cua-keep-region-after-copy t) 

;;;; packages init
(require 'package) ;M-x list-packages
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)


;;cedet need sit at very first.
;;jump back: inner file: C-x C-x; inter file: C-xb


;;CC-mode
(add-hook 'c-mode-common-hook
		  '(lambda ()
			 (let ((path "~/tools/cedet/cedet-devel-load.el"))
			   (if (file-exists-p path)
				   (progn
					 (load-file path)
					 (setq semantic-default-submodes
						   '(global-semantic-idle-scheduler-mode
							 global-semantic-idle-completions-mode
							 ;global-semantic-show-unmatched-syntax-mode
							 global-semanticdb-minor-mode
							 global-semantic-idle-summary-mode
							 global-semantic-mru-bookmark-mode))
					 )))
			 (semantic-mode 1)			;enable semantic
			 (global-set-key [f9] 'semantic-ia-fast-jump)
			 (global-set-key (kbd "M-<f9>") 'semantic-ia-complete-symbol-menu)
			 (global-set-key (kbd "M-S-<f9>") 'semantic-symref)))

;;;;CC-mode
(setq c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "linux")))
(setq-default indent-tabs-mode t) ;disable tab to spaces conversion
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(electric-pair-mode 0) ;;auto insert close bracket.
(setq c-hanging-semi&comma-criteria nil) ;disable auto new line

(which-function-mode t)



;;;;open recent files, Emacs 22+
(require 'recentf)
(recentf-mode 1)
;(setq recentf-auto-cleanup 'never)
;;;M-x recentf-open-files


;;;;paredit
;;move: C-M-b/f/u/d
;;delete: C-k/d DEL/M-DEL
;;add: M-(/"
;;slurping/barfing: C-(/{
(autoload 'enable-paredit-mode "paredit-beta" 
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;;;chicken-slime
;;(add-to-list 'load-path "~/tools/chicken/lib/chicken/8/")   ; Where Eggs are installed
;;(setq slime-csi-path "~/tools/chicken/bin/csi")
;;(autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)
;;(setq swank-chicken-path "/home/ludi/.emacs.d/scheme/swank-chicken.scm")
;;(add-hook 'scheme-mode-hook (lambda () (slime-mode t)))

;;;  ~/.emacs.d/elisp/iuscheme.el
(autoload 'scheme-mode "iuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "iuscheme" "Switch to interactive Scheme buffer." t)
(setq auto-mode-alist (cons '("\\.ss" . scheme-mode) auto-mode-alist))
(setq scheme-program-name "scheme")

(defun scheme-split-window ()
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    (split-window-vertically (floor (* 0.68 (window-height))))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window 1))
   ((not (find "*scheme*"
               (mapcar (lambda (w) (buffer-name (window-buffer w)))
                       (window-list))
               :test 'equal))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window -1))))


(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))


(defun scheme-send-definition-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-definition))

(add-hook 'scheme-mode-hook
  (lambda ()
    (paredit-mode 1)
    (define-key scheme-mode-map (kbd "<f5>") 'scheme-send-last-sexp-split-window)
    (define-key scheme-mode-map (kbd "<f6>") 'scheme-send-definition-split-window)))

;;;; slime
;(load (expand-file-name "~/quicklisp/slime-helper.el"))

(setq inferior-lisp-program "sbcl")
(setq slime-auto-connect 'ask)

(defun my-hyperspec-setup ()
  (let ((dir (locate-dominating-file invocation-directory "HyperSpec/")))
    (if dir
        (progn
          (setq common-lisp-hyperspec-root (expand-file-name "HyperSpec/" dir)))
      (warn "No HyperSpec directory found"))))

(defun my-slime-setup ()
  (my-hyperspec-setup) ;common lisp doc
  (require 'slime)
  (slime-setup))
(defvar my--slime-setup-done nil)
(defun my-slime-setup-once ()
  (unless my--slime-setup-done
    (my-slime-setup)
    (setq my--slime-setup-done t)))
(defadvice lisp-mode (before my-slime-setup-once activate)
  (my-slime-setup-once))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default default default italic underline success warning error])
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (wombat)))
 '(safe-local-variable-values (quote ((Syntax . Common-Lisp))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "宋体" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))

;;;display
;;;; cursor display M-x hl-line-mode or global-hl-line-mode
(setq blink-cursor-blinks -1)
(set-cursor-color "#ffffff")
(setq-default cursor-type '(bar . 3))
