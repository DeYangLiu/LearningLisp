;;;; .emacs

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

(add-to-list 'load-path "~/.emacs.d/elisp/")
(package-initialize) ;auto set load-path of installed pkg and load them
(setq package-enable-at-startup nil)

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


;;;; slime
(load (expand-file-name "~/quicklisp/slime-helper.el"))

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
 )
