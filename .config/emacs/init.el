(use-package no-littering)
(use-package diminish)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 0)

(setq use-dialog-box nil
  overflow-newline-into-fringe nil
  use-short-answers t
  inhibit-startup-message t
  echo-keystrokes 0
  mode-line-percent-position '(6 "%q")
  initial-buffer-choice default-directory)

(setq-default resize-mini-windows t
  cursor-in-non-selected-windows nil)

(let ((rc-font "Hack Nerd Font Mono-11"))
  (set-face-attribute 'default nil :font rc-font)
  (add-to-list 'default-frame-alist `(font . ,rc-font)))

(use-package ef-themes
  :config
  (load-theme 'ef-maris-dark t))

(global-display-line-numbers-mode 1)
(column-number-mode 1)
(setq display-line-numbers-type 'relative
  display-line-numbers-width-start t)

(global-set-key (kbd "C-x C-c") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x M-c") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-c M-m") 'man)

(savehist-mode 1)
(recentf-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)

(setq enable-recursive-minibuffers t
  isearch-repeat-on-direction-change t
  global-auto-revert-non-file-buffers t)

(setq make-backup-files nil
  create-lockfiles nil
  custom-file (locate-user-emacs-file "var/void.el"))

(setq completion-ignore-case t
  read-file-name-completion-ignore-case t
  read-buffer-completion-ignore-case t)

(use-package orderless
  :config
  (add-to-list 'completion-styles 'orderless))

(use-package vertico
  :config
  (vertico-mode 1))

(use-package consult
  :config
  (setq xref-show-xrefs-function 'consult-xref
    xref-show-definitions-function 'consult-xref
    completion-in-region-function 'consult-completion-in-region)
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap imenu] 'consult-imenu)
  (global-set-key [remap bookmark-jump] 'consult-bookmark))
  
(use-package marginalia
  :diminish
  :config
  (marginalia-mode 1))

(require 'dired)
(setq dired-listing-switches "-lah"
  dired-free-space 'separate
  dired-recursive-deletes 'always
  dired-auto-revert-buffer t)

(define-key dired-mode-map [remap quit-window] 'delete-window)

(setq auth-source-save-behavior nil)

(global-set-key (kbd "C-c M-t") 'ansi-term)

(setq compile-command ""
  compilation-ask-about-save nil
  compilation-scroll-output 'first-error
  find-ls-option '("-exec ls -ldh {} +" . "-ldh"))

(require 'project)

(defun rc--read-command (prompt history &optional directory)
  (let ((default-directory (if directory directory default-directory)))
    (save-some-buffers t)
    (read-shell-command prompt (car (symbol-value history)) `(,history . 1))))

(defun rc--project-root ()
  (if (equal (project-current) default-directory)
    default-directory
    (project-root (project-current))))

(defun rc--find (directory find-expr)
  (split-window-right)
  (other-window 1)
  (find-dired-with-command directory (concat find-expr " " (car find-ls-option))))

(defun rc-find (find-expr)
  (interactive
    (list (rc--read-command "Find command: " 'find-command-history)))
  (rc--find default-directory find-expr))

(defun rc-grep (grep-expr)
  (interactive
    (list (rc--read-command "Grep command: " 'grep-history)))
  (grep grep-expr))

(defun rc-project-find (find-expr)
  (interactive
    (list (rc--read-command "Find command: " 'find-command-history (rc--project-root))))
  (rc--find (project-root (project-current)) find-expr))

(defun rc-project-grep (grep-expr)
  (interactive
    (list (rc--read-command "Grep command: " 'grep-history (rc--project-root))))
  (let ((default-directory (project-root (project-current))))
    (grep grep-expr)))

(global-set-key (kbd "C-c b c") 'compile)
(global-set-key (kbd "C-c b f") 'rc-find)
(global-set-key (kbd "C-c b g") 'rc-grep)

(global-set-key (kbd "C-x p C-c") 'rc-project-find)
(global-set-key (kbd "C-x p C") 'rc-project-grep)

(global-set-key (kbd "C-c k c") 'kill-compilation)
(global-set-key (kbd "C-c k f") 'kill-find)
(global-set-key (kbd "C-c k g") 'kill-grep)

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(defun rc-open-line-above ()
  (interactive)
  (previous-line 1)
  (move-end-of-line 1)
  (newline 1))

(global-set-key (kbd "C-o") 'rc-open-line-above)
(global-set-key (kbd "C-S-o") 'open-line)
    
(defun rc-duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

(global-set-key (kbd "C-q") 'rc-duplicate-line)

(require 'eglot)
(setq eglot-ignored-server-capabilities '(:documentHighlightProvider
                                       :inlayHintProvider))
(global-set-key (kbd "C-c l s") 'eglot)
(define-key eglot-mode-map (kbd "C-c l S") 'eglot-shutdown)
(define-key eglot-mode-map (kbd "C-c l d") 'eglot-find-declaration)
(define-key eglot-mode-map (kbd "C-c l t") 'eglot-find-typeDefinition)
(define-key eglot-mode-map (kbd "C-c l i") 'eglot-find-implementation)
(define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)
(define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
(when (package-installed-p 'consult)
  (define-key eglot-mode-map (kbd "C-c l e") 'consult-flymake))

(use-package magit)

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))
