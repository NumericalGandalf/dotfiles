(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t
  use-dialog-box nil)

(setq echo-keystrokes 0)

(fringe-mode 0)
(setq overflow-newline-into-fringe nil)

(require 'savehist)
(savehist-mode 1)

(recentf-mode 1)
(save-place-mode 1)

(require 'autorevert)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(setq mode-line-percent-position '(6 "%q"))

(setq-default cursor-in-non-selected-windows nil)

(require 'display-line-numbers)
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(setq display-line-numbers-type 'relative
  display-line-numbers-width-start t)

(let ((rc/font "Hack Nerd Font Mono-11"))
  (set-face-attribute 'default nil :font rc/font)
  (add-to-list 'default-frame-alist `(font . ,rc/font)))

(setq make-backup-files nil
  create-lockfiles nil
  custom-file (expand-file-name "var/void.el" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "etc/" user-emacs-directory))

(require 'dired)
(setq dired-listing-switches "-lah"
  dired-deletion-confirmer 'y-or-n-p)

(setq compile-command ""
  compilation-ask-about-save nil
  find-ls-option '("-exec ls -ldh {} +" . "-ldh")
  grep-save-buffers t)

(require 'grep)
(require 'find-dired)

(defun rc/find (find-expr)
  (interactive (list (read-string "Find: " nil 'find-command-history)))
  (find-dired-with-command default-directory
    (concat find-expr " " (car find-ls-option))))

(defun rc/grep (grep-expr)
  (interactive (list (read-string "Grep: " nil 'grep-history)))
  (grep grep-expr))

(defun rc/project-find (find-expr)
  (interactive (list (read-string "Find: " nil 'find-command-history)))
  (find-dired-with-command (project-root (project-current))
    (concat find-expr " " (car find-ls-option))))

(defun rc/project-grep (grep-expr)
  (interactive (list (read-string "Grep: " nil 'grep-history)))
  (let ((default-directory (project-root (project-current))))
    (grep grep-expr)))

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c C-c") 'rc/find)
(global-set-key (kbd "C-c C") 'rc/grep)

(global-set-key (kbd "C-x p C-c") 'rc/project-find)
(global-set-key (kbd "C-x p C") 'rc/project-grep)

(defun rc/duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

(global-set-key (kbd "C-q") 'rc/duplicate-line)

(global-set-key (kbd "C-x C-c")
  (lambda () (interactive) (save-buffers-kill-terminal t)))
(global-set-key (kbd "C-x M-c")
  (lambda () (interactive) (save-buffers-kill-emacs t)))

(setq isearch-repeat-on-direction-change t)
(global-set-key (kbd "C-c C-s") 'query-replace)

(global-set-key (kbd "C-c M-m") 'man)

(require 'package)
(setq package-user-dir (expand-file-name "var/elpa/" user-emacs-directory)
  package-gnupghome-dir (expand-file-name "var/elpa/gnupg/" user-emacs-directory))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents t))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package diminish)

(use-package magit)

(use-package no-littering
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-c m i") 'mc/edit-lines)
  (global-set-key (kbd "C-c m I") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "C-c m C-i") 'mc/edit-ends-of-lines))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (custom-set-faces
    '(wgrep-face ((t (:background "grey30"))))
    '(wgrep-done-face ((t (:background nil))))
    '(wgrep-file-face ((t (:background "gray30" :foreground "white"))))))

(use-package which-key
  :diminish
  :init
  (setq which-key-idle-delay 1)
  :config
  (which-key-mode 1))

(use-package helm
  :diminish
  :config
  (helm-mode 1)
  (helm-autoresize-mode t)
  (setq helm-autoresize-min-height 5
    helm-display-header-line nil)
  (custom-set-faces
    '(helm-M-x-key ((t (:extend t :foreground "#DFAF8F"))))
    '(helm-M-x-short-doc ((t (:foreground "DimGray"))))
    '(helm-M-x-key ((t (:extend t :foreground "#DFAF8F"))))
    '(helm-M-x-short-doc ((t (:foreground "DimGray"))))
    '(helm-ff-dotted-directory ((t (:extend t :foreground "#DFAF8F"))))
    '(helm-ff-dotted-symlink-directory ((t (:extend t :foreground "#DFAF8F"))))
    '(helm-buffer-directory ((t (:extend t :foreground "#DFAF8F"))))
    '(helm-ff-file ((t (:extend t :foreground "#DCDCCC")))))
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-mini))

(defun rc/helm-company-hook ()
  (local-set-key (kbd "C-c h") 'helm-company))

(use-package company
  :after
  helm
  :diminish
  :config
  (global-company-mode 0)
  (require 'helm-company)
  (setq company-frontends ()
    helm-company-show-icons nil
    helm-company-initialize-pattern-with-prefix t
    helm-company-candidate-number-limit 500
    company-tooltip-align-annotations t)
  (custom-set-faces
    '(company-tooltip-annotation ((t (:background nil)))))
  (dolist (hook
            '(emacs-lisp-mode-hook
               c-mode-hook
               c++-mode-hook
               sh-mode-hook
               text-mode-hook))
    (add-hook hook 'rc/helm-company-hook)))

(use-package flycheck
  :diminish
  :config
  (dolist (hook
            '(c-mode-hook))
    (add-hook hook 'flycheck-mode)))

(use-package lsp-mode
   :after
  (which-key flycheck)
  :diminish
  lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)
  (setq lsp-completion-provider :none
    lsp-lens-enable nil
    lsp-headerline-breadcrumb-enable nil
    lsp-modeline-code-actions-segments '(count)
    lsp-enable-symbol-highlighting nil))

(use-package helm-lsp
  :after
  lsp-mode)

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))
