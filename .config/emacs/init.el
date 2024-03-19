(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)

(setq scroll-step 1
  scroll-margin 15)

(fringe-mode '(0 . 0))

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
  custom-file (expand-file-name "var/void.el" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "etc/" user-emacs-directory))

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))

(setq isearch-repeat-on-direction-change t)
(global-set-key (kbd "C-c C-s") 'query-replace-regexp)

(setq compile-command nil)
(global-set-key (kbd "C-c C-b") 'compile)
(global-set-key (kbd "C-c b") 'recompile)
(add-hook 'compilation-finish-functions 'switch-to-buffer-other-window 'compilation)

(require 'package)
(setq package-user-dir (expand-file-name "var/elpa/" user-emacs-directory)
  package-gnupghome-dir (expand-file-name "var/elpa/gnupg/" user-emacs-directory))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents t))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package diminish)

(use-package no-littering
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory))

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package evil
  :init
  (setq evil-want-keybinding nil
    evil-want-C-u-scroll t
    evil-want-C-u-delete t
    evil-want-fine-undo t)
  :config
  (evil-set-undo-system 'undo-redo)
  (setq evil-vsplit-window-right t
    evil-split-window-below t
    evil-insert-state-cursor nil)
  (evil-mode 1))

(use-package evil-collection
  :after
  evil
  :diminish
  evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package which-key
  :diminish
  :init
  (setq which-key-idle-delay 1)
  :config
  (which-key-mode 1))

(use-package helpful
  :config
  (global-set-key [remap describe-function] 'helpful-callable)
  (global-set-key [remap describe-variable] 'helpful-variable)
  (global-set-key [remap describe-symbol] 'helpful-symbol)
  (global-set-key [remap describe-key] 'helpful-key)
  (global-set-key [remap describe-command] 'helpful-command))

(use-package helm
  :diminish
  :config
  (helm-mode 1)
  (helm-autoresize-mode t)
  (setq helm-autoresize-min-height 5
    helm-display-header-line nil)
  (custom-set-faces
    '(helm-M-x-key
       ((t (:extend t :foreground "#DFAF8F"))))
    '(helm-M-x-short-doc
       ((t (:foreground "DimGray"))))
    '(helm-M-x-key
       ((t (:extend t :foreground "#DFAF8F"))))
    '(helm-M-x-short-doc
       ((t (:foreground "DimGray"))))
    '(helm-ff-dotted-directory
       ((t (:extend t :foreground "#DFAF8F"))))
    '(helm-ff-dotted-symlink-directory
       ((t (:extend t :foreground "#DFAF8F")))))
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-s") 'helm-occur))

(use-package company
  :after
  helm
  :diminish
  :config
  (global-company-mode 0)
  (require 'helm-company)
  (setq helm-company-show-icons nil
    helm-company-initialize-pattern-with-prefix t
    helm-company-candidate-number-limit 500
    company-tooltip-align-annotations t)
  (custom-set-faces
    '(company-tooltip-annotation
       ((t (:background nil)))))
  (dolist (hook
            '(emacs-lisp-mode-hook
               c-ts-mode-hook
               c++-ts-mode-hook
               bash-ts-mode-hook
               text-mode-hook))
    (add-hook hook
      (lambda ()
        (interactive)
        (define-key evil-insert-state-local-map
          (kbd "C-;") 'helm-company)))))

(use-package flycheck
  :diminish
  :hook
  ((c-ts-mode . flycheck-mode)))

(use-package lsp-mode
  :after
  (which-key flycheck company)
  :diminish
  lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
    (c-ts-mode . lsp)
    (c++-ts-mode . lsp)
    (bash-ts-mode . lsp))
  :config
  (setq lsp-completion-provider :none
    lsp-headerline-breadcrumb-enable nil
    lsp-enable-symbol-highlighting nil))

(use-package lsp-treemacs
  :after
  lsp-mode
  :config
  (setq lsp-treemacs-theme "Iconless"))

(use-package magit)

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))
