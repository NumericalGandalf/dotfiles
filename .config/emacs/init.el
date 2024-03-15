(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)

(setq scroll-step 1
  scroll-margin 15)

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

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))

(setq isearch-repeat-on-direction-change t)
(global-set-key (kbd "C-c C-s") 'query-replace-regexp)

(require 'package)
(setq package-user-dir (expand-file-name "var/elpa/" user-emacs-directory)
  package-gnupghome-dir (expand-file-name "var/elpa/gnupg/" user-emacs-directory))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless package-archive-contents (package-refresh-contents t))
(package-initialize)

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
  (setq evil-vsplit-window-right t)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
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
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command))

(use-package helm
  :diminish
  :config
  (helm-mode 1)
  (helm-autoresize-mode t)
  (custom-set-faces '(helm-M-x-key ((t (:extend t :foreground "orange"))))
    '(helm-M-x-short-doc ((t (:foreground "DimGray")))))
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-mini))

(use-package flycheck
  :diminish
  :hook
  ((c-ts-mode . flycheck-mode)))

(use-package company
  :diminish
  :hook
  ((text-mode . company-mode)
    (emacs-lisp-mode . company-mode))
  :config
  (define-key company-active-map (kbd "C-y") 'company-complete)
  (setq company-frontends '(company-pseudo-tooltip-frontend)
    company-minimum-prefix-length 1
    company-idle-delay 0.0
    company-format-margin-function nil))

(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-doc-delay 0.0
    company-box-doc-frame-parameters company-box-frame-parameters
    company-box-enable-icon nil
    company-box-scrollbar nil))

(use-package lsp-mode
  :after (which-key flycheck company)
  :diminish lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
    (lsp-mode . company-mode)
    (c-ts-mode . lsp-deferred)
    (c++-ts-mode . lsp-deferred)
    (bash-ts-mode . lsp-deferred))
  :config
  (setq lsp-ui-sideline-enable nil
    lsp-headerline-breadcrumb-enable nil
    lsp-enable-symbol-highlighting nil))

(use-package lsp-treemacs
  :after lsp-mode
  :config
  (setq lsp-treemacs-theme "Iconless"))

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))
