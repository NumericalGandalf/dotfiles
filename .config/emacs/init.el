(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq scroll-step 1
  scroll-margin 15)

(global-display-line-numbers-mode 1)
(column-number-mode 1)
(setq display-line-numbers-type 'relative
  display-line-numbers-width-start t)

(setq custom-file (expand-file-name "void.el")
  make-backup-files nil)

(set-face-attribute 'default nil :font "Hack Nerd Font Mono-11")

(setq help-window-select t) 

(require 'eshell)
(setq eshell-banner-message "")

(require 'em-smart)
(add-to-list 'eshell-modules-list 'eshell-rebind)
(add-to-list 'eshell-modules-list 'eshell-smart)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path default-directory)
(require 'gandalf)

(use-package diminish)

(use-package modus-themes
  :config
  (load-theme 'modus-vivendi-tinted :no-confirm))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (setq evil-vsplit-window-right t)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

(use-package which-key
  :diminish
  :init
  (setq which-key-idle-delay 1)
  :config
  (which-key-mode 1))

(use-package ivy
  :diminish
  :init
  (setq ivy-use-virtual-buffers t)
  :config
  (setq ivy-re-builder-alist '((t . ivy--regex-ignore-order)))
  (setq ivy-initial-inputs-alist ())
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :diminish
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-symbol] . counsel-describe-symbol)
  :config
  (counsel-mode))

(use-package ivy-rich
  :after counsel
  :config
  (ivy-rich-mode 1))

(use-package helpful
  :after ivy-rich
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)

(use-package general
  :after evil
  :config
  (general-create-definer gandalf/leader
    :keymaps '(normal insert visual emacs)
    :global-prefix "C-SPC")
  (gandalf/leader
    "t" '(lambda () (gandalf/test))))
