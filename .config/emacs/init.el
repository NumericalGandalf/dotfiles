(setq inhibit-startup-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq scroll-step 1)
(setq scroll-margin 15)

(global-display-line-numbers-mode 1)
(column-number-mode 1)
(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)

(setq custom-file (expand-file-name "custom.el"))

(setq make-backup-files nil)

(set-face-attribute 'default nil :font "Hack Nerd Font Mono-11")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melp.org/packages/"))
(setq use-package-always-ensure t) 

(use-package modus-themes
  :config
  (load-theme 'modus-vivendi-tinted :no-confirm))

(use-package evil
  :config
  (setq evil-want-C-u-scroll t)
  (evil-mode 1))

(use-package editorconfig
  :config
  (editorconfig-mode 1))
