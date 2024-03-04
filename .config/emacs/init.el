(setq inhibit-startup-message t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(set-face-attribute 'default nil :font "Hack Nerd Font Mono-11")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melp.org/packages/"))

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi-tinted :no-confirm))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(custom-set-variables
  '(package-selected-packages '(evil modus-themes)))
(custom-set-faces)
