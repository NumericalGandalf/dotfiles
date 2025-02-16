(menu-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(scroll-bar-mode 0)

(let ((font "Iosevka-14"))
    (set-face-attribute 'default nil :font font)
    (set-face-attribute 'fixed-pitch nil :font font)
    (set-face-attribute 'fixed-pitch-serif nil :font font)
    (set-face-attribute 'variable-pitch nil :font font))

(setq inhibit-startup-screen t
      use-short-answers t
      use-dialog-box nil
      use-file-dialog nil
      confirm-nonexistent-file-or-buffer nil
      resize-mini-windows t

      mode-line-percent-position '(6 "%q")

      xref-auto-jump-to-first-definition t
      xref-auto-jump-to-first-xref t
      xref-file-name-display 'abs
      xref-prompt-for-identifier nil

      compilation-ask-about-save nil
      compile-command nil
      compilation-auto-jump-to-first-error t

      ido-enable-flex-matching t
      ido-enable-regexp t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-url-at-point t
      ido-use-virtual-buffers t

      search-upper-case t
      isearch-repeat-on-direction-change t
      isearch-allow-scroll 'unlimited

      c-default-style '((other . "user"))

      dired-listing-switches "-lah"
      dired-free-space 'separate
      dired-recursive-deletes 'always
      dired-dwim-target t
      dired-clean-confirm-killing-deleted-buffers nil

      display-line-numbers-width-start t
      display-line-numbers-grow-only t
      display-line-numbers-type 'relative

      use-package-always-ensure t
      use-package-always-defer t

      custom-file (locate-user-emacs-file "custom.el"))

(setq-default tab-width 4
              indent-tabs-mode nil)

(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . js-json-mode))

(require 'package)
(require 'use-package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents t)

(use-package gruber-darker-theme
  :init
  (load-theme 'gruber-darker t))

(use-package ido-completing-read+
  :init
  (ido-mode 0)
  (ido-everywhere 1)
  (ido-ubiquitous-mode 1))

(use-package magit)

(use-package company
  :init
  (global-company-mode 1)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0)
  (company-format-margin-function #'company-text-icons-margin)
  (company-tooltip-flip-when-above t)
  (company-tooltip-align-annotations t)
  (company-tooltip-scrollbar-width 0))

(use-package switch-window
  :custom
  (switch-window-background t))

(use-package editorconfig)

(use-package lsp-mode
  :hook
  ((c-mode c++-mode java-mode) . lsp)
  :custom
  (lsp-enable-symbol-highlighting nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil)
  (lsp-enable-snippet nil)
  (lsp-enable-folding nil)
  (lsp-auto-guess-root t)
  (lsp-modeline-code-actions-segments '(count)))

(use-package lsp-java
  :after lsp-mode)

(use-package rust-mode)
(use-package yaml-mode)
(use-package cmake-mode)
(use-package meson-mode)

(use-package general
  :init 
  (general-define-key
   "C-<tab>" #'align-regexp
   "M-<tab>" #'company-complete
   "C-x o" #'switch-window
   "C-`" (lambda ()
           (interactive)
           (if (project-current)
               (project-compile)
             (call-interactively (compile)))))

  (general-def company-active-map
    "<return>" nil
    "RET" nil
    "<tab>" #'company-complete-selection
    "TAB" #'company-complete-selection)

  (general-def lsp-mode-map
    "C-|" #'flymake-show-project-diagnostics
    "C-." #'lsp-rename
    "M-." #'lsp-find-definition
    "M-?" #'lsp-find-references
    "C-h ." #'lsp-describe-thing-at-point))

(editorconfig-mode 1)
(electric-pair-mode 1)

(global-visual-line-mode 1)
(global-display-line-numbers-mode 1)
(column-number-mode 1)

(savehist-mode 1)
(save-place-mode 1)
