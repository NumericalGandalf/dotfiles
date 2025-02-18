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

      xref-file-name-display 'abs
      xref-prompt-for-identifier nil

      compile-command nil
      compilation-ask-about-save nil

      ido-enable-flex-matching t
      ido-enable-regexp t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-url-at-point t

      search-upper-case t
      isearch-repeat-on-direction-change t
      isearch-allow-scroll 'unlimited

      c-default-style '((other . "user"))

      dired-listing-switches "-lah"
      dired-free-space 'separate
      dired-recursive-deletes 'always
      dired-dwim-target t
      dired-clean-confirm-killing-deleted-buffers nil

      eldoc-echo-area-use-multiline-p 1
      
      eglot-report-progress nil
      eglot-extend-to-xref t
      eglot-ignored-server-capabilities '(:documentHighlightProvider
                                          :codeLensProvider
                                          :documentOnTypeFormattingProvider
                                          :foldingRangeProvider
                                          :inlayHintProvider)

      display-line-numbers-width-start t
      display-line-numbers-grow-only t
      display-line-numbers-type 'relative

      use-package-always-ensure t
      use-package-always-defer t

      custom-file (locate-user-emacs-file "custom.el"))

(setq-default tab-width 4
              indent-tabs-mode nil)

(column-number-mode 1)
(global-display-line-numbers-mode 1)

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

(use-package company
  :hook
  (eglot-managed-mode . company-mode)
  :bind
  (:map company-active-map
        ("<return>" . nil)
        ("RET" . nil)
        ("<tab>" . #'company-complete-selection)
        ("TAB" . #'company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0)
  (company-frontends '(company-pseudo-tooltip-frontend))
  (company-format-margin-function #'company-text-icons-margin)
  (company-tooltip-flip-when-above t)
  (company-tooltip-align-annotations t)
  (company-tooltip-scrollbar-width 0))

(use-package treesit-auto
  :demand
  :config
  (global-treesit-auto-mode 1))

(use-package magit)

(use-package meson-mode)
