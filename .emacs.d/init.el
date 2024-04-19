(require 'use-package)
(setq use-package-always-ensure t)

(use-package diminish)

(use-package
  no-littering
  :init (setq no-littering-etc-directory user-emacs-directory))

(setq
  use-dialog-box nil
  use-short-answers t
  echo-keystrokes 0
  inhibit-startup-message t)

(column-number-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(save-place-mode 1)

(global-display-line-numbers-mode 1)
(setq
  display-line-numbers-type 'relative
  display-line-numbers-width-start t)

(global-auto-revert-mode 1)
(setq
  global-auto-revert-non-file-buffers t
  auto-revert-remote-files t)

(use-package orderless
  :config
  (add-to-list 'completion-styles 'orderless))

(use-package vertico :config (vertico-mode 1))

(use-package marginalia :diminish :config (marginalia-mode 1))

(with-eval-after-load 'dired
  (setq
    dired-listing-switches "-lah"
    find-ls-option '("-exec ls -ldh {} +" . "-ldh")
    dired-free-space 'separate
    dired-recursive-deletes 'always
    dired-kill-when-opening-new-dired-buffer t
    dired-auto-revert-buffer t))

(with-eval-after-load 'tramp
  (setq auth-source-save-behavior nil))

(use-package wgrep :defer)

(use-package markdown-mode :defer)

(use-package magit :defer)

(use-package editorconfig :diminish :config (editorconfig-mode 1))

(use-package
  elisp-autofmt
  :defer
  :config (setq elisp-autofmt-style 'fixed))

(use-package
  corfu
  :init
  (defun rc-corfu-in-minibuffer ()
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  :config
  (setq
    corfu-auto t
    corfu-cycle t)
  (global-corfu-mode 1)
  (add-hook 'minibuffer-setup-hook 'rc-corfu-in-minibuffer))

(with-eval-after-load 'eglot
  (setq eglot-ignored-server-capabilities
    '
    (:documentHighlightProvider
      :codeLensProvider
      :foldingRangeProvider
      :colorProvider
      :inlayHintProvider))
  (define-key eglot-mode-map (kbd "C-c l d") 'eglot-find-declaration)
  (define-key
    eglot-mode-map
    (kbd "C-c l t")
    'eglot-find-typeDefinition)
  (define-key
    eglot-mode-map
    (kbd "C-c l i")
    'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)
