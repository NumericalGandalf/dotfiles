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
(global-display-line-numbers-mode 1)
(setq
  display-line-numbers-type 'relative
  display-line-numbers-width-start t)

(global-auto-revert-mode 1)
(setq
  global-auto-revert-non-file-buffers t
  auto-revert-remote-files t)

(use-package
  orderless
  :config (add-to-list 'completion-styles 'orderless))

(use-package vertico :config (vertico-mode 1))

(use-package marginalia :diminish :config (marginalia-mode 1))

(with-eval-after-load 'dired
  (setq
    dired-listing-switches "-lah"
    find-ls-option '("-exec ls -ldh {} +" . "-ldh")
    dired-free-space 'separate
    dired-recursive-deletes 'always
    dired-kill-when-opening-new-dired-buffer t
    dired-dwim-target t
    dired-auto-revert-buffer t))

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
  :config
  (setq
    corfu-auto t
    corfu-cycle t)
  (global-corfu-mode 1)
  (add-hook
    'minibuffer-setup-hook
    (lambda ()
      (interactive)
      (when (local-variable-p 'completion-at-point-functions)
        (setq-local corfu-auto nil)
        (corfu-mode 1)))))

(global-set-key (kbd "C-c l s") 'eglot)
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
  (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l y") 'gud-gdb))

(with-eval-after-load 'cc-vars
  (add-to-list 'c-offsets-alist '(case-label . 4)))

(global-set-key (kbd "C-c q g") 'grep)
(global-set-key (kbd "C-c q f") 'find-dired)
(global-set-key (kbd "C-c q m") 'man)
(global-set-key (kbd "C-c q y") 'yank-from-kill-ring)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
