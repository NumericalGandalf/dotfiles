(setq compilation-ask-about-save nil
      compile-command nil)

(electric-pair-mode)

(setq-default indent-tabs-mode nil
              tab-width 4
	          c-basic-offset tab-width)

(use-package rust-mode)
(use-package cmake-mode)
(use-package yaml-mode)
(use-package lua-mode)

(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . js-json-mode))

(use-package magit
  :init
  (setq magit-auto-revert-mode nil))

(use-package editorconfig
  :demand
  :config 
  (editorconfig-mode 1))

(use-package lsp-mode
  :hook
  ((prog-mode-hook . lsp-deferred)
   (lsp-mode-hook . lsp-enable-which-key-integration))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-warn-no-matched-clients nil)
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-lens-enable nil))

(use-package flycheck
  :after
  lsp-mode
  :hook
  (lsp-mode-hook . flycheck-mode))

(use-package yasnippet
  :after
  lsp-mode)

(use-package lsp-ui
  :after
  lsp-mode
  :custom
  (lsp-ui-imenu-auto-refresh t)
  (lsp-ui-doc-position 'at-point))

(use-package consult-lsp
  :after
  (consult lsp-mode))

(use-package lsp-java
  :after
  lsp-mode)

(use-package dap-mode
  :after
  lsp-mode)

(provide 'programming)
