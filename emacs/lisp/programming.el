(setq compilation-ask-about-save nil
      compile-command nil)

(electric-pair-mode)

(setq-default indent-tabs-mode nil
              tab-width 4
	          c-basic-offset tab-width)

(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . js-json-mode))

(use-package magit
  :defer
  :init
  (setq magit-auto-revert-mode nil))

(use-package lsp-mode
  :hook
  (prog-mode-hook . lsp)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-warn-no-matched-clients nil)
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-lens-enable nil))

(use-package consult-lsp
  :after (consult lsp-mode)
  :general
  (lsp-mode-map
   "M-?" 'consult-lsp-symbols))

(use-package lsp-ui
  :after lsp-mode
  :general
  (lsp-mode-map
   "C-h ." 'lsp-ui-doc-toggle
   "M-g i" 'lsp-ui-imenu)
  :custom
  (lsp-ui-imenu-auto-refresh t)
  (lsp-ui-doc-position 'at-point))

(use-package lsp-java
  :after lsp-mode)

(use-package dap-mode
  :after lsp-mode)

(use-package flycheck
  :after lsp-ui
  :general
  (lsp-mode-map
   "M-g f" 'lsp-ui-flycheck-list)
  :hook
  (lsp-mode-hook . flycheck-mode))

(provide 'programming)
