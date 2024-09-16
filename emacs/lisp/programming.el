(setq-default tab-width 4)

(with-eval-after-load 'compile
  (setq compilation-ask-about-save nil
        compile-command nil))

(with-eval-after-load 'cc-vars
  (setq-default c-basic-offset 4))

(with-eval-after-load 'simple
  indent-tabs-mode nil)

(use-package magit
  :init
  (setq magit-auto-revert-mode nil))

(use-package lsp-mode
  :hook
  ((prog-mode-hook . lsp-maybe)
   (lsp-mode-hook . lsp-enable-which-key-integration))
  :init
  (defun lsp-maybe ()
    "Maybe run `lsp-deferred'."
    (unless (member major-mode '(emacs-lisp-mode
                                 lisp-interaction-mode))
      (lsp-deferred)))
  :custom
  (lsp-warn-no-matched-clients nil)
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-lens-enable nil))

(use-package consult-lsp
  :after (consult lsp-mode))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-imenu-auto-refresh t)
  (lsp-ui-doc-position 'at-point))

(use-package lsp-java
  :after lsp-mode)

(use-package dap-mode
  :after lsp-mode)

(use-package flycheck
  :after lsp-ui
  :hook
  (lsp-mode-hook . flycheck-mode))

(provide 'programming)
