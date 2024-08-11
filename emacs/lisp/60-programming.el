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

(use-package treesit-auto
  :if
  (treesit-available-p)
  :demand
  :config
  (global-treesit-auto-mode))

(rc-fun (treesit-ensure-all)
    (treesit-available-p)
  "Ensure all available tree-sitter libraries."
  (interactive)
  (when-let ((outdir (nth 0 treesit-extra-load-path)))
    (dolist (source (treesit-auto--build-treesit-source-alist))
      (let ((lang (nth 0 source)))
        (unless (or (member lang '(latex markdown janet))
                    (treesit-ready-p lang t))
          (apply 'treesit--install-language-grammar-1 outdir source))))))

(when (treesit-available-p)
  (setq-default c-ts-mode-indent-offset c-basic-offset
                json-ts-mode-indent-offset c-basic-offset)
  (add-to-list 'treesit-extra-load-path (rc-cache "tree-sitter/"))
  (add-hook 'dots-deploy-hook 'treesit-ensure-all))

(use-package magit
  :init
  (setq magit-auto-revert-mode nil))

(use-package editorconfig
  :demand
  :config 
  (editorconfig-mode 1))

(use-package lsp-mode
  :hook
  ((prog-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration)
   (lsp-mode . flycheck-mode))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-warn-no-matched-clients nil)
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil))

(use-package yasnippet
  :after
  lsp)

(use-package flycheck
  :after
  lsp)

(use-package lsp-ui
  :after
  lsp
  :custom
  (lsp-ui-imenu-auto-refresh t)
  (lsp-ui-doc-position 'at-point))

(use-package consult-lsp
  :after
  (consult lsp))

(use-package lsp-java
  :after
  lsp)

(use-package dap-mode)
