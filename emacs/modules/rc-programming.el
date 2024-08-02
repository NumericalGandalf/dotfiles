(setq compilation-ask-about-save nil
      compile-command nil)

(electric-pair-mode)

(setq-default indent-tabs-mode nil
	      c-basic-offset 4)

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

(defun treesit-ensure-all ()
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
  (add-to-list 'treesit-extra-load-path (rc-cache-file "tree-sitter/"))
  (add-hook 'dots-deploy-hook 'treesit-ensure-all))

(use-package magit
  :init
  (setq magit-auto-revert-mode nil))

(use-package editorconfig
  :hook
  (prog-mode . editorconfig-mode))

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

(use-package yasnippet)

(use-package flycheck)

(use-package lsp-ui
  :custom
  (lsp-ui-imenu-auto-refresh t)
  (lsp-ui-doc-position 'at-point))

(use-package consult-lsp)

(use-package lsp-java)

(use-package dap-mode)

(provide 'rc-programming)
