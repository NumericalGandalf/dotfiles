(use-package treesit-auto
  :demand
  :config
  (global-treesit-auto-mode))

(defun treesit-ensure-all ()
  (interactive)
  (setq treesit-language-source-alist
        (treesit-auto--build-treesit-source-alist))
  (dolist (source treesit-language-source-alist)
    (let ((lang (nth 0 source))
          (outdir treesit-user-load-path))
      (unless (or (member lang treesit-ignore-langs)
                  (treesit-language-available-p lang))
        (let ((message-log-max nil)
              (inhibit-message t))
          (apply 'treesit--install-language-grammar-1 outdir source))
        (when-let (library (directory-files outdir t (symbol-name lang)))
          (message "Built tree-sitter library %s" (nth 0 library)))))))

(add-to-list 'treesit-extra-load-path treesit-user-load-path)
(add-to-list 'dots-deploy-hook 'treesit-ensure-all)

(setq compilation-ask-about-save nil
      compile-command nil)

(electric-pair-mode)

(setq-default indent-tabs-mode nil
	      c-basic-offset 4
	      c-ts-mode-indent-offset c-basic-offset)

(use-package magit
  :config
  (when global-auto-revert-mode
    (magit-auto-revert-mode 0)))

(use-package editorconfig
  :demand
  :config
  (editorconfig-mode))

(use-package yasnippet)

(use-package lsp-mode
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (c-ts-mode . lsp)
   (cpp-ts-mode . lsp))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil))

(use-package lsp-ui
  :custom
  (lsp-ui-imenu-auto-refresh t)
  (lsp-ui-doc-position 'at-point))

(use-package consult-lsp)

(use-package dap-mode)

(provide 'rc-programming)
