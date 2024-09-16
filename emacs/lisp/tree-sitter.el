(add-to-list 'treesit-extra-load-path (rc-cache "tree-sitter/"))

(with-eval-after-load 'files
  (add-to-list 'auto-mode-alist '("\\.jsonc\\'" . json-ts-mode)))

(with-eval-after-load 'c-ts-mode
  (setq-default c-ts-mode-indent-offset 4))

(use-package treesit-auto
  :demand
  :config
  (global-treesit-auto-mode))

(defun treesit-ensure-all (&optional prefix)
	"Ensure all available tree-sitter libraries.
If optional PREFIX is non-nil, force all builds."
	(interactive "P")
	(when-let (outdir (nth 0 treesit-extra-load-path))
      (dolist (source (treesit-auto--build-treesit-source-alist))
		(let ((lang (nth 0 source)))
          (when (and (or prefix
						 (not (treesit-ready-p lang t)))
					 (not (member lang '(janet latex markdown))))
			(apply 'treesit--install-language-grammar-1 outdir source))))))

(add-hook 'posix-deploy-hook 'treesit-ensure-all)
(add-hook 'mswin-deploy-hook 'treesit-ensure-all)

(provide 'tree-sitter)
