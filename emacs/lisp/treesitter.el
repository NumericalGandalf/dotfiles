(use-package treesit-auto
  :demand
  :config
  (global-treesit-auto-mode))

(defun treesit-ensure-all ()
  "Ensure all available tree-sitter libraries."
  (interactive)
  (mapc
   (lambda (source)
     (when-let ((outdir (nth 0 treesit-extra-load-path))
                (lang (nth 0 source)))
       (unless (or (member lang '(latex markdown janet))
                   (treesit-ready-p lang t))
         (apply 'treesit--install-language-grammar-1 outdir source))))
   (treesit-auto--build-treesit-source-alist)))

(setq-default c-ts-mode-indent-offset c-basic-offset
              json-ts-mode-indent-offset c-basic-offset)

(add-to-list 'treesit-extra-load-path (rc-cache "tree-sitter/"))
(add-hook 'dots-deploy-hook 'treesit-ensure-all)

(provide 'treesitter)
