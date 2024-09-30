(add-to-list 'treesit-extra-load-path (rc/cache "tree-sitter/"))

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
  (let ((outdir (nth 0 treesit-extra-load-path))
        (fun #'treesit--install-language-grammar-1)
        (sources (treesit-auto--build-treesit-source-alist)))
    (when prefix
      (delete-directory outdir t))
    (dolist (source sources)
      (let ((lang (nth 0 source)))
        (unless (or (treesit-ready-p lang t)
                    (member lang '(janet latex markdown)))
          (message "Building tree-sitter library for language: %s" lang)
          (let ((inhibit-message t)
                (message-log-max nil))
            (apply fun outdir source)))))))

(rc/deploy (treesit-ensure-all))
