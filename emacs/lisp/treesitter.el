(defcustom treesit-ignore-langs '(janet latex markdown)
  "Treesitter languages to ignore."
  :type '(repeat symbol))

(use-package treesit-auto
  :ensure
  (:fetcher github :repo "NumericalGandalf/treesit-auto")
  :init
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
                   (not (member lang treesit-ignore-langs)))
          (message
           "Building tree-sitter library %s"
           (prin1-to-string (symbol-name lang)))
          (let ((inhibit-message t)
                (message-log-max nil))
            (apply
             'treesit--install-language-grammar-1 outdir source)))))))

(setq-default c-ts-mode-indent-offset c-basic-offset
              json-ts-mode-indent-offset c-basic-offset)

(add-to-list 'treesit-extra-load-path (rc-cache "tree-sitter/"))
(add-hook 'dots-deploy-hook 'treesit-ensure-all)

(provide 'treesitter)
