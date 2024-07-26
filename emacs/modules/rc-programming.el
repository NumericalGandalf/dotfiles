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

(dolist (mode '(c c++ rust java))
  (add-hook (intern (concat (symbol-name mode) "-ts-mode-hook")) 'eglot-ensure))

(with-eval-after-load 'eglot
  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider
          :codeLensProvider
          :documentOnTypeFormattingProvider
          :colorProvider
          :foldingRangeProvider
          :inlayHintProvider))
  (define-key eglot-mode-map (kbd "C-c l d") 'eglot-find-declaration)
  (define-key eglot-mode-map (kbd "C-c l t") 'eglot-find-typeDefinition)
  (define-key eglot-mode-map (kbd "C-c l i") 'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions))

(provide 'rc-programming)
