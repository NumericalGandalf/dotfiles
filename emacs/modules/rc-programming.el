(defcustom treesit-ignore-langs '(latex markdown janet)
  "List of languages to ignore in `treesit-ensure-all'."
  :type '(repeat symbol))

(defcustom treesit-user-load-path (rc-cache-file "tree-sitter/")
  "User tree-sitter library load path."
  :type 'string)

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(defun treesit-ensure-all ()
  (interactive)
  (setq treesit-language-source-alist
        (treesit-auto--build-treesit-source-alist))
  (dolist (source treesit-language-source-alist)
    (let ((lang (nth 0 source)))
      (unless (or (treesit-ready-p lang t)
                  (member lang treesit-ignore-langs))
        (apply 'treesit--install-language-grammar-1
               treesit-user-load-path source)))))

(add-to-list 'treesit-extra-load-path treesit-user-load-path)
;; (add-to-list 'dots-deploy-hook 'treesit-ensure-all)

(setq compilation-ask-about-save nil
      compile-command nil)

(electric-pair-mode)

(setq-default indent-tabs-mode nil
	      c-basic-offset 4
	      c-ts-mode-indent-offset c-basic-offset)

(use-package magit
  :defer  
  :config
  (when global-auto-revert-mode
    (magit-auto-revert-mode 0)))

(use-package editorconfig
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
