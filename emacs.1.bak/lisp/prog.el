(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package rust-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package cmake-mode)
(use-package yaml-mode)
(use-package json-mode)

(setq compilation-ask-about-save nil
      compile-command nil)

(electric-pair-mode)

(setq indent-tabs-mode nil)
(setq-default c-basic-offset 4
	      c-ts-mode-indent-offset c-basic-offset)

(use-package magit
  :config
  (when global-auto-revert-mode
    (setq magit-auto-revert-mode nil)))

(use-package editorconfig
  :config
  (editorconfig-mode))

(use-package company
  :config
  (setq company-tooltip-scrollbar-width 0
        company-tooltip-idle-delay 0
        company-tooltip-align-annotations t)
  (global-company-mode))

(dolist (mode '(c c++ rust java))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook")) 'eglot-ensure)
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

(provide 'prog)
