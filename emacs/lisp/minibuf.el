(savehist-mode)

(setq resize-mini-windows t
      enable-recursive-minibuffers t)

(setq tab-always-indent 'complete)

(setq read-extended-command-predicate
      'command-completion-default-include-p)

(use-package orderless
  :init
  (setq completion-styles '(orderless))
  :custom
  (completion-category-defaults nil))

(use-package marginalia
  :after
  orderless
  :demand
  :config
  (marginalia-mode))

(use-package vertico
  :hook
  (emacs-startup-hook . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 15))

(use-package consult
  :custom
  (consult-line-start-from-top t)
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref))

(use-package consult-lsp
  :after
  (consult lsp-mode))

(use-package embark
  :custom
  (prefix-help-command 'embark-prefix-help-command))

(use-package embark-consult
  :after
  (embark consult)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package corfu
  :hook
  ((prog-mode-hook . corfu-mode)
   (shell-mode-hook . corfu-mode)
   (eshell-mode-hook . corfu-mode)
   (minibuffer-mode-hook . corfu-mode))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-min-width 45)
  (corfu-separator ?\s)
  (corfu-quit-no-match t)
  (global-corfu-minibuffer
   (lambda ()
     (not (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map))))))

(provide 'minibuf)
