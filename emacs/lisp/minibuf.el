(savehist-mode)

(setq resize-mini-windows t
      enable-recursive-minibuffers t)

(setq tab-always-indent 'complete)

(setq read-extended-command-predicate
      'command-completion-default-include-p)

(setq completions-detailed t
      completion-ignore-case  t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

(use-package orderless
  :init
  (setq completion-styles '(orderless))
  :custom
  (completion-category-defaults nil))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package vertico
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 17))

(use-package consult
  :custom
  (consult-line-start-from-top t)
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref))

(use-package embark
  :custom
  (prefix-help-command 'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-min-width 45)
  (corfu-separator ?\s)
  (corfu-quit-no-match t))

(provide 'minibuf)
