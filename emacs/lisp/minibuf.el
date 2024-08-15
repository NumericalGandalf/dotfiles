(savehist-mode)

(setq resize-mini-windows t
      enable-recursive-minibuffers t)

(use-package orderless
  :demand
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil))

(use-package vertico
  :demand
  :custom
  (vertico-cycle t)
  (vertico-count 15)
  :config
  (vertico-multiform-mode)
  (vertico-mode))

(use-package marginalia
  :demand
  :config
  (marginalia-mode))

(use-package consult
  :custom
  (consult-line-start-from-top t)
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref))

(use-package embark
  :custom
  (prefix-help-command 'embark-prefix-help-command))

(use-package embark-consult
  :after
  embark
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package corfu
  :demand
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  (tab-always-indent 'complete)
  (global-corfu-minibuffer
   (lambda ()
     (not (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map)))))
  :config
  (global-corfu-mode))

(use-package which-key
  :demand
  :custom
  (which-key-idle-delay 1.5)
  :config
  (which-key-mode))

(use-package helpful)

(provide 'minibuf)
