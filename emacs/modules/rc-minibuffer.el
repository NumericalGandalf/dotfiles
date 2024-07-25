(savehist-mode)

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil))

(use-package vertico
  :custom
  (vertico-cycle t)
  (enable-recursive-minibuffers t)
  :config
  (vertico-mode))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package consult
  :defer
  :custom
  (consult-line-start-from-top t)
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref))

(use-package embark
  :defer
  :custom
  (prefix-help-command 'embark-prefix-help-command))

(use-package embark-consult
  :defer
  :after
  embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(defun corfu-minibuffer-p ()
  "Predicate whether corfu should be enabled in the minibuffer."
  (not (or (bound-and-true-p mct--active)
           (bound-and-true-p vertico--input)
           (eq (current-local-map) read-passwd-map))))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  (tab-always-indent 'complete)
  (global-corfu-minibuffer 'corfu-minibuffer-p)
  :config
  (global-corfu-mode))

(provide 'rc-minibuffer)
