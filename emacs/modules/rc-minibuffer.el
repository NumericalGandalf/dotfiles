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
  :bind
  (([remap yank-from-kill-ring] . consult-yank-pop)
   ([remap isearch-forward] . consult-line)
   ("C-x M-:" . consult-complex-command)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap bookmark-jump] . consult-bookmark)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)
   ("M-g o" . consult-outline)
   ("M-g i" . consult-imenu)
   ("M-o m" . consult-man)
   ("M-o f" . consult-find)
   ("M-o y" . consult-grep)
   ("M-o g" . consult-git-grep)
   :map minibuffer-local-map
   ("M-r" . consult-history))
  :custom
  (consult-line-start-from-top t)
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwin)
   ("C-h B" . embark-bindings))
  :custom
  (prefix-help-command 'embark-prefix-help-command))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package company
  :custom
  (company-tooltip-scrollbar-width 0)
  (company-tooltip-idle-delay 0)
  (company-tooltip-align-annotations t)
  :config
  (global-company-mode))

(provide 'rc-minibuffer)
