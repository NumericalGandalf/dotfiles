(use-package orderless
  :config
  (setq completion-styles '(orderless)
	completion-category-defaults nil))

(use-package vertico
  :config
  (setq vertico-cycle t
	enable-recursive-minibuffers t)
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
  :config
  (setq consult-line-start-from-top t
        xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function 'consult-xref))

(use-package embark)

(use-package embark-consult
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwin))
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'minibuf)
