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
  :config
  (setq completion-styles '(orderless))
  :custom
  (completion-category-defaults nil))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package vertico
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 17))

(use-package consult
  :general
  ("M-y" 'consult-yank-pop
   "C-s" 'consult-line)
  (:prefix "C-x"
           "b" 'consult-buffer
           "4 b" 'consult-buffer-other-window
           "5 b" 'consult-buffer-other-frame
           "r b" 'consult-bookmark)
  (:prefix "C-c"
           "r m" 'consult-man
           "r f" 'consult-fd
           "r g" 'consult-ripgrep
           "r y" 'consult-git-grep
           "z t" 'consult-theme)
  (:prefix "M-g"
           "e" 'consult-compile-error
           "o" 'consult-outline
           "i" 'consult-imenu)
  (minibuffer-local-map
   "M-r" 'consult-history)
  :custom
  (consult-line-start-from-top t)
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref))

(use-package embark
  :general
  (minibuffer-local-map
   "C-." 'embark-act
   "C-;" 'embark-dwin
   "C-h B" 'embark-bindings)
  :custom
  (prefix-help-command 'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package corfu
  :general
  (corfu-map
   "RET" nil
   "SPC" 'corfu-insert-separator)
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
