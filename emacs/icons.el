(use-package nerd-icons
  :init
  (rc/deploy (nerd-icons-install-fonts t)))

(use-package nerd-icons-dired
  :after dired
  :hook
  (dired-mode-hook . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :after ibuffer
  :hook
  (ibuffer-mode-hook . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :after marginalia
  :hook
  (marginalia-mode-hook . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :after corfu
  :hook
  (corfu-margin-formatters . nerd-icons-corfu-formatter))
