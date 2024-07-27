(tool-bar-mode 0)
(scroll-bar-mode 0)

(rc-load-font t)

(use-package catppuccin-theme
  :demand
  :custom
  (catppuccin-flavor 'macchiato)
  (catppuccin-italic-comments t)
  (catppuccin-italic-blockquotes t)
  (catppuccin-italic-variables t)
  :config
  (load-theme 'catppuccin t))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :straight
  (:host github :repo "maxecharel/nerd-icons-completion" :branch "contrib")
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package all-the-icons)

(defun icons-install-fonts ()
  "Install fonts for `nerd-icons' and `all-the-icons'."
  (interactive)
  (nerd-icons-install-fonts t)
  (all-the-icons-install-fonts t))

(add-hook 'dots-deploy-hook 'icons-install-fonts)

(use-package doom-modeline
  :demand
  :init
  (doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'file-name-with-project)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-highlight-modified-buffer-name nil))

(use-package emojify
  :demand
  :custom
  (emojify-download-emojis-p t)
  :config
  (global-emojify-mode))

(use-package dashboard
  :custom
  (inhibit-startup-message t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-display-icons-p t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-heading-shorcut-format "")
  (dashboard-startup-banner (dots-expand-asset "banner.txt"))
  (dashboard-items '((recents   . 5)
		     (projects  . 5)
		     (bookmarks . 5)
		     (agenda    . 5)))
  (dashboard-startupify-list '(dashboard-insert-banner
			       dashboard-insert-newline
			       dashboard-insert-footer
			       dashboard-insert-items
			       dashboard-insert-newline
			       dashboard-insert-init-info))
  (dashboard-item-names '(("Recent Files:" . "Recent Files")
			  ("Projects:" . "Projects")
			  ("Bookmarks:" . "Bookmarks")
			  ("Agenda for the coming week:" . "Agenda")
			  ("Registers:" . "Registers")))
  :config
  (add-hook 'window-size-change-functions 'dashboard-resize-on-hook)
  (add-hook 'window-setup-hook 'dashboard-resize-on-hook))

(defun dashboard-ensure ()
  "Open dashboard if window is not splitted and current buffer is scratch."
  (interactive)
  (when (and (string= (buffer-name) "*scratch*")
	     (= (length (window-list)) 1))
    (dashboard-open)))

(defun dashboard-force ()
  "Force dashboard and delete all other windows."
  (interactive)
  (delete-other-windows)
  (dashboard-open))

(add-hook 'emacs-startup-hook 'dashboard-ensure)
(add-hook 'server-after-make-frame-hook 'dashboard-ensure)

(provide 'rc-theming)
