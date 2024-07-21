(require 'rc)

(tool-bar-mode 0)
(scroll-bar-mode 0)

(use-package catppuccin-theme
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

(unless (package-installed-p 'nerd-icons-completion)
  (package-vc-install
   '(nerd-icons-completion
     :url "https://github.com/maxecharel/nerd-icons-completion.git"
     :branch "contrib")))
(add-hook 'marginalia-mode-hook 'nerd-icons-completion-marginalia-setup)

(use-package doom-modeline
  :init
  (doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'file-name-with-project)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-highlight-modified-buffer-name nil))

(defun display-startup-echo-area-message ())
(setq server-client-instructions nil)

(defun dashboard-ensure (&optional prefix)
  "Open dashboard if window is not splitted and current buffer is scratch.
If PREFIX is non-nil, open the dashboard anyway."
  (interactive "P")
  (when (or prefix (and (string= (buffer-name) "*scratch*")
			(= (length (window-list)) 1)))
    (delete-other-windows)
    (dashboard-open)))

(use-package dashboard
  :custom
  (inhibit-startup-message t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-display-icons-p t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-startup-banner (locate-user-emacs-file "banner.txt"))
  (dashboard-heading-shorcut-format "")
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
  (add-hook 'window-setup-hook 'dashboard-resize-on-hook)
  (add-hook 'emacs-startup-hook 'dashboard-ensure)
  (add-hook 'server-after-make-frame-hook 'dashboard-ensure))

(provide 'candy)
