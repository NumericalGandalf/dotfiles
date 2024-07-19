(require 'rc)
(require 'dots)

(tool-bar-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)
(menu-bar-mode 0)

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'macchiato
	catppuccin-italic-comments t
	catppuccin-italic-blockquotes t
	catppuccin-italic-variables t)
  (load-theme 'catppuccin t))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil))

(unless (package-installed-p 'all-the-icons-completion)
  (package-vc-install
   '(all-the-icons-completion
     :url "https://github.com/NumericalGandalf/all-the-icons-completion.git")))
(require 'all-the-icons-completion)
(add-hook 'marginalia-mode-hook 'all-the-icons-completion-marginalia-setup)

(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package doom-modeline
  :init
  (doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'file-name-with-project
	doom-modeline-buffer-modification-icon nil
	doom-modeline-highlight-modified-buffer-name nil))

(defun display-startup-echo-area-message ())
(setq server-client-instructions nil)

(defun dashboard-ensure (&optional prefix)
  "Opens dashboard if window is not splitted and current buffer is scratch.
If PREFIX is non-nil, open the dashboard anyway."
  (interactive "P")
  (when (or prefix (and (string= (buffer-name) "*scratch*")
			(= (length (window-list)) 1)))
    (delete-other-windows)
    (dashboard-open)))

(use-package dashboard
  :config
  (setq inhibit-startup-message t
	dashboard-icon-type 'all-the-icons
	dashboard-display-icons-p t
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-center-content t
	dashboard-startup-banner (dots-expand-file "assets/Emacs.txt")
	dashboard-heading-shorcut-format ""
	dashboard-items '((recents   . 5)
			  (projects  . 5)
			  (bookmarks . 5)
			  (agenda    . 5))
	dashboard-heading-icons '((recents   . "history")
				  (projects  . "rocket")
				  (bookmarks . "bookmark")
				  (agenda    . "calendar")
				  (registers . "database"))
	dashboard-startupify-list '(dashboard-insert-banner
				    dashboard-insert-newline
				    dashboard-insert-footer
				    dashboard-insert-items
				    dashboard-insert-newline
				    dashboard-insert-init-info)
	dashboard-item-names '(("Recent Files:" . "Recent Files")
			       ("Projects:" . "Projects")
			       ("Bookmarks:" . "Bookmarks")
			       ("Agenda for the coming week:" . "Agenda")
			       ("Registers:" . "Registers")))
  (add-hook 'window-size-change-functions 'dashboard-resize-on-hook)
  (add-hook 'window-setup-hook 'dashboard-resize-on-hook)
  (add-hook 'emacs-startup-hook 'dashboard-ensure)
  (add-hook 'server-after-make-frame-hook 'dashboard-ensure))

(provide 'candy)
