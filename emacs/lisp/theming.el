(setq inhibit-startup-message t
      server-client-instructions nil)

(add-hook
 'after-init-hook
 (lambda ()
   (unless (string=
            inhibit-startup-echo-area-message user-login-name)
     (customize-save-variable
      'inhibit-startup-echo-area-message user-login-name))))

(use-package doom-themes
  :demand
  :config
  (load-theme 'doom-palenight t)
  (doom-themes-org-config)
  (doom-themes-treemacs-config))

(use-package all-the-icons
  :hook
  (dots-deploy-hook . (lambda () (all-the-icons-install-fonts t))))

(use-package nerd-icons
  :hook
  (dots-deploy-hook . (lambda () (nerd-icons-install-fonts t))))

(use-package nerd-icons-dired
  :hook
  (dired-mode-hook . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook
  (ibuffer-mode-hook . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :hook
  (marginalia-mode-hook . nerd-icons-completion-marginalia-setup))

(use-package doom-modeline
  :init
  (doom-modeline-mode)
  :custom
  (doom-modeline-percent-position '(6 "%q"))
  (doom-modeline-buffer-file-name-style 'file-name-with-project)
  (doom-modeline-highlight-modified-buffer-name nil))

(use-package dashboard
  :hook
  (emacs-startup-hook
   . (lambda ()
       (when (and (string= (buffer-name) "*scratch*")
	              (= (length (window-list)) 1))
         (dashboard-open))
       (setq initial-buffer-choice
             (lambda ()
               (get-buffer-create dashboard-buffer-name)))))
  (window-size-change-functions . dashboard-resize-on-hook)
  (window-setup-hook . dashboard-resize-on-hook)
  :custom
  (dashboard-icon-type 'nerd-icons)
  (dashboard-display-icons-p t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-show-shortcuts nil)
  (dashboard-startup-banner (rc-expand "banner.txt"))
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
  (dashboard-heading-icons '((recents   . "nf-oct-history")
                             (bookmarks . "nf-oct-bookmark")
                             (agenda    . "nf-oct-calendar")
                             (projects  . "nf-oct-rocket")
                             (registers . "nf-oct-database"))))

(use-package beacon
  :custom
  (beacon-blink-when-focused t)
  (beacon-blink-when-point-moves-vertically 5)
  (beacon-dont-blink-commands ()))

(use-package emojify
  :custom
  (emojify-download-emojis-p t))

(use-package colorful-mode)

(provide 'theming)
