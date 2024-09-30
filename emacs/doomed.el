(use-package doom-modeline
  :init
  (doom-modeline-mode)
  :custom
  (doom-modeline-percent-position 'mode-line-percent-position)
  (doom-modeline-buffer-file-name-style 'file-name-with-project)
  (doom-modeline-highlight-modified-buffer-name nil))

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

(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  :hook
  (emacs-startup-hook
   . (lambda () (setq initial-buffer-choice dashboard-buffer-name)))
  :custom
  (dashboard-icon-type 'nerd-icons)
  (dashboard-display-icons-p t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-show-shortcuts nil)
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
                             (registers . "nf-oct-database")))
  (dashboard-startup-banner 'ascii)
  (dashboard-banner-ascii
   "           _______  _______  _______  _______  _______           
 /\\    /\\ (  ____ \\(       )(  ___  )(  ____ \\(  ____ \\ /\\    /\\
 \\ \\  / / | (    \\/| () () || (   ) || (    \\/| (    \\/ \\ \\  / /
 _) () (_ | (__    | || || || (___) || |      | (_____  _) () (_
(_      _)|  __)   | |(_)| ||  ___  || |      (_____  )(_      _)
  ) () (  | (      | |   | || (   ) || |            ) |  ) () (
 / /  \\ \\ | (____/\\| )   ( || )   ( || (____/\\/\\____) | / /  \\ \\
 \\/    \\/ (_______/|/     \\||/     \\|(_______/\\_______) \\/    \\/ "))
