(setq use-short-answers t
      inhibit-startup-screen t
      ring-bell-function nil
      echo-keystrokes 0
      scroll-step 1
      scroll-margin 4
      scroll-preserve-screen-position t)

(with-eval-after-load 'simple
  (setq suggest-key-bindings nil))

(with-eval-after-load 'server
  (setq server-client-instructions nil))

(with-eval-after-load 'display-line-numbers
  (setq display-line-numbers-width-start t
        display-line-numbers-grow-only t
        display-line-numbers-type 'relative))

(use-package doom-themes
  :init
  (load-theme 'doom-palenight t)
  :config
  (doom-themes-org-config)
  (doom-themes-treemacs-config)
  (doom-themes-set-faces nil
    '(wgrep-face :weight 'normal
                 :foreground 'unspecified
                 :background 'unspecified)
    '(wgrep-done-face :foreground 'unspecified
                      :background 'unspecified)))

(use-package all-the-icons
  :hook
  (posix-deploy-hook . (lambda () (all-the-icons-install-fonts t))))

(use-package nerd-icons
  :hook
  (posix-deploy-hook . (lambda () (nerd-icons-install-fonts t))))

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
  :config
  (add-to-list 'corfu-margin-formatters 'nerd-icons-corfu-formatter))

(use-package doom-modeline
  :init
  (doom-modeline-mode)
  :custom
  (doom-modeline-percent-position '(6 "%q"))
  (doom-modeline-buffer-file-name-style 'file-name-with-project)
  (doom-modeline-highlight-modified-buffer-name nil))

(use-package dashboard
  :hook
  ((window-size-change-functions . dashboard-resize-on-hook)
   (window-setup-hook . dashboard-resize-on-hook)
   (emacs-startup-hook . (lambda ()
                           (when (and (string= (buffer-name) "*scratch*")
                                      (= (length (window-list)) 1))
                             (dashboard-open)))))
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  :custom
  (dashboard-icon-type 'nerd-icons)
  (dashboard-display-icons-p t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-show-shortcuts nil)
  (dashboard-startup-banner 'ascii)
  (dashboard-banner-ascii (rc/expand "banner.txt"))
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

(unless (daemonp)
  (defun display-startup-echo-area-message ()))

(pixel-scroll-mode)
(pixel-scroll-precision-mode)

(global-display-line-numbers-mode)
(column-number-mode)
(global-visual-line-mode)

(provide 'theming)
