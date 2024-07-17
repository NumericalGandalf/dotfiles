(tool-bar-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)

(defun display-startup-echo-area-message ())
(setq server-client-instructions nil)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil))

(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-palenight t))

(use-package doom-modeline
  :init
  (doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'file-name-with-project))

(defun rc-ensure-dashboard(&optional prefix)
  "Opens dashboard if window is not splitted and current buffer is scratch.
If PREFIX is non-nil, open the dashboard anyway."
  (interactive "P")
  (when (or prefix (and (string= (buffer-name) "*scratch*")
		       (= (length (window-list)) 1)))
    (dashboard-open)))

(use-package dashboard
  :config
  (setq inhibit-startup-message t
	dashboard-icon-type 'all-the-icons
	dashboard-display-icons-p t
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-center-content t
	dashboard-startup-banner (no-littering-expand-etc-file-name "banner.txt")
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
  (add-hook 'emacs-startup-hook 'rc-ensure-dashboard)
  (add-hook 'server-after-make-frame-hook 'rc-ensure-dashboard))

(use-package ligature
  :config
  (ligature-set-ligatures
   'prog-mode
   '(("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
     (";" (rx (+ ";")))
     ("&" (rx (+ "&")))
     ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
     ("?" (rx (or ":" "=" "\." (+ "?"))))
     ("%" (rx (+ "%")))
     ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]" "-" "=" ))))
     ("\\" (rx (or "/" (+ "\\"))))
     ("+" (rx (or ">" (+ "+"))))
     (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
     ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!" "="))))
     ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
     ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
     ("*" (rx (or ">" "/" ")" (+ "*"))))
     ("w" (rx (+ "w")))
     ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!" "-"  "/" "|" "="))))
     (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
     ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_" (+ "#"))))
     ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
     ("_" (rx (+ (or "_" "|"))))
     ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
     "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
     "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  (global-ligature-mode))
