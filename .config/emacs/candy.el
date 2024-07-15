(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)

(let ((font "Fira Code"))
  (set-face-attribute 'default nil :font font :height 130)
  (set-face-attribute 'fixed-pitch nil :family font)
  (set-face-attribute 'fixed-pitch-serif nil :family font)
  (set-face-attribute 'variable-pitch nil :family font))

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

(use-package dashboard
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))
	dashboard-icon-type 'all-the-icons
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-center-content t
	dashboard-vertically-center-content t
	dashboard-items '((recents   . 5)
			  (bookmarks . 5)
			  (projects  . 5)
			  (agenda    . 5)
			  (registers . 5))
	dashboard-heading-icons '((recents   . "history")
                                  (bookmarks . "bookmark")
                                  (agenda    . "calendar")
                                  (projects  . "rocket")
                                  (registers . "database")))
  (dashboard-setup-startup-hook))

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
