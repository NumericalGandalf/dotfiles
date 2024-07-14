(setq use-short-answers t
      inhibit-startup-message t
      custom-file (locate-user-emacs-file "var/custom.el"))
(load custom-file :noerror :nomessage)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)

(require 'package)
(require 'use-package)
(setq use-package-always-ensure t
      package-user-dir (locate-user-emacs-file "var/elpa/")
      package-gnupghome-dir (expand-file-name "gnupg/" package-user-dir))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(use-package no-littering)

(with-eval-after-load 'frame
  (let ((font "Fira Code"))
    (set-face-attribute 'default nil :font font :height 130)
    (set-face-attribute 'fixed-pitch nil :family font)
    (set-face-attribute 'fixed-pitch-serif nil :family font)
    (set-face-attribute 'variable-pitch nil :family font))
  (blink-cursor-mode 0))

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

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil))

(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(with-eval-after-load 'simple
  (setq display-line-numbers-type 'relative)
  (column-number-mode)
  (global-display-line-numbers-mode)
  (global-visual-line-mode)
  (save-place-mode))

(with-eval-after-load 'ffap
  (setq ffap-require-prefix t)
  (ffap-bindings))

(with-eval-after-load 'dired
  (setq dired-listing-switches "-lah"
	dired-free-space 'separate
	dired-recursive-deletes 'always
	dired-dwim-target t
	dired-auto-revert-buffer t)
  (require 'dired-x))

(with-eval-after-load 'files
  (setq backup-directory-alist `(("." . ,(locate-user-emacs-file "var/backups/")))
	backup-by-copying t
	version-control t
	delete-old-versions t))

(with-eval-after-load 'autorevert
  (setq global-auto-revert-non-file-buffers t
	auto-revert-remote-files t)
  (global-auto-revert-mode))

(with-eval-after-load 'compile
  (setq compilation-ask-about-save nil
	compile-command nil))

(with-eval-after-load 'electric
  (setq indent-tabs-mode nil)
  (setq-default c-basic-offset 4
		c-ts-mode-indent-offset c-basic-offset)
  (electric-pair-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless)
	completion-category-defaults nil))

(use-package vertico
  :config
  (vertico-mode))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark)
(use-package embark-consult)

(use-package consult
  :bind
  (([remap yank-from-kill-ring] . consult-yank-pop)
   ([remap isearch-forward] . consult-line)
   ("C-x M-:" . consult-complex-command)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap bookmark-jump] . consult-bookmark)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)
   ("M-g o" . consult-outline)
   ("M-g i" . consult-imenu)
   ("M-p m" . consult-man)
   ("M-p f" . consult-find)
   ("M-p y" . consult-grep)
   ("M-p g" . consult-git-grep)
   :map minibuffer-local-map
   ("M-r" . consult-history))
  :config
  (setq consult-line-start-from-top t
        xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function 'consult-xref))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/unmark-next-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/edit-lines))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package rust-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package cmake-mode)
(use-package yaml-mode)
(use-package json-mode)

(use-package magit)

(use-package editorconfig
  :config
  (editorconfig-mode))

(use-package company
  :config
  (setq company-tooltip-scrollbar-width 0
        company-tooltip-idle-delay 0
        company-tooltip-align-annotations t)
  (global-company-mode))

(dolist (mode '(c c++ rust java))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook")) 'eglot-ensure)
  (add-hook (intern (concat (symbol-name mode) "-ts-mode-hook")) 'eglot-ensure))

(with-eval-after-load 'eglot
  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider
          :codeLensProvider
          :documentOnTypeFormattingProvider
          :colorProvider
          :foldingRangeProvider
          :inlayHintProvider))
  (define-key eglot-mode-map (kbd "C-c l d") 'eglot-find-declaration)
  (define-key eglot-mode-map (kbd "C-c l t") 'eglot-find-typeDefinition)
  (define-key eglot-mode-map (kbd "C-c l i") 'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions))

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

(use-package vterm)

(unless (package-installed-p 'emapl)
  (package-vc-install '(emapl :url "https://github.com/NumericalGandalf/emapl.git")))
(global-set-key (kbd "M-D") 'emapl-run)
