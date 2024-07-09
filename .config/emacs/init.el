(setq custom-file (locate-user-emacs-file "var/custom.el"))
(load custom-file :noerror :nomessage)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(blink-cursor-mode 0)
(fringe-mode 0)

(electric-pair-mode 1)
(ffap-bindings)

(setq display-line-numbers-type 'relative)
(global-visual-line-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-remote-files t)

(setq use-short-answers t
      inhibit-startup-message t)

(setq backup-directory-alist `(("." . ,(locate-user-emacs-file "var/backups/")))
      backup-by-copying t
      version-control t
      delete-old-versions t)

(setq-default c-basic-offset 4
              c-ts-mode-indent-offset c-basic-offset)

(setq dired-listing-switches "-lah"
      dired-free-space 'separate
      dired-recursive-deletes 'always
      dired-dwim-target t
      dired-auto-revert-buffer t)

(setq compilation-ask-about-save nil
      compile-command nil)

(let ((font "DejaVu Sans Mono"))
  (set-face-attribute 'default nil :font font :height 130)
  (set-face-attribute 'fixed-pitch nil :family font)
  (set-face-attribute 'fixed-pitch-serif nil :family font)
  (set-face-attribute 'variable-pitch nil :family font))

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

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-gruvbox-dark-variant "hard")
  (load-theme 'doom-gruvbox t))

(use-package vertico
  :config
  (vertico-mode 1))

(use-package orderless
  :config
  (add-to-list 'completion-styles 'orderless))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package embark)
(use-package embark-consult)

(use-package consult
  :bind
  (("M-y" . consult-yank-pop)
   ("C-s" . consult-line)
   ("C-x M-:" . consult-complex-command)
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x t b" . consult-buffer-other-tab)
   ("C-x r b" . consult-bookmark)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)
   ("M-g o" . consult-outline)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-s m" . consult-man)
   ("M-s f" . consult-find)
   ("M-s y" . consult-grep)
   ("M-s g" . consult-git-grep)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)
   :map minibuffer-local-map
   ("M-s" . consult-history))
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
  (global-treesit-auto-mode 1))

(use-package rust-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package cmake-mode)
(use-package yaml-mode)

(use-package magit)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package company
  :config
  (setq company-tooltip-scrollbar-width 0
        company-tooltip-idle-delay 0
        company-tooltip-align-annotations t)
  (global-company-mode 1))

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

;; (unless (package-installed-p 'emapl)
;;   (package-vc-install '(emapl :url "https://github.com/NumericalGandalf/emapl.git")))

(load "~/Programming/emapl/emapl.el")
