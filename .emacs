(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :noerror :nomessage)

(menu-bar-mode 0)
(tool-bar-mode 0)
(fringe-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(global-visual-line-mode 1)

(setq use-dialog-box nil
      use-short-answers t
      inhibit-startup-message t
      isearch-repeat-on-direction-change t)

(column-number-mode 1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-remote-files t)

(setq backup-directory-alist `(("." . ,(locate-user-emacs-file "backups/")))
      backup-by-copying t
      version-control t
      delete-old-versions t)

(setq-default indent-tabs-mode nil
              c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "stroustrup")))

(setq dired-listing-switches "-lah"
      dired-free-space 'separate
      dired-recursive-deletes 'always
      dired-dwim-target t
      dired-auto-revert-buffer t)

(let ((font "DejaVu Sans Mono"))
  (set-face-attribute 'default nil :font font :height 130)
  (set-face-attribute 'fixed-pitch nil :family font)
  (set-face-attribute 'fixed-pitch-serif nil :family font)
  (set-face-attribute 'variable-pitch nil :family font))

(require 'package)
(require 'use-package)
(setq use-package-always-ensure t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t))

(use-package ido-completing-read+
  :config 
  (setq completion-ignore-case t
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        ido-use-filename-at-point 'guess
        ido-use-url-at-point 'guess)
  (ido-mode 1)
  (ido-everywhere 1)
  (ido-ubiquitous-mode 1))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/unmark-next-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/edit-lines))

(use-package rust-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package cmake-mode)
(use-package yaml-mode)

(use-package magit)
(use-package editorconfig)

(use-package company
  :config
  (setq company-tooltip-scrollbar-width 0
        company-tooltip-idle-delay 0
        company-tooltip-align-annotations t)
  (global-company-mode 1))

(global-set-key (kbd "C-c l s") 'eglot)
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
  (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l n") 'flymake-goto-next-error)
  (define-key eglot-mode-map (kbd "C-c l p") 'flymake-goto-prev-error)
  (define-key eglot-mode-map (kbd "C-c l e") 'flymake-show-project-diagnostics))
