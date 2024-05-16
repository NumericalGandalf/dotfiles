(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :noerror :nomessage)

(menu-bar-mode 0)
(tool-bar-mode 0)
(fringe-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(global-visual-line-mode 1)

(setq
 use-dialog-box nil
 use-short-answers t
 inhibit-startup-message t)

(column-number-mode 1)
(global-display-line-numbers-mode 1)
(setq
 display-line-numbers-type 'relative
 display-line-numbers-width-start t)

(global-auto-revert-mode 1)
(setq
 global-auto-revert-non-file-buffers t
 auto-revert-remote-files t)

(setq
 backup-directory-alist
 `(("." . ,(locate-user-emacs-file "backups/")))
 backup-by-copying t
 version-control t
 delete-old-versions t)

(with-eval-after-load 'dired
  (setq
   dired-listing-switches "-lah"
   find-ls-option '("-exec ls -ldh {} +" . "-ldh")
   dired-free-space 'separate
   dired-recursive-deletes 'always
   dired-dwim-target t
   dired-auto-revert-buffer t))

(let ((font "DejaVu Sans Mono"))
  (set-face-attribute 'default nil :font font :height 150)
  (set-face-attribute 'fixed-pitch nil :family font)
  (set-face-attribute 'fixed-pitch-serif nil :family font)
  (set-face-attribute 'variable-pitch nil :family font))

(require 'package)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package
 zendalf-theme
 :vc (:url "https://github.com/BinaryGandalf/zendalf.git")
 :config (load-theme 'zendalf))

(use-package diminish)

(use-package
 orderless
 :config (add-to-list 'completion-styles 'orderless))

(use-package vertico :config (vertico-mode 1))

(use-package marginalia :diminish :config (marginalia-mode 1))

(use-package wgrep :defer)

(use-package
 multiple-cursors
 :config
 (global-set-key (kbd "C->") 'mc/mark-next-like-this)
 (global-set-key (kbd "C-<") 'mc/unmark-next-like-this)
 (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
 (global-set-key (kbd "C-c C-<") 'mc/edit-lines))

(use-package rust-mode :defer)
(use-package lua-mode :defer)
(use-package markdown-mode :defer)

(use-package magit :defer)

(use-package editorconfig :defer)

(use-package
 corfu
 :config
 (setq
  corfu-auto t
  corfu-cycle t)
 (global-corfu-mode 1))

(with-eval-after-load 'eglot
  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider
          :codeLensProvider
          :documentOnTypeFormattingProvider
          :colorProvider
          :foldingRangeProvider
          :inlayHintProvider))
  (define-key eglot-mode-map (kbd "C-c l d") 'eglot-find-declaration)
  (define-key
   eglot-mode-map (kbd "C-c l t") 'eglot-find-typeDefinition)
  (define-key
   eglot-mode-map (kbd "C-c l i") 'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l n") 'flymake-goto-next-error)
  (define-key eglot-mode-map (kbd "C-c l p") 'flymake-goto-prev-error)
  (define-key
   eglot-mode-map (kbd "C-c l e") 'flymake-show-project-diagnostics))
