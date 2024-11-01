(require 'package)
(require 'use-package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents t)

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-hook-name-suffix nil)

(setq custom-file (locate-user-emacs-file "custom.el")
      create-lockfiles nil)

(setq use-short-answers t
      inhibit-startup-screen t
      ring-bell-function #'ignore
      echo-keystrokes 0)

(setq resize-mini-windows t
      enable-recursive-minibuffers t
      completion-ignore-case  t
      read-buffer-completion-ignore-case t
      tab-always-indent 'complete)

(setq scroll-step 1
      scroll-margin 4
      scroll-preserve-screen-position t)

(setq mode-line-percent-position '(6 "%q"))

(with-eval-after-load 'compile
  (setq compilation-ask-about-save nil
        compile-command nil
        compilation-auto-jump-to-first-error t))

(with-eval-after-load 'cc-vars
  (setq-default c-basic-offset 4))

(with-eval-after-load 'simple
  (setq-default indent-tabs-mode nil))

(with-eval-after-load 'minibuffer
  (setq completions-detailed t
        read-file-name-completion-ignore-case t))

(with-eval-after-load 'vc-hooks
  (setq vc-follow-symlinks t))

(with-eval-after-load 'dired
  (setq dired-listing-switches "-lah"
        dired-free-space 'separate
        dired-recursive-deletes 'always
        dired-dwim-target t
        dired-auto-revert-buffer t
        dired-clean-confirm-killing-deleted-buffers nil))

(with-eval-after-load 'display-line-numbers
  (setq display-line-numbers-width-start t
        display-line-numbers-grow-only t
        display-line-numbers-type 'relative))

(with-eval-after-load 'files
  (setq make-backup-files nil)
  (setq-default auto-save-default nil)
  (add-to-list 'auto-mode-alist '("\\.jsonc\\'" . js-json-mode)))

(with-eval-after-load 'auth-source
  (setq auth-source-save-behavior nil))

(with-eval-after-load 'autorevert
  (setq global-auto-revert-non-file-buffers t
        auto-revert-remote-files t
        auto-revert-verbose nil))

(with-eval-after-load 'ibuffer
  (setq ibuffer-use-other-window t))

(with-eval-after-load 'help
  (setq help-window-select t))

(with-eval-after-load 'man
  (setq Man-notify-method 'aggressive))

(use-package no-littering :demand)
(use-package diminish)

(use-package doom-themes
  :init
  (load-theme 'doom-gruvbox t))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t)
  (vertico-preselect 'no-prompt))

(use-package consult
  :custom
  (consult-line-start-from-top t)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package cape
  :hook
  ((completion-at-point-functions . cape-dabbrev)
   (completion-at-point-functions . cape-file)))

(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0)
  (corfu-min-width 45)
  (corfu-separator ?\s)
  (corfu-quit-no-match t))

(use-package magit
  :init
  (setq magit-auto-revert-mode nil))

(use-package projectile
  :diminish
  :init
  (projectile-mode 1))

(use-package sudo-edit)
(use-package move-text)

(use-package rust-mode)
(use-package yaml-mode)
(use-package cmake-mode)

(use-package flycheck
  :hook
  (lsp-mode-hook . flycheck-mode))

(use-package lsp-mode
  :hook
  ((c-mode-hook c++-mode-hook rust-mode-hook python-mode-hook) . lsp-deferred)
  :custom
  (lsp-completion-provider :none)
  (lsp-auto-guess-root t)
  (lsp-headerline-breadcrumb-enable nil))

(use-package yasnippet
  :after lsp-mode)

(use-package all-the-icons
  :after lsp-mode)

(use-package lsp-java
  :after lsp-mode)

(unless (package-installed-p 'app-launcher)
  (package-vc-install '(app-launcher :url "https://github.com/NumericalGandalf/app-launcher.git")))

(defun app-launcher ()
  "Create frame for `app-launcher-run-app'"
  (interactive)
  (let* ((height 21)
         (vertico-count (1- height)))
    (with-selected-frame (make-frame `((name . "app-launcher.el")
                                       (height . ,height)
                                       (width . ,(* height 5))
                                       (minibuffer . only)))
      (unwind-protect
          (app-launcher-run-app t)
        (delete-frame)))))

(use-package general
  :init
  (general-define-key
   "M-y" #'consult-yank-pop
   "C-s" #'consult-line
   "M-P" #'move-text-up
   "M-N" #'move-text-down)

  (general-define-key
   :prefix "C-x"
   "C-b" #'ibuffer
   "b" #'consult-buffer
   "u" #'sudo-edit-find-file
   "U" #'sudo-edit
   "4 b" #'consult-buffer-other-window
   "5 b" #'consult-buffer-other-frame
   "r b" #'consult-bookmark)

  (general-define-key
   :prefix "M-g"
   "e" #'consult-compile-error
   "o" #'consult-outline
   "i" #'consult-imenu)

  (general-define-key
   :prefix "C-c"
   "f" #'consult-find
   "g" #'consult-grep
   "y" #'consult-git-grep
   "q" #'query-replace-regexp)

  (general-def minibuffer-local-map
    "C-," #'embark-act
    "C-." #'embark-export)

  (general-def corfu-map
    "RET" nil)

  (general-def projectile-mode-map
    "C-x p" #'projectile-command-map)

  (setq lsp-keymap-prefix "C-,")
  (general-def lsp-mode-map
    "C-." #'lsp-describe-thing-at-point
    "M-," #'lsp-find-definition
    "M-?" #'lsp-find-references
    "C-r" #'lsp-rename)

  (general-def flycheck-mode-map
    "M-n" #'flycheck-next-error
    "M-p" #'flycheck-previous-error))

(global-display-line-numbers-mode 1)
(column-number-mode 1)
(global-visual-line-mode 1)

(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)

(global-auto-revert-mode 1)
(auto-save-visited-mode 1)

(use-package editorconfig)
(editorconfig-mode 1)

(ffap-bindings)
(electric-pair-mode 1)

(when (file-exists-p custom-file)
  (load-file custom-file))
