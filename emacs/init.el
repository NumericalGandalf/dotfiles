(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(let ((font "Iosevka-14"))
  (set-face-attribute 'default nil :font font)
  (add-to-list 'default-frame-alist `(font . ,font)))

(require 'package)
(require 'use-package)

(setq package-user-dir (rc/cache "packages/")
      package-gnupghome-dir (rc/expand "gnupg/" package-user-dir))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents t)

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-hook-name-suffix nil)

(setq custom-file (rc/cache "custom.el")
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

(with-eval-after-load 'eglot
  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider
          :codeLensProvider
          :documentOnTypeFormattingProvider
          :colorProvider
          :foldingRangeProvider
          :inlayHintProvider)))

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

(use-package no-littering
  :demand
  :init
  (setq no-littering-etc-directory (rc/expand)
        no-littering-var-directory (rc/cache))
  :custom
  (server-auth-dir (rc/cache "server/")))

(use-package zenburn-theme
  :init
  (load-theme 'zenburn t)
  :config
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     `(fringe ((t (:inherit default))))
     `(mode-line ((t (:foreground ,zenburn-green+1 :background ,zenburn-bg-1))))
     `(mode-line-inactive ((t (:foreground ,zenburn-green-2 :background ,zenburn-bg-05))))
     `(vertical-border ((t (:foreground ,zenburn-fg-1))))
     `(line-number ((t (:inherit default :foreground ,zenburn-bg+3))))))
  (enable-theme 'zenburn))

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

(use-package embark
  :custom
  (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package cape
  :hook
  ((completion-at-point-functions . cape-dabbrev)
   (completion-at-point-functions . cape-file)))

(use-package corfu
  :defer 0
  :config
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

(use-package wgrep
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t))

(use-package sudo-edit)
(use-package multiple-cursors)
(use-package move-text)

(use-package rust-mode)
(use-package yaml-mode)
(use-package cmake-mode)

(use-package general
  :defer 0
  :config
  (general-define-key
   "C-S-N" #'next-buffer
   "C-S-P" #'previous-buffer
   "M-y" #'consult-yank-pop
   "C-s" #'consult-line
   "C-r" #'sudo-edit
   "C-S-r" #'sudo-edit-find-file
   "M-P" #'move-text-up
   "M-N" #'move-text-down
   "C->" #'mc/mark-next-like-this
   "C-<" #'mc/unmark-next-like-this)

  (general-define-key
   :prefix "C-x"
   "b" #'consult-buffer
   "4 b" #'consult-buffer-other-window
   "5 b" #'consult-buffer-other-frame
   "r b" #'consult-bookmark
   "C-b" #'ibuffer)

  (general-define-key
   :prefix "M-g"
   "e" #'consult-compile-error
   "o" #'consult-outline
   "i" #'consult-imenu)

  (general-define-key
   :prefix "C-c"
   "r f" #'consult-fd
   "r g" #'consult-ripgrep
   "r y" #'consult-git-grep
   "C->" #'mc/mark-all-like-this
   "C-<" #'mc/edit-lines
   "l s" #'eglot)

  (general-def minibuffer-local-map
    "C-." #'embark-export
    "C-;" #'embark-act)

  (general-def corfu-map
    "RET" nil)

  (general-def eglot-mode-map
    :prefix "C-c l"
    "d" #'eglot-find-declaration
    "t" #'eglot-find-typeDefinition
    "i" #'eglot-find-implementation
    "r" #'eglot-rename
    "f" #'eglot-format
    "a" #'eglot-code-actions
    "e" #'consult-flymake)

  (general-def vterm-mode-map
    "C-j" #'vterm-send-C-c))

(global-display-line-numbers-mode 1)
(column-number-mode 1)
(global-visual-line-mode 1)

(recentf-mode 1)
(savehist-mode 1)

(global-auto-revert-mode 1)
(auto-save-visited-mode 1)

(use-package editorconfig)
(editorconfig-mode 1)

(ffap-bindings)
(electric-pair-mode 1)

(dolist (file `(,(cond (rc/posix-p "posix.el")
                       (rc/mswin-p "mswin.el"))))
  (load (rc/expand file) t t))

(when (file-exists-p custom-file)
  (load-file custom-file))
