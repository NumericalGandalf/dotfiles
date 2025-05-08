(require 'server)
(require 'package)
(require 'use-package)

(setq use-short-answers t
      use-dialog-box nil
      use-file-dialog nil
      inhibit-startup-screen t

      split-width-threshold nil
      split-height-threshold 0
      
      ffap-require-prefix t
      confirm-nonexistent-file-or-buffer nil
      
      ibuffer-use-other-window t
      Man-notify-method 'aggressive

      scroll-step 1
      scroll-margin 4
      scroll-preserve-screen-position t

      mode-line-percent-position '(6 "%q")
      eldoc-echo-area-use-multiline-p nil

      xref-file-name-display 'abs
      xref-prompt-for-identifier nil

      compile-command nil
      compilation-ask-about-save nil

      find-file-visit-truename t
      vc-follow-symlinks t
      
      auth-source-save-behavior nil

      c-default-style '((other . "user"))
      c-ts-mode-indent-offset 4

      dired-dwim-target t
      dired-listing-switches "-lah"
      dired-free-space 'separate
      dired-recursive-deletes 'always
      dired-auto-revert-buffer t

      display-line-numbers-width-start t
      display-line-numbers-grow-only t
      display-line-numbers-type 'relative

      backup-directory-alist `(("." . ,(rc/expand-cache-file "backups/")))
      delete-old-versions t
      kept-old-versions 0
      kept-new-versions 5
      backup-by-copying t
      
      eglot-report-progress nil
      eglot-extend-to-xref t
      eglot-ignored-server-capabilities '(:documentHighlightProvider
                                          :codeLensProvider
                                          :documentOnTypeFormattingProvider
                                          :foldingRangeProvider
                                          :inlayHintProvider)

      package-user-dir (rc/expand-cache-file "packages/")
      package-gnupghome-dir (expand-file-name "gnupg/" package-user-dir)

      use-package-always-ensure t
      use-package-always-defer (not (daemonp))
      use-package-verbose t)

(setq-default tab-width 4
              indent-tabs-mode nil)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents t)

(use-package no-littering
  :demand
  :preface
  (setq no-littering-var-directory (rc/expand-cache-file))
  :custom
  (custom-file (rc/expand-cache-lisp-file "custom.el"))
  (server-auth-dir (rc/expand-cache-file "server/")))

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
  (vertico-resize t)
  (vertico-cycle t)
  (vertico-preselect 'no-prompt))

(use-package embark
  :bind
  (:map minibuffer-local-map
   ("C-," . embark-act)
   ("C-." . embark-export)))

(use-package consult
  :bind
  (("C-x M-:" . consult-complex-command)
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x t b" . consult-buffer-other-tab)
   ("C-x p b" . consult-project-buffer)
   ("C-x r b" . consult-bookmark)
   ("C-s" . consult-line)
   ("M-y" . consult-yank-pop)
   ("M-g g" . consult-goto-line)
   ("C-c f" . consult-find)
   ("C-c g" . consult-grep)
   ("C-c G" . consult-git-grep)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))
  :config
  (add-to-list 'consult-preview-allowed-hooks #'rc/line-numbers-here)
  :custom
  (consult-line-start-from-top t)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-eglot
  :after consult
  :bind
  (:map eglot-mode-map
   ("C-c l i" . flymake-show-project-diagnostics)
   ("C-c l I" . flymake-show-buffer-diagnostics)
   ("C-c l S" . elgot-shutdown)
   ("C-c l f" . eglot-format)
   ("C-c l r" . eglot-rename)
   ("C-c l a" . eglot-code-actions)
   ("C-c l t" . eglot-find-typeDefinition)
   ("C-c l p" . consult-eglot-symbols)))

(use-package consult-eglot-embark
  :after (embark-consult consult-eglot)
  :init
  (consult-eglot-embark-mode 1))

(use-package company
  :hook
  ((emacs-lisp-mode eglot-mode) . company-mode)
  :bind
  (:map company-mode-map
   ("M-<tab>" . #'company-complete)
   :map company-active-map
   ("<return>" . nil)
   ("RET" . nil)
   ("<tab>" . #'company-complete-selection)
   ("TAB" . #'company-complete-selection))
  :custom
  (company-idle-delay 0)
  (company-frontends '(company-pseudo-tooltip-frontend))
  (company-format-margin-function #'company-text-icons-margin)
  (company-tooltip-flip-when-above t)
  (company-tooltip-align-annotations t)
  (company-tooltip-scrollbar-width 0))

(use-package treesit-auto
  :demand
  :config 
  (global-treesit-auto-mode 1))

(use-package vterm
  :bind
  (:map vterm-mode-map
   ("C-j" . (lambda ()
              (interactive)
              (vterm-send "C-c"))))
  :config
  (setq vterm-timer-delay nil)
  (when-let* ((_ (string-match-p "bash" vterm-shell))
              (file (locate-user-emacs-file "vterm.sh"))
              (args (format "--init-file %s" file)))
    (setq vterm-shell (format "%s %s" vterm-shell args)))
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=Off")
  (vterm-always-compile-module t)
  (vterm-clear-scrollback-when-clearing t)
  (vterm-max-scrollback 50000))

(use-package magit)

(use-package move-text
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down)))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c ." . mc/unmark-next-like-this)
   ("C-c ," . mc/unmark-next-previous-this)
   ("C-c >" . mc/skip-to-next-like-this)
   ("C-c <" . mc/skip-to-previous-like-this)
   ("C-c C->" . mc/edit-lines)
   ("C-c C-<" . mc/mark-all-like-this)))

(use-package switch-window
  :bind
  ("C-x o" . switch-window)
  :custom
  (switch-window-background t))

(use-package sudo-edit)

(use-package meson-mode)

(add-to-list 'after-change-major-mode-hook #'rc/line-numbers-here)

(bind-key "C-x C-b" #'ibuffer)
(bind-key "C-z" #'duplicate-line)

(column-number-mode 1)
(editorconfig-mode 1)

(ffap-bindings)
(electric-pair-mode 1)

(when (daemonp)
  (mapcar #'load-file (nthcdr 2 (directory-files (rc/expand-lisp-file) t))))

(when (file-exists-p (rc/expand-cache-lisp-file))
  (mapcar #'load-file (nthcdr 2 (directory-files (rc/expand-cache-lisp-file) t))))

(unless (or (daemonp)
            (server-running-p))
  (server-start))
