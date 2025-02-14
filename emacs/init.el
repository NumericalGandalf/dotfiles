(require 'server)
(require 'package)
(require 'use-package)

(defun font-apply ()
  "Apply font."
  (interactive)
  (let ((font "Iosevka-14"))
    (set-face-attribute 'default nil :font font)
    (set-face-attribute 'fixed-pitch nil :font font)
    (set-face-attribute 'fixed-pitch-serif nil :font font)
    (set-face-attribute 'variable-pitch nil :font font)))

(defun stow-dotfiles (&optional prefix dir)
  "Stow dotfiles."
  (interactive "P\ni")
  (dolist (target (cdr (cdr (directory-files
                             (expand-dot-file (or dir "./")) t))))
    (if (or (file-regular-p target)
            (cl-dolist (parent '(".config/"))
              (let ((parent (expand-dot-file parent)))
                (when (and (file-in-directory-p target parent)
                           (not (file-equal-p target parent)))
                  (cl-return target)))))
        (let* ((rel-name (file-relative-name (expand-dot-file target)
                                             (expand-dot-file "./")))
               (link-name (expand-file-name rel-name "~/")))
          (when (file-exists-p link-name)
            (cond ((file-symlink-p link-name)
                   (delete-file link-name))
                  ((file-directory-p link-name)
                   (delete-directory link-name t))
                  (t (delete-file link-name))))
          (unless prefix
            (make-symbolic-link target link-name)))
      (stow-dotfiles prefix target))))

(defun compile-this ()
  "Either `project-compile' or `compile'."
  (interactive)
  (if (project-current)
      (project-compile)
    (call-interactively (compile))))

(defun line-numbers-here ()
  "Enable (maybe) line numbers in current major mode."
  (when (derived-mode-p 'prog-mode 'conf-mode)
    (display-line-numbers-mode 1)))

(defun treesit-install-grammars ()
  "Install tree-sitter language grammmars."
  (interactive)
  (let ((outdir (nth 0 treesit-extra-load-path)))
    (dolist (source (treesit-auto--build-treesit-source-alist))
      (let ((grammar (nth 0 source)))
        (unless (or (treesit-ready-p grammar t)
                    (member grammar '(janet latex markdown)))
          (message "Installing tree-sitter language grammer: %s" grammar)
          (let ((inhibit-message t)
                (message-log-max nil))
            (apply #'treesit--install-language-grammar-1 outdir source)))))))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'font-apply)
  (font-apply))

(setq use-short-answers t
      use-dialog-box nil
      use-file-dialog nil
      inhibit-startup-screen t
      resize-mini-windows t
      mouse-autoselect-window t

      scroll-step 1
      scroll-margin 4
      scroll-preserve-screen-position t

      mode-line-percent-position '(6 "%q")

      xref-auto-jump-to-first-definition t
      xref-auto-jump-to-first-xref t
      xref-file-name-display 'abs
      xref-prompt-for-identifier nil

      compilation-ask-about-save nil
      compile-command nil
      compilation-auto-jump-to-first-error t

      search-upper-case t
      isearch-repeat-on-direction-change t
      isearch-allow-scroll 'unlimited

      c-default-style '((other . "user"))

      enable-recursive-minibuffers t
      completions-detailed t
      read-file-name-completion-ignore-case t
      confirm-nonexistent-file-or-buffer nil

      dired-listing-switches "-lah"
      dired-free-space 'separate
      dired-recursive-deletes 'always
      dired-dwim-target t
      dired-auto-revert-buffer t
      dired-clean-confirm-killing-deleted-buffers nil

      display-line-numbers-width-start t
      display-line-numbers-grow-only t
      display-line-numbers-type 'relative

      backup-directory-alist `(("." . ,(expand-cache-file "backups/")))
      delete-old-versions t
      kept-old-versions 0
      kept-new-versions 5
      backup-by-copying t

      find-file-visit-truename t
      vc-follow-symlinks t

      auth-source-save-behavior nil
      server-auth-dir (expand-cache-file "server/")

      auto-revert-verbose nil
      global-auto-revert-non-file-buffers t
      auto-revert-remote-files t

      Man-notify-method 'aggresive
      help-window-select t

      package-user-dir (expand-cache-file "packages/")
      package-gnupghome-dir (expand-file-name "gnupg/" package-user-dir)

      use-package-always-ensure t
      use-package-always-defer (not (daemonp))
      use-package-verbose init-file-debug
      use-package-compute-statistics use-package-verbose)

(setq-default tab-width 4
              indent-tabs-mode nil)

(add-to-list 'after-change-major-mode-hook #'line-numbers-here)

(add-to-list 'treesit-extra-load-path (expand-cache-file "tree-sitter/"))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents t)

(use-package diminish)
(use-package delight)

(use-package doom-themes
  :init
  (load-theme 'doom-tomorrow-night t)
  :custom
  (doom-themes-enable-bold nil))

(use-package no-littering
  :demand
  :preface
  (setq no-littering-var-directory (expand-cache-file "./")
        no-littering-etc-directory no-littering-var-directory))

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
  :config
  (add-to-list 'consult-preview-allowed-hooks #'line-numbers-here)
  :custom
  (consult-line-start-from-top t)
  (consult-find-args "find .")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package magit
  :init
  (setq magit-auto-revert-mode nil))

(use-package company
  :diminish
  :init
  (global-company-mode 1)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0)
  (company-format-margin-function #'company-text-icons-margin)
  (company-tooltip-flip-when-above t)
  (company-tooltip-align-annotations t)
  (company-tooltip-scrollbar-width 0))

(use-package projectile
  :diminish
  :init
  (projectile-mode 1))

(use-package switch-window
  :custom
  (switch-window-background t))

(use-package multiple-cursors)
(use-package move-text)

(use-package sudo-edit)
(use-package editorconfig)

(use-package lsp-mode
  :diminish
  :hook
  ((c-ts-mode c++-ts-mode java-ts-mode) . lsp)
  :custom
  (lsp-enable-symbol-highlighting nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil)
  (lsp-enable-snippet nil)
  (lsp-enable-folding nil)
  (lsp-auto-guess-root t)
  (lsp-modeline-code-actions-segments '(count)))

(use-package lsp-java
  :after lsp-mode)

(use-package flycheck
  :after lsp-mode)

(use-package consult-lsp
  :after (consult lsp-mode))

(use-package meson-mode)

(use-package treesit-auto
  :demand
  :config
  (global-treesit-auto-mode 1))

(use-package vterm
  :config
  (setq vterm-timer-delay nil)
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=Off")
  (vterm-always-compile-module t)
  (vterm-max-scrollback 10000)
  (vterm-clear-scrollback-when-clearing t))

(use-package general
  :init
  (ffap-bindings)
  
  (general-define-key
   "C-\\" #'compile-this
   "C-<tab>" #'align-regexp
   "M-<tab>" #'company-complete
   "M-y" #'consult-yank-pop
   "C-s" #'consult-line
   "M-P" #'move-text-up
   "M-N" #'move-text-down
   "C->" #'mc/mark-next-like-this
   "C-<" #'mc/unmark-next-like-this)

  (general-define-key
   :prefix "C-x"
   "C-b" #'ibuffer-other-window
   "o" #'switch-window
   "O" #'switch-window-then-swap-buffer
   "!" #'shell-command
   "&" #'async-shell-command
   "b" #'consult-buffer
   "4 b" #'consult-buffer-other-window
   "5 b" #'consult-buffer-other-frame
   "r b" #'consult-bookmark)

  (general-define-key
   :prefix "M-g"
   "o" #'consult-outline
   "i" #'consult-imenu)

  (general-define-key
   :prefix "C-c"
   "f" #'consult-find
   "g" #'consult-grep
   "y" #'consult-git-grep
   "q" #'query-replace-regexp
   "C->" #'mc/mark-all-like-this
   "C-<" #'mc/edit-lines)

  (general-def minibuffer-local-map
    "C-," #'embark-act
    "C-." #'embark-export)

  (general-def company-active-map
    "<return>" nil
    "RET" nil
    "<tab>" #'company-complete-selection
    "TAB" #'company-complete-selection)

  (general-def projectile-mode-map
    "C-x p" #'projectile-command-map)

  (general-def vterm-mode-map
    "C-j" (lambda ()
            (interactive)
            (vterm-send "C-c")))

  (general-def lsp-mode-map
    "C-." #'lsp-rename
    "C-," #'consult-lsp-symbols
    "C-|" #'flycheck-list-errors
    "M-g i" #'consult-lsp-file-symbols
    "M-." #'lsp-find-definition
    "M-?" #'lsp-find-references
    "C-h ." #'lsp-describe-thing-at-point))

(editorconfig-mode 1)
(electric-pair-mode 1)

(global-visual-line-mode 1)
(column-number-mode 1)

(global-auto-revert-mode 1)

(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)

(let ((file (locate-user-emacs-file "app-launcher.el")))
  (autoload #'app-launcher-run-app file t)
  (autoload #'app-launcher-frame file t))

(setq custom-file (expand-cache-file "custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))

(unless (or (daemonp)
            (server-running-p))
  (server-start))
