(defun font-apply ()
  "Apply font."
  (interactive)
  (let ((font "Iosevka-14"))
    (set-face-attribute 'default nil :font font)
    (set-face-attribute 'fixed-pitch nil :font font)
    (set-face-attribute 'fixed-pitch-serif nil :font font)
    (set-face-attribute 'variable-pitch nil :font font)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'font-apply)
  (font-apply))

(load-theme 'gruvbox t)

(setq use-short-answers t
      use-dialog-box nil
      use-file-dialog nil
      inhibit-startup-screen t
      resize-mini-windows t
      mouse-autoselect-window t)

(setq scroll-step 1
      scroll-margin 4
      scroll-preserve-screen-position t)

(setq mode-line-percent-position '(6 "%q"))

(with-eval-after-load 'compile
  (setq compilation-ask-about-save nil
        compile-command nil
        compilation-auto-jump-to-first-error t))

(with-eval-after-load 'simple
  (setq-default indent-tabs-mode nil))

(with-eval-after-load 'c-ts-mode
  (setq c-ts-mode-indent-offset 4))

(with-eval-after-load 'minibuffer
  (setq enable-recursive-minibuffers t
        completions-detailed t
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
  (setq make-backup-files nil
        create-lockfiles nil)
  (setq-default auto-save-default nil))

(with-eval-after-load 'treesit
  (add-to-list 'treesit-extra-load-path (expand-cache-file "tree-sitter/")))

(with-eval-after-load 'auth-source
  (setq auth-source-save-behavior nil))

(with-eval-after-load 'autorevert
  (setq global-auto-revert-non-file-buffers t
        auto-revert-remote-files t
        auto-revert-verbose nil))

(with-eval-after-load 'server
  (setq server-auth-dir (expand-cache-file "server/")))

(with-eval-after-load 'ibuffer
  (setq ibuffer-use-other-window t))

(with-eval-after-load 'shell
  (setq shell-kill-buffer-on-exit t))

(with-eval-after-load 'help
  (setq help-window-select t))

(with-eval-after-load 'man
  (setq Man-notify-method 'aggressive))

(require 'package)
(require 'use-package)

(setq package-user-dir (expand-cache-file "packages/")
      package-gnupghome-dir (expand-file-name "gnupg/" package-user-dir))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents t)

(setq use-package-always-ensure t
      use-package-always-defer t)

(when init-file-debug
  (setq use-package-compute-statistics t
        use-package-verbose t))

(use-package diminish)

(use-package no-littering
  :demand
  :preface
  (setq no-littering-var-directory "~/.cache/emacs/"
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
  :custom
  (consult-line-start-from-top t)
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
  (company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
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

(use-package sudo-edit)

(use-package multiple-cursors)

(use-package editorconfig
  :init
  (editorconfig-mode 1))

(use-package treesit-auto
  :demand
  :config
  (global-treesit-auto-mode 1)

  (defun treesit-ensure-all (&optional prefix)
    "Ensure all available tree-sitter libraries.
If optional PREFIX is non-nil, force all to build."
    (interactive "P")
    (let ((outdir (nth 0 treesit-extra-load-path)))
      (when prefix (delete-directory outdir t))
      (dolist (source (treesit-auto--build-treesit-source-alist))
        (let ((lang (nth 0 source)))
          (unless (or (treesit-ready-p lang t) (member lang '(janet latex markdown)))
            (message "Building tree-sitter library for language: %s" lang)
            (let ((inhibit-message t)
                  (message-log-max nil))
              (apply #'treesit--install-language-grammar-1 outdir source))))))))

(use-package wgrep
  :custom
  (wgrep-enable-key "e"))

(use-package move-text)

(use-package meson-mode)

(use-package lsp-mode
  :hook
  (prog-mode . lsp-maybe)
  :init
  (defun lsp-maybe ()
    "Maybe run `lsp'."
    (interactive)
    (unless (derived-mode-p 'emacs-lisp-mode) (lsp)))

  :custom
  (lsp-auto-guess-root t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil)
  (lsp-modeline-code-actions-segments '(count))
  (lsp-keep-workspace-alive nil)
  (lsp-warn-no-matched-clients nil))

(use-package flycheck
  :after lsp-mode)

(use-package yasnippet
  :after lsp-mode
  :hook
  (lsp-mode . yas-minor-mode))

(use-package consult-lsp
  :after (consult lsp-mode))

(use-package lsp-java
  :after lsp-mode)

(use-package dap-mode
  :after lsp-mode
  :hook
  ((dap-terminated . dap-hide-output-win)
   (dap-session-created . (lambda (_) (dap-delete-all-sessions))))
  :config
  (with-eval-after-load 'dap-ui
    (setq dap-ui-buffer-configurations
          `((,dap-ui--breakpoints-buffer . ((side . right) (slot . 0) (window-width . 0.2)))
            (,dap-ui--locals-buffer . ((side . right) (slot . 1)))
            (,dap-ui--expressions-buffer . ((side . right) (slot . 2)))
            (,dap-ui--sessions-buffer . ((side . right) (slot . 3)))
            (,dap-ui--repl-buffer . ((side . bottom) (slot . 0) (window-height . 0.2))))))

  (defun dap-go-to-output-buffer (&optional no-select)
    "Go to output buffer."
    (interactive)
    (let* ((buf (dap--debug-session-output-buffer (dap--cur-session-or-die)))
           (win (display-buffer-in-side-window buf `((side . bottom) (slot . -1)))))
      (unless no-select (select-window win))))
 
  (defun dap-hide-output-win (session)
    "Hide output-buffer window after `session' termination."
    (when-let* ((buf (dap--debug-session-output-buffer session))
                (win (get-buffer-window buf)))
      (delete-window win)))
  
  :custom
  (dap-auto-configure-features '(breakpoints locals expressions repl))
  (dap-ui-repl-history-dir (expand-cache-file "dap/")))

(use-package vterm
  :config
  (setq vterm-timer-delay nil)
  :custom
  (vterm-max-scrollback 10000)
  (vterm-clear-scrollback-when-clearing t)
  (vterm-always-compile-module t))

(use-package general
  :init
  (general-define-key
   "C-<tab>" #'fill-region
   "M-<tab>" #'company-complete
   "M-y" #'consult-yank-pop
   "C-s" #'consult-line
   "M-P" #'move-text-up
   "M-N" #'move-text-down
   "C->" #'mc/mark-next-like-this
   "C-<" #'mc/unmark-next-like-this)

  (general-define-key
   :prefix "C-x"
   "C-b" #'ibuffer
   "C-c" #'save-buffers-kill-emacs
   "o" #'switch-window
   "O" #'switch-window-then-swap-buffer
   "b" #'consult-buffer
   "u" #'sudo-edit-find-file
   "C-u" #'sudo-edit
   "!" #'shell-command
   "&" #'async-shell-command
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
    "C-j" (lambda () (interactive) (vterm-send "C-c")))

  (setq lsp-keymap-prefix "C-z")
  (general-def lsp-mode-map
    "C-." #'lsp-describe-thing-at-point
    "C-;" #'lsp-rename
    "M-." #'lsp-find-definition
    "M-?" #'lsp-find-references
    "M-g j" #'flycheck-list-errors
    "M-g k" #'consult-lsp-diagnostics)
  (general-def lsp-mode-map
    :prefix lsp-keymap-prefix
    "C-," #'consult-lsp-symbols
    "C-." #'consult-lsp-file-symbols
    "j r" #'dap-java-debug
    "j m" #'dap-java-run-test-method
    "j M" #'dap-java-run-test-class
    "j e" #'lsp-java-extract-method
    "j t" #'lsp-java-open-super-implementation
    "j i" #'lsp-java-organize-imports
    "j h" #'lsp-java-type-hierarchy
    "j o" #'lsp-java-generate-overrides
    "j s" #'lsp-java-generate-to-string
    "j g" #'lsp-java-generate-getters-and-setters
    "j =" #'lsp-java-generate-equals-and-hash-code)

  (dolist (map '(lsp-mode-map dap-mode-map))
    (general-define-key
     :keymaps map
     "C-5" #'dap-debug-last
     "C-6" #'dap-debug-restart
     "C-7" #'dap-continue
     "C-8" #'dap-breakpoint-toggle
     "C-9" #'dap-next
     "C-0" #'dap-step-in
     "C--" #'dap-step-out
     "C-=" #'dap-disconnect)
    (general-define-key
     :keymaps map
     :prefix lsp-keymap-prefix
     "d e" #'dap-eval
     "d c" #'dap-breakpoint-condition
     "d h" #'dap-breakpoint-hit-condition
     "d l" #'dap-ui-breakpoints-list
     "d t" #'dap-switch-thread
     "d T" #'dap-stop-thread)))

(let ((file (locate-user-emacs-file "app-launcher.el")))
  (autoload #'app-launcher-run-app file t)
  (autoload #'app-launcher-frame file t))

(global-display-line-numbers-mode 1)
(global-visual-line-mode 1)
(column-number-mode 1)

(desktop-save-mode 1)
(global-auto-revert-mode 1)
(auto-save-visited-mode 1)

(ffap-bindings)
(electric-pair-mode 1)

(setq custom-file (expand-cache-file "custom.el"))
(when (file-exists-p custom-file) (load-file custom-file))

(require 'server)
(unless (or (daemonp) (server-running-p)) (server-start))
