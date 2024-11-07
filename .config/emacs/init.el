(menu-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(scroll-bar-mode 0)

(let ((font "Iosevka-14"))
  (if (daemonp)
      (add-to-list 'default-frame-alist `(font . ,font))
    (set-frame-font font nil t)))

(load-theme 'zenburn t)

(require 'package)
(require 'use-package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents t)

(setq use-package-always-ensure t
      use-package-always-defer t)

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
  (setq make-backup-files nil
        create-lockfiles nil)
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

(with-eval-after-load 'shell
  (setq shell-kill-buffer-on-exit t))

(with-eval-after-load 'help
  (setq help-window-select t))

(with-eval-after-load 'man
  (setq Man-notify-method 'aggressive))

(use-package diminish)
(use-package delight)

(use-package no-littering
  :demand
  :custom
  (custom-file (no-littering-expand-var-file-name "custom.el")))

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
  (company-tooltip-flip-when-above t)
  (company-tooltip-align-annotations t)
  (company-tooltip-offset-display 'lines))

(use-package projectile
  :diminish
  :init
  (projectile-mode 1))

(use-package flycheck
  :init
  (global-flycheck-mode 1))

(use-package switch-window
  :custom
  (switch-window-background t))

(use-package sudo-edit)

(use-package editorconfig
  :init
  (editorconfig-mode 1))

(use-package move-text)
(use-package buffer-move)

(use-package rust-mode)
(use-package yaml-mode)
(use-package cmake-mode)

(use-package lsp-mode
  :init
  (dolist (mode '(c c++ rust java python))
    (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'lsp-deferred))
  :custom
  (lsp-auto-guess-root t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-code-actions-segments '(count))
  (lsp-keep-workspace-alive nil))

(use-package consult-lsp
  :after (consult lsp-mode))

(use-package yasnippet
  :after lsp-mode
  :hook
  (lsp-mode . yas-minor-mode))

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
  (dap-ui-repl-history-dir (no-littering-expand-var-file-name "dap/")))

(unless (package-installed-p 'app-launcher)
  (package-vc-install '(app-launcher :url "https://github.com/NumericalGandalf/app-launcher.git")))

(defun app-launcher ()
  "Create frame for `app-launcher-run-app'."
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
   "M-<tab>" #'company-complete
   "M-y" #'consult-yank-pop
   "C-s" #'consult-line
   "M-P" #'move-text-up
   "M-N" #'move-text-down)

  (general-define-key
   :prefix "C-x"
   "C-b" #'ibuffer
   "o" #'switch-window
   "O f" #'switch-window-then-find-file
   "O d" #'switch-window-then-dired
   "O k" #'switch-window-then-delete
   "O =" #'switch-window-then-swap-buffer
   "b" #'consult-buffer
   "u" #'sudo-edit-find-file
   "C-u" #'sudo-edit
   "4 b" #'consult-buffer-other-window
   "5 b" #'consult-buffer-other-frame
   "r b" #'consult-bookmark)

  (general-define-key
   :prefix "M-g"
   "j" #'flycheck-list-errors
   "k" #'consult-lsp-diagnostics
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

  (general-def company-active-map
    "<return>" nil
    "RET" nil
    "<tab>" #'company-complete-selection
    "TAB" #'company-complete-selection)

  (general-def projectile-mode-map
    "C-x p" #'projectile-command-map)

  (setq lsp-keymap-prefix "C-z")
  (general-def lsp-mode-map
    "C-." #'lsp-describe-thing-at-point
    "C-r" #'lsp-rename
    "M-." #'lsp-find-definition
    "M-?" #'lsp-find-references
    "C-5" #'dap-debug-last
    "C-6" #'dap-debug-restart
    "C-7" #'dap-continue
    "C-8" #'dap-breakpoint-toggle
    "C-9" #'dap-next
    "C-0" #'dap-step-in
    "C--" #'dap-step-out
    "C-=" #'dap-disconnect)
  (general-def lsp-mode-map
    :prefix lsp-keymap-prefix
    "C-." #'consult-lsp-symbols
    "C-;" #'consult-lsp-file-symbols
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
    "j =" #'lsp-java-generate-equals-and-hash-code
    "d e" #'dap-eval
    "d c" #'dap-breakpoint-condition
    "d h" #'dap-breakpoint-hit-condition
    "d l" #'dap-ui-breakpoints-list
    "d t" #'dap-switch-thread
    "d T" #'dap-stop-thread))

(dolist (mode '(prog conf))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'display-line-numbers-mode))

(column-number-mode 1)
(global-visual-line-mode 1)

(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)

(global-auto-revert-mode 1)
(auto-save-visited-mode 1)

(ffap-bindings)
(electric-pair-mode 1)

(when (file-exists-p custom-file)
  (load-file custom-file))
