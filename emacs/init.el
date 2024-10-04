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

(setq scroll-step 1
      scroll-margin 4
      scroll-preserve-screen-position t)

(setq resize-mini-windows t
      enable-recursive-minibuffers t
      completion-ignore-case  t
      read-buffer-completion-ignore-case t
      tab-always-indent 'complete)

(setq mode-line-percent-position '(6 "%q"))

(setq-default tab-width 4)

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

(with-eval-after-load 'simple
  (setq suggest-key-bindings nil))

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
  (auto-save-visited-mode 1))

(with-eval-after-load 'auth-source
  (setq auth-source-save-behavior nil))

(with-eval-after-load 'shell
  (setq shell-kill-buffer-on-exit t))

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

(use-package diminish)

(use-package orderless
  :init
  (setq completion-styles '(orderless))
  :custom
  (completion-category-defaults nil))

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

(use-package sudo-edit)

(use-package multiple-cursors)

(use-package buffer-move)

(use-package move-text
  :config
  (defun move-text@indent (&rest _)
    "Indent region after text moved."
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  (advice-add 'move-text-up :after #'move-text@indent)
  (advice-add 'move-text-down :after #'move-text@indent))

(use-package wgrep
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t))

(defun rc/temp (&optional unique file)
  "Expand (FILE) from a (UNIQUE) temp directory."
  (let ((dir (rc/expand "emacs/" (temporary-file-directory))))
    (when unique
      (let ((key (int-to-string (time-to-seconds))))
        (when rc/posix-p
          (setq key (format "%d.%s" (user-uid) key)))
        (setq dir (rc/expand (concat key "/") dir))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (rc/expand file dir)))

(defun rc/script (script)
  "Execute SCRIPT located in the dotfiles `scripts' directory.
If GET-ONLY is non-nil, only return the scripts path."
  (let* ((path (rc/expand script (rc/expand "../scripts/")))
         (prefix (pcase (file-name-extension path)
                   ("sh" "bash")
                   ("ps1" "powershell"))))
    (call-process-shell-command (format "%s %s" prefix path))))

(defmacro rc/dot (file)
  "Expand FILE from the dotfiles `dots' directory."
  `(rc/expand ,file (rc/expand "../dots/")))

(defmacro rc/load (file)
  "Load user config file FEATURE."
  `(load (rc/expand (symbol-name ,file)) t t))

(cond (rc/posix-p (rc/load 'posix))
      (rc/mswin-p (rc/load 'mswin)))

(rc/load 'nerd-fonts)
(rc/load 'keybindings)

(global-display-line-numbers-mode 1)
(column-number-mode 1)

(global-visual-line-mode 1)
(diminish 'visual-line-mode)

(pixel-scroll-mode 1)
(pixel-scroll-precision-mode 1)

(save-place-mode 1)
(savehist-mode 1)

(global-auto-revert-mode 1)

(editorconfig-mode 1)
(diminish 'editorconfig-mode)

(ffap-bindings)
(electric-pair-mode 1)

(which-key-mode 1)
(diminish 'which-key-mode)

(load custom-file t t)
