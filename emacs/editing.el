(setq use-short-answers t
      inhibit-startup-screen t
      ring-bell-function #'ignore
      echo-keystrokes 0)

(setq scroll-step 1
      scroll-margin 4
      scroll-preserve-screen-position t)

(setq create-lockfiles nil)

(setq mode-line-percent-position '(6 "%q"))

(with-eval-after-load 'simple
  (setq suggest-key-bindings nil))

(with-eval-after-load 'server
  (setq server-client-instructions nil))

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
        save-silently t)
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

(use-package sudo-edit)

(use-package multiple-cursors)

(use-package buffer-move)

(use-package wgrep
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t))

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

;; (defun duplicate-region (&optional n)
;;   "Duplicates")

(unless (daemonp)
  (defun display-startup-echo-area-message ()))

(global-display-line-numbers-mode)
(column-number-mode)

(global-visual-line-mode)
(diminish 'visual-line-mode)

(pixel-scroll-mode)
(pixel-scroll-precision-mode)

(save-place-mode)
(savehist-mode)
(global-auto-revert-mode)

(editorconfig-mode)
(diminish 'editorconfig-mode)

(ffap-bindings)
(electric-pair-mode)

(which-key-mode)
(diminish 'which-key-mode)
