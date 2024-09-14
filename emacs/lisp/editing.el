(setq create-lockfiles nil)

(with-eval-after-load 'vc-hooks
  (setq vc-follow-symlinks t))

(with-eval-after-load 'dired
  (setq dired-listing-switches "-lah"
        dired-free-space 'separate
        dired-recursive-deletes 'always
        dired-dwim-target t
        dired-auto-revert-buffer t
        dired-clean-confirm-killing-deleted-buffers nil))

(with-eval-after-load 'files
  (setq-default auto-save-default nil)
  (setq make-backup-files nil)
  (auto-save-visited-mode 1))

(with-eval-after-load 'auth-source
  (setq auth-source-save-behavior nil))

(with-eval-after-load 'shell
  (setq shell-kill-buffer-on-exit t))

(with-eval-after-load 'autorevert
  (setq global-auto-revert-non-file-buffers t
        auto-revert-remote-files t
        auto-revert-verbose nil))

(use-package sudo-edit)

(use-package multiple-cursors)

(use-package buffer-move)

(use-package wgrep
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t))

(use-package move-text
  :config
  (defun move-text@indent-after (&rest _)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  (advice-add 'move-text-up :after 'move-text@indent-after)
  (advice-add 'move-text-down :after 'move-text@indent-after))

(global-auto-revert-mode)

(use-package editorconfig)
(editorconfig-mode)

(electric-pair-mode)

(ffap-bindings)
(use-package which-key)
(which-key-mode)

(save-place-mode)
(savehist-mode)

(provide 'editing)
