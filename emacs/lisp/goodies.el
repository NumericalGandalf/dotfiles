(setq use-short-answers t
      vc-follow-symlinks t)

(setq display-line-numbers-type 'relative)
(column-number-mode)
(global-display-line-numbers-mode)
(global-visual-line-mode)

(save-place-mode)
(savehist-mode)

(setq ffap-require-prefix t)
(ffap-bindings)

(require 'dired-x)
(setq dired-listing-switches "-lah"
      dired-free-space 'separate
      dired-recursive-deletes 'always
      dired-dwim-target t
      dired-auto-revert-buffer t)

(setq make-backup-files nil
      create-lockfiles nil)

(setq global-auto-revert-non-file-buffers t
      auto-revert-remote-files t
      auto-revert-verbose nil)
(global-auto-revert-mode 1)

(setq-default auto-save-default nil)
(auto-save-visited-mode 1)

(setq auth-source-save-behavior nil)

(defun move-text-indent-after-advice (&rest _)
  "Advice for indenting region after move text function."
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(use-package move-text
  :config
  (global-set-key (kbd "M-P") 'move-text-up)
  (global-set-key (kbd "M-N") 'move-text-down)
  (advice-add 'move-text-up :after 'move-text-indent-after-advice)
  (advice-add 'move-text-down :after 'move-text-indent-after-advice))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/unmark-next-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/edit-lines))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

(provide 'goodies)
