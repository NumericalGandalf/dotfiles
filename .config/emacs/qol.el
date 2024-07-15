(setq use-short-answers t)

(setq display-line-numbers-type 'relative)
(column-number-mode)
(global-display-line-numbers-mode)
(global-visual-line-mode)
(save-place-mode)

(setq ffap-require-prefix t)
(ffap-bindings)

(setq dired-listing-switches "-lah"
      dired-free-space 'separate
      dired-recursive-deletes 'always
      dired-dwim-target t
      dired-auto-revert-buffer t)
(require 'dired-x)

(setq backup-directory-alist `(("." . ,(locate-user-emacs-file "var/backups/")))
      backup-by-copying t
      version-control t
      delete-old-versions t)

(setq global-auto-revert-non-file-buffers t
      auto-revert-remote-files t)
(global-auto-revert-mode)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(global-set-key (kbd "C-q") 'duplicate-line)

(use-package move-text
  :config
  (global-set-key (kbd "M-P") 'move-text-up)
  (global-set-key (kbd "M-N") 'move-text-down))
