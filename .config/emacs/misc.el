(setq use-short-answers t)

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

(setq backup-directory-alist `(("." . ,(locate-user-emacs-file "var/backups/")))
      backup-by-copying t
      version-control t
      delete-old-versions t)

(setq global-auto-revert-non-file-buffers t
      auto-revert-remote-files t)
(global-auto-revert-mode)

(defun rc-duplicate-line(&optional n)
  "Duplicates the current line N times."
  (interactive "p")
  (dotimes (_ n)
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank)))

(global-set-key (kbd "C-q") 'rc-duplicate-line)

(defun rc-indent-after-move-text-advice (&rest _)
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
  (advice-add 'move-text-up :after 'rc-indent-after-move-text-advice)
  (advice-add 'move-text-down :after 'rc-indent-after-move-text-advice))
