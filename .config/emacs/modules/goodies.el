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

(defun rc-sudo-buffer (&optional prefix)
  "Open current buffer as root.
If PREFIX is non-nil, free current buffer from root."
  (interactive "P")
  (let* ((buf (current-buffer))
	 (file (or (buffer-file-name)
		   (when (derived-mode-p 'dired-mode)
		     (expand-file-name default-directory))))
	 (match (string-match "^/sudo:" file)))
    (when file
      (if prefix
	  (when match
	    (find-file (tramp-file-local-name file))
	    (kill-buffer buf))
	(unless match
	  (find-file (concat "/sudo::" file))
	  (kill-buffer buf))))))

(global-set-key (kbd "C-r") 'rc-sudo-buffer)

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/unmark-next-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/edit-lines))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(provide 'goodies)
