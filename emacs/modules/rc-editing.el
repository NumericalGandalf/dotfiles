(setq ffap-require-prefix t)
(ffap-bindings)

(save-place-mode)

(setq-default auto-save-default nil)
(auto-save-visited-mode 1)

(setq make-backup-files nil
      create-lockfiles nil)

(setq global-auto-revert-non-file-buffers t
      auto-revert-remote-files t
      auto-revert-verbose nil)
(global-auto-revert-mode 1)

(defun rc-sudo-buffer (&optional prefix)
  "Open current buffer as root.
If PREFIX is non-nil, free current buffer from root."
  (interactive "P")
  (let* ((buf (current-buffer))
	 (file (or (buffer-file-name)
		   (when (derived-mode-p 'dired-mode)
		     default-directory)))
	 (match (string-match "^/sudo:" file)))
    (when file
      (if prefix
	  (when match
	    (find-file (tramp-file-local-name file))
	    (kill-buffer buf))
	(unless match
	  (find-file (concat "/sudo::" file))
	  (kill-buffer buf))))))

(defun rc-duplicate-line (&optional n)
  "Duplicate the current line N times."
  (interactive "p")
  (dotimes (_ n)
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank)))

(defun move-text-indent-after (&rest _)
  "Indent region after move text."
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(use-package move-text
  :config
  (global-set-key (kbd "M-P") 'move-text-up)
  (global-set-key (kbd "M-N") 'move-text-down)
  (advice-add 'move-text-up :after 'move-text-indent-after)
  (advice-add 'move-text-down :after 'move-text-indent-after))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/unmark-next-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/edit-lines))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

(provide 'rc-editing)
