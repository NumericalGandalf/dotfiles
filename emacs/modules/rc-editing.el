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

(setq display-line-numbers-width-start t
      display-line-numbers-grow-only t
      display-line-numbers-type 'relative)

(dolist (mode '(conf text prog dired vterm helpful))
  (add-hook
   (intern (concat (symbol-name mode) "-mode-hook"))
   'display-line-numbers-mode))

(column-number-mode)
(global-visual-line-mode)

(setq scroll-step 1
      scroll-preserve-screen-position t)

(defun rc-sudo-buffer (&optional prefix)
  "Open current buffer as root.
If current buffer is already opened as root,
open buffer as normal user again.
If PREFIX is non-nil, do not kill current buffer."
  (interactive "P")
  (when-let ((buf (current-buffer))
	     (file (or (buffer-file-name)
		       (when (derived-mode-p 'dired-mode)
                         (rc-expand default-directory)))))
    (save-buffer)
    (if (string-match "^/sudo:" file)
        (find-file (tramp-file-local-name file))
      (find-file (concat "/sudo::" file)))
    (unless prefix
      (kill-buffer buf))))

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
  (advice-add 'move-text-up :after 'move-text-indent-after)
  (advice-add 'move-text-down :after 'move-text-indent-after))

(use-package multiple-cursors)

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

(provide 'rc-editing)
