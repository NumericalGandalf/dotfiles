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
(global-display-line-numbers-mode)

(dolist (mode '(image))
  (add-hook
   (intern (concat (symbol-name mode) "-mode-hook"))
   (lambda () (display-line-numbers-mode 0))))

(column-number-mode)
(global-visual-line-mode)

(setq scroll-step 1
      scroll-preserve-screen-position t)

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

(use-package sudo-edit)

(use-package move-text)

(use-package multiple-cursors)

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))
