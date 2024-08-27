(ffap-bindings)
(save-place-mode)

(setq vc-follow-symlinks t)

(setq-default auto-save-default nil)
(auto-save-visited-mode 1)

(setq make-backup-files nil
      create-lockfiles nil)

(setq auth-source-save-behavior nil)

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

(pixel-scroll-mode 1)
(pixel-scroll-precision-mode 1)
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

(use-package which-key
  :config
  (which-key-mode)
  :custom
  (which-key-idle-delay 1.5))

(use-package editorconfig
  :config
  (editorconfig-mode))

(use-package sudo-edit
  :if rc-posix-p
  :general
  ("C-r" 'sudo-edit
   "C-S-r" 'sudo-edit-find-file))

(use-package move-text
  :general
  ("M-P" 'move-text-up
   "M-N" 'move-text-down))

(use-package multiple-cursors
  :general
  ("C->" 'mc/mark-next-like-this
   "C-<" 'mc/unmark-next-like-this)
  (:prefix "C-c"
           "C->" 'mc/mark-all-like-this
           "C-<" 'mc/edit-lines))

(use-package wgrep
  :defer
  :custom
  (wgrep-auto-save-buffer t))

(use-package buffer-move
  :general
  (:prefix "C-c w"
           "h" 'buf-move-left
           "j" 'buf-move-down
           "k" 'buf-move-up
           "l" 'buf-move-right))

(general-define-key
 "C-S-N" 'next-buffer
 "C-S-P" 'previous-buffer
 "C-x C-b" 'ibuffer-other-window
 "C-c b r" 'revert-buffer-quick)

(general-define-key
 :prefix "C-c f"
 "r" 'recentf
 "l" 'find-library)

(provide 'editing)
