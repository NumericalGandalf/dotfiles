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

(use-package which-key
  :hook
  (lsp-mode-hook . lsp-enable-which-key-integration)
  :init
  (which-key-mode)
  :custom
  (which-key-idle-delay 1.5))

(use-package editorconfig
  :config
  (editorconfig-mode))

(use-package sudo-edit
  :general
  ("C-r" 'sudo-edit
   "C-S-r" 'sudo-edit-find-file))

(use-package move-text
  :general
  ("M-P" 'move-text-up
   "M-N" 'move-text-down)
  :config
  (defun move-text@indent-after (&rest res)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate))
    res)
  
  (advice-add 'move-text-up :after 'move-text@indent-after)
  (advice-add 'move-text-down :after 'move-text@indent-after))

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
  (wgrep-enable-key "r")
  (wgrep-auto-save-buffer t))

(use-package buffer-move
  :general
  (:prefix "C-c w"
           "h" 'buf-move-left
           "j" 'buf-move-down
           "k" 'buf-move-up
           "l" 'buf-move-right))

(provide 'editing)
