(use-package helpful
  :general
  (:prefix "C-h"
           "f" 'helpful-callable
           "v" 'helpful-variable
           "k" 'helpful-key
           "C-." 'helpful-at-point))

(use-package projectile
  :general
  (projectile-mode-map
   "C-x p" 'projectile-command-map)
  :config
  (projectile-mode))

(use-package dired+
  :ensure
  (:host github :repo "emacsmirror/dired-plus")
  :after dired
  :custom
  (dired-listing-switches "-lah")
  (dired-free-space 'separate)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (diredp-hide-details-initially-flag nil))

(use-package files+
  :ensure
  (:host github :repo "emacsmirror/files-plus"))

(use-package ls-lisp+
  :ensure
  (:host github :repo "emacsmirror/ls-lisp-plus"))

(use-package thingatpt+
  :ensure
  (:host github :repo "emacsmirror/thingatpt-plus")
  :after thingatpt
  :config
  (tap-put-thing-at-point-props)
  (tap-redefine-std-fns))

(use-package replace+
  :ensure
  (:host github :repo "emacsmirror/replace-plus")
  :general
  ("C-c r q" 'query-replace-w-options))

(use-package shell-command-x
  :ensure
  (:host github :repo "elizagamedev/shell-command-x.el")
  :config
  (shell-command-x-mode))

(use-package shell-command+
  :general
  ("C-c r s" 'shell-command+))

(use-package info+
  :ensure
  (:host github :repo "emacsmirror/info-plus")
  :after info)

(use-package bookmark+
  :ensure
  (:host github :repo "emacsmirror/bookmark-plus"))

(provide 'enhancements)
