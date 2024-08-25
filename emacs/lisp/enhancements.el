(use-package helpful)

(use-package projectile
  :init
  (projectile-mode))

(use-package dired+
  :ensure
  (:fetcher github :repo "emacsmirror/dired-plus")
  :after dired
  :demand
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
  (:fetcher github :repo "emacsmirror/files-plus"))

(use-package ls-lisp+
  :ensure
  (:fetcher github :repo "emacsmirror/ls-lisp-plus")
  :demand)

(use-package thingatpt+
  :ensure
  (:fetcher github :repo "emacsmirror/thingatpt-plus")
  :after thingatpt
  :init
  (tap-put-thing-at-point-props)
  (tap-redefine-std-fns))

(use-package replace+
  :ensure
  (:fetcher github :repo "emacsmirror/replace-plus"))

(use-package shell-command-x
  :ensure
  (:fetcher github :repo "elizagamedev/shell-command-x.el")
  :init
  (shell-command-x-mode))

(use-package shell-command+)

(use-package info+
  :ensure
  (:fetcher github :repo "emacsmirror/info-plus")
  :after info
  :demand)

(use-package bookmark+
  :ensure
  (:fetcher github :repo "emacsmirror/bookmark-plus")
  :demand)

(use-package transient)

(provide 'enhancements)
