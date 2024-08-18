(use-package dired+
  :straight
  (:host github :repo "emacsmirror/dired-plus")
  :after
  dired
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
  :straight
  (:host github :repo "emacsmirror/files-plus"))

(use-package ls-lisp+
  :straight
  (:host github :repo "emacsmirror/ls-lisp-plus")
  :demand)

(use-package thingatpt+
  :straight
  (:host github :repo "emacsmirror/thingatpt-plus")
  :after
  thingatpt
  :demand
  :config
  (tap-put-thing-at-point-props)
  (tap-redefine-std-fns))

(use-package replace+
  :straight
  (:host github :repo "emacsmirror/replace-plus")
  :custom-face
  (replacep-msg-emphasis ((t (:foreground "Red"))))
  (replacep-msg-emphasis2 ((t (:foreground "Green")))))

(use-package shell-command-x
  :straight
  (:host github :repo "elizagamedev/shell-command-x.el")
  :hook
  (emacs-startup-hook . shell-command-x-mode))

(use-package shell-command+)

(use-package info+
  :straight
  (:host github :repo "emacsmirror/info-plus")
  :after
  info
  :demand)

(use-package bookmark+
  :straight
  (:host github :repo "emacsmirror/bookmark-plus")
  :demand)

(use-package helpful)

(provide 'enhancements)
