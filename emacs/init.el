(setq use-short-answers t
      suggest-key-bindings nil
      vc-follow-symlinks t)

(setq warning-minimum-level :emergency
      warning-minimum-log-level :warning
      ad-redefinition-action 'accept)

(setq auth-source-save-behavior nil)

(setq dired-listing-switches "-lah"
      dired-free-space 'separate
      dired-recursive-deletes 'always
      dired-dwim-target t
      dired-auto-revert-buffer t
      dired-clean-confirm-killing-deleted-buffers nil)

(add-to-list 'load-path (locate-user-emacs-file "modules/"))

(require 'rc-utils)
(require 'rc-font)
(require 'rc-dotfiles)
(require 'rc-packages)
(require 'rc-theming)
(require 'rc-minibuffer)
(require 'rc-editing)
(require 'rc-programming)
(require 'rc-applications)
(require 'rc-keybindings)

(setq custom-file (rc-cache "custom.el"))
(load custom-file :noerror :nomessage)

(define-advice custom-save-all (:around (fun &rest args) silent)
  "Save all custom variables silently."
  (let ((save-silently t))
    (apply fun args)))
