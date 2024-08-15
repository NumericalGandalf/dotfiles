(setq use-short-answers t
      suggest-key-bindings nil
      vc-follow-symlinks t)

(setq warning-minimum-level :emergency
      ad-redefinition-action 'accept)

(setq auth-source-save-behavior nil)

(add-to-list
 'load-path (file-truename (locate-user-emacs-file "lisp/")))

(require 'utils)
(require 'packages)

(when rc-posix-p
  (require 'fonts)
  (require 'dotfiles)
  (require 'applications))

(require 'theming)

(require 'minibuf)
(require 'enhancements)
(require 'editing)

(require 'programming)
(when (treesit-available-p)
  (require 'treesitter))

(require 'keybindings)

(setq custom-file (rc-cache "custom.el"))
(load custom-file t t)

(define-advice custom-save-all (:around (fun &rest args) silent)
  "Save all custom variables silently."
  (let ((save-silently t))
    (apply fun args)))
