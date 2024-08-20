(setq use-short-answers t
      suggest-key-bindings nil
      vc-follow-symlinks t)

(setq warning-minimum-level :error)

(add-to-list
 'load-path (file-truename (locate-user-emacs-file "lisp/")))

(require 'utils)
(require 'packages)

(require 'font)
(font-nerds-mode rc-posix-p)

(setq custom-file (rc-cache "custom.el"))
(add-hook
 'elpaca-after-init-hook (lambda () (load custom-file t t)) -90)

(define-advice custom-save-all
    (:around (fun &rest args) silent)
  "Save custom variables silently."
  (let ((save-silently t))
    (apply fun args)))

(when rc-posix-p
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

(require 'server)
(unless (or (daemonp)
            (server-running-p))
  (server-start))
