(add-to-list
 'load-path (file-truename (locate-user-emacs-file "lisp/")))

(require 'utils)
(require 'packages)

(setq custom-file (rc-cache "custom.el"))
(add-hook
 'elpaca-after-init-hook (lambda () (load custom-file t t)) -90)

(when rc-posix-p
  (require 'posix)
  (require 'dotfiles)

  (require 'server)
  (unless (or (daemonp)
              (server-running-p))
    (server-start)))

(require 'font)
(require 'theming)

(require 'minibuf)
(require 'enhancements)
(require 'editing)

(require 'programming)
(when (treesit-available-p)
  (require 'treesitter))

(require 'keybindings)
