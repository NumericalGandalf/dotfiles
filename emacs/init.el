(setq ad-redefinition-action 'accept)

(add-to-list 'load-path (rc-expand "lisp/"))

(require 'elpaca-setup)

(elpaca (no-littering :wait t)
  (setq no-littering-etc-directory (rc-expand)
        no-littering-var-directory (rc-cache))
  (require 'no-littering)
  (setq server-auth-dir (rc-cache "server/")))

(setq custom-file (rc-cache "custom.el"))
(add-hook 'elpaca-after-init-hook
          (lambda () (load custom-file t t)) -90)

(define-advice custom-save-all (:around (fun &rest args) silent)
  "Save custom variables silently."
  (let ((save-silently t))
    (apply fun args)))

(require 'keybindings)

(cond (rc-posix-p (require 'posix))
      (rc-windows-p (require 'mswin)))

(require 'font)
(require 'theming)

(require 'minibuf)
(require 'editing)

(require 'programming)

(when (treesit-available-p)
  (require 'tree-sitter))
