(setq ad-redefinition-action 'accept)

(add-to-list 'load-path (rc-expand "lisp/"))

(require 'package-setup)

(cond (rc-posix-p (require 'posix))
      (rc-mswin-p (require 'mswin)))

(require 'font)
(require 'theming)

(require 'completions)
(require 'editing)

(require 'programming)

(when (treesit-available-p)
  (require 'tree-sitter))

(require 'keybindings)

(setq custom-file (rc-cache "custom.el"))
(load custom-file t t)

(add-hook 'kill-emacs-hook 'custom-save-all)

(define-advice custom-save-all (:around (fun &rest args) silent)
  "Save custom variables silently."
  (let ((save-silently t))
    (apply fun args)))
