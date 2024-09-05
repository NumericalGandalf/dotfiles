(setq ad-redefinition-action 'accept)

(defmacro rc-shell (command &optional success error)
  "Run shell command COMMAND and evaluate SUCCESS or ERROR."
  (declare (indent 1))
  `(when ,command
     (if (= (call-process-shell-command ,command) 0)
         ,success
       ,error)))

(add-to-list 'load-path (rc-expand "lisp/"))

(require 'package-setup)

(setq custom-file (rc-cache "custom.el"))
(add-hook 'elpaca-after-init-hook
          (lambda () (load custom-file t t)) -90)

(define-advice custom-save-all (:around (fun &rest args) silently)
  "Save custom variables silently."
  (let ((save-silently t))
    (apply fun args)))

(when rc-posix-p
  (require 'dotfiles)
  (require 'posix-setup))

(require 'font)
(require 'theming)

(require 'minibuf)
(require 'enhancements)
(require 'editing)

(require 'programming)

(when (treesit-available-p)
  (require 'tree-sitter))

(require 'keybindings)
