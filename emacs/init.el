(defun rc/dot (file)
  "Expand FILE from the dotfiles `dots' directory."
  (rc/expand file (rc/expand "../dots/")))

(defun rc/script (name)
  "Expand NAME from the dotfiles `scripts' directory."
  (rc/expand name (rc/expand "../scripts/")))

(add-to-list 'load-path (rc/expand "lisp/"))

(require 'package-setup)

(cond (rc/posix-p (require 'posix))
      (rc/mswin-p (require 'mswin)))

(require 'font)
(require 'theming)

(require 'completions)
(require 'editing)

(require 'programming)

(when (treesit-available-p)
  (require 'tree-sitter))

(require 'keybindings)

(setq custom-file (rc/cache "custom.el"))
(load custom-file t t)
