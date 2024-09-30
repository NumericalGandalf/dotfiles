(setq custom-theme-directory user-emacs-directory)
(load-theme 'zenburn t)

(rc/require 'font)

(rc/require 'package-setup)

(cond (rc/posix-p (rc/require 'posix))
      (rc/mswin-p (rc/require 'mswin)))

(rc/require 'completions)
(rc/require 'editing)
(rc/require 'programming)

(when (treesit-available-p)
  (rc/require 'tree-sitter))

(if (rc/cmdline-p "--doomed")
    (rc/require 'doomed)
  (rc/deploy (rc/require 'doomed)))

(rc/require 'keybindings)

(setq custom-file (rc/cache "custom.el"))
(load custom-file t t)
