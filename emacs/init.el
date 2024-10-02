(rc/load 'font)

(rc/load 'package-setup)

(cond (rc/posix-p (rc/load 'posix))
      (rc/mswin-p (rc/load 'mswin)))

(rc/load 'completions)
(rc/load 'editing)
(rc/load 'programming)

(when (treesit-available-p)
  (rc/load 'tree-sitter))

(if (rc/cmdline-p "--icons")
    (rc/load 'icons)
  (rc/deploy (rc/load 'icons)))

(rc/load 'keybindings)

(setq custom-file (rc/cache "custom.el"))
(load custom-file t t)
