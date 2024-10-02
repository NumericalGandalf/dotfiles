(rc/load 'package-setup)

(cond (rc/posix-p (rc/load 'posix))
      (rc/mswin-p (rc/load 'mswin)))

(rc/load 'completions)
(rc/load 'editing)
(rc/load 'programming)

(when (treesit-available-p)
  (rc/load 'tree-sitter))

(if (rc/cmdline-p "--doomed")
    (rc/load 'doomed)
  (rc/deploy (rc/load 'doomed)))

(rc/load 'keybindings)

(setq custom-file (rc/cache "custom.el"))
(load custom-file t t)
