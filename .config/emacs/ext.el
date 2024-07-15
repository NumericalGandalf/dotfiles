(use-package vterm
  :config
  (let ((rcfile (locate-user-emacs-file "vterm.sh")))
    (when (string= shell-file-name "/bin/bash")
      (setq vterm-shell (concat shell-file-name " --rcfile " rcfile))))
  (global-set-key (kbd "M-T") 'vterm))

(unless (package-installed-p 'emapl)
  (package-vc-install '(emapl :url "https://github.com/NumericalGandalf/emapl.git")))
(require 'emapl)
(global-set-key (kbd "M-D") 'emapl-run)
