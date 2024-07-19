(require 'dots)

(use-package vterm
  :config
  (setq vterm-timer-delay 0.01
	vterm-max-scrollback 1000
	vterm-clear-scrollback-when-clearing t)
  (let ((rcfile (dots-expand-file "scripts/vtermrc.sh")))
    (when (string= shell-file-name "/bin/bash")
      (setq vterm-shell (combine-and-quote-strings
			 `(,shell-file-name "--rcfile" ,rcfile))))))

(unless (package-installed-p 'emapl)
  (package-vc-install
   '(emapl :url "https://github.com/NumericalGandalf/emapl.git")))
(require 'emapl)

(provide 'apps)
