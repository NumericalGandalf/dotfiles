(use-package vterm
  :config
  (let ((rcfile (rc-expand-file "scripts/vtermrc.sh")))
    (when (string= shell-file-name "/bin/bash")
      (setq vterm-shell (s-join " " `(,shell-file-name "--rcfile" ,rcfile)))))
  (global-set-key (kbd "M-T") 'vterm))

(unless (package-installed-p 'emapl)
  (package-vc-install '(emapl :url "https://github.com/NumericalGandalf/emapl.git")))
(require 'emapl)

(defun rc-connect-airpods(&optional prefix)
  "Connect AirPods Max via bluez bluetoothctl.
If PREFIX is non-nil, run disconnection."
  (interactive "P")
  (let ((method (if prefix "disconnect" "connect"))
	(addr "90:9C:4A:DA:5C:9F"))
    (shell-command (s-join " " `("bluetoothctl" ,method ,addr ">" "/dev/null")))))

(provide 'extensions)
