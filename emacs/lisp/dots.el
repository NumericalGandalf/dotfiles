(defgroup dots nil
  "Dotfile Management."
  :group 'local
  :prefix "-dots")

(defun dots-expand-file (file)
  "Expands FILE from config root."
  (expand-file-name file user-emacs-directory))

(defun dots-bluetooth-connect (&optional prefix)
  "Connect AirPods Max via bluez bluetoothctl.
If PREFIX is non-nil, run disconnection."
  (interactive "P")
  (let ((method (if prefix "disconnect" "connect"))
	(addr "90:9C:4A:DA:5C:9F"))
    (shell-command (combine-and-quote-strings
		    `("bluetoothctl" ,method ,addr ">" "/dev/null")))))

(provide 'dots)
