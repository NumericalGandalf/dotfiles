(require 'rc)

(defgroup dots nil
  "Dotfile Management."
  :group 'local
  :prefix "-dots")

(defun dots-expand-file (&optional file)
  "Expand FILE from dotfiles root directory."
  (expand-file-name
   (if file file "./")
   (concat (file-truename user-emacs-directory) "../dots/")))

(directory-files (dots-expand-file) t)
(file-directory-p (dots-expand-file ".config"))

(f-write "this is a\ntest" 'utf-8 "~/test.txt")

(defun dots-sway-reload ()
  "Reload sway."
  (interactive)
  (shell-command "swaymsg reload"))

(defun dots-sway-write-font ()
  "Write `rc-font' and `rc-font-height' into sway config."
  (interactive)
  (f-write (rc-join (concat "font pango:" rc-font)
	    (int-to-string (- rc-font-height 3)))
   'utf-8
   (dots-expand-file ".config/sway/font")))

(defun dots-alacritty-write-font ()
  "Write `rc-font' and `rc-font-height' into alacritty config."
  (interactive)
  (f-write (s-join "\n" `("[font]"
			  ,(rc-join "normal" "=" "{" "family" "="
				    (prin1-to-string rc-font) "}")
			  ,(rc-join "size" "=" (int-to-string rc-font-height))))
	   'utf-8
	   (dots-expand-file ".config/alacritty/font.toml")))

(defun dots-bluetooth-connect (&optional prefix)
  "Connect AirPods Max via bluez bluetoothctl.
If PREFIX is non-nil, run disconnection."
  (interactive "P")
  (let ((method (if prefix "disconnect" "connect"))
	(addr "90:9C:4A:DA:5C:9F"))
    (message (rc-join "bluetoothctl" method addr "&> /dev/null"))))

(provide 'dots)
