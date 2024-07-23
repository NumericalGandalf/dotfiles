(require 'cl-lib)

(defgroup dots nil
  "Dotfile Management."
  :group 'local
  :prefix "dots-")

(defcustom dots-stow-parents '(".config/")
  "List of stow parent directories.

The child directories of these are stowed as they are
and will not be traversed any further.

These directories are relative to dotfiles dots directory."
  :type '(repeat string))

(defun dots-expand-file (&optional file)
  "Expand FILE from dotfiles dots directory."
  (expand-file-name (if file file "./")
		    (concat (file-truename user-emacs-directory) "../dots/")))

(defun dots-stow-destination (&optional file)
  "Get stow destination of FILE.

FILE may be absolute or relative to dotfiles dots directory."
  (expand-file-name
   (file-relative-name (dots-expand-file file) (dots-expand-file)) "~/"))

(defun dots-stow-entry (entry)
  "Stow ENTRY."
  (let* ((dest (dots-stow-destination entry)))
    (when (file-exists-p dest)
      (if (or (f-file-p dest) (f-symlink-p dest))
          (delete-file dest)
	(delete-directory dest t)))
    (make-symbolic-link entry dest)))

(defun dots-stow-all (&optional dir)
  "Recursively iterate over DIR and stow files.

Whether a child dir is stowed depends on `dots-stow-parents'."
  (interactive)
  (dolist
      (entry (cdr (cdr (directory-files (dots-expand-file dir) t))))
    (if (or (file-regular-p entry)
	    (cl-dolist (parent dots-stow-parents)
	      (let ((parent (dots-expand-file parent)))
		(when (and (file-in-directory-p entry parent)
			   (not (file-equal-p entry parent)))
		  (cl-return entry)))))
	(dots-stow-entry entry)
      (dots-stow-all entry))))

(defun dots-sway-reload ()
  "Reload sway."
  (interactive)
  (shell-command "swaymsg reload"))

(defun dots-sway-write-font ()
  "Write `rc-font' into sway config."
  (interactive)
  (rc-with-file (dots-expand-file ".config/sway/font")
    (insert (rc-join (concat "font pango:" rc-font)
		     (int-to-string (- rc-font-height 3))))))

(defun dots-waybar-write-font ()
  "Write `rc-font' into waybar config."
  (interactive)
  (rc-with-file (dots-expand-file ".config/waybar/font.css")
    (rc-insert "* {")
    (rc-insert (rc-join "font-family:"
			(concat (prin1-to-string rc-font) ";")))
    (rc-insert (rc-join "font-size:"
			(concat (int-to-string rc-font-height) "px;")))
    (insert "}")
    (css-mode)
    (indent-region (point-min) (point-max))))

(defun dots-alacritty-write-font ()
  "Write `rc-font' into alacritty config."
  (interactive)
  (rc-with-file (dots-expand-file ".config/alacritty/font.toml")
    (rc-insert "[font]")
    (rc-insert (rc-join "normal" "=" "{" "family" "="
			(prin1-to-string rc-font) "}"))
    (insert (rc-join "size" "=" (int-to-string rc-font-height)))))

(defun dots-bluetooth-connect (&optional prefix)
  "Connect AirPods Max via bluez bluetoothctl.
If PREFIX is non-nil, run disconnection."
  (interactive "P")
  (let ((method (if prefix "disconnect" "connect"))
	(addr "90:9C:4A:DA:5C:9F"))
    (message (rc-join "bluetoothctl" method addr "&> /dev/null"))))

(provide 'rc-dotfiles)
