(require 'cl-lib)

(defun dots-expand-file (&optional file)
  "Expand FILE from the dotfiles dots directory."
  (rc-expand file (rc-expand "../dots/")))

(defun dots-expand-asset (&optional file)
  "Expand FILE from the dotfiles asset directory."
  (rc-expand file (rc-expand "../assets/")))

(defun dots-open-files ()
  "Open `dots-expand-file' in dired."
  (interactive)
  (find-file (dots-expand-file)))

(defun dots-open-assets ()
  "Open `dots-expand-asset' in dired."
  (interactive)
  (find-file (dots-expand-asset)))

(defun dots-stow-destination (&optional file)
  "Get stow destination of FILE.

FILE may be absolute or relative to the dotfiles dots directory."
  (expand-file-name
   (file-relative-name (dots-expand-file file) (dots-expand-file)) "~/"))

(defun dots-stow-entry (entry &optional unstow)
  "Stow ENTRY.
If UNSTOW is non-nil, unstow entry."
  (let ((dest (dots-stow-destination entry)))
    (when (file-exists-p dest)
      (cond ((f-symlink-p dest)
             (delete-file dest))
            ((f-directory-p dest)
             (delete-directory dest t))
            (t (delete-file dest))))
    (if unstow
        (message "Unstowed %s" entry)
      (progn
        (make-symbolic-link entry dest)
        (message "Stowed %s" dest)))))

(defun dots-stow-all (&optional prefix dir)
  "Recursively iterate over DIR and stow files.
If PREFIX is non-nil, unstow files.

Whether a child dir is stowed depends on `dots-stow-parents'."
  (interactive "P")
  (dolist (entry (cdr (cdr (directory-files (dots-expand-file dir) t))))
    (if (or (file-regular-p entry)
	    (cl-dolist (parent dots-stow-parents)
	      (let ((parent (dots-expand-file parent)))
		(when (and (file-in-directory-p entry parent)
			   (not (file-equal-p entry parent)))
		  (cl-return entry)))))
	(dots-stow-entry entry prefix)
      (dots-stow-all prefix entry))))

(defun dots-gsettings-apply (&optional prefix)
  "Applies gsettings specified in `dots-gsettings'.
If PREFIX is non-nil, reset the scheme keys."
  (interactive "P")
  (dolist (tuple dots-gsettings)
    (let* ((scheme (nth 0 tuple))
           (key (nth 1 tuple))
           (raw (nth 2 tuple))
           (unwrapped (if (functionp raw)
                          (funcall raw) raw))
           (value (prin1-to-string
                   (cond ((string= unwrapped "@FONT")
                          (rc-join
                           rc-font
                           (int-to-string
                            (+ rc-font-height
                               dots-gtk-font-height-offset))))
                         (t unwrapped)))))
      (if prefix
          (rc-shell (rc-join "gsettings reset" scheme key)
            (message "Reset %s %s" scheme key))
        (rc-shell (rc-join "gsettings set" scheme key value)
          (message "Set %s %s %s" scheme key value))))))

(defun dots-sway-reload ()
  "Reload sway."
  (interactive)
  (rc-shell "swaymsg reload"
    (message "Reloaded sway")))

(defun dots-sway-write-wallpaper ()
  "Write wallpaper into sway config."
  (interactive)
  (rc-with-file (dots-expand-file ".config/sway/wallpaper")
    (insert (rc-join "set $wallpaper"
                     (dots-expand-asset "butterfly.png")))))

(defun dots-sway-write-font ()
  "Write `rc-font' into sway config."
  (interactive)
  (rc-with-file (dots-expand-file ".config/sway/font")
    (insert (rc-join (concat "font pango:" rc-font)
		     (int-to-string
                      (+ rc-font-height dots-sway-font-height-offset))))))

(defun dots-waybar-write-font ()
  "Write `rc-font' into waybar config."
  (interactive)
  (rc-with-file (dots-expand-file ".config/waybar/font.css")
    (css-mode)
    (rc-insert "* {")
    (rc-insert (rc-join "font-family:"
			(concat (prin1-to-string rc-font) ";")))
    (rc-insert (rc-join "font-size:"
			(concat
                         (int-to-string
                          dots-waybar-font-height-offset)
                         "px;")))
    (insert "}")
    (indent-region (point-min) (point-max))))

(defun dots-kitty-write-font ()
  "Write `rc-font' into kitty config."
  (interactive)
  (rc-with-file (dots-expand-file ".config/kitty/font.conf")
    (rc-insert (rc-join "font_family" rc-font))
    (rc-insert "bold_font auto")
    (rc-insert "italic_font auto")
    (rc-insert "bold_italic_font auto")
    (insert (rc-join "font_size" (int-to-string rc-font-height)))))

(defun dots-alacritty-write-font ()
  "Write `rc-font' into alacritty config."
  (interactive)
  (rc-with-file (dots-expand-file ".config/alacritty/font.toml")
    (rc-insert "[font]")
    (rc-insert (rc-join "normal" "=" "{" "family" "="
			(prin1-to-string rc-font) "}"))
    (insert (rc-join "size" "=" (int-to-string rc-font-height)))))

(defun dots-deploy-all ()
  "Deploy dotfiles and run hooks `dots-deploy-hook'."
  (interactive)
  (dots-sway-write-font)
  (dots-sway-write-wallpaper)
  (dots-waybar-write-font))

(defun dots-bluetooth-connect (&optional prefix)
  "Connect AirPods Max via bluez bluetoothctl.
If PREFIX is non-nil, run disconnection."
  (interactive "P")
  (let ((method (if prefix "disconnect" "connect"))
	(addr "90:9C:4A:DA:5C:9F"))
    (message (rc-join "bluetoothctl" method addr "&> /dev/null"))))

(provide 'rc-dotfiles)
