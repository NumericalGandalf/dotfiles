(require 'cl-lib)

(defgroup dots nil
  "Dotfile Management."
  :group 'local
  :prefix "dots-")

(defcustom dots-deploy-hook nil
  "Hooks to run on dotfiles deploy."
  :type 'hook)

(defcustom dots-sway-font-height-offset -3
  "Offset of sway font height."
  :type 'integer)

(defcustom dots-gtk-font-height-offset -1
  "Offset of gtk font height."
  :type 'integer)

(defcustom dots-waybar-font-height-offset 0
  "Offset of gtk font height."
  :type 'integer)

(defcustom dots-stow-parents '(".config/")
  "List of stow parent directories.

The child directories of these are stowed as they are
and will not be traversed any further.

These directories are relative to the dotfiles dots directory."
  :type '(repeat string))

(defcustom dots-gsettings
  '(("org.gnome.desktop.interface" "font-name" "@FONT")
    ("org.gnome.desktop.interface" "monospace-font-name" "@FONT")
    ("org.gnome.desktop.interface" "gtk-key-theme" "Emacs")
    ("org.gnome.desktop.interface" "color-scheme" "prefer-dark"))
  "List of gsettings in form SCHEME, KEY, VALUE.

Expansions [@]:
    FONT -> concatenated font string."
  :type '(list string string string))

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
        (message "Unstowing %s" entry)
      (progn
        (message "Stowing %s" dest)
        (make-symbolic-link entry dest)))))

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
          (progn 
            (message "Resetting %s %s" scheme key)
            (rc-shell (rc-join "gsettings reset" scheme key)))
        (progn
          (message "Setting %s %s %s" scheme key value)
          (rc-shell (rc-join "gsettings set" scheme key value)))))))

(defun dots-sway-reload ()
  "Reload sway."
  (interactive)
  (message "Reloading sway")
  (rc-shell "swaymsg reload"))

(defun dots-sway-write-wallpaper ()
  "Write wallpaper into sway config."
  (interactive)
  (message "Writing sway wallpaper")
  (rc-with-file (dots-expand-file ".config/sway/wallpaper")
    (insert (rc-join "set $wallpaper"
                     (dots-expand-asset "butterfly.png")))))

(defun dots-sway-write-font ()
  "Write `rc-font' into sway config."
  (interactive)
  (message "Writing sway font.")
  (rc-with-file (dots-expand-file ".config/sway/font")
    (insert (rc-join (concat "font pango:" rc-font)
		     (int-to-string
                      (+ rc-font-height dots-sway-font-height-offset))))))

(defun dots-waybar-write-font ()
  "Write `rc-font' into waybar config."
  (interactive)
  (message "Writing waybar font.")
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

(defun dots-deploy ()
  "Deploy dotfiles and run hooks `dots-deploy-hook'."
  (interactive)
  (message "Deploying dotfiles")
  (dots-sway-write-font)
  (dots-sway-write-wallpaper)
  (dots-waybar-write-font)
  (run-hooks 'dots-deploy-hook)
  (dots-sway-reload))

(defun dots-bluetooth-connect (&optional prefix)
  "Connect AirPods Max via bluez bluetoothctl.
If PREFIX is non-nil, run disconnection."
  (interactive "P")
  (let ((method (if prefix "disconnect" "connect"))
	(addr "90:9C:4A:DA:5C:9F"))
    (message (rc-join "bluetoothctl" method addr "&> /dev/null"))))

(provide 'rc-dotfiles)
