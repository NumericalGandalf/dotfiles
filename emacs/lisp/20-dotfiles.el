(defgroup dotfiles nil
  "Dotfiles Management"
  :prefix "dots-"
  :group 'emacs)

(defcustom dots-stow-parents '(".config/")
  "List of stow parent directories.

The child directories of these are stowed as they are
and will not be traversed any further.

These directories are relative to the dotfiles dots directory."
  :type '(repeat string))

(defcustom dots-gsettings
  '(("org.gnome.desktop.interface" "gtk-key-theme" "Emacs")
    ("org.gnome.desktop.interface" "color-scheme" "prefer-dark"))
  "List of gsettings in form SCHEME, KEY, VALUE."
  :type '(list string string string))

(defcustom dots-stow-hook nil
  "Hooks to run when stowing dotfiles."
  :type 'hook)

(defcustom dots-deploy-hook nil
  "Hooks to run when deploying dotfiles."
  :type 'hook)

(defcustom dots-load-font-hook nil
  "Hooks to run when loading font.")

(defun dots-expand (&optional file)
  "Expand FILE from the dotfiles dots directory."
  (rc-expand file (rc-expand "../dots/")))

(defun dots-stow-destination (&optional file)
  "Get stow destination of FILE.

FILE may be absolute or relative to the dotfiles dots directory."
  (expand-file-name
   (file-relative-name (dots-expand file) (dots-expand)) "~/"))

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

(defun dots-stow (&optional prefix dir)
  "Recursively iterate over DIR and stow files.
If PREFIX is non-nil, unstow files.

Whether a child dir is stowed depends on `dots-stow-parents'.

Also run `dots-stow-hook' when stowing files."
  (interactive "P")
  (dolist (entry (cdr (cdr (directory-files (dots-expand dir) t))))
    (if (or (file-regular-p entry)
	        (cl-dolist (parent dots-stow-parents)
	          (let ((parent (dots-expand parent)))
		        (when (and (file-in-directory-p entry parent)
			               (not (file-equal-p entry parent)))
		          (cl-return entry)))))
	    (dots-stow-entry entry prefix)
      (dots-stow prefix entry)))
  (unless prefix
    (run-hooks 'dots-stow-hooks)))

(add-hook 'dots-deploy-hook 'dots-stow)

(defun dots-gsettings-apply (&optional prefix)
  "Applies gsettings specified in `dots-gsettings'.
If PREFIX is non-nil, reset the scheme keys."
  (interactive "P")
  (mapcar
   (lambda (tuple)
     (let ((scheme (nth 0 tuple))
           (key (nth 1 tuple))
           (value (nth 2 tuple)))
       (if prefix
           (rc-shell (rc-join "gsettings reset" scheme key)
             (message "Resetting %s %s" scheme key))
         (rc-shell (rc-join "gsettings set" scheme key value)
           (message "Setting %s %s %s" scheme key value)))))
   dots-gsettings))

(add-hook 'dots-stow-hook 'dots-gsettings-apply)

(defun dots-gsettings-load-font (&optional prefix)
  "Load `font-name' for gsettings.
If PREFIX is non-nil, reset gsettings font."
  (interactive "P")
  (let ((scheme "org.gnome.desktop.interface")
        (keys '("font-name" "monospace-font-name" "document-font-name"))
        (font (prin1-to-string
               (rc-join font-name (int-to-string (font-height -1))))))
    (mapcar
     (lambda (key)
       (if prefix
           (rc-shell (rc-join "gsettings reset" scheme key)
             (message "Resetting font %s %s" scheme key))
         (rc-shell (rc-join "gsettings set" scheme key font)
           (message "Setting font %s %s %s" scheme key font))))
     keys)))

(add-hook 'dots-load-font-hook 'dots-gsettings-load-font)

(defun dots-sway-write-wallpaper ()
  "Write wallpaper into sway config."
  (interactive)
  (message "Writing sway wallpaper")
  (rc-file (dots-expand ".config/sway/wallpaper")
    (insert (rc-join "set $wallpaper"
                     (dots-expand ".config/sway/butterfly.png")))))

(add-hook 'dots-stow-hook 'dots-sway-write-wallpaper)

(defun dots-desktop-load-font ()
  "Load `font-name' for sway and waybar config."
  (interactive)
  (message "Loading desktop font")
  (rc-file (dots-expand ".config/sway/font")
    (insert (rc-join (concat "font pango:" font-name)
		             (int-to-string (font-height -3)))))
  (rc-file (dots-expand ".config/waybar/font.css")
    (css-mode)
    (rc-insert "* {")
    (rc-insert (rc-join "font-family:"
			            (concat (prin1-to-string font-name) ";")))
    (rc-insert (rc-join "font-size:" (int-to-string (font-height))))
    (insert "}")
    (indent-region (point-min) (point-max)))
  (rc-shell "swaymsg reload"))

(add-hook 'dots-load-font-hook 'dots-desktop-load-font)

(defun dots-load-font (&optional prefix)
  "Load dotfiles font. Run `dots-load-font-hook'.
If PREFIX is non-nil, prompt for the font"
  (interactive "P")
  (when prefix
    (call-interactively 'font-load))
  (run-hooks 'dots-load-font-hook))

(add-hook 'dots-deploy-hook 'dots-load-font)

(defun dots-deploy ()
  "Deploy dotfiles. Run `dots-deploy-hook'."
  (interactive)
  (message "Deploying dotfiles")
  (run-hooks 'dots-deploy-hook))

(defun dots-bluetooth-connect (&optional prefix)
  "Connect AirPods Max via bluez bluetoothctl.
If PREFIX is non-nil, run disconnection."
  (interactive "P")
  (let ((method (if prefix "disconnect" "connect"))
	    (addr "90:9C:4A:DA:5C:9F"))
    (message (rc-join "bluetoothctl" method addr "&> /dev/null"))))
