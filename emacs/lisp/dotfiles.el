(defgroup dotfiles nil
  "Dotfiles Management."
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

(defun dots-expand (&optional file)
  "Expand FILE from the dotfiles dots directory."
  (rc-expand file (rc-expand "../dots/")))

(defun dots-stow-destination (&optional file)
  "Get stow destination of FILE.

FILE may be absolute or relative to the dotfiles dots directory."
  (expand-file-name
   (file-relative-name (dots-expand file) (dots-expand)) "~/"))

(defun dots-gsettings-apply (&optional prefix)
  "Applies gsettings specified in `dots-gsettings'.
If PREFIX is non-nil, reset the scheme keys."
  (interactive "P")
  (dolist (item dots-gsettings)
    (let* ((scheme (nth 0 item))
           (scheme1 (prin1-to-string scheme))
           (key (nth 1 item))
           (key1 (prin1-to-string key))
           (value (prin1-to-string (nth 2 item))))
      (if prefix
          (rc-shell (rc-join "gsettings reset" scheme key)
            (message "Resetting %s at %s" key1 scheme1))
        (rc-shell (rc-join "gsettings set" scheme key value)
          (message "Setting %s for %s at %s" value key1 scheme1))))))

(defun dots-sway-write-wallpaper ()
  "Write wallpaper into sway config."
  (interactive)
  (message "Writing sway wallpaper")
  (rc-file (dots-expand ".config/sway/wallpaper")
    (insert (rc-join "set $wallpaper"
                     (dots-expand ".config/sway/butterfly.png")))))

(defun dots-stow-files (&optional prefix dir)
  "Recursively iterate over DIR and stow files.
If PREFIX is non-nil, unstow files.

Whether a child dir is stowed depends on `dots-stow-parents'."
  (dolist (entry (cdr (cdr (directory-files (dots-expand dir) t))))
    (if (or (file-regular-p entry)
	        (cl-dolist (parent dots-stow-parents)
	          (let ((parent (dots-expand parent)))
		        (when (and (file-in-directory-p entry parent)
			               (not (file-equal-p entry parent)))
		          (cl-return entry)))))
        (let ((dest (dots-stow-destination entry)))
          (when (file-exists-p dest)
            (cond ((f-symlink-p dest)
                   (delete-file dest))
                  ((f-directory-p dest)
                   (delete-directory dest t))
                  (t (delete-file dest))))
          (if prefix
              (message "Unstowing %s" entry)
            (progn
              (message "Stowing %s" dest)
              (make-symbolic-link entry dest))))
      (dots-stow-files prefix entry))))

(defun dots-stow (&optional prefix)
  "Stow dotfiles.
If PREFIX is non-nil, unstow dotfiles.

Also run `dots-stow-hook' when stowing files."
  (interactive "P")
  (dots-stow-files prefix)
  (dots-gsettings-apply prefix)
  (unless prefix
    (dots-sway-write-wallpaper)
    (run-hooks 'dots-stow-hooks)))

(defun dots-gsettings-load-font (&optional prefix)
  "Load `font-name' for gsettings.
If PREFIX is non-nil, reset gsettings font."
  (interactive "P")
  (let* ((scheme "org.gnome.desktop.interface")
         (scheme1 (prin1-to-string scheme))
         (list `(("font-name" . font-name)
                 ("monospace-font-name" . font-name)
                 ("document-font-name" . font-name-var))))
    (dolist (item list)
      (let* ((key (car item))
             (key1 (prin1-to-string key))
             (val (prin1-to-string
                   (rc-join
                    (symbol-value (cdr item))
                    (int-to-string (1- font-height))))))
        (if prefix
            (rc-shell (rc-join "gsettings reset" scheme key)
              (message "Resetting %s at %s" key1 scheme1)))
        (rc-shell (rc-join "gsettings set" scheme key val)
          (message
           "Setting font %s for %s at %s" val key1 scheme1))))))

(defun dots-desktop-load-font ()
  "Load `font-name' for sway and waybar config."
  (interactive)
  (message "Loading desktop font")
  (rc-file (dots-expand ".config/sway/font")
    (insert (rc-join (concat "font pango:" font-name)
		             (int-to-string (- font-height 3)))))
  (rc-file (dots-expand ".config/waybar/font.css")
    (css-mode)
    (rc-insert "* {")
    (rc-insert (rc-join "font-family:"
			            (concat (prin1-to-string font-name) ";")))
    (rc-insert (rc-join "font-size:" (int-to-string font-height)))
    (insert "}")
    (indent-region (point-min) (point-max)))
  (rc-shell "swaymsg reload"))

(defun dots-load-font (&optional prefix)
  "Load dotfiles font.
If PREFIX is non-nil, prompt for the font."
  (interactive "P")
  (when prefix
    (font-load t))
  (dots-gsettings-load-font)
  (dots-desktop-load-font))

(defun dots-deploy ()
  "Deploy dotfiles and run `dots-deploy-hook'."
  (interactive)
  (message "Deploying dotfiles")
  (dots-stow)
  (dots-load-font)
  (run-hooks 'dots-deploy-hook))

(provide 'dotfiles)
