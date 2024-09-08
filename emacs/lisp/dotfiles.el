(defgroup dotfiles nil
  "Dotfiles Management."
  :prefix "dots-"
  :group 'emacs)

(defcustom dots-gsettings
  '(("org.gnome.desktop.interface" "gtk-key-theme" "Emacs")
    ("org.gnome.desktop.interface" "color-scheme" "prefer-dark"))
  "List of gsettings in form SCHEME, KEY, VALUE."
  :type '(repeat (list string string string)))

(defcustom dots-stow-parents '(".config/")
  "List of stow parent directories.

The child directories of these are stowed as they are
and will not be traversed any further.

These directories are relative to the dotfiles dots directory."
  :type '(repeat string))

(defcustom dots-priv-dir (rc-cache "priv-backup/")
  "Backup directory for `dots-priv-files'."
  :type 'string)

(defcustom dots-priv-files
  '(".ssh"
    ".mozilla/firefox/v2pr6fd9.gandalf")
  "List of private files not stored in a git repository.

These files are relative to the users home directory."
  :type '(repeat string))

(defcustom dots-deploy-hook nil
  "Hooks to run when deploying dotfiles."
  :type 'hook)

(defun dots-expand (&optional file)
  "Expand FILE from the dotfiles dots directory."
  (rc-expand file (rc-expand "../dots/")))

(defun dots-gsettings-apply (&optional reset)
  "Applies gsettings specified in `dots-gsettings'.
If RESET is non-nil, reset the scheme keys."
  (dolist (item dots-gsettings)
    (let* ((scheme (nth 0 item))
           (scheme1 (prin1-to-string scheme))
           (key (nth 1 item))
           (key1 (prin1-to-string key))
           (value (prin1-to-string (nth 2 item))))
      (if reset
          (shell-command
           (format "gsettings reset %s %s" scheme key))
        (shell-command
            (format "gsettings set %s %s %s" scheme key value))))))

(defun dots-sway-write-wallpaper ()
  "Write wallpaper into sway config."
  (with-temp-file (dots-expand ".config/sway/wallpaper")
    (erase-buffer)
    (insert (format
             "set $wallpaper %s"
             (dots-expand ".config/sway/butterfly.png"))))
  (shell-command "sway reload"))

(defun dots-stow-files (&optional unstow dir)
  "Recursively iterate over DIR and stow files.
If UNSTOW is non-nil, unstow them.

Whether a child dir is stowed depends on `dots-stow-parents'."
  (dolist (target (cdr (cdr (directory-files (dots-expand dir) t))))
    (if (or (file-regular-p target)
            (cl-dolist (parent dots-stow-parents)
              (let ((parent (dots-expand parent)))
                (when (and (file-in-directory-p target parent)
                           (not (file-equal-p target parent)))
                  (cl-return target)))))
        (let ((link-name (expand-file-name
                          (file-relative-name
                           (dots-expand target) (dots-expand))
                          "~/")))
          (when (file-exists-p link-name)
            (cond ((file-symlink-p link-name)
                   (delete-file link-name))
                  ((file-directory-p link-name)
                   (delete-directory link-name t))
                  (t (delete-file link-name))))
          (unless unstow
            (make-symbolic-link target link-name)))
      (dots-stow-files unstow entry))))

(defun dots-stow (&optional prefix)
  "Stow dotfiles.
If PREFIX is non-nil, unstow dotfiles."
  (interactive "P")
  (dots-stow-files prefix)
  (dots-gsettings-apply prefix)
  (unless prefix
    (dots-sway-write-wallpaper)))

(defun dots-font-write-gsettings ()
  "Write `font-name' into gsettings.
If RESET is non-nil, reset gsettings font."
  (let* ((scheme "org.gnome.desktop.interface")
         (scheme1 (prin1-to-string scheme))
         (list `(("font-name" . font-name)
                 ("monospace-font-name" . font-name)
                 ("document-font-name" . font-name-var))))
    (dolist (item list)
      (let* ((key (car item))
             (key1 (prin1-to-string key))
             (val (prin1-to-string
                   (format "%s %s"
                           (symbol-value (cdr item))
                           (int-to-string (1- font-height))))))
        (shell-command
            (format "gsettings set %s %s %s" scheme key val))))))

(defun dots-font-write-waybar ()
  "Write `font-name' into waybar config."
  (with-temp-file (dots-expand ".config/waybar/font.css")
    (erase-buffer)
    (insert (format "*{font-family:%s;font-size:%d}"
                    (prin1-to-string font-name)
                    font-height))))

(defun dots-font-write-sway ()
  "Write `font-name' into sway config."
  (with-temp-file (dots-expand ".config/sway/font")
    (erase-buffer)
    (insert
     (format "font pango:%s %d" font-name (- font-height 3))))
  (shell-command "sway reload"))

(define-advice font-load (:after (&rest res) dots)
  "Apply fonts for dotfiles if called interactively."
  (when (called-interactively-p)
    (dots-font-write-gsettings)
    (dots-font-write-waybar)
    (dots-font-write-sway))
  res)

(defun dots-link-emacs-dir (&optional prefix)
  "Link emacs init directory.
If optional PREFIX is non-nil, unlink it."
  (interactive "P")
  (dolist (file '(".emacs" ".emacs.d" ".config/emacs"))
    (let ((file (expand-file-name file "~/")))
      (when (file-exists-p file)
        (cond ((file-symlink-p file)
               (delete-file file))
              ((file-directory-p file)
               (delete-directory file t))
              (t (delete-file file))))))
  (unless prefix
    (make-symbolic-link
     (rc-expand)
     (expand-file-name ".config/emacs/" "~/"))))

(defun dots-priv-backup ()
  "Backup `dots-priv-files' to `dots-priv-dir'."
  (interactive)
  (when (file-exists-p dots-priv-dir)
    (delete-directory dots-priv-dir t))
  (dolist (file dots-backup-files)
    (let ((file (expand-file-name file "~/"))
          (target (file-name-parent-directory
                   (expand-file-name file dots-priv-dir))))
      (make-directory target t)
      (if (file-directory-p file)
          (copy-directory file target)
        (copy-file file target)))))

(defun dots-deploy ()
  "Deploy dotfiles and run `dots-deploy-hook'."
  (interactive)
  (dots-link-emacs-dir)
  (dots-stow)
  (funcall-interactively 'font-load t)
  (run-hooks 'dots-deploy-hook))

(provide 'dotfiles)
