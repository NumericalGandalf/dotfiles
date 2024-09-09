(defgroup posix nil
  "Posix OS Management."
  :prefix "posix-"
  :group 'emacs)

(defcustom posix-launcher-height 25
  "Height of `posix-launcher' frame."
  :type 'natnum)

(defcustom posix-gsettings
  '(("org.gnome.desktop.interface" "gtk-key-theme" "Emacs")
    ("org.gnome.desktop.interface" "color-scheme" "prefer-dark"))
  "List of gsettings in form SCHEME, KEY, VALUE."
  :type '(repeat (list string string string)))

(defcustom posix-stow-parents '(".config/")
  "List of stow parent directories.

The child directories of these are stowed as they are
and will not be traversed any further.

These directories are relative to the dotfiles dots directory."
  :type '(repeat string))

(defcustom posix-loose-dir (rc-cache "loose-backup/")
  "Backup directory for `posix-loose-files'."
  :type 'string)

(defcustom posix-loose-files
  '(".ssh"
    ".mozilla/firefox/v2pr6fd9.gandalf")
  "List of files not stored in a git repository.

These files are relative to the users home directory."
  :type '(repeat string))

(defcustom posix-deploy-hook nil
  "Hooks to run after posix config deployment."
  :type 'hook)

(use-package vterm
  :general
  ("C-c r t" 'vterm)
  (vterm-mode-map
   "C-j" (lambda ()
           (interactive)
           (vterm-send "C-c")))
  :config
  (setq vterm-timer-delay nil)
  :custom
  (vterm-max-scroll-back 5000)
  (vterm-clear-scrollback-when-clearing t)
  (vterm-always-compile-module t))

(use-package guix
  :general
  ("C-c y g" 'guix))

(use-package restart-emacs
  :general
  (:prefix "C-c r"
           "r" 'restart-emacs
           "R" 'restart-emacs-start-new-emacs))

(use-package app-launcher
  :ensure
  (:host github :repo "NumericalGandalf/app-launcher")
  :defer)

(defun posix-gsettings-apply (&optional reset)
  "Applies gsettings specified in `posix-gsettings'.
If optional RESET is non-nil, reset the scheme keys."
  (dolist (item posix-gsettings)
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

(defun posix-sway-write-wallpaper ()
  "Write wallpaper into sway config."
  (with-temp-file (rc-dots ".config/sway/wallpaper")
    (erase-buffer)
    (insert (format
             "set $wallpaper %s"
             (rc-dots ".config/sway/butterfly.png"))))
  (shell-command "sway reload"))

(defun posix-stow-dots (&optional unstow dir)
  "Recursively iterate over DIR and stow its files.
If DIR is nil, start from the dotfiles dots directory.

If optional UNSTOW is non-nil, unstow the files.

Whether a child dir is stowed depends on `posix-stow-parents'."
  (dolist (target (cdr (cdr (directory-files (rc-dots dir) t))))
    (if (or (file-regular-p target)
            (cl-dolist (parent posix-stow-parents)
              (let ((parent (rc-dots parent)))
                (when (and (file-in-directory-p target parent)
                           (not (file-equal-p target parent)))
                  (cl-return target)))))
        (let ((link-name (expand-file-name
                          (file-relative-name
                           (rc-dots target) (rc-dots))
                          "~/")))
          (when (file-exists-p link-name)
            (cond ((file-symlink-p link-name)
                   (delete-file link-name))
                  ((file-directory-p link-name)
                   (delete-directory link-name t))
                  (t (delete-file link-name))))
          (unless unstow
            (make-symbolic-link target link-name)))
      (posix-stow-dots unstow target))))

(defun posix-stow (&optional prefix)
  "Stow dotfiles.
If PREFIX is non-nil, unstow them."
  (interactive "P")
  (posix-stow-dots prefix)
  (posix-gsettings-apply prefix)
  (unless prefix
    (posix-sway-write-wallpaper)))

(defun posix-font-write-gsettings ()
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

(defun posix-font-write-waybar ()
  "Write `font-name' into waybar config."
  (with-temp-file (rc-dots ".config/waybar/font.css")
    (erase-buffer)
    (insert (format "*{font-family:%s;font-size:%d}"
                    (prin1-to-string font-name)
                    font-height))))

(defun posix-font-write-sway ()
  "Write `font-name' into sway config."
  (with-temp-file (rc-dots ".config/sway/font")
    (erase-buffer)
    (insert
     (format "font pango:%s %d" font-name (- font-height 3))))
  (shell-command "sway reload"))

(define-advice font-load (:after (&rest _) dotfiles)
  "Apply fonts for dotfiles if called interactively."
  (when (called-interactively-p)
    (posix-font-write-gsettings)
    (posix-font-write-waybar)
    (posix-font-write-sway)))

(defun posix-loose-backup ()
  "Backup `posix-loose-files' to `posix-loose-dir'."
  (interactive)
  (when (file-exists-p posix-loose-dir)
    (delete-directory posix-loose-dir t))
  (dolist (file posix-backup-files)
    (let ((file (rc-expand file "~/"))
          (target (file-name-parent-directory
                   (rc-expand file posix-loose-dir))))
      (make-directory target t)
      (if (file-directory-p file)
          (copy-directory file target)
        (copy-file file target)))))

(defun posix-link-emacs-dir (&optional unlink)
  "Link emacs init directory.
If optional UNLINK is non-nil, unlink it."
  (let ((config-dir (expand-file-name ".config/emacs/" "~/")))
    (unless (file-equal-p config-dir user-emacs-directory)
      (dolist (file '(".emacs" ".emacs.d" ".config/emacs"))
        (let ((file (expand-file-name file "~/")))
          (cond ((file-symlink-p file)
                 (delete-file file))
                ((file-directory-p file)
                 (delete-directory file t))
                ((file-exists-p file)
                 (delete-file file)))))
      (unless unlink
        (make-symbolic-link (rc-expand) config-dir)))))

(defun posix-deploy ()
  "Deploy posix configs and run `posix-deploy-hook'."
  (interactive)
  (posix-link-emacs-dir)
  (posix-stow)
  (funcall-interactively 'font-load t)
  (run-hooks 'posix-deploy-hook))

(defmacro posix-program (program &optional doc &rest body)
  "Define function for PROGRAM with DOC and BODY.

This will check for an environment variable PROGRAM first
and, if non-nil, run it, or execute BODY otherwise."
  (declare (indent 1) (doc-string 2))
  (let ((name (symbol-name program)))
    `(defun ,(intern (concat "posix-run-" name)) ()
       ,(if (stringp doc)
            doc
          (format "Run posix %s." name))
       (interactive)
       (if-let ((command (getenv (upcase ,name))))
           (call-process-shell-command command nil 0 nil)
         (ignore-errors
           ,@body)))))

(posix-program launcher
  "Run posix launcher or create frame for `app-launcher-run-app'."
  (with-selected-frame
      (make-frame `((name . "posix-launcher")
                    (height . ,posix-launcher-height)
                    (width . ,(* posix-launcher-height 4))
                    (minibuffer . only)))
    (unwind-protect
        (let ((vertico-count (1- posix-launcher-height)))
          (app-launcher-run-app t))
      (delete-frame))))

(posix-program browser)

(posix-program terminal
  "Run posix terminal or `vterm'."
  (with-selected-frame (make-frame)
    (vterm)))

(require 'server)
(unless (or (daemonp)
            (server-running-p))
  (server-start))

(provide 'posix)
