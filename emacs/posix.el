(defgroup posix nil
  "Posix OS Management."
  :prefix "posix-"
  :group 'emacs)

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

(defcustom posix-loose-dir (rc/cache "loose/")
  "Backup directory for `posix-loose-files'."
  :type 'string)

(defcustom posix-loose-files
  '(".ssh"
    ".mozilla/firefox/v2pr6fd9.gandalf")
  "List of files not stored in a git repository.

These files are relative to the users home directory."
  :type '(repeat string))

(defcustom posix-launcher-height 25
  "Height of `posix-run-launcher' frame."
  :type 'natnum)

(use-package vterm
  :config
  (setq vterm-timer-delay nil)
  :custom
  (vterm-max-scrollback 5000)
  (vterm-clear-scrollback-when-clearing t)
  (vterm-always-compile-module t))

(use-package guix)

(unless (package-installed-p 'app-launcher)
  (package-vc-install
   '(app-launcher :url "https://github.com/NumericalGandalf/app-launcher.git")))

;; (use-package app-launcher
;;   :vc
;;   (:url "https://github.com/NumericalGandalf/app-launcher.git")
;;   :defer)

(defun posix-link-emacs-dir (&optional unlink)
  "Link emacs init directory.
If optional UNLINK is non-nil, unlink it."
  (let ((config-dir (expand-file-name ".config/emacs/" "~/")))
    (unless (string= (expand-file-name config-dir)
                     (expand-file-name user-emacs-directory))
      (dolist (file '(".emacs" ".emacs.d" ".config/emacs"))
        (let ((file (expand-file-name file "~/")))
          (cond ((file-symlink-p file)
                 (delete-file file))
                ((file-directory-p file)
                 (delete-directory file t))
                ((file-exists-p file)
                 (delete-file file)))))
      (unless unlink
        (make-symbolic-link (rc/expand) config-dir)))))

(defun posix-loose-backup ()
  "Backup `posix-loose-files' to `posix-loose-dir'."
  (interactive)
  (when (file-exists-p posix-loose-dir)
    (delete-directory posix-loose-dir t))
  (dolist (file posix-loose-files)
    (let ((file (rc/expand file "~/"))
          (target (file-name-parent-directory
                   (rc/expand file posix-loose-dir))))
      (make-directory target t)
      (if (file-directory-p file)
          (copy-directory file target)
        (copy-file file target)))))

(defun posix-font-set-gsettings (&optional reset)
  "Set `font-name' for gsettings.
If optional RESET is non-nil, reset them."
  (let* ((scheme "org.gnome.desktop.interface")
         (scheme1 (prin1-to-string scheme))
         (list `(("font-name" . font-name)
                 ("monospace-font-name" . font-name)
                 ("document-font-name" . font-name-var))))
    (dolist (item list)
      (let* ((key (car item))
             (key1 (prin1-to-string key))
             (name (symbol-value (cdr item)))
             (height (int-to-string (1- font-height)))
             (val (prin1-to-string (format "%s %s" name height)))
             (cmd (if reset
                      (format "gsettings reset %s %s" scheme key)
                    (format "gsettings set %s %s %s" scheme key val))))
        (call-process-shell-command cmd nil 0)))))

(defun posix-font-write-waybar ()
  "Write `font-name' into waybar config."
  (let ((name (prin1-to-string font-name)))
    (with-temp-file (rc/dot ".config/waybar/font.css")
      (erase-buffer)
      (insert (format "*{font-family:%s;font-size:%d}" name font-height)))))

(defun posix-font-write-sway ()
  "Write `font-name' into sway config."
  (with-temp-file (rc/dot ".config/sway/font")
    (erase-buffer)
    (insert (format "font pango:%s %d" font-name (- font-height 3))))
  (call-process-shell-command "sway reload" nil 0))

(define-advice font-load (:after (&rest _) posix)
  "Apply fonts for system if called interactively."
  (when (called-interactively-p)
    (posix-font-set-gsettings)
    (posix-font-write-waybar)
    (posix-font-write-sway)))

(defun posix-gsettings-apply (&optional reset)
  "Applies gsettings specified in `posix-gsettings'.
If optional RESET is non-nil, reset the scheme keys."
  (dolist (item posix-gsettings)
    (let* ((scheme (nth 0 item))
           (scheme1 (prin1-to-string scheme))
           (key (nth 1 item))
           (key1 (prin1-to-string key))
           (value (prin1-to-string (nth 2 item)))
           (cmd (if reset
                    (format "gsettings reset %s %s" scheme key)
                  (format "gsettings set %s %s %s" scheme key value))))
      (call-process-shell-command cmd nil 0)))
  (posix-font-set-gsettings reset))

(defun posix-sway-write-wallpaper ()
  "Write wallpaper into sway config."
  (let ((wallpaper (rc/dot ".config/sway/butterfly.png")))
    (with-temp-file (rc/dot ".config/sway/wallpaper")
      (erase-buffer)
      (insert (format "set $wallpaper %s" wallpaper)))
    (call-process-shell-command "sway reload" nil 0)))

(defun posix-stow-dots (&optional unstow dir)
  "Recursively iterate over DIR and stow its files.
If DIR is nil, start from the dotfiles dots directory.

If optional UNSTOW is non-nil, unstow the files.

Whether a child dir is stowed depends on `posix-stow-parents'."
  (dolist (target (cdr (cdr (directory-files (rc/dot dir) t))))
    (if (or (file-regular-p target)
            (cl-dolist (parent posix-stow-parents)
              (let ((parent (rc/dot parent)))
                (when (and (file-in-directory-p target parent)
                           (not (file-equal-p target parent)))
                  (cl-return target)))))
        (let* ((rel-name (file-relative-name (rc/dot target) (rc/dot "./")))
               (link-name (expand-file-name rel-name "~/")))
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

(rc/deploy
 (posix-link-emacs-dir)
 (posix-stow))

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
           (call-process-shell-command command nil 0)
         (progn
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

(posix-program terminal
               "Run posix terminal or `vterm'."
               (with-selected-frame (make-frame)
                 (vterm)))

(posix-program browser)

(require 'server)
(unless (or (daemonp)
            (server-running-p))
  (server-start))
