(defgroup posix nil
  "POSIX config management."
  :prefix "posix-"
  :group 'emacs)

(use-package vterm
  :config
  (setq vterm-timer-delay nil)
  :custom
  (vterm-max-scrollback 5000)
  (vterm-clear-scrollback-when-clearing t)
  (vterm-always-compile-module t))

(defun posix-link-emacs-dir (&optional prefix)
  "Link emacs init directory.
If optional PREFIX is non-nil, unlink it."
  (interactive "P")
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
      (unless prefix
        (make-symbolic-link (rc/expand) config-dir)))))

(defun posix-stow (&optional prefix dir)
  "Recursively iterate over DIR and stow its files.
If DIR is nil, start from the dotfiles dots directory.

If optional PREFIX is non-nil, unstow the files."
  (interactive "P\ni")
  (dolist (target (cdr (cdr (directory-files (rc/dot dir) t))))
    (if (or (file-regular-p target)
            (cl-dolist (parent '(".config/"))
              (let ((parent (rc/dot parent)))
                (when (and (file-in-directory-p target parent)
                           (not (file-equal-p target parent)))
                  (cl-return target)))))
        (let* ((rel-name (file-relative-name (rc/dot target) (rc/dot)))
               (link-name (expand-file-name rel-name "~/")))
          (when (file-exists-p link-name)
            (cond ((file-symlink-p link-name)
                   (delete-file link-name))
                  ((file-directory-p link-name)
                   (delete-directory link-name t))
                  (t (delete-file link-name))))
          (unless prefix
            (make-symbolic-link target link-name)))
      (posix-stow prefix target))))

(defun posix-loose-backup (dir)
  "Backup loose files to DIR."
  (interactive "DTarget directory: ")
  (dolist (file '(".ssh"
                  ".mozilla/firefox/v2pr6fd9.gandalf"))
    (let ((file (rc/expand file "~/"))
          (target (file-name-parent-directory (rc/expand file dir))))
      (make-directory target t)
      (if (file-directory-p file)
          (copy-directory file target)
        (copy-file file target)))))
