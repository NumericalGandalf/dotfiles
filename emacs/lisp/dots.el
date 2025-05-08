;;;###autoload
(defun dots/expand-file (&optional file)
  "Expand FILE from dotfiles `dots' directory."
  (let* ((emacs-dir (file-truename user-emacs-directory))
         (dir (expand-file-name "../dots/" emacs-dir)))
    (expand-file-name (or file "./") dir)))

;;;###autoload
(defun dots/stow-self ()
  "Stow self."
  (interactive)
  (let ((target (directory-file-name user-emacs-directory))
        (link-name (expand-file-name "~/.config/emacs")))
    (when (file-exists-p link-name)
      (cond ((file-symlink-p link-name)
             (delete-file link-name))
            ((file-directory-p link-name)
             (delete-directory link-name t))
            (t (delete-file link-name)))
      (message "Unlink: %s" link-name))
    (make-symbolic-link target link-name)
    (message "Link: %s -> %s" target link-name)))

;;;###autoload
(defun dots/stow-files (&optional prefix dir)
  "Stow dotfiles.
If PREFIX is non-nil, unstow dotfiles."
  (interactive "P\ni")
  (dolist (target (nthcdr 2 (directory-files (dots/expand-file (or dir "./")) t)))
    (if (or (file-regular-p target)
            (cl-dolist (parent '(".config/"))
              (let ((parent (dots/expand-file parent)))
                (when (and (file-in-directory-p target parent)
                           (not (file-equal-p target parent)))
                  (cl-return target)))))
        (let* ((rel-name (file-relative-name (dots/expand-file target)
                                             (dots/expand-file "./")))
               (link-name (expand-file-name rel-name "~/")))
          (when (file-exists-p link-name)
            (cond ((file-symlink-p link-name)
                   (delete-file link-name))
                  ((file-directory-p link-name)
                   (delete-directory link-name t))
                  (t (delete-file link-name)))
            (when prefix
              (message "Unlink: %s" link-name)))
          (unless prefix
            (make-symbolic-link target link-name)
            (message "Link: %s -> %s" target link-name)))
      (dots/stow-files prefix target))))

(provide 'dots)
