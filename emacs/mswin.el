(defgroup mswin nil
  "MS Windows config management."
  :prefix "mswin-"
  :group 'emacs)

(use-package powershell)

(defun mswin-chemacs-setup ()
  "Setup chemacs2 for config management."
  (interactive)
  (let* ((home-dir (expand-file-name "./" (getenv "APPDATA")))
         (emacs-dir (expand-file-name ".emacs.d/" home-dir))
         (chemacs-repo "https://github.com/plexus/chemacs2.git")
         (cmd (format "git clone %s %s" chemacs-repo emacs-dir)))
    (dolist (file '(".emacs" ".emacs-profiles.el" ".emacs.d/"))
      (let ((file (expand-file-name file home-dir)))
        (cond ((file-symlink-p file)
               (delete-file file))
              ((file-directory-p file)
               (delete-directory file t))
              ((file-exists-p file)
               (delete-file file)))))
    (call-process-shell-command cmd nil 0)
    (with-temp-file (expand-file-name ".emacs-profiles.el" home-dir)
      (insert (format "((%s . ((user-emacs-directory . %s))))"
                      (prin1-to-string "default")
                      (prin1-to-string (rc/expand)))))))