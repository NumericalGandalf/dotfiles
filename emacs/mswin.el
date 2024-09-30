(defgroup mswin nil
  "Windows OS management."
  :prefix "mswin-"
  :group 'emacs)

(defun mswin-chemacs-setup ()
  "Setup chemacs2 for config management."
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

(define-advice nerd-icons-install-fonts (:around (&rest _) mswin)
  "Install fonts for `nerd-icons' on MS Windows."
  (let* ((url "https://raw.githubusercontent.com/rainstormstudio/nerd-icons.el/main/fonts/")
         (dest (rc/temp t))
         (sysdir (rc/cache "../../Local/Microsoft/Windows/Fonts/"))
         (default-directory dest))
    (dolist (font nerd-icons-font-names)
      (let ((file (rc/expand font dest))
            (sysfile (rc/expand font sysdir)))
        (unless (file-exists-p sysfile)
          (url-copy-file (concat url font) file t))))
    (rc/script "fonts-install.ps1")))

(rc/deploy
 (mswin-chemacs-setup))
