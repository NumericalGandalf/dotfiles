(defconst rc/posix-p (pcase system-type
                       ('gnu t)
                       ('gnu/linux t)
                       ('gnu/kfreebsd t))
  "Non-nil means the system follows POSIX standards.")

(defconst rc/mswin-p (pcase system-type
                       ('windows-nt t)
                       ('cygwin t)
                       ('ms-dos t))
  "Non-nil means the system is MS Windows.")

(defun rc/expand (&optional file dir)
  "Expand FILE from DIR.
If DIR is nil, expand from `user-emacs-directory'."
  (let ((file (or file "./"))
        (dir (or dir (file-truename user-emacs-directory))))
    (file-truename (expand-file-name file dir))))

(defun rc/cache (&optional file)
  "Expand FILE from emacs cache directory."
  (let ((dir (cond (rc/posix-p "~/.cache/emacs/")
                   (rc/mswin-p (rc/expand "Emacs/" (getenv "APPDATA"))))))
    (rc/expand file dir)))

(when (native-comp-available-p)
  (startup-redirect-eln-cache (rc/cache "eln/"))
  (setq native-comp-async-report-warnings-errors 'silent))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(font . "Iosevka-14"))
