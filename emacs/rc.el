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
  "Expand FILE from emacs cache directory.

Cache directories are system dependent:
    POSIX      -> ~/.cache/emacs
    MS Windows -> %APPDATA%/emacs"
  (let ((dir (cond (rc/posix-p "~/.cache/emacs/")
                   (rc/mswin-p (rc/expand "Emacs/" (getenv "APPDATA"))))))
    (rc/expand file dir)))

(defun rc/temp (&optional unique file)
  "Expand (FILE) from a (UNIQUE) temp directory."
  (let ((dir (rc/expand "emacs/" (temporary-file-directory))))
    (when unique
      (let ((key (int-to-string (time-to-seconds))))
        (when rc/posix-p
          (setq key (format "%d.%s" (user-uid) key)))
        (setq dir (rc/expand (concat key "/") dir))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (rc/expand file dir)))

(defun rc/dot (file)
  "Expand FILE from the dotfiles `dots' directory."
  (rc/expand file (rc/expand "../dots/")))

(defun rc/cmdline-p (key)
  "Non-nil if `command-line-args' has KEY."
  (when (member key command-line-args)
    (setq command-line-args (delete key command-line-args))
    t))

(defun rc/shell (command &optional async)
  "Execute (ASYNC) COMMAND in a shell."
  (eq (call-process-shell-command command nil (if async 0 nil)) 0))

(defun rc/script (script &optional get-only)
  "Execute SCRIPT located in the dotfiles `scripts' directory.
If GET-ONLY is non-nil, only return the scripts path."
  (let ((path (rc/expand script (rc/expand "../scripts/"))))
    (when (and (file-exists-p path) (not get-only))
      (let ((prefix (pcase (file-name-extension path)
                      ("sh" "sh")
                      ("ps1" "powershell"))))
        (rc/shell (format "%s %s" prefix path))))
    path))

(defmacro rc/deploy (&rest body)
  "Run BODY on config deployment."
  (when (boundp 'rc/deploy)
    `(add-hook 'rc/deploy-hook (lambda () ,@body))))

(when (rc/cmdline-p "--deploy")
  (defvar rc/deploy t)
  (defvar rc/deploy-hook nil)
  (defvar rc/deploy-fun (lambda () (run-hooks 'rc/deploy-hook)))
  (add-hook 'window-setup-hook 'rc/deploy-fun 100))
