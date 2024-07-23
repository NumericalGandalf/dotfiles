(defgroup rc nil
  "User Emacs utilities."
  :group 'local
  :prefix "rc-")

(defcustom rc-font "Hack Nerd Font"
  "User nerd font."
  :type 'string)

(defcustom rc-font-asset-name "Hack"
  "Asset name of user font in nerd-fonts repo."
  :type 'string)

(defcustom rc-font-height 13
  "Default height of user font."
  :type 'natnum)

(defcustom rc-after-load-font-hook nil
  "Hook to run after user font gets loaded.

During those hooks, FONT is bound to the fonts name
and HEIGHT is bound to the fonts height."
  :type 'hook)

(defun rc-var-file (&optional file)
  (expand-file-name
   (if file file "./")
   (if (eq system-type 'gnu/linux)
       "~/.cache/emacs/var/"
     (locate-user-emacs-file "var/"))))

(defun rc-join (&rest strings)
  "Join STRINGS with space as seperator."
  (s-join " " strings))

(defun rc-insert (string &optional nobreak)
  "Insert STRING into current buffer.
If NOBREAK is non-nil, do not break line afterwards."
  (insert string)
  (unless nobreak
    (newline)))

(defmacro rc-with-file (file &rest body)
  "Erase contents of FILE and evaluate BODY."
  (declare (indent 1))
  `(with-temp-file ,file
     (erase-buffer)
     ,@body))

(setq use-short-answers t
      vc-follow-symlinks t
      auth-source-save-behavior nil)

(setq dired-listing-switches "-lah"
      dired-free-space 'separate
      dired-recursive-deletes 'always
      dired-dwim-target t
      dired-auto-revert-buffer t)

(defun rc-load-font (&optional prefix)
  "Load user font and run `rc-after-load-font-hook'.
If optional PREFIX is non-nil, do not run hooks."
  (interactive "P")
  (let ((height (* rc-font-height 10)))
    (set-face-attribute 'default nil :font rc-font :height height)
    (set-face-attribute 'fixed-pitch nil :family rc-font  :height height)
    (set-face-attribute 'fixed-pitch-serif nil :family rc-font :height height)
    (set-face-attribute 'variable-pitch nil :family rc-font :height height))
  (unless prefix
    (run-hooks 'rc-after-load-font-hook)))

(rc-load-font t)

(defun rc-install-font ()
  "Download and install user font from nerd-fonts repo."
  (interactive)
  (let* ((asset-name rc-font-asset-name)
	 (default-directory (concat
			     "~/.local/share/fonts/" asset-name "/"))
	 (font-archive (concat asset-name ".tar.xz"))
	 (link (concat
		"https://github.com/ryanoasis/nerd-fonts/releases/latest/download/"
		font-archive)))
    (make-directory default-directory t)
    (shell-command (rc-join "curl -sLO" link
			    "&& tar xJf" font-archive
			    "&& fc-cache -f"
			    "&& rm" font-archive))
    (message (rc-join "Extracted archive" font-archive "to" default-directory))))

(provide 'rc-base)
