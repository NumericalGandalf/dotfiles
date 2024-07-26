(defun rc-cache-file (&optional file)
  "Expand FILE from emacs cache directory.

Cache directories are system dependent:
    gnu/linux -> ~/.cache/emacs"
  (expand-file-name (or file "./")
		    (if (eq system-type 'gnu/linux)
			"~/.cache/emacs/"
		      (locate-user-emacs-file "var/"))))

(defun rc-open-init-file ()
  "Open `user-init-file'."
  (interactive)
  (find-file (file-truename user-init-file)))

(defun rc-open-cache-dir ()
  "Open `rc-cache-file' root in dired."
  (interactive)
  (find-file (file-truename (rc-cache-file))))

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
	 (default-directory (concat "~/.local/share/fonts/" asset-name "/"))
	 (font-archive (concat asset-name ".tar.xz"))
	 (link (concat
		"https://github.com/ryanoasis/nerd-fonts/releases/latest/download/"
		font-archive)))
    (make-directory default-directory t)
    (call-process-shell-command (rc-join "curl -sLO" link
                                         "&& tar xJf" font-archive
                                         "&& fc-cache -f"
                                         "&& rm" font-archive))
    (message "Extracted archive %s to %s" font-archive default-directory)))

(provide 'rc-base)
