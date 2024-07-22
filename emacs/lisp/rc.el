(defgroup rc nil
  "User Emacs utilities."
  :group 'local
  :prefix "rc-")

(defcustom rc-font "Hack Nerd Font Mono"
  "User nerd font."
  :type 'string)

(defcustom rc-font-asset-name "Hack"
  "Asset name of user font in nerd-fonts repo."
  :type 'string)

(defcustom rc-font-height 13
  "Default height of user font."
  :type 'natnum)

(defcustom rc-after-load-font-hook nil
  "Hook to run after user font gets loaded."
  :type 'hook)

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

(defun rc-load-font (&optional prefix)
  "Load user font and run `rc-after-load-font-hook'.
If optional PREFIX is non-nil, do not run hooks."
  (interactive)
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

(defun rc-sudo-buffer (&optional prefix)
  "Open current buffer as root.
If PREFIX is non-nil, free current buffer from root."
  (interactive "P")
  (let* ((buf (current-buffer))
	 (file (or (buffer-file-name)
		   (when (derived-mode-p 'dired-mode)
		     default-directory)))
	 (match (string-match "^/sudo:" file)))
    (when file
      (if prefix
	  (when match
	    (find-file (tramp-file-local-name file))
	    (kill-buffer buf))
	(unless match
	  (find-file (concat "/sudo::" file))
	  (kill-buffer buf))))))

(global-set-key (kbd "C-r") 'rc-sudo-buffer)

(defun rc-duplicate-line (&optional n)
  "Duplicate the current line N times."
  (interactive "p")
  (dotimes (_ n)
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank)))

(global-set-key (kbd "C-q") 'rc-duplicate-line)

(provide 'rc)
