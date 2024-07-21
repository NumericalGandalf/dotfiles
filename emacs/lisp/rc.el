(defgroup rc nil
  "User Emacs utilities."
  :group 'local
  :prefix "-rc")

(defcustom rc-font "Hack Nerd Font Mono"
  "User nerd font."
  :type 'string)

(defcustom rc-font-asset-name "Hack"
  "Asset name of user font in nerd-fonts repo."
  :type 'string)

(defcustom rc-font-height 13
  "Default height of user font."
  :type 'natnum)

(defun rc-load-font ()
  "Loads user font for all pitches.
Loaded font is `rc-font', font height is `rc-font-height'."
  (interactive)
  (let ((height (* rc-font-height 10)))
    (set-face-attribute 'default nil :font rc-font :height height)
    (set-face-attribute 'fixed-pitch nil :family rc-font  :height height)
    (set-face-attribute 'fixed-pitch-serif nil :family rc-font :height height)
    (set-face-attribute 'variable-pitch nil :family rc-font :height height)))

(rc-load-font)

(defun rc-install-font ()
  "Downloads and installs user font from nerd-fonts repo.
Name of downloaded font asset is `rc-font-asset-name'."
  (interactive)
  (let* ((default-directory (concat
			     "~/.local/share/fonts/" rc-font-asset-name "/"))
	 (font-archive (concat rc-font-asset-name ".tar.xz"))
	 (link (concat
		"https://github.com/ryanoasis/nerd-fonts/releases/latest/download/"
		font-archive)))
    (make-directory default-directory t)
    (shell-command (rc-join "curl -sLO" link
			    "&& tar xJf" font-archive
			    "&& fc-cache -f"
			    "&& rm" font-archive))
    (message (rc-join "Extracted archive" font-archive "to" default-directory))))

(defun rc-join (&rest args)
  "Join strings ARGS with space as seperator."
  (s-join " " args))

(defun rc-sudo-buffer (&optional prefix)
  "Open current buffer as root.
If PREFIX is non-nil, free current buffer from root."
  (interactive "P")
  (let* ((buf (current-buffer))
	 (file (or (buffer-file-name)
		   (when (derived-mode-p 'dired-mode)
		     (expand-file-name default-directory))))
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
