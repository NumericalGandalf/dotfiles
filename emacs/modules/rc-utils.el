(defvar rc-font "UbuntuMono Nerd Font"
  "User nerd font.")

(defvar rc-font-height 15
  "Default height of the user font.")

(defcustom rc-load-font-hook nil
  "Hooks to run after user font gets loaded."
  :type 'hook)

(defun rc-expand (&optional file directory)
  "Expands FILE to canonical path from DIRECTORY."
  (file-truename
   (expand-file-name (or file "./")
                     (or directory
                         (file-truename user-emacs-directory)))))

(defun rc-cache-file (&optional file)
  "Expand FILE from emacs cache directory.

Cache directories are system dependent:
    gnu/linux -> ~/.cache/emacs"
  (rc-expand file (cond ((eq system-type 'gnu/linux)
		         "~/.cache/emacs/")
                        (t (locate-user-emacs-file "var/")))))

(defun rc-open-init-file ()
  "Open `user-init-file'."
  (interactive)
  (find-file (rc-expand user-init-file)))

(defun rc-open-cache-dir ()
  "Open `rc-cache-file' root in dired."
  (interactive)
  (find-file (rc-cache-file)))

(defun rc-join (&rest strings)
  "Join STRINGS with char space as seperator."
  (s-join (char-to-string ?\s) strings))

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

(defmacro rc-shell (command &optional success error)
  (declare (indent 1))
  `(if (= (call-process-shell-command ,command) 0)
       ,success
     ,error))

(defun rc-font-height (&optional offset)
  "Get font height by OFFSET."
  (+ rc-font-height (floor (* (or offset 0) (/ rc-font-height 13)))))

(defun rc-fetch-font-asset-name (font)
  "Fetch asset name of `FONT' from nerd-font repo."
  (with-current-buffer
      (url-retrieve-synchronously "https://www.nerdfonts.com/font-downloads")
    (re-search-forward (concat "/assets/img/previews/" font))
    (re-search-forward "nerd-font-invisible-text...")
    (current-word)))

(defun rc-install-font ()
  "Download and install user font from nerd-fonts repo."
  (interactive)
  (let* ((asset-name (rc-fetch-font-asset-name rc-font))
	 (default-directory
          (rc-expand (concat "~/.local/share/fonts/" asset-name "/")))
	 (font-archive (concat asset-name ".tar.xz"))
	 (link (concat
                "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/"
                font-archive)))
    (make-directory default-directory t)
    (rc-shell (rc-join "curl -sLO" link "&&"
                       "tar xJf" font-archive "&&"
                       "fc-cache -f &&"
                       "rm" font-archive)
      (message "Extracted archive %s to %s" font-archive default-directory))))

(defun rc-load-font (&optional prefix)
  "Load user font and run `rc-load-font-hook'.
If PREFIX is non-nil, do not run hooks."
  (interactive)
  (let ((height (* rc-font-height 10)))
    (set-face-attribute 'default nil :font rc-font :height height)
    (set-face-attribute 'fixed-pitch nil :family rc-font  :height height)
    (set-face-attribute 'fixed-pitch-serif nil :family rc-font :height height)
    (set-face-attribute 'variable-pitch nil :family rc-font :height height))
  (unless prefix
    (run-hooks 'rc-load-font-hook)))

(setq custom-file (rc-cache-file "custom.el"))

(provide 'rc-utils)
