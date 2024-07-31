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
  "Run shell command COMMAND and evaluate SUCCESS or ERROR."
  (declare (indent 1))
  `(if (= (call-process-shell-command ,command) 0)
       ,success
     ,error))

(defconst custom-file (rc-cache-file "custom.el"))

(provide 'rc-utils)
