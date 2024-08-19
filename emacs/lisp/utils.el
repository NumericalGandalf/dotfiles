(defvar rc-posix-p (pcase system-type
                     ('gnu)
                     ('gnu/linux t)
                     ('gnu/kfreebsd t))
  "Pred whether system is posix.")

(defvar rc-windows-p (pcase system-type
                       ('windows-nt)
                       ('cygwin))
  "Pred whether system is windows.")

(defun rc-expand (&optional file directory)
  "Expands FILE to canonical path from DIRECTORY.
If DIRECTORY is nil, expand from `user-emacs-directory'."
  (file-truename
   (expand-file-name (or file "./")
                     (or directory
                         (file-truename user-emacs-directory)))))

(defun rc-cache (&optional file)
  "Expand FILE from emacs cache directory.

Cache directories are system dependent:
    gnu/linux -> ~/.cache/emacs"
  (rc-expand file (cond (rc-posix-p "~/.cache/emacs/")
                        (t (locate-user-emacs-file "var/")))))

(defun rc-join (&rest strings)
  "Join STRINGS with char space as seperator."
  (string-join strings (char-to-string ?\s)))

(defun rc-insert (string &optional nobreak)
  "Insert STRING into current buffer.
If NOBREAK is non-nil, do not break line afterwards."
  (insert string)
  (unless nobreak
    (newline)))

(defun rc-load (file)
  "Load FILE within block 'return."
  (catch 'return
    (load file (file-truename file) t t)))

(defmacro rc-file (file &rest body)
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

(provide 'utils)
