(defconst rc-posix-p (pcase system-type
                       ('gnu t)
                       ('gnu/linux t)
                       ('gnu/kfreebsd t))
  "Non-nil means system is posix.")

(defconst rc-mswin-p (pcase system-type
                         ('windows-nt t)
                         ('cygwin t)
                         ('ms-dos t))
  "Non-nil means system is windows.")

(defun rc-expand (&optional file directory)
  "Expand FILE from DIRECTORY.
If DIRECTORY is nil, expand from `user-emacs-directory'."
  (file-truename
   (expand-file-name (or file "./")
                     (or directory
                         (file-truename user-emacs-directory)))))

(defun rc-cache (&optional file)
  "Expand FILE from emacs cache directory.

Cache directories are system dependent:
    posix   -> ~/.cache/emacs
    windows -> %APPDATA%/emacs
    others  -> `user-emacs-directory'/var"
  (rc-expand file
             (cond (rc-posix-p "~/.cache/emacs/")
                   (rc-mswin-p (rc-expand "Emacs/"
                                            (getenv "APPDATA")))
                   (t (rc-expand "var/")))))

(defun rc-dots (&optional file)
  "Expand FILE from the dotfiles dots directory."
  (rc-expand file (rc-expand "../dots/")))

(when (native-comp-available-p)
  (startup-redirect-eln-cache (rc-cache "eln/"))
  (setq native-comp-async-report-warnings-errors 'silent))

(when init-file-debug
  (setq debug-on-error t)
  (profiler-start 'cpu+mem))

(setq load-prefer-newer t)

(setq package-enable-at-startup nil)

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
