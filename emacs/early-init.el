(defun rc/expand-lisp-file (&optional file)
  "Expand FILE from `lisp' directory."
  (expand-file-name (or file "./") (locate-user-emacs-file "lisp/")))

(defun rc/expand-cache-file (&optional file)
  "Expand FILE from `cache' directory."
  (expand-file-name (or file "./") "~/.cache/emacs/"))

(defun rc/expand-cache-lisp-file (&optional file)
  "Expand FILE from `cache/lisp' directory."
  (expand-file-name (or file "./") (rc/expand-cache-file "lisp/")))

(defun rc/generate-autoloads ()
  "Generate and load autoloads of `lisp' directory."
  (interactive)
  (let ((file (rc/expand-cache-lisp-file "autoloads.el")))
    (make-directory (file-name-parent-directory file) t)
    (when (file-exists-p file)
      (delete-file file))
    (loaddefs-generate (rc/expand-lisp-file) file)
    (load-file file)))

(defun rc/apply-font ()
  "Apply font."
  (interactive)
  (let ((font "Iosevka-14" ))
    (set-face-attribute 'default nil :font font)
    (set-face-attribute 'fixed-pitch nil :font font)
    (set-face-attribute 'fixed-pitch-serif nil :font font)
    (set-face-attribute 'variable-pitch nil :font font)))

(defun rc/line-numbers-here ()
  "Maybe enable numbers in current major mode."
  (when (derived-mode-p 'prog-mode 'conf-mode)
    (display-line-numbers-mode 1)))

(add-to-list 'load-path (rc/expand-lisp-file))
(add-to-list 'load-path (rc/expand-cache-lisp-file))
(add-to-list 'custom-theme-load-path (rc/expand-lisp-file))

(when (native-comp-available-p)
  (startup-redirect-eln-cache (rc/expand-cache-file "eln-cache/")))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)

(load-theme 'gruber-darker t)

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'rc/apply-font)
  (if after-init-time
      (rc/apply-font)
    (add-hook 'emacs-startup-hook #'rc/apply-font)))
