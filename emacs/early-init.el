(defun rc/expand-lisp-file (&optional file)
  "Expand FILE from `lisp' directory."
  (expand-file-name (or file "./") (locate-user-emacs-file "lisp/")))

(defun rc/expand-cache-file (&optional file)
  "Expand FILE from `cache' directory."
  (expand-file-name (or file "./") "~/.cache/emacs/"))

(defun rc/expand-cache-lisp-file (&optional file)
  "Expand FILE from `cache/lisp' directory."
  (expand-file-name (or file "./") (rc/expand-cache-file "lisp/")))

(when (native-comp-available-p)
  (startup-redirect-eln-cache (rc/expand-cache-file "eln-cache/")))

(add-to-list 'load-path (rc/expand-lisp-file))
(add-to-list 'load-path (rc/expand-cache-lisp-file))
(add-to-list 'custom-theme-load-path (rc/expand-lisp-file))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)

(load-theme 'gruber-darker t)
