(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache "var/eln-cache/"))

(require 'server)
(unless (server-running-p)
  (server-start))

(defgroup rc nil
  "User dotfiles configuration."
  :version "26.1"
  :group 'local
  :prefix "rc-")

(setq custom-file (locate-user-emacs-file "var/custom.el"))
(load custom-file t t)

(defun rc-expand-file (file)
  "Expands FILE from config root."
  (expand-file-name file user-emacs-directory))

(require 'package)
(require 'use-package)
(setq package-user-dir (locate-user-emacs-file "var/elpa/")
      package-gnupghome-dir (expand-file-name "gnupg/" package-user-dir)
      use-package-always-ensure t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(use-package no-littering)

(add-to-list 'load-path (rc-expand-file "modules/"))

(dolist (file '("candy" "goodies" "minibuf" "prog" "extensions"))
  (load (locate-user-emacs-file (concat "modules/" file)) nil t))
