(require 'server)
(unless (or (server-running-p) (daemonp))
  (server-start))

(setq custom-file (locate-user-emacs-file "var/custom.el"))
(load custom-file :noerror)

(require 'package)
(require 'use-package)
(setq package-user-dir (locate-user-emacs-file "var/elpa/")
      package-gnupghome-dir (expand-file-name "gnupg/" package-user-dir)
      use-package-always-ensure t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(use-package no-littering
  :init
  (setq no-littering-etc-directory user-emacs-directory))

(dolist (file '("candy" "qol" "minibuf" "prog" "ext"))
  (load (no-littering-expand-etc-file-name file)))
